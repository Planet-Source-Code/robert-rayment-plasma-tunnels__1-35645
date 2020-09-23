;Tunnel.asm  by Robert Rayment  June 2002

;VB

;'PlasmaTunnel MCode Structure
;Public Type PStruc
;   PtrIndexArray As Long      ' byte array (1-256,1-256) values 0-255
;   PtrColArray As Long        ' byte array (1-256,1-256) values 0-255
;   PtrLineStore As Long       ' byte array (1-256) values 0-255
;   PtrCoordsX As Long         ' integer array (1-256) values 1-256
;   PtrCoordsY As Long         ' integer array (1-256) values 1-256
;   RandSeed As Long           ' (0-.99999) * 255
;   PlasmaGraininess As Long   ' 0 - 16
;   PlasmaScale As Long        ' 4 - 64
;   Twirl As Long              ' 0 No Twirl, 1 Do Twirl
;   TwirlCount As Long         ' 0-500
;   Speed As Long			   ' 1-4	

;End Type
;Public PlasmaStruc As PStruc
;' Fixed values
;' sizex As Long           ' 256
;' sizey As Long           ' 256
;' PaletteMaxIndex As Long ' 255
;Public TunnelMC() As Byte  ' Array to hold machine code
;Public Const Plasma0 = 0
;Public Const DoSmoothing1 = 1
;Public Const Transform2 = 2
;Public Const ScrollDown3 = 3 & TransformLUT4
;Public Const TransformLUT4 = 4

;'res = CallWindowProc(ptrMC, ptrStruc, OpCode&, 0&, 0&)
;                             8         12       16  20

%macro movab 2      ;name & num of parameters
  push dword %2     ;2nd param
  pop dword %1      ;1st param
%endmacro           ;use  movab %1,%2
;Allows eg  movab bmW,[ebx+4]

%define PtrIndexArray       [ebp-4]
%define PtrColArray         [ebp-8] 
%define PtrLineStore        [ebp-12]
%define PtrCoordsX          [ebp-16]
%define PtrCoordsY          [ebp-20]
%define RandSeed            [ebp-24]
%define Noise               [ebp-28]        ; PlasmaGraininess
%define Stepsize            [ebp-32]        ; PlasmaScale
%define StartStepsize       [ebp-36]        ; PlasmaScale

%define zdx         [ebp-40]
%define zdy         [ebp-44]
%define ixs         [ebp-48]
%define iys         [ebp-52]
%define kx          [ebp-56]
%define ky          [ebp-60]
%define n255        [ebp-64]

%define lo32        [ebp-68]
%define hi32        [ebp-72]

%define RndNoise    [ebp-76]
%Define Col         [ebp-80]
%Define zmul        [ebp-84]
%define index       [ebp-88]
%Define sdiv        [ebp-92]
%Define iyup        [ebp-96]
%Define ixup        [ebp-100]
%define smin        [ebp-104]
%define smax        [ebp-108]

%define ix          [ebp-112]
%define iy          [ebp-116]

%define zAngle      [ebp-120]   ; real
%define Radius      [ebp-124]   ; int
%define n10         [ebp-128]

%define Mask4       [ebp-132]
%define SUMMER      [ebp-136]

%define Twirl       [ebp-140]
%define TwirlCount  [ebp-144]
%define Speed		[ebp-148]

[bits 32]

    push ebp
    mov ebp,esp
    sub esp,148
    push edi
    push esi
    push ebx
    
    mov eax,255
    mov n255,eax

    mov eax,[ebp+12]        ; Opcode&
    
    cmp eax,0
    jne T1
    Call Plasma
    jmp GETOUT
T1:
    cmp eax,1
    jne T2
    Call Plasma
    Call DoSmoothing
    Call DoSmoothing
    jmp GETOUT
T2:
    cmp eax,2
    jne T3
    Call Transform
    jmp GETOUT
T3:
    cmp eax,3
    jne T4
    mov ebx,[ebp+8]
	movab Speed,		[ebx+40]
	mov ecx,Speed
TSpeed:

	push ecx
    Call ScrollDown
    pop ecx
	dec ecx
	jnz TSpeed

	Call TransformLUT
    jmp GETOUT
T4:
    cmp eax,4
    jne GETOUT
    Call TransformLUT

GETOUT:

    pop ebx
    pop esi
    pop edi
    mov esp,ebp
    pop ebp
    ret 16
;###################################################

Plasma:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Get structure 
    mov ebx,[ebp+8]

    movab PtrIndexArray,    [ebx]
    movab PtrColArray,      [ebx+4]
    movab PtrLineStore,     [ebx+8]
    movab PtrCoordsX,       [ebx+12]
    movab PtrCoordsY,       [ebx+16]
    movab RandSeed,         [ebx+20]
    movab Noise,            [ebx+24]    ; PlasmaGraininess
    movab Stepsize,         [ebx+28]    ; PlasmaScale

    mov eax,Stepsize
    mov StartStepsize,eax

    xor eax,eax
    mov RndNoise,eax

DO:
    mov ecx,1
FIY:
    mov iy,ecx
    push ecx

    mov ecx,1
FIX:
    mov ix,ecx
    push ecx
    
    ;-----------
    mov eax,Noise
    cmp eax,0
    je NoiseDone        ; RndNoise = 0

    Call Random         ; eax = Rnd * 255 = 0-255

    shl eax,1
    mov ebx,Noise
    mul ebx             ; (Rnd*255)*2*Noise
    mov RndNoise,eax
    
    fild dword RndNoise
    fild dword n255      ; 255,RndNoise
    fdivp st1            ; RndNoise/255
    fistp dword RndNoise ; Rnd*2*Noise
    
    mov eax,RndNoise
    mov ebx,Noise
    sub eax,ebx          ; Rnd*2*Noise - Noise
    mov ebx,Stepsize
    mul ebx              ; (Rnd*2*Noise)*Stepsize
    mov RndNoise,eax

NoiseDone:
    mov eax,iy
    add eax,Stepsize
    cmp eax,256          ; iy+Stepsize <= sizey
    jb Tix
        ; Col=IndexArray(ix,1)+RndNoise     
        mov eax,ix
        mov kx,eax
        mov eax,1
        mov ky,eax
        Call near IndexArrayAddr    ; edi->addr
        mov BL,[edi]
        movsx eax,BL
        add eax,RndNoise
        mov Col,eax
        jmp DoBox
Tix:
    mov eax,ix
    add eax,Stepsize
    cmp eax,256
    jb Tstart
        ; Col=IndexArray(1,iy)+RndNoise     
        mov eax,1
        mov kx,eax
        mov eax,iy
        mov ky,eax
        Call near IndexArrayAddr    ; edi->addr
        mov BL,[edi]
        movsx eax,BL
        add eax,RndNoise
        mov Col,eax
        jmp DoBox
Tstart:
    mov eax,Stepsize
    cmp eax,StartStepsize
    jne FourPoint
    ; Col = (Rnd*255)*(2*255)/255-255

    Call Random         ; eax = Rnd * 255 = 0-255

    shl eax,1           ; 2*(Rnd*255)
    mov ebx,255
    mul ebx             ; (Rnd*255)*2*255
    mov index,eax
    
    fild dword index
    fild dword n255     ; 255,index
    fdivp st1           ; index/255
    fistp dword index   ; Rnd*2*255
    
    mov eax,index
    mov ebx,255
    sub eax,ebx         ; (Rnd*2*255)- 255
    mov Col,eax
    jmp DoBox

FourPoint:

    ; Col=IndexArray(ix,iy)
        mov eax,ix
        mov kx,eax
        mov eax,iy
        mov ky,eax
        Call near IndexArrayAddr    ; edi->addr (ebx & edx used)
        mov BL,[edi]
        movsx eax,BL
        mov Col,eax
        
        mov edx,Stepsize
        shl edx,1       ; edx=2*Stepsize

    ; Col = Col + IndexArray(ix+Stepsize,iy)
        add edi,edx 
        mov BL,[edi]
        movsx eax,bx
        add Col,eax

    ; Col = Col + IndexArray(ix+Stepsize,iy+Stepsize)
        push edx
        mov eax,edx
        mov ebx,255
        mul ebx         ; eax = 2*Stepsize*255  (edx changed)
        add edi,eax
        mov BL,[edi]
        movsx eax,BL
        add Col,eax
    
    ; Col = Col + IndexArray(ix,iy+Stepsize)
        pop edx
        sub edi,edx
        mov BL,[edi]
        movsx eax,BL
        add Col,eax

    ; Col = Col\4 + RndNoise
        mov eax,Col
        shr eax,2
        
        add eax,RndNoise
        mov Col,eax
DoBox:
    mov eax,Col
    cmp eax,0
    jg tgr255
    mov eax,1
    jmp fillCol
tgr255:    
    cmp eax,255
    jle fillCol
    mov eax,255
fillCol:    
    mov Col,eax
    
    mov eax,Stepsize
    cmp eax,1
    jbe SinglePt
        ; For ky = iy to iy + Stepsize
        ;   If ky <= 256 Then
        ;       For kx = ix to ix + Stepsize
        ;           If kx <= 256 then
        ;               IndexArray(kx,ky) = Col
        ;           End if
        ;       Next kx
        ;   End If
        ; Next ky

        mov eax,iy
        add eax,Stepsize
        mov iyup,eax

        mov eax,ix
        add eax,Stepsize
        mov ixup,eax

        mov ecx,iy
    Fy:
        mov ky,ecx
        push ecx

        cmp ecx,256         ; If ky <= 256 Then
        ja Ny
            mov ecx,ix
        Fx:
            cmp ecx,256     ; If kx <= 256 then
            ja Ny

            mov kx,ecx
            Call near IndexArrayAddr    ; edi->addr
            mov eax,Col
            mov [edi],AL
            inc ecx
            cmp ecx,ixup    ; ecx-ixup
        jbe Fx
    Ny:
        pop ecx
        inc ecx
        cmp ecx,iyup        ; ecx-iyup
    jbe Fy

    jmp NIX

SinglePt:
    ; IndexArray(ix,iy) = Col
    mov eax,ix
    mov kx,eax
    mov eax,iy
    mov ky,eax
    Call near IndexArrayAddr    ; edi->addr
    mov eax,Col
    mov [edi],AL  ;ax

    ;-----------
NIX:
    pop ecx
    add ecx,Stepsize
    cmp ecx,256             ; ix <= sizex
    jbe near FIX
NIY:
    pop ecx
    add ecx,Stepsize
    cmp ecx,256             ; iy <= sizey
    jbe near FIY

    mov eax,Stepsize
    shr eax,1               ; Stepsize=Stepsize\2
    mov Stepsize,eax
    cmp eax,1
    ja near DO
;-----------------

    ; Get Col max/min

    mov eax,10000
    mov smin,eax
    neg eax
    mov smax,eax

    mov ecx,256             ; iy=sizey
SAy:
    push ecx
    mov ky,ecx
    
    mov ecx,256             ; ix=sizex
SAx:
    mov kx,ecx
    Call near IndexArrayAddr    ; edi->addr
    mov BL,[edi]
    movsx eax,BL
    cmp eax,smin
    jge Tsmax
    mov smin,eax
Tsmax:
    cmp eax,smax
    jle Nax 
    mov smax,eax    
Nax:
    dec ecx
    jnz SAx
    
    pop ecx
    dec ecx
    jnz SAy

    ; Calc Stretch color factor zmul

    mov eax,smax
    sub eax,smin
    cmp eax,0
    jg Getzmul
    mov eax,1
Getzmul:
    mov sdiv,eax

    fild dword n255  
    fild dword sdiv         ; sdiv, 255
    fdivp st1               ; 255/sdiv
    fstp dword zmul         ; Real
    ;------------

    ; Fill IndexArray(ix.iy) with new index
    mov ecx,256             ; sizey=256
CAy:
    push ecx
    mov ky,ecx
    
    mov ecx,256             ; sizex=256
CAx:
    mov kx,ecx
    Call near IndexArrayAddr    ; edi->addr
    mov BL,[edi]
    movsx eax,BL
    mov ebx,smin
    sub eax,ebx
    mov index,eax           ; (Col-smin)

    fild dword index
    fld dword zmul
    fmulp st1
    fistp dword index       ; Index = (Col-smin)*zmul (0 - 511)
    ;............
    mov eax,index

    ; Check Index in range
    cmp eax,0
    jge L1
    mov eax,0
L1:
    cmp eax,255
    jle L2
    mov eax,255
L2:
    mov index,eax

    mov [edi],AL           ; Put in new Index value
;...................................    

    dec ecx
    jnz CAx
    
    pop ecx
    dec ecx
    jnz CAy

RET
;---------------------------------------------------
; %define zAngle        [ebp-120]   ; real
; %define Radius        [ebp-124]   ; int
; %define n10           [ebp-128]

Transform:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Get structure 
    mov ebx,[ebp+8]

    movab PtrIndexArray,    [ebx]
    movab PtrColArray,      [ebx+4]
    movab PtrLineStore,     [ebx+8]
    movab PtrCoordsX,       [ebx+12]
    movab PtrCoordsY,       [ebx+16]
    movab RandSeed,         [ebx+20]
    movab Noise,            [ebx+24]    ; PlasmaGraininess
    movab Stepsize,         [ebx+28]    ; PlasmaScale

    mov eax,10
    mov n10,eax

    ; Make sure CoordsX & CoordsY have at least 1 in them
    mov ecx,256
CooY:
    push ecx
    mov ky,ecx

    mov ecx,256
CooX:
    mov kx,ecx

    Call CoordsXAddr
    mov eax,1
    mov [edi],ax
    Call CoordsYAddr
    mov eax,1
    mov [edi],ax

    dec ecx
    jnz CooX
    pop ecx
    dec ecx
    jnz CooY
    ;-----------

    mov ecx,256
Coo2Y:
    push ecx
    mov ky,ecx

    mov ecx,256
Coo2X:
    mov kx,ecx
    ;--------------
    mov eax,kx
    sub eax,128
    mov zdx,eax     ;zdx = kx-128 int
    mov eax,ky
    sub eax,128
    mov zdy,eax     ;zdy = ky-128 int

    fild dword zdx
    fild dword zdx
    fmulp st1
    fild dword zdy
    fild dword zdy
    fmulp st1
    faddp st1
    fsqrt           ; Sqr(zdx^2+zdy^2)
    fistp dword Radius

    mov eax,256
    sub eax,Radius
    mov iys,eax     ; iys = 256 - Radius
    
    cmp eax,1
    jl near NexCooX
    cmp eax,256
    jg near NexCooX

    fild dword zdx      ; Want zAngle = -zATan2(zdx,zdy)
    fild dword zdy      ; zdy,zdx  ;nb reversed
    fpatan              ; ATan(st1/st0) = ATan(zdx/zdy)
    fchs
    fstp dword zAngle   ; zAngle = -zATan2(zdx,zdy)
    
    mov eax,zdx
    cmp eax,0
    jl noadd2pi
    ; else add 2 pi
    fldpi
    fldpi
    faddp st1
    fld dword zAngle
    faddp st1
    fstp dword zAngle   ; = zAngle + 2*pi
noadd2pi:
    ; get ixs
    fild dword n255
    fld1
    faddp st1           ; 256
    fld dword zAngle
    fmulp st1           ; 256*zAngle
    fldpi
    fldpi
    faddp st1           ; 2*pi,256*zAngle
    fdivp st1           ; st1/st0 = 256*zAngle/2*pi

    fld1
    fld1
    faddp st1           ; 2
    fild dword n10      ; 10,2
    fdivp st1           ; st1/st0 = 2/10 = .2
    faddp st1           ; (256*zAngle/2*pi)+0.2
    fistp dword ixs     ; ixs = (256*zAngle/2*pi)+0.2

    mov eax,ixs
    cmp eax,1
    jl NexCooX
    cmp eax,256
    jg NexCooX

        Call ColArrayAddr   ; edi->ColArray(kx,ky)

        mov esi,PtrIndexArray
        mov eax,iys
        dec eax
        mov ebx,256
        mul ebx
        mov ebx,ixs
        dec ebx
        add eax,ebx
        add esi,eax     ; esi->IndexArray(ixs,iys)
    
        movsb           ; ColArray(kx,ky)=IndexArray(ixs,iys) 

        Call CoordsXAddr
        mov eax,ixs
        mov [edi],ax    ; int
        Call CoordsYAddr
        mov eax,iys
        mov [edi],ax    ; int

    ;--------------
NexCooX:    
    dec ecx
    jnz near Coo2X
    
    pop ecx
    dec ecx
    jnz near Coo2Y

RET
;---------------------------------------------------
ScrollDown: ; & TransformLUT


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Get structure 
    mov ebx,[ebp+8]

    movab PtrIndexArray,    [ebx]
    movab PtrColArray,      [ebx+4]
    movab PtrLineStore,     [ebx+8]
    movab PtrCoordsX,       [ebx+12]
    movab PtrCoordsY,       [ebx+16]
    ;movab RandSeed,         [ebx+20]
    ;movab Noise,            [ebx+24]    ; PlasmaGraininess
    ;movab Stepsize,         [ebx+28]    ; PlasmaScale
	movab Twirl,			[ebx+32]
	movab TwirlCount,		[ebx+36]


    ; Save bottom line
    mov edi,PtrLineStore    ; edi -> LineStore(1)
    mov esi,PtrIndexArray   ; esi -> IndexArray(1,1)
    mov ebx,8               ; esi,edi step

    mov ecx,4               ; 64*4=256
SaveBLine:
    movq mm0,[esi]
    add esi,ebx
    movq mm1,[esi]
    add esi,ebx
    movq mm2,[esi]
    add esi,ebx
    movq mm3,[esi]
    add esi,ebx
    movq mm4,[esi]
    add esi,ebx
    movq mm5,[esi]
    add esi,ebx
    movq mm6,[esi]
    add esi,ebx
    movq mm7,[esi]
    add esi,ebx

    movq [edi],mm0
    add edi,ebx
    movq [edi],mm1
    add edi,ebx
    movq [edi],mm2
    add edi,ebx
    movq [edi],mm3
    add edi,ebx
    movq [edi],mm4
    add edi,ebx
    movq [edi],mm5
    add edi,ebx
    movq [edi],mm6
    add edi,ebx
    movq [edi],mm7
    add edi,ebx

    dec ecx
    jnz SaveBLine


    ; Move lines 2-256 to 1-255
    mov esi,PtrIndexArray   ; esi -> IndexArray(1,1)
    mov eax,256
    add esi,eax             ; esi -> IndexArray(1,2)
    mov edi,PtrIndexArray   ; edi -> IndexArray(1,1)

    mov ebx,8
    mov ecx,1020            ; 255*256/64
ScrD:
    movq mm0,[esi]
    add esi,ebx
    movq mm1,[esi]
    add esi,ebx
    movq mm2,[esi]
    add esi,ebx
    movq mm3,[esi]
    add esi,ebx
    movq mm4,[esi]
    add esi,ebx
    movq mm5,[esi]
    add esi,ebx
    movq mm6,[esi]
    add esi,ebx
    movq mm7,[esi]
    add esi,ebx


    movq [edi],mm0
    add edi,ebx
    movq [edi],mm1
    add edi,ebx
    movq [edi],mm2
    add edi,ebx
    movq [edi],mm3
    add edi,ebx
    movq [edi],mm4
    add edi,ebx
    movq [edi],mm5
    add edi,ebx
    movq [edi],mm6
    add edi,ebx
    movq [edi],mm7
    add edi,ebx
    
    dec ecx
    jnz ScrD


    ; Put back top line from LineStore
    mov esi,PtrLineStore    ; esi -> LineStore(1)
    mov edi,PtrIndexArray   ; edi -> IndexArray(1,1)
    mov eax,256
    dec eax
    mov ebx,256
    mul ebx
    mov ebx,1
    dec ebx
    add eax,ebx
    add edi,eax     ; edi -> IndexArray(1,256)

    mov ebx,8               ; esi,edi step
    mov ecx,4               ; 64*4=256
PutTLine:
    movq mm0,[esi]
    add esi,ebx
    movq mm1,[esi]
    add esi,ebx
    movq mm2,[esi]
    add esi,ebx
    movq mm3,[esi]
    add esi,ebx
    movq mm4,[esi]
    add esi,ebx
    movq mm5,[esi]
    add esi,ebx
    movq mm6,[esi]
    add esi,ebx
    movq mm7,[esi]
    add esi,ebx

    movq [edi],mm0
    add edi,ebx
    movq [edi],mm1
    add edi,ebx
    movq [edi],mm2
    add edi,ebx
    movq [edi],mm3
    add edi,ebx
    movq [edi],mm4
    add edi,ebx
    movq [edi],mm5
    add edi,ebx
    movq [edi],mm6
    add edi,ebx
    movq [edi],mm7
    add edi,ebx

    dec ecx
    jnz PutTLine

    emms            ;Clear FP/MMX stack

RET  
;---------------------------------------------------
TransformLUT:

    mov ecx,256

    mov edi,PtrColArray     ;[3]
    mov eax,256
    dec eax
    mov ebx,256
    mul ebx
    mov ebx,ecx;kx
    dec ebx
    add eax,ebx
    add edi,eax
    push edi        ; [esp+12]
    
    mov edi,PtrCoordsY      ;[2]
    mov eax,256
    dec eax
    mov ebx,256
    mul ebx
    mov ebx,ecx;kx
    dec ebx
    add eax,ebx
    shl eax,1       ; *2
    add edi,eax
    push edi        ; [esp+8]

    mov edi,PtrCoordsX      ;[1]
    mov eax,256
    dec eax
    mov ebx,256
    mul ebx
    mov ebx,ecx;kx
    dec ebx
    add eax,ebx
    shl eax,1       ; *2
    add edi,eax
    push edi        ; [esp+4]

LUTY:
    push ecx
    mov ky,ecx
    
    mov ecx,256

LUTX:
    ;--------------
    mov edi,[esp+4]
        xor eax,eax
        mov ax,[edi]
        mov ixs,eax         ; ixs=CoordsX(kx,ky)
    dec edi
    dec edi
    mov [esp+4],edi

    mov edi,[esp+8]
        xor eax,eax
        mov ax,[edi]
        mov iys,eax         ; iys=CoordsY(kx,ky)
    dec edi
    dec edi
    mov [esp+8],edi

        mov esi,PtrIndexArray
        mov eax,iys
        dec eax
        mov ebx,256
        mul ebx
        mov ebx,ixs
        dec ebx
        add eax,ebx
        add esi,eax     ; esi->IndexArray(ixs,iys)
        
    mov edi,[esp+12]
        mov AL,[esi]    ; ColArray(kx,ky)=IndexArray(ixs,iys)
        mov [edi],AL
    dec edi
    mov [esp+12],edi

    ;--------------
NexLUTX:
    
    dec ecx
    jnz near LUTX

    pop ecx
    dec ecx
    jnz near LUTY

    pop edi
    pop edi
    pop edi

;Twirl
    mov eax,Twirl
    cmp eax,0
    je TwirlDone
    
    mov eax,TwirlCount
    cmp eax,500
    jg TwirlDone
    
    mov ecx,256
TWY:
    push ecx
    mov ky,ecx
    
    mov ecx,256
TWX:
    mov kx,ecx
    
    Call IndexArrayAddr     ; edi-> IndexArray(kx,ky)
    push edi
    pop esi                 ; esi-> IndexArray(kx,ky)
    
    mov eax,kx              ; Swap kx,ky
	
	push dword ky
	pop dword kx
	mov ky,eax

	;mov ebx,ky
    ;mov ky,eax
	;mov kx,ebx
    
    Call IndexArrayAddr     ; edi-> IndexArray(ky,kx)
    mov AL,[esi]
    mov [edi],AL

    mov eax,kx              ; Swap back kx,ky

	push dword ky
	pop dword kx
	mov ky,eax

	;mov ebx,ky
    ;mov ky,eax
	;mov kx,ebx

    dec ecx
    jnz TWX
    pop ecx
    dec ecx
    jnz TWY
    
TwirlDone:

RET
;---------------------------------------------------

;================================================
DoSmoothing:    ; 8-point smoothing

;%define Mask4       [ebp-132]
;%define SUMMER      [ebp-136]

    mov eax,0F8F8F8F8h
    mov Mask4,eax
    
    mov ecx,256;sizey

ForY8:
    mov iy,ecx
    mov ky,ecx
    push ecx

    mov ecx,256;sizex
ForX8:
    mov ix,ecx
    mov kx,ecx
    
    Call IndexArrayAddr  ; edi->center x,y
    push edi
    pop esi             ; esi->center x,y
    
    mov eax,ix
    dec eax             ; ix-1
    cmp eax,0
    jg Left_ixm1
    mov eax,256;sizex
Left_ixm1:
    mov kx,eax          ; kx set at ix-1
    
    mov eax,iy
    mov ky,eax          ; ix-1,iy
    Call IndexArrayAddr
    xor eax,eax
    mov AL,[edi]
    ;and eax,Mask4
    shr eax,3
    mov SUMMER,eax
    
    mov eax,iy
    dec eax             ; iy-1
    cmp eax,0
    jg Left_iym1
    mov eax,256;sizey
Left_iym1:
    mov ky,eax
    Call IndexArrayAddr
    xor eax,eax
    mov AL,[edi]
    ;and eax,Mask4
    shr eax,3
    add SUMMER,eax
    
    mov eax,iy
    inc eax             ; iy+1
    cmp eax,256;sizey
    jle Left_iyp1
    mov eax,1
Left_iyp1:
    mov ky,eax
    Call IndexArrayAddr
    xor eax,eax
    mov AL,[edi]
    ;and eax,Mask4
    shr eax,3
    add SUMMER,eax
    ;.. left col done ..............

    mov eax,ix
    mov kx,eax          ; kx reset

    mov eax,iy
    dec eax             ; iy-1
    cmp eax,0
    jg Cen_iym1
    mov eax,256;sizey
Cen_iym1:
    mov ky,eax
    Call IndexArrayAddr
    xor eax,eax
    mov AL,[edi]
    ;and eax,Mask4
    shr eax,3
    add SUMMER,eax
    
    mov eax,iy
    inc eax             ; iy+1
    cmp eax,256;sizey
    jle Cen_iyp1
    mov eax,1
Cen_iyp1:
    mov ky,eax
    Call IndexArrayAddr
    xor eax,eax
    mov AL,[edi]
    ;and eax,Mask4
    shr eax,3
    add SUMMER,eax
    ;.. center col done

    mov eax,ix
    inc eax             ; ix+1
    cmp eax,256;sizex
    jle Right_ixm1
    mov eax,1
Right_ixm1:
    mov kx,eax          ; kx set at ix+1
    
    mov eax,iy
    mov ky,eax          ; ix+1,iy
    Call IndexArrayAddr
    xor eax,eax
    mov AL,[edi]
    ;and eax,Mask4
    shr eax,3
    add SUMMER,eax
    
    mov eax,iy
    dec eax             ; iy-1
    cmp eax,0
    jg Right_iym1
    mov eax,256;sizey
Right_iym1:
    mov ky,eax
    Call IndexArrayAddr
    xor eax,eax
    mov AL,[edi]
    ;and eax,Mask4
    shr eax,3
    add SUMMER,eax
    
    mov eax,iy
    inc eax             ; iy+1
    cmp eax,256;sizey
    jle Right_iyp1
    mov eax,1
Right_iyp1:
    mov ky,eax
    Call IndexArrayAddr
    xor eax,eax
    mov AL,[edi]
    ;and eax,Mask4
    shr eax,3           ; /8
    add SUMMER,eax
    ;.. Right col done ..............

    ; Set color at IndexArray(ix,iy)

    mov eax,SUMMER
    mov [esi],AL
    
;-----
NexX8;
    dec ecx
    jnz near ForX8
NexY8:
    pop ecx
    dec ecx
    jnz near ForY8

RET
;================================================


Random:      ; Out: aL & RandSeed = rand(0-255)
    mov eax,011813h     ; 71699 prime 
    imul DWORD RandSeed
    add eax, 0AB209h    ; 700937 prime
    rcr eax,1           ; leaving out gives vertical lines plus
                        ; faint horizontal ones, tartan

    ;----------------------------------------
    jc ok              ; these 2 have little effect
    rol eax,1          ;
ok:                     ;
    ;----------------------------------------
    
    ;----------------------------------------
    ;dec eax            ; these produce vert lines
    ;inc eax            ; & with fsin marble arches
    ;----------------------------------------

    mov RandSeed,eax    ; save seed
    and eax,255

RET
;=================================================
;===========================
IndexArrayAddr:  ; In: NB BYTE ARRAY, kx,ky,256x256  Out: edi->addr
    ;B = edi + (ky-1) * 256 + (kx-1)
    
    mov edi,PtrIndexArray

    jmp CommonByteAddr

    ;mov eax,ky
    ;dec eax
    ;mov ebx,256
    ;mul ebx
    ;mov ebx,kx
    ;dec ebx
    ;add eax,ebx
    ;add edi,eax
RET
;===========================
ColArrayAddr:  ; In: NB BYTE ARRAY, kx,ky,256x256  Out: edi->addr
    ;B = edi + (ky-1) * 256 + (kx-1)
    
    mov edi,PtrColArray

CommonByteAddr:

    mov eax,ky
    dec eax
    mov ebx,256
    mul ebx
    mov ebx,kx
    dec ebx
    add eax,ebx
    add edi,eax
RET
;===========================
CoordsXAddr:  ; Addr = edi + 2*[(ky-1) * 256 + (kx-1)]
;PtrCoordsX  Integer Array

    mov edi,PtrCoordsX
    jmp CommonXY

RET
;===========================
CoordsYAddr:  ; Addr = edi + 2*[(ky-1) * 256 + (kx-1)]
;PtrCoordsY  Integer Array

    mov edi,PtrCoordsY
CommonXY:

    mov eax,ky
    dec eax
    mov ebx,256
    mul ebx
    mov ebx,kx
    dec ebx
    add eax,ebx
    shl eax,1       ; *2
    add edi,eax
RET
;===========================
