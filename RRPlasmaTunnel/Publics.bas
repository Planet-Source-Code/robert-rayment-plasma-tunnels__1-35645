Attribute VB_Name = "Module1"
'Module1: Publics.bas  by Robert Rayment June 2002

' Mainly to hold Publics

Option Base 1  ' Arrays base 1
DefLng A-W     ' All variable Long
DefSng X-Z     ' unless singles
               ' unless otherwise defined
               
' As suggested by John Galanopoulos
' Used in Sub Tunnel
Public Const QS_KEY = &H1
Public Const QS_PAINT = &H20
Public Const QS_MOUSEBUTTON = &H4
Public Declare Function GetQueueStatus Lib "user32" _
(ByVal qsFlags As Long) As Long

'--------------------------------------------------------------------------
' Shaping APIs

Public Declare Function CreateEllipticRgn Lib "gdi32" _
(ByVal X1 As Long, ByVal Y1 As Long, ByVal X2 As Long, ByVal Y2 As Long) As Long

Public Declare Function SetWindowRgn Lib "user32" _
(ByVal hwnd As Long, ByVal hRgn As Long, ByVal bRedraw As Boolean) As Long

Public Declare Function DeleteObject Lib "gdi32" _
(ByVal hObject As Long) As Long

'------------------------------------------------------------------------------

' Structures for StretchDIBits
Public Type BITMAPINFOHEADER ' 40 bytes
   biSize As Long
   biwidth As Long
   biheight As Long
   biPlanes As Integer
   biBitCount As Integer
   biCompression As Long
   biSizeImage As Long
   biXPelsPerMeter As Long
   biYPelsPerMeter As Long
   biClrUsed As Long
   biClrImportant As Long
End Type

Public Type RGBQUAD
        rgbBlue As Byte
        rgbGreen As Byte
        rgbRed As Byte
        rgbReserved As Byte
End Type

Public Type BITMAPINFO
   bmiH As BITMAPINFOHEADER
   Colors(0 To 255) As RGBQUAD
End Type
Public bm As BITMAPINFO

' For transferring drawing in an integer array to Form or PicBox
Public Declare Function StretchDIBits Lib "gdi32" (ByVal hdc As Long, _
ByVal X As Long, ByVal Y As Long, _
ByVal DesW As Long, ByVal DesH As Long, _
ByVal SrcXOffset As Long, ByVal SrcYOffset As Long, _
ByVal PICWW As Long, ByVal PICHH As Long, _
lpBits As Any, lpBitsInfo As BITMAPINFO, _
ByVal wUsage As Long, ByVal dwRop As Long) As Long

Public Const DIB_PAL_COLORS = 1 '  color table in palette indices
Public Const DIB_RGB_COLORS = 0 '  color table in RGBs

'------------------------------------------------------------------------------
'Copy one array to another of same number of bytes

Public Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" _
(Destination As Any, Source As Any, ByVal Length As Long)

'------------------------------------------------------------------------------

' For calling machine code
Public Declare Function CallWindowProc Lib "user32" Alias "CallWindowProcA" _
(ByVal lpMCode As Long, _
ByVal Long1 As Long, ByVal Long2 As Long, _
ByVal Long3 As Long, ByVal Long4 As Long) As Long
'-----------------------------------------------------------------
Public ptrMC, ptrStruc        ' Ptrs to Machine Code & Structure

'PlasmaTunnel MCode Structure
Public Type PStruc
   PtrIndexArray As Long      ' byte array (1-256,1-256) values 0-255
   PtrColArray As Long        ' byte array (1-256,1-256) values 0-255
   PtrLineStore As Long       ' byte array (1-256) values 0-255
   PtrCoordsX As Long         ' integer array (1-256) values 1-256
   PtrCoordsY As Long         ' integer array (1-256) values 1-256
   RandSeed As Long           ' (0-.99999) * 255
   PlasmaGraininess As Long   ' 0 - 16
   PlasmaScale As Long        ' 4 - 64
   Twirl As Long              ' 0 No Twirl, 1 Do Twirl
   TwirlCount As Long         ' 0-500
   Speed As Long
End Type
Public PlasmaStruc As PStruc
' Fixed values
' sizex As Long           ' 256
' sizey As Long           ' 256
' PaletteMaxIndex As Long ' 255
Public TunnelMC() As Byte  ' Array to hold machine code
'res = CallWindowProc(ptrMC, ptrStruc, OpCode&, 0&, 0&)
Public TunnelDone As Boolean
Public Const Plasma0 = 0
Public Const DoSmoothing1 = 1
Public Const Transform2 = 2
Public Const ScrollDown3 = 3
Public Const TransformLUT4 = 4

' RGB components
Public red As Byte, green As Byte, blue As Byte

'Public PaletteMaxIndex           ' Max Palette size = 255
'Public sizex, sizey
Public IndexArray() As Byte   ' To hold palette indexes 0-255
Public ColArray() As Byte     ' To hold colors for displaying 0-255
Public LineStore() As Byte    ' LineStore(1-256)
Public CoordsX() As Integer
Public CoordsY() As Integer

Public PathSpec$, PalDir$        ' App & Pal paths

Public FormWidth     ' frmTunnel width
Public FormHeight    ' frmTunnel height

Public PlasmaGraininess 'StartNoise   ' graininess
Public PlasmaScale  'StartStepsize   ' scale
Public Smooth  ' 0 unchecked, 1 checked
Public Speed   ' 1-4
Public Twirl  ' 0 unchecked, 1 checked
Public TwirlCount

Public GOGO As Boolean

Public Const pi# = 3.1415926535898
Public Const d2r# = pi# / 180
Public Const r2d# = 1 / d2r#


Public Sub Loadmcode(InFile$, MCCode() As Byte)
'Load machine code into InCode() byte array
On Error GoTo InFileErr
If Dir$(InFile$) = "" Then
   MsgBox (InFile$ & " missing")
   DoEvents
   Unload frmPlasTunnel
   End
End If
Open InFile$ For Binary As #1
MCSize& = LOF(1)
If MCSize& = 0 Then
InFileErr:
   MsgBox (InFile$ & " missing")
   DoEvents
   Unload frmPlasTunnel
   End
End If
ReDim MCCode(MCSize&)
Get #1, , MCCode
Close #1
On Error GoTo 0
End Sub

Public Sub FillBMPStruc(ByVal bwidth, ByVal bheight)

  With bm.bmiH
   .biSize = 40
   .biwidth = bwidth
   .biheight = bheight
   .biPlanes = 1
   .biBitCount = 8            ' Sets up 8-bit colors
   .biCompression = 0
   .biSizeImage = Abs(bwidth) * Abs(biheight)
   .biXPelsPerMeter = 0
   .biYPelsPerMeter = 0
   .biClrUsed = 0
   .biClrImportant = 0
 End With

End Sub


'##### PALETTE INPUT #####################################

Public Sub ReadPAL(Palspec$)
' Read JASC-PAL palette file
' Any error shown by PalSpec$ = ""
' Else RGB into Colors(i) Long

'Dim red As Byte, green As Byte, blue As Byte
On Error GoTo palerror

'ReDim Colors(0 To 255)

Open Palspec$ For Input As #1
Line Input #1, A$
p = InStr(1, A$, "JASC")
If p = 0 Then Palspec$ = "": Exit Sub
   
   'JASC-PAL
   '0100
   '256
   Line Input #1, A$
   Line Input #1, A$

   For n = 0 To 255
      If EOF(1) Then Exit For
      Line Input #1, A$
      ParsePAL A$ ', red, green, blue
      bm.Colors(n).rgbBlue = blue
      bm.Colors(n).rgbGreen = green
      bm.Colors(n).rgbRed = red
      bm.Colors(n).rgbReserved = 0
      'Colors(N) = RGB(red, green, blue)
      'Colors(n) = RGB(blue, green, red) ' Reverse R & B
   Next n
   Close #1
   
Exit Sub
'===========
palerror:
Palspec$ = ""
   
   MsgBox "Palette file error or not there"
   Err.Clear

End Sub

Public Sub ParsePAL(ain$) ', red As Byte, green As Byte, blue As Byte)
'Input string ain$, with 3 numbers(R G B) with
'space separators and then any text
On Error GoTo parserror
ain$ = LTrim(ain$)
lena = Len(ain$)
R$ = ""
G$ = ""
B$ = ""
num = 0 'R
nt = 0
For i = 1 To lena
   C$ = Mid$(ain$, i, 1)
   
   If C$ <> " " Then
      If nt = 0 Then num = num + 1
      nt = 1
      If num = 4 Then Exit For
      If Asc(C$) < 48 Or Asc(C$) > 57 Then Exit For
      If num = 1 Then R$ = R$ + C$
      If num = 2 Then G$ = G$ + C$
      If num = 3 Then B$ = B$ + C$
   Else
      nt = 0
   End If
Next i
red = Val(R$): green = Val(G$): blue = Val(B$)
Exit Sub
'=====
parserror:
Resume Next
End Sub

Public Function zATan2(ByVal zy As Single, ByVal zx As Single)
' Find angle Atan from -pi#/2 to +pi#/2
' Public pi#
If zx <> 0 Then
   zATan2 = Atn(zy / zx)
   If (zx < 0) Then
      If (zy < 0) Then zATan2 = zATan2 - pi# Else zATan2 = zATan2 + pi#
   End If
Else  ' zx=0
   If Abs(zy) > Abs(zx) Then   'Must be an overflow
      If zy > 0 Then zATan2 = pi# / 2 Else zATan2 = -pi# / 2
   Else
      zATan2 = 0   'Must be an underflow
   End If
End If
End Function

