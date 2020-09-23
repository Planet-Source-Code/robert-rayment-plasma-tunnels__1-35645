VERSION 5.00
Begin VB.Form frmTunnel 
   Appearance      =   0  'Flat
   BackColor       =   &H00000000&
   BorderStyle     =   0  'None
   Caption         =   "Dbl_Click here to Maximize"
   ClientHeight    =   6150
   ClientLeft      =   105
   ClientTop       =   105
   ClientWidth     =   9825
   ControlBox      =   0   'False
   ForeColor       =   &H8000000E&
   LinkTopic       =   "Form2"
   MinButton       =   0   'False
   ScaleHeight     =   410
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   655
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
End
Attribute VB_Name = "frmTunnel"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
' frmTunnel.frm by Robert Rayment  June 2002

Option Base 1  ' Arrays base 1
DefLng A-W     ' All variable Long
DefSng X-Z     ' unless singles


Private Sub Form_DblClick()
frmTunnel.WindowState = vbMaximized
End Sub

Private Sub Form_KeyPress(KeyAscii As Integer)

   TunnelDone = True
   Cls
   DoEvents
   Unload frmTunnel

End Sub

Private Sub Form_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)

If Button = 1 And TunnelDone = True Then
   
   Tunnel

ElseIf Button = 2 Then
   
   TunnelDone = True
   Cls
   DoEvents
   Unload frmTunnel

End If

End Sub

Private Sub Form_Load()

'Width = 800 * Screen.TwipsPerPixelX
'Height = 600 * Screen.TwipsPerPixelY

TunnelDone = True
Cls
Show
DoEvents
   
FormWidth = frmTunnel.Width \ Screen.TwipsPerPixelX
FormHeight = frmTunnel.Height \ Screen.TwipsPerPixelY

ReDim CoordsX(256, 256)
ReDim CoordsY(256, 256)
ReDim LineStore(256)

MousePointer = vbHourglass

' Create Plasma in byte IndexArray()
Plasma

If Smooth = 1 Then DoSmoothing

Transform
FillBMPStruc 256, 256

MousePointer = vbDefault
DoEvents

DisplayOnce

End Sub

Private Sub Tunnel()

ReDim LineStore(256)

frmTunnel.AutoRedraw = False

TwirlCount = 0
PlasmaStruc.Twirl = Twirl
PlasmaStruc.Speed = Speed
PlasmaStruc.PtrIndexArray = VarPtr(IndexArray(1, 1))
PlasmaStruc.PtrLineStore = VarPtr(LineStore(1))

PtrColArray = VarPtr(ColArray(1, 1))
ptrMC = VarPtr(TunnelMC(1))


TunnelDone = False

Do
   
   'PlasmaStruc.Twirl = Twirl
   PlasmaStruc.TwirlCount = TwirlCount
   '  ScrollDown     ' VB IndexArray()
   '  TransformLUT   ' VB IndexArray() -> ColArray()

   'ScrollDown & TransformLUT
   res = CallWindowProc(ptrMC, ptrStruc, ScrollDown3, 0&, 0&)
   
   ' Blit ColArray() to frmTunnel
   If StretchDIBits(frmTunnel.hdc, _
   0, 0, FormWidth, FormHeight, _
   0, 0, 256, 256, _
   ColArray(1, 1), bm, _
   DIB_RGB_COLORS, vbSrcCopy) = 0 Then
      MsgBox "Blit error"
      Unload frmTunnel
      End
   End If

   'frmTunnel.Refresh
   
   
   If Twirl = 1 And TwirlCount < 500 Then TwirlCount = TwirlCount + 1
   
      
   If GetQueueStatus(QS_MOUSEBUTTON Or QS_KEY) <> 0 Then DoEvents
      
      'VB Twirl
'   If Twirl = 1 TwirlCount < 500 Then
'      For ix = 256 To 1 Step -1
'      For iy = 256 To 1 Step -1
'         IndexArray(iy, ix) = IndexArray(ix, iy)
'      Next iy
'      Next ix
'      TwirlCount = TwirlCount + 1
'   End If

Loop Until TunnelDone

End Sub

'###### PLASMA ################################################

Private Sub Plasma()

'Public PlasmaGraininess 'StartNoise   ' graininess
'Public PlasmaScale  'StartStepsize   ' scale

' CORE PLASMA ROUTINE

' Defaults
'PlasmaGraininess = 0
'PlasmaScale = 16

'PaletteMaxIndex = 255
'sizex = 256
'sizey = 256

ReDim IndexArray(1 To 256, 1 To 256)   ' byte array
ReDim ColArray(1 To 256, 1 To 256)     ' byte array

Randomize Timer
RandSeed = Rnd * 255

PlasmaStruc.PtrIndexArray = VarPtr(IndexArray(1, 1))
PlasmaStruc.PtrColArray = VarPtr(ColArray(1, 1))
PlasmaStruc.PtrLineStore = VarPtr(LineStore(1))
PlasmaStruc.PtrCoordsX = VarPtr(CoordsX(1, 1))
PlasmaStruc.PtrCoordsY = VarPtr(CoordsY(1, 1))
PlasmaStruc.RandSeed = RandSeed
PlasmaStruc.PlasmaGraininess = PlasmaGraininess
PlasmaStruc.PlasmaScale = PlasmaScale
ptrMC = VarPtr(TunnelMC(1))
ptrStruc = VarPtr(PlasmaStruc.PtrIndexArray)

res = CallWindowProc(ptrMC, ptrStruc, Plasma0, 0&, 0&)
Exit Sub
''''''''''''''''''''''
' VB VB VB

Noise = PlasmaGraininess
Stepsize = PlasmaScale
RndNoise = 0

Do
   
   For iy = 1 To 256 Step Stepsize
   For ix = 1 To 256 Step Stepsize
      
      If Noise > 0 Then RndNoise = (Rnd * (2 * Noise) - Noise) * Stepsize
      
      If iy + Stepsize >= 256 Then
           Col = IndexArray(ix, 1) + RndNoise ' Gives vertical wrapping
      ElseIf ix + Stepsize >= 256 Then
            Col = IndexArray(1, iy) + RndNoise  ' Gives horizontal wrapping
      ElseIf Stepsize = PlasmaScale Then
         Col = (Rnd * (2 * 255) - 255)
      Else
          Col = IndexArray(ix, iy)
          Col = Col + IndexArray(ix + Stepsize, iy)
          Col = Col + IndexArray(ix, iy + Stepsize)
          Col = Col + IndexArray(ix + Stepsize, iy + Stepsize)
          Col = Col \ 4 + RndNoise
      End If
      
      If Col < 0 Then Col = 1
      If Col > 255 Then Col = 255
      
      If Stepsize > 1 Then 'BoxLine
          For ky = iy To iy + Stepsize
              If ky <= 256 Then
                  For kx = ix To ix + Stepsize
                      If kx <= 256 Then IndexArray(kx, ky) = Col 'Byte
                  Next kx
              End If
          Next ky
      
      End If
      
      If Stepsize <= 1 Then IndexArray(ix, iy) = Col 'Byte
   
   Next ix
   Next iy

Stepsize = Stepsize \ 2

Loop Until Stepsize <= 1

' Find Col max/min
smax = -10000
smin = 10000
For iy = 256 To 1 Step -1
For ix = 256 To 1 Step -1
   Col = IndexArray(ix, iy)
   If Col < smin Then smin = Col
   If Col > smax Then smax = Col
Next ix
Next iy

' Scale colors indices to range
' & put new index in IndexArray()

sdiv = (smax - smin)
If sdiv <= 0 Then sdiv = 1
zmul = 255 / sdiv

For iy = 256 To 1 Step -1
For ix = 256 To 1 Step -1
   
   Index = (IndexArray(ix, iy) - smin) * zmul
   ' Check Index in range
   If Index < 0 Then
      Index = 0
   End If
   If Index > 255 Then
      Index = 255
   End If
   
   IndexArray(ix, iy) = Index

Next ix
Next iy

End Sub

Private Sub DoSmoothing()
' H&V smoothing  NO VB SUB

res = CallWindowProc(ptrMC, ptrStruc, DoSmoothing1, 0&, 0&)

End Sub

Private Sub Transform()

ReDim CoordsX(256, 256)
ReDim CoordsY(256, 256)

PlasmaStruc.PtrIndexArray = VarPtr(IndexArray(1, 1))
PlasmaStruc.PtrColArray = VarPtr(ColArray(1, 1))
PlasmaStruc.PtrLineStore = VarPtr(LineStore(1))
PlasmaStruc.PtrCoordsX = VarPtr(CoordsX(1, 1))
PlasmaStruc.PtrCoordsY = VarPtr(CoordsY(1, 1))
PlasmaStruc.RandSeed = RandSeed
PlasmaStruc.PlasmaGraininess = PlasmaGraininess
PlasmaStruc.PlasmaScale = PlasmaScale
ptrMC = VarPtr(TunnelMC(1))
ptrStruc = VarPtr(PlasmaStruc.PtrIndexArray)

res = CallWindowProc(ptrMC, ptrStruc, Transform2, 0&, 0&)
Exit Sub

''''''''''''''''''''''
' VB VB VB

For iy = 1 To 256
For ix = 1 To 256
   CoordsX(ix, iy) = 1
   CoordsY(ix, iy) = 1
Next ix
Next iy

For ky = 256 To 1 Step -1 '1 To 256
For kx = 256 To 1 Step -1 '1 To 256
   zdx = kx - 128 'ixdc
   zdy = ky - 128 'iydc
   zRadius = Sqr(zdx ^ 2 + zdy ^ 2)
   iys = 256 - zRadius
   If iys >= 1 And iys <= 256 Then
      ' Get angle 0 to 2*pi
      zAngle = -zATan2(zdx, zdy)
      If zdx >= -0 Then zAngle = zAngle + 2 * pi#

      ' Proportion along 256 as zAngle to 2*pi
      ixs = 256 * zAngle / (2 * pi#) + 0.2   ' +.2 gives better integerization
                                               ' ie no missed radial lines
      If ixs >= 1 And ixs <= 256 Then    ' This test not necessary
         ColArray(kx, ky) = IndexArray(ixs, iys)
         ' Make a LUT, values 1-256 ie integer arrays
         CoordsX(kx, ky) = ixs
         CoordsY(kx, ky) = iys
      End If
      ' so each kx,ky has an associated ixs,iys
   End If
Next kx
Next ky

End Sub

'###  SCROLLDOWN & TRANSFORMLUT ################################

Private Sub ScrollDown()
ReDim LineStore(256)

PlasmaStruc.PtrIndexArray = VarPtr(IndexArray(1, 1))
PlasmaStruc.PtrLineStore = VarPtr(LineStore(1))

'Public Const ScrollDown3 = 3
res = CallWindowProc(ptrMC, ptrStruc, ScrollDown3, 0&, 0&)
Exit Sub
''''''''''''''''''''''
' VB VB VB

' Save bottom line
CopyMemory LineStore(1), IndexArray(1, 1), 256
For iy = 2 To 256
   CopyMemory IndexArray(1, iy - 1), IndexArray(1, iy), 256
Next iy
' Put bottom line to top
CopyMemory IndexArray(1, 256), LineStore(1), 256
End Sub

Private Sub TransformLUT()
' Use LUT

'Public Const TransformLUT4 = 4
res = CallWindowProc(ptrMC, ptrStruc, TransformLUT4, 0&, 0&)
Exit Sub
''''''''''''''''''''''
' VB VB VB

For ky = 256 To 1 Step -1
For kx = 256 To 1 Step -1
   ColArray(kx, ky) = IndexArray(CoordsX(kx, ky), CoordsY(kx, ky))
Next kx
Next ky

End Sub

'###  DISPLAY ONCE  ################################

Private Sub DisplayOnce()
   frmTunnel.AutoRedraw = True
   
   FillBMPStruc 256, 256
   
   ' Blit ColArray() to frmTunnel
   If StretchDIBits(frmTunnel.hdc, _
   0, 0, FormWidth, FormHeight, _
   0, 0, 256, 256, _
   ColArray(1, 1), bm, _
   DIB_RGB_COLORS, vbSrcCopy) = 0 Then
      MsgBox "Blit error"
      Unload frmTunnel
      End
   End If
   
   'frmTunnel.Refresh

End Sub


Private Sub Form_Resize()
FormWidth = frmTunnel.Width \ Screen.TwipsPerPixelX
FormHeight = frmTunnel.Height \ Screen.TwipsPerPixelY
End Sub
