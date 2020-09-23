VERSION 5.00
Begin VB.Form frmPlasTunnel 
   AutoRedraw      =   -1  'True
   BackColor       =   &H00404040&
   BorderStyle     =   0  'None
   Caption         =   "Form1"
   ClientHeight    =   7650
   ClientLeft      =   1995
   ClientTop       =   195
   ClientWidth     =   9420
   DrawStyle       =   5  'Transparent
   BeginProperty Font 
      Name            =   "MS Sans Serif"
      Size            =   13.5
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H00FFC0FF&
   Icon            =   "PlasTunn.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   510
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   628
   ShowInTaskbar   =   0   'False
   Begin VB.TextBox txtSpeed 
      Enabled         =   0   'False
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Left            =   6045
      TabIndex        =   14
      Text            =   "0"
      Top             =   4860
      Width           =   390
   End
   Begin VB.HScrollBar HScrSpeed 
      Height          =   240
      Left            =   5250
      Max             =   4
      Min             =   1
      TabIndex        =   13
      Top             =   5220
      Value           =   4
      Width           =   1215
   End
   Begin VB.CheckBox chkTwirl 
      BackColor       =   &H00000000&
      Caption         =   "Twirl"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   300
      Left            =   4860
      TabIndex        =   12
      Top             =   5880
      Width           =   1035
   End
   Begin VB.CheckBox chkSmooth 
      BackColor       =   &H00000000&
      Caption         =   "Smooth"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   300
      Left            =   4860
      TabIndex        =   11
      Top             =   5550
      Width           =   1035
   End
   Begin VB.TextBox txtScale 
      Enabled         =   0   'False
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Left            =   6060
      TabIndex        =   10
      Text            =   "0"
      Top             =   4170
      Width           =   390
   End
   Begin VB.HScrollBar HScrScale 
      Height          =   240
      LargeChange     =   4
      Left            =   5250
      Max             =   64
      Min             =   4
      TabIndex        =   9
      Top             =   4515
      Value           =   4
      Width           =   1215
   End
   Begin VB.TextBox txtGraininess 
      Enabled         =   0   'False
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   285
      Left            =   6030
      TabIndex        =   7
      Text            =   "0"
      Top             =   3495
      Width           =   405
   End
   Begin VB.HScrollBar HScrGraininess 
      Height          =   240
      LargeChange     =   2
      Left            =   5205
      Max             =   16
      TabIndex        =   5
      Top             =   3840
      Width           =   1245
   End
   Begin VB.FileListBox File1 
      BackColor       =   &H00000000&
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   -1  'True
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H0000FFFF&
      Height          =   1455
      Left            =   3405
      Pattern         =   "*.pal"
      TabIndex        =   2
      Top             =   3585
      Width           =   1425
   End
   Begin VB.CommandButton cmdExit 
      BackColor       =   &H008080FF&
      Caption         =   "Exit"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   390
      Left            =   2700
      Style           =   1  'Graphical
      TabIndex        =   1
      Top             =   5640
      Width           =   540
   End
   Begin VB.CommandButton cmdGO 
      BackColor       =   &H00C0FFC0&
      Caption         =   "GO"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   13.5
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   840
      Left            =   3690
      Style           =   1  'Graphical
      TabIndex        =   0
      Top             =   5400
      Width           =   915
   End
   Begin VB.Label Label5 
      BackColor       =   &H00000000&
      Caption         =   "Speed"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   225
      Left            =   5205
      TabIndex        =   15
      Top             =   4905
      Width           =   630
   End
   Begin VB.Label Label4 
      BackColor       =   &H00000000&
      Caption         =   "Scale"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   225
      Left            =   5265
      TabIndex        =   8
      Top             =   4230
      Width           =   630
   End
   Begin VB.Label Label3 
      BackColor       =   &H00000000&
      Caption         =   "Graininess"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   225
      Left            =   5190
      TabIndex        =   6
      Top             =   3555
      Width           =   840
   End
   Begin VB.Shape Shape1 
      BorderColor     =   &H00FFFFFF&
      Height          =   5430
      Left            =   1365
      Shape           =   3  'Circle
      Top             =   1500
      Width           =   5775
   End
   Begin VB.Label Label2 
      BackColor       =   &H00404040&
      Caption         =   "by  Robert Rayment"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFC0FF&
      Height          =   285
      Left            =   3570
      TabIndex        =   4
      Top             =   2640
      Width           =   1575
   End
   Begin VB.Label Label1 
      BackColor       =   &H00000000&
      Caption         =   "Label1"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFF80&
      Height          =   1725
      Left            =   1800
      TabIndex        =   3
      Top             =   3570
      Width           =   1530
   End
End
Attribute VB_Name = "frmPlasTunnel"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'Form1 PlasTunn.frm

' PLASMA TUNNELS by  Robert Rayment  June 2002

Option Base 1  ' Arrays base 1
DefLng A-W     ' All variable Long
DefSng X-Z     ' unless singles
               ' unless otherwise defined
Dim FormTop
Dim FormLeft
Dim Palspec$


Private Sub cmdGO_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)

If GOGO Then
      
   
   frmTunnel.Hide    ' Allows vbModal
   frmTunnel.Show vbModal

Else

   GOGO = Not GOGO

End If

frmPlasTunnel.SetFocus

End Sub

Private Sub Form_Load()

' Instructions
A$ = ""
A$ = A$ & " Select a pal file" & vbCrLf
A$ = A$ & " && press GO  then" & vbCrLf
A$ = A$ & " left-click to start" & vbCrLf
A$ = A$ & " tunnel, right-click" & vbCrLf
A$ = A$ & " to get out.   Dbl_Click to" & vbCrLf
A$ = A$ & "    maximize"

Label1.Caption = A$

ShapeForm

zStartAngle = pi# * (1 - 0.18)   ' T&E
CurvedText frmPlasTunnel, "PLASMA TUNNELS", zStartAngle

Show

' Starting pal file
Palspec$ = "Demo.pal"
ReadPAL Palspec$     ' RGBs into Colors(0 To 255)

HScrScale.Value = 16
HScrSpeed.Value = 4

' Load machine code
Loadmcode "Tunnel.bin", TunnelMC()

GOGO = True

End Sub

Private Sub Form_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
' To move form

FormTop = Y
FormLeft = X

End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
' To move form

If Button <> 0 Then
   Me.Top = Me.Top + Y - FormTop
   Me.Left = Me.Left + X - FormLeft
End If

End Sub

Private Sub File1_Click()
' This gets the entry clicked in the FileListBox
Palspec$ = File1.List(File1.ListIndex)
ReadPAL Palspec$     ' RGBs into Colors(0 To 255)
End Sub

Private Sub ShapeForm()

Width = 7000
Height = 7000

Top = (Screen.Height - Me.Height) / (2 * Screen.TwipsPerPixelY)
Left = (Screen.Width - Me.Width) / 2 - 1000


RoundReg = CreateEllipticRgn _
(100, 100, Me.Width / Screen.TwipsPerPixelX, _
Me.Height / Screen.TwipsPerPixelY)

SetWindowRgn Me.hwnd, RoundReg, False
DeleteObject RoundReg

End Sub

Private Sub cmdExit_Click()
Unload Me
End
End Sub

Private Sub HScrGraininess_Change()
txtGraininess.Text = HScrGraininess.Value
PlasmaGraininess = HScrGraininess.Value
End Sub

Private Sub HScrScale_Change()
txtScale.Text = HScrScale.Value
PlasmaScale = HScrScale.Value
End Sub

Private Sub HScrSpeed_Change()
txtSpeed.Text = HScrSpeed.Value
Speed = HScrSpeed.Value
End Sub

Private Sub chkSmooth_Click()
Smooth = chkSmooth.Value  ' 0 unchecked, 1 checked
End Sub

Private Sub chkTwirl_Click()
Twirl = chkTwirl.Value  ' 0 unchecked, 1 checked
End Sub

