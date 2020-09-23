Attribute VB_Name = "RotText"
'RotText.bas

DefLng A-W
DefSng X-Z

Public Sub CurvedText(FRM As Form, Title$, ByVal zAngle As Single)
   
   ' Based partly on Ulli's Rotating Text
   
If Len(Title$) < 1 Then Exit Sub
zSizeFact = 1.5   ' T&E
' T&E adjustment for circular form
cx = FRM.ScaleWidth / 2 + 45
cy = FRM.ScaleHeight / 2 + 70

zdTheta = 0.6 * pi# / FRM.TextWidth(Title$)  ' T&E
' > 0.6 spreads characters out more & vice versa

' T&E adjustment for circular form
xRadius = FRM.ScaleWidth * 0.4
yRadius = FRM.ScaleHeight * 0.4

FRM.FontBold = True
For i = 1 To Len(Title$)
   Char$ = Mid$(Title$, i, 1)
   ' Find rotated x,y point for each character
   FRM.CurrentX = cx + xRadius * Cos(zAngle) - _
   Cos(zAngle) * FRM.TextHeight(Char$) / zSizeFact
   
   FRM.CurrentY = cy - yRadius * Sin(zAngle) - _
   Sin(zAngle) * FRM.TextWidth(Char$) / zSizeFact
   
   FRM.Print Char$;
   
   zAngle = zAngle - zdTheta * FRM.TextWidth(Char$)
Next i
End Sub

