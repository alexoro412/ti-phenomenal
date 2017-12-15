{}→⌊BACK
{I,E,A}→⌊BACK
Lbl C
ClrHome
Disp "PERCENT ERROR CALCULATOR"
Disp ""
Disp "ENTER IDEAL VALUE"
Output(10,1,"ENTER 0 TO QUIT")
Prompt I
If I=0
Then
Goto A
End
Lbl B
ClrHome
Output(10,1,"ENTER 0 TO QUIT")
Disp "IDEAL: "+toString(I)+" (I)"
Disp ""
Disp "ENTER ACTUAL"
Prompt A
If A≠0
Then
((A-I)/I)→E
Disp ""
Disp "ERROR: "+toString((E*100))
Output(10,1,"PRESS ENTER TO CONTINUE")
Pause 
Goto B
Else
ClrHome
Goto C
End
Lbl A
⌊BACK(1)→I
⌊BACK(2)→E
⌊BACK(3)→A
DelVar ⌊BACK
Stop