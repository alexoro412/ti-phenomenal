{0}→⌊BACK
{θ,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z}→⌊BACK
ClrHome
Disp ""
Disp ""
Disp "PHYSICS V2.1"
Pause 
ClrHome
Lbl AD
Menu("PHYSICS","MOTION",AB,"FORCE",AA,"ENERGY",Z,"COLLISIONS",F,"SOUND",E,"PAGE 2",AC,"EXIT",A)
Lbl AC
Menu("PHYSICS 2","STATIC",D,"CIRCULAR MOTION",C,"NUCLEAR",B,"BACK",AD,"EXIT",A)
Lbl AB
Lbl AA
Lbl Z
Menu("ENERGY","E=.5MV²",J,"U=MGH",Y,"W=FXCOS(θ)",T,"G-HO",O,"BACK",AD,"EXIT",A)
Lbl Y
Menu("FIND","U",X,"M",W,"G",V,"H",U,"BACK",Z,"EXIT",A)
Lbl X
Prompt M,G,H
(M*G*H)→U
Disp "U="+toString(U)+" J"
Goto A
Lbl W
Prompt U,G,H
(U/(G*H))→M
Disp "M="+toString(M)+" KG"
Goto A
Lbl V
Prompt U,M,H
(U/(M*H))→G
Disp "G="+toString(G)+toString(" M/S²")
Goto A
Lbl U
Prompt U,M,G
(U/(M*G))→H
Disp "H="+toString(H)+" M"
Goto A
Lbl T
Menu("FIND","W",S,"F",R,"X",Q,"θ",P,"BACK",Z,"EXIT",A)
Lbl S
Prompt F,X,θ
(F*X*cos(θ))→W
Disp "W="+toString(W)+" NM"
Goto A
Lbl R
Prompt W,X,θ
(W/(X*cos(θ)))→F
Disp "F="+toString(F)+" N"
Goto A
Lbl Q
Prompt W,F,θ
(W/(F*cos(θ)))→X
Disp "X="+toString(X)+" M"
Goto A
Lbl P
Prompt W,F,X
cos((W/(F*X)))→θ
Disp "θ="+toString(θ)+" DEG"
Goto A
Lbl O
Menu("FIND","H INITIAL (H)",N,"H FINAL (L)",M,"V INITIAL (I)",L,"V FINAL (F)",K,"BACK",Z,"EXIT",A)
Lbl N
Prompt L,I,F
((9.8*L)+(.5*(F^2))-(.5*(I^2)))→H
Disp "HEIGHT INITIAL="
Disp toString(H)+" M"
Goto A
Lbl M
Prompt H,I,F
((9.8*H)+(.5*(I^2))-(.5*(F^2)))→H
Disp "HEIGHT FINAL="
Disp toString(H)+" M"
Goto A
Lbl L
Prompt H,L,F
((9.8*L)+(.5*(F^2))-(9.8*H))→V
Disp "VELOCITY INITIAL="
Disp toString(V)+" M/S"
Goto A
Lbl K
Prompt H,L,I
((9.8*H)+(.5*(I^2))-(9.8*L))→V
Disp "VELOCITY FINAL="
Disp toString(V)+" M/S"
Goto A
Lbl J
Menu("FIND","E",G,"M",H,"V",I,"BACK",Z,"EXIT",A)
Lbl I
Prompt E,M
√((2*E/M))→V
Disp "V="+toString(V)+" M/S"
Goto A
Lbl H
Prompt E,V
(2*E/(V^2))→M
Disp "M="+toString(M)+" KG"
Goto A
Lbl G
Prompt M,V
(.5*M*V^2)→E
Disp "E="+toString(E)+" J"
Goto A
Lbl F
Lbl E
Lbl D
Lbl C
Lbl B
Lbl A
⌊BACK(1)→θ
⌊BACK(2)→A
⌊BACK(3)→B
⌊BACK(4)→C
⌊BACK(5)→D
⌊BACK(6)→E
⌊BACK(7)→F
⌊BACK(8)→G
⌊BACK(9)→H
⌊BACK(10)→I
⌊BACK(11)→J
⌊BACK(12)→K
⌊BACK(13)→L
⌊BACK(14)→M
⌊BACK(15)→N
⌊BACK(16)→O
⌊BACK(17)→P
⌊BACK(18)→Q
⌊BACK(19)→R
⌊BACK(20)→S
⌊BACK(21)→T
⌊BACK(22)→U
⌊BACK(23)→V
⌊BACK(24)→W
⌊BACK(25)→X
⌊BACK(26)→Y
⌊BACK(27)→Z
DelVar ⌊BACK
Stop