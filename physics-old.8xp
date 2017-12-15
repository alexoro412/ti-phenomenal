ClrHome
Lbl ZZ
Menu("PHYSICS","MOTION",Z,"FORCE",BA,"ENERGY",energy,"COLLISIONS",ZA,"SOUND",GA,"EXIT",F,"PAGE 2",ZF)
Lbl ZF
Menu("PHYSICS 2","STATIC",ST,"CIRCULAR MOTION",PA,"NUCLEAR",TA,"BACK",ZG,"EXIT",F)
Lbl ZG
Goto ZZ
Lbl energy
Menu("ENERGY","E=.5MV²",kinetic,"U=MGH",CC,"W=FXcos(θ)",CD,"G-Ho",NA,"BACK",BC,"EXIT",F)
# Lbl CD
# Menu("FIND","W",workFind,"F",workFindForce,"X",workFindX,"θ",workFindTheta,"BACK",energy,"EXIT",F)
# Lbl workFind
# Prompt F,X,θ
# FXcos(θ)→W
# Disp "W="
# Disp W
# Disp "Newton Meters"
# Goto F
# Lbl workFindForce
# Prompt W,X,θ
# W/(Xcos(θ))→F
# Disp "F="
# Disp F
# Disp "Newtons"
# Goto F
# Lbl workFindX
# Prompt W,F,θ
# W/(Fcos(θ)→X
# Disp "X="
# Disp X
# Disp "Meters"
# Goto F
# Lbl workFindTheta
# Prompt W,F,X
# cos(W/(FX))→θ
# Disp "θ="
# Disp θ
# Disp "Degrees"
# Goto F
# Lbl CC
# Menu("FIND","U",CH,"M",CI,"G",CJ,"H",CK,"BACK",energy,"EXIT",F)
# Lbl CH
# Prompt M,G,H
# MGH→U
# Disp "U="
# Disp U
# Disp "Joules"
# Goto F
# Lbl CI
# Prompt U,G,H
# (U/(GH))→M
# Disp "M="
# Disp M
# Disp "Kilograms"
# Goto F
# Lbl CJ
# Prompt U,M,H
# (U/(MH))→G
# Disp "G="
# Disp G
# Disp "M/S²"
# Goto F
# Lbl CK
# Prompt U,M,G
# (U/(MG))→H
# Disp "H="
# Disp H
# Disp "Meters"
# Goto F
# Lbl kinetic
# Menu("FIND","E",CE,"M",CF,"V",CG,"BACK",energy,"EXIT",F)
# Lbl CE
# Prompt M,V
# .5MV²→E
# Disp "E="
# Disp E
# Disp "Joules"
# Goto F
# Lbl CF
# Prompt E,V
# (2E/(V²))→M
# Disp "M="
# Disp M
# Disp "Kilograms"
# Goto F
# Lbl CG
# Prompt E,M
# √(2E/M)→V
# Disp "V="
# Disp V
# Disp "M/S"
# Goto F
# Lbl BC
# Goto ZZ
# Lbl NA
# Menu("FIND","H Initial (H)",NB,"H Final (L)",NC,"V Initial (I)",ND,"V Final (F)",NE,"BACK",NF,"EXIT",F)
# Lbl NB
# Prompt L,I,F
# (9.8*L)+(.5*(F^2))-(.5*(I^2))→H
# Disp "Hight Initial="
# Disp H
# Disp "Meters"
# Goto F
# Lbl NC
# Prompt H,I,F
# (9.8*H)+(.5*(I^2))-(.5*(F^2))→H
# Disp "Hight Final ="
# Disp H
# Disp "Meters"
# Goto F
# Lbl ND
# Prompt H,L,F
# (9.8*L)+(.5*(F^2))-(9.8*H)→V
# Disp "Velocity Initial ="
# Disp V
# Disp "M/S"
# Goto F
# Lbl NE
# Prompt H,L,I
# (9.8*H)+(.5*(I^2))-(9.8*L)→V
# Disp "Velocity Final ="
# Disp V
# Disp "M/S"
# Goto F
Lbl NF
Goto PA
Lbl PH
Goto ZZ
Lbl BA
Menu("FORCE","F=MA",BB,"F=GMN/R²",BF,"BACK",ZZ,"EXIT",F)
Lbl BF
Menu("FIND","F",BG,"G",BH,"M",BI,"N",BJ,"R",BK,"BACK",BA,"EXIT",F)
Lbl BG
Prompt G,M,N,R
GMN/(R²)→F
Disp "F="
Disp F
Goto F
Lbl BH
Prompt F,R,M,N
((F(R²))/(MN))→G
Disp "G="
Disp G
Goto F
Lbl BI
Prompt F,R,G,N
((F(R²))/(GN))→M
Disp "M="
Disp M
Goto F
Lbl BJ
Prompt F,R,G,M
((F(R²))/(GM))→N
Disp "N="
Disp N
Goto F
Lbl BK
Prompt G,M,N,F
√((GMN)/F)→R
Disp "R="
Disp R
Goto F
Lbl BB
Menu("FIND","F",BC,"A",BD,"M",BE,"BACK",BA,"EXIT",F)
Lbl BC
Prompt M,A
MA→F
Disp "F="
Disp F
Disp "Newtons"
Goto F
Lbl BD
Prompt F,M
F/M→A
Disp "A="
Disp A
Disp "M/S²"
Goto F
Lbl BE
Prompt F,A
F/A→M
Disp "M="
Disp M
Disp "Kilograms"
Goto F


Lbl Z
Menu("MOTION EQUATIONS","FIND V",A,"FIND U",B,"FIND A",C,"FIND X",D,"FIND T",E,"BACK",ZZ,"EXIT",F)
Lbl A
Menu("KNOWN VARIABLES","U,A,T",ZC,"U,A,X",H,"X,U,T",I,"X,A,T",J,"BACK",Z,"EXIT",F)
Lbl ZC
Prompt U
Prompt A
Prompt T
A
T
U+AT→V
Disp "V="
Disp V
Disp "M/S"
Goto F
Lbl H
Prompt U
Prompt A
Prompt X
√(U²+2AX)→V
Disp "V="
Disp V
Disp "M/S"
Goto F
Lbl I
Prompt X
Prompt U
Prompt T
((2X-UT)/T)→V
Disp "V="
Disp V
Disp "M/S"
Goto F
Lbl J
Prompt X
Prompt A
Prompt T
((X+(.5A(T)²))/T)→V
Disp "V="
Disp V
Disp "M/S"
Goto F
Lbl B
Menu("KNOWN VARIABLES","V,A,T",K,"V,A,X",L,"X,V,T",M,"X,A,T",ZD,"BACK",Z,"EXIT",F)
Lbl K
Prompt V
Prompt A
Prompt T
V-AT→U
Disp "U="
Disp U
Disp "M/S"
Goto F
Lbl L
Prompt V
Prompt A
Prompt X
√(V²-(2AX))→U
Disp "U="
Disp U
Disp "M/S"
Goto F
Lbl M
Prompt X
Prompt V
Prompt T
((2X-VT)/T)→U
Disp "U="
Disp U
Disp "M/S"
Goto F
Lbl ZD
Prompt X
Prompt A
Prompt T
√((X-(.5AT²))→U
Disp "U="
Disp U
Disp "M/S"
Goto F
Lbl C
Menu("KNOWN VARIABLES","V,U,T",O,"V,U,X",ZE,"V,X,T",Q,"X,U,T",R,"BACK",Z,"EXIT",F)
Goto Z
Lbl O
Prompt V
Prompt U
Prompt T
((V-U)/T)→A
Disp "A="
Disp A
Disp "M/S²"
Goto F
Lbl ZE
Prompt V
Prompt U
Prompt X
((V²-U²)/(2X))→A
Disp "A="
Disp A
Disp "M/S²"
Goto F
Lbl Q
Prompt V,X,T
((2VT-2X)/T²)→A
Disp "A="
Disp A
Disp "M/S²"
Goto F
Lbl R
Prompt X,U,T
((2X-2UT)/(T²))→A
Disp "A="
Disp A
Disp "M/S²"
Goto F
Lbl D
Menu("KNOWN VARIABLES","U,V,T",S,"U,A,T",T,"V,A,T",U,"V,U,A",V,"BACK",Z,"EXIT",F)
Lbl S
Prompt U,V,T
(.5(U+V)T)→X
Disp "X="
Disp X
Disp "Meters"
Goto F
Lbl T
Prompt U,A,T
(UT+(.5AT²))→X
Disp "X="
Disp X
Disp "Meters"
Goto F
Lbl U
Prompt V,A,T
(VT-(.5AT²))→X
Disp "X="
Disp X
Disp "Meters"
Goto F
Lbl V
Prompt V,U,A
((V²-U²)/(2A))→X
Disp "X="
Disp X
Disp "Meters"
Goto F
Lbl E
Menu("KNOWN VARIABLES","V,U,A",X,"X,U,V",Y,"V,A,X",AA,"U,A,X",AB,"BACK",Z,"EXIT",F)
Lbl X
Prompt V,U,A
((V-U)/A)→T
Disp "T="
Disp T
Disp "Seconds"
Goto F
Lbl Y
Prompt X,U,V
((2X)/(U+V))→T
Disp "T="
Disp T
Disp "Seconds"
Goto F
Lbl AA
Prompt V,A,X
((V+√(V²-(2AX)))/A)→B
((V-√(V²-(2AX)))/A)→C
If B=C
Then
Goto A1
Else
Goto A2
Lbl A1
Disp "T="
Disp B
Disp "Seconds"
Goto F
Lbl A2
Disp "T1="
Disp B
Disp "Seconds"
Disp "T2="
Disp C
Disp "Seconds"
Goto F
Lbl AB
Prompt U,A,X
((­U+√(U²+(2AX)))/(A))→B
((­U-√(U²+(2AX)))/(A))→C
If A=C
Then
Goto A3
Else
Goto A4
Lbl A3
Disp "T="
Disp T
Disp "Seconds"
Goto F
Lbl A4
Disp "T1="
Disp B
Disp "Seconds"
Disp "T2"
Disp C
Disp "Seconds"
Goto F

Lbl ZA
Menu("COLLISIONS","Momentum",NG,"Perfectly Inelastic",NH,"Inelastic",NI,"Elastic",NJ,"BACK",NK,"EXIT",F)
Lbl NG
Menu("FIND","Momentum (N)",NL,"Mass",NM,"Velocity",NN,"BACK",NO,"EXIT",F)
Lbl NL
Prompt M,V
M*V→K
Disp "Momentum="
Disp K
Disp "(KG*M)/S"
Goto F
Lbl NM
Prompt N,V
N/V→K
Disp "Mass="
Disp K
Disp "KG"
Goto F
Lbl NN
Prompt N,M
N/M→V
Disp "Velocity="
Disp V
Disp "M/S"
Goto F
Lbl NO
Goto ZA
Lbl NH
Menu("FIND","Mass 1 (M)",NP,"Mass 2 (N)",NQ,"V Final (F)",NR,"VI 1 (O)",NS,"VI 2 (I)",NT,"BACK",NU,"EXIT",F)
Lbl NP
Prompt N,F,O,I
((N*VI)-(N*F))/(F-O)→M
Disp "Mass 1="
Disp M
Disp "KG"
Goto F
Lbl NQ
Prompt M,F,O,I
((M*VO)-(M*VF))/(F-I)→M
Disp "Mass 2"
Disp M
Disp "KG"
Goto F
Lbl NR
Prompt M,N,O,I
((M*O)+(N*I))/(M+N)→V
Disp "Velocity Final="
Disp V
Disp "M/S"
Goto F
Lbl NS
Prompt M,N,F,I
(((M+N)*F)-(N*I))/M→V
Disp "Vi 1="
Disp V
Disp "M/S"
Goto F
Lbl NT
Prompt M,N,F,O
(((M+N)*F)-(M*O))/N→V
Disp "Vi 2="
Disp V
Disp "M/S"
Goto F
Lbl NU
Goto ZA
Lbl NI
Menu("FIND","Mass 1 (M)",NV,"Mass 2 (N)",NW,"Vi 1 (O)",NX,"Vi 2 (I)",NY,"BACK",OB,"EXIT",F,"Page 2",ZJ)
Lbl ZJ
Menu("Page 2","Vf 1 (F)",NZ,"Vf 2 (C)",OA,"BACK",ZK,"EXIT",F)
Lbl ZK
Goto NI
Lbl NV
Prompt N,F,C,O,I
((N*I)-(N*C))/(F-O)→M
Disp "Mass 1="
Disp M
Disp "KG"
Goto F
Lbl NW
Prompt M,F,C,O,I
((M*O)-(M*F))/(C-I)→M
Disp "Mass 2"
Disp M
Disp "KG"
Goto F
Lbl NX
Prompt M,N,I,F,C
(((M*F)+(N*C))-(N*I))/M→V
Disp "Vi 1="
Disp V
Disp "M/S"
Goto F
Lbl NY
Prompt M,N,O,F,C
(((M*F)+(N*C))-(M*O))/N→V
Disp "Vi 2="
Disp V
Disp "M/S"
Goto F
Lbl NZ
Prompt M,N,O,I,C
(((M*O)+(N*I))-(N*C))/M→V
Disp "Vf 1="
Disp V
Disp "M/S"
Goto F
Lbl OA
Prompt M,N,O,I,F
(((M*O)+(N*I))-(M*F))/N→V
Disp "Vf 2="
Disp V
Disp "M/S"
Goto F
Lbl OB
Goto ZA
Lbl NJ
Menu("FIND","Vi 1 (O)",OC,"Vi 2 (I)",OD,"Vf 1 (F)",OE,"Vf 2 (C)",OF,"BACK",OG,"EXIT",F)
Lbl OC
Prompt I,F,C
(C-I)+F→V
Disp "Vi 1="
Disp V
Disp "M/S"
Goto F
Lbl OD
Prompt O,F,C
O-(C-F)→V
Disp "Vi 2"
Disp V
Disp "M/S"
Goto F
Lbl OE
Prompt I,O,C
(C-O+I)→V
Disp "Vf 1="
Disp V
Disp "M/S"
Goto F
Lbl OF
Prompt I,O,F
(O-I)+F→V
Disp "Vf 2="
Disp V
Disp "M/S"
Goto F
Lbl OG
Goto ZA
Lbl NK
Goto ZZ

Lbl GA
Menu("SOUND","OPEN TUBE",GB,"CLOSED TUBE",GC,"STRING",GD,"DOPPLER",GE,"BACK",GF,"EXIT",F)
Lbl GB
Menu("FIND","Velocity",GG,"Frequency",GH,"Length",GI,"Harmonic Tone",GJ,"BACK",GK,"EXIT",F)
Lbl GG
Prompt F,L,N
F*L*(2/N)→V
Disp "V="
Disp V
Disp "M/S"
Goto F
Lbl GH
Prompt V,L,N
V/(L*(2/N))→F
Disp "F="
Disp F
Disp "Hz"
Goto F
Lbl GI
Prompt V,F,N
V/(F*(2/N))→L
Disp "L="
Disp L
Disp "Meters"
Goto F
Lbl GJ
Prompt V,F,L
V/(F*L)→N
Disp "N="
Disp N
Disp "No Unit"
Goto F
Lbl GK
Goto GA
Lbl GC
Menu("FIND","Velocity",GL,"Frequency",GM,"Length",GN,"Harmonic Tone",GO,"BACK",GP,"EXIT",F)
Lbl GL
Prompt F,L,N
F*L*(4/N)→V
Disp "V="
Disp V
Disp "M/S"
Goto F
Lbl GM
Prompt V,L,N
V/(L*(4/N))→F
Disp "F="
Disp F
Disp "Hz"
Goto F
Lbl GN
Prompt V,F,N
V/(F*(4/N))→L
Disp "L="
Disp L
Disp "Meters"
Goto F
Lbl GO
Prompt V,F,L
V/(F*L)→N
Disp "N="
Disp N
Disp "No Unit"
Goto F
Lbl GP
Goto GA
Lbl GD
Menu("FIND","Velocity",GQ,"Frequency (Q)",GR,"Length",GS,"Harmonic Tone",GT,"BACK",GW,"EXIT",F,"Page 2",ZH)
Lbl ZH
Menu("PAGE 2","Force",GU,"Mass",GV,"BACK",ZI,"EXIT",F)
Lbl ZI
Goto GD
Lbl GQ
Menu("FIND","F,M,L",GX,"F,N,L",GY,"BACK",GZ,"EXIT",F)
Lbl GX
Prompt F,M,L
2√(F*L/M)→V
Disp "V="
Disp V
Disp "M/S"
Goto F
Lbl GY
Prompt F,L,N
F*L*(2/N)→V
Disp "V="
Disp V
Disp "M/S"
Goto F
Lbl GZ
Goto GD
Lbl GR
Prompt L,N,F,M
(2√(F*L/M))/(L*(2/N))→Q
Disp "Q="
Disp Q
Disp "Hz"
Goto F
Lbl GS
Prompt Q,N,F,M
1/((Q*M*(4/(N^2)))/F)→L
Disp "L="
Disp L
Disp "Meters"
Goto F
Lbl GT
Prompt L,F,Q,M
((2√(F*L/M))/(Q*L))→N
Disp "N="
Disp N
Disp "No Unit"
Goto F
Lbl GU
Prompt Q,M,N,L
1/((Q*M*(4/(N^2)))/L)→F
Disp "Force="
Disp F
Disp "Newtons"
Goto F
Lbl GV
Prompt V,F,L
1/((Q*(4/(N^2)))/(L*F))→M
Disp "Mass="
Disp M
Disp "Kilograms"
Goto F
Lbl GW
Goto GA

Lbl GE
Menu("FIND","F Detector (Q)",HA,"F Source (F)",HB,"Velocity (V)",HC,"V Detector (D)",HD,"V Source (S)",HE,"BACK",HF,"EXIT",F)
Lbl HA
Prompt F,V,D,S
F*((V-D)/(V-S))→Q
Disp "Frequency Detector="
Disp Q
Disp "Hz"
Goto F
Lbl HB
Prompt Q,V,D,S
Q/((V-D)/(V-S))→F
Disp "Frenquency Source="
Disp F
Disp "Hz"
Goto F
Lbl HC
Prompt Q,F,D,S
(((Q*S)-(F*D))/(Q-F))→V
Disp "Velocity="
Disp V
Disp "M/S"
Goto F
Lbl HD
Prompt Q,F,V,S
((((Q*(V-S))/F)/(­1))(­V))→D
Disp "Velocity Detector="
Disp D
Disp "M/S"
Goto F
Lbl HE
Prompt Q,F,V,D
((((Q*(V-D))/F)/(­1))(­V))→S
Disp "Velocity Source="
Disp S
Disp "M/S"
Goto F
Lbl HF
Goto ZZ

Lbl ST
Menu("STATIC","F=KQ1Q2/R²",MA,"E=KQ1/R²",MB,"V=KQ1/R",MC,"BACK",MD,"EXIT",F)
Lbl MA
Menu("FIND","F",SA,"Q1",SB,"Q2",SC,"R",SD,"BACK",SE,"EXIT",F)
Lbl SA
Prompt Q,D,R
((9*10^9)*Q*D)/(R^2)→F
Disp "F="
Disp F
Disp "Newtons"
Goto F
Lbl SB
Prompt F,D,R
(F*(R^2))/((9*10^9)*D)→Q
Disp "Q1="
Disp Q
Disp "Coulomb"
Goto F
Lbl SC
Prompt F,Q,R
(F*(R^2))/((9*10^9)*Q)→D
Disp "Q2="
Disp D
Disp "Coulomb"
Goto F
Lbl SD
Prompt F,Q,D
2√(((9*10^9)Q*D)/(F))→R
Disp "R="
Disp R
Disp "Meters"
Goto F
Lbl SE
Goto ST
Lbl MB
Menu("FIND","E",SG,"Q1",SH,"R",SI,"BACK",SJ,"EXIT",F)
Lbl SG
Prompt Q,R
((9*(10^9))*Q)/(R^2)→E
Disp "E="
Disp E
Disp "N/C"
Goto F
Lbl SH
Prompt E,R
(E*(R^2))/(9*(10^9))→Q
Disp "Q="
Disp Q
Disp "Coulomb"
Goto F
Lbl SI
Prompt E,Q
(2√(((9*(10^9))*Q)/(E)))→R
Disp "R="
Disp R
Disp "Meters"
Goto F
Lbl SJ
Goto ST
Lbl MC
Menu("FIND","V",SK,"Q1",SL,"R",SM,"BACK",SN,"EXIT",F)
Lbl SK
Prompt Q,R
(((9*(10^9))*Q)/(R))→V
Disp "V="
Disp V
Disp "Volts"
Goto F
Lbl SL
Prompt V,R
(V*R/(9*10^9))→Q
Disp "Q="
Disp Q
Disp "Coulomb"
Goto F
Lbl SM
Prompt V,Q
(((9*10^9)*Q)/(V))→R
Disp "R="
Disp R
Disp "Meters"
Goto F
Lbl SN
Goto ST
Lbl MD
Goto ZZ

Lbl PA
Menu("Circular Motion","Tangential Velocity",PB,"Cen Acceleration",PC,"Cen Force",PD,"Gravitation",PE,"BACK",PH,"EXIT",F,"PAGE 2",ZL)
Lbl ZL
Menu("Page 2","Accel Gravity",PF,"Basic Velocity",PG,"BACK",ZM,"EXIT",F)
Lbl ZM
Goto PA
Lbl PB
Menu("FIND","Tangential V",PI,"Radius",PJ,"Revolutions (L)",PK,"Time",PL,"BACK",PM,"EXIT",F)
Lbl PI
Prompt R,L,T
(2*π*R*L)/T→V
Disp "Tangential V="
Disp V
Disp "M/S"
Goto F
Lbl PJ
Prompt V,L,T
(V*T)/(2*π*L)→R
Disp "Radius ="
Disp R
Disp "M"
Goto F
Lbl PK
Prompt V,R,T
(V*T)/(2*π*R)→W
Disp "Revolutions ="
Disp W
Disp "No Unit"
Goto F
Lbl PL
Prompt V,R,L
(2*π*R*L)/V→T
Disp "Time="
Disp T
Disp "Seconds"
Goto F
Lbl PM
Goto PA
Lbl PC
Menu("FIND","Cen Acceleration",PN,"Velocity Tan",PO,"Radius",HG,"BACK",PQ,"EXIT",F)
Lbl PN
Prompt V,R
(V^2)/R→A
Disp "Cen Acceleration="
Disp A
Disp "M/S^2"
Goto F
Lbl PO
Prompt A,R
2√(A*R)→V
Disp "Velocity Tan ="
Disp V
Disp "M/S"
Goto F
Lbl HG
Prompt A,V
(V^2)*A→R
Disp "Radius ="
Disp R
Disp "M"
Goto F
Lbl PQ
Goto PA
Lbl PD
Menu("FIND","Cen Force",PR,"Mass",PS,"Velocity",PT,"Radius",PU,"BACK",QG,"EXIT",F)
Lbl PR
Prompt M,V,R
(M*(V^2))/R→F
Disp "Cen Force="
Disp F
Disp "Newtons"
Goto F
Lbl PS
Prompt F,V,R
(F*R)/(V^2)→M
Disp "Mass ="
Disp M
Disp "KG"
Goto F
Lbl PT
Prompt F,M,R
2√((F*R)/M)→V
Disp "Velocity Tan="
Disp V
Disp "M/S"
Goto F
Lbl PU
Prompt F,M,V
(M*(V^2))/F→R
Disp "Radius ="
Disp R
Disp "Meters"
Goto F
Lbl QG
Goto PA
Lbl PE
Menu("FIND","Force Gravity",PW,"Mass 1 (M)",PX,"Mass 2 (N)",PY,"Radius",QA,"BACK",PZ,"EXIT",F)
Lbl PW
Prompt M,N,R
((6.67*(10^­11))*M*N)/(R^2)→F
Disp "Force G="
Disp F
Disp "Newtons"
Goto F
Lbl PX
Prompt F,N,R
(F*(R^2))/((6.67*(10^­11))*N)→M
Disp "Mass 1 ="
Disp M
Disp "KG"
Goto F
Lbl PY
Prompt F,M,R
(F*(R^2))/((6.67*(10^­11))*M)→M
Disp "Mass 2 ="
Disp M
Disp "KG"
Goto F
Lbl QA
Prompt F,M,N
2√(((6.67*(10^­11))*M*N)/F)→V
Disp "Radius ="
Disp V
Disp "M="
Goto F
Lbl PZ
Goto PA
Lbl PF
Menu("FIND","Acc Gravity",QB,"Mass (Larger)",QC,"Radius",QD,"BACK",QF,"EXIT",F)
Lbl QB
Prompt M,R
((6.67*(10^­11))*M)/(R^2)→A
Disp "Acc Gravity="
Disp A
Disp "M/S^2"
Goto F
Lbl QC
Prompt A,R
(A*(R^2))/(6.67*(10^­11))→M
Disp "Mass ="
Disp M
Disp "KG"
Goto F
Lbl QD
Prompt A,M
2√(((6.67*(10^­11))*M)/A)→R
Disp "Radius ="
Disp M
Disp "M"
Goto F
Lbl QF
Goto PA
Lbl PG
Menu("FIND","Velocity",QE,"Radius",HH,"Time",QH,"BACK",QI,"EXIT",F)
Lbl QE
Prompt R,T
(2*π*R)/T→V
Disp "Velocity="
Disp V
Disp "M/S"
Goto F
Lbl HH
Prompt V,T
(V*T)/(2*π)→R
Disp "Radius ="
Disp R
Disp "Meters"
Goto F
Lbl QH
Prompt V,R
(2*π*R)/V→T
Disp "Time ="
Disp T
Disp "Seconds"
Goto F
Lbl QI
Goto PA
Lbl PH
Goto ZZ

Lbl TA
Menu("Nuclear","E=MC^2",TB,"Half-Life",TC,"BACK",TD,"EXIT",F)
Lbl TB
Menu("FIND","Energy",TE,"Mass",TF,"BACK",TG,"EXIT",F)
Lbl TE
Prompt M
M*(299792458^2)→E
Disp "Energy="
Disp E
Disp "Joules"
Goto F
Lbl TF
Prompt E
E/(299792458^2)→M
Disp "Mass="
Disp M
Disp "Kg"
Goto F
Lbl TG
Goto TA
Lbl TC
Menu("FIND","Half-Life (H)",TH,"Num Rem (R)",TI,"Num Orig (O)",TJ,"Time",TK,"BACK",TL,"EXIT",F)
Lbl TH
Prompt R,O,T
T/((log(R/O))/(log(.5)))→H
Disp "Half Life="
Disp H
Disp "Unit T is"
Goto F
Lbl TI
Prompt O,L,T
O*(.5^(T/L))→R
Disp "# R="
Disp R
Disp "Unit O is"
Goto F
Lbl TJ
Prompt R,L,T
R/(.5^(T/L))→O
Disp "# O="
Disp O
Disp "Unit O is"
Goto F
Lbl TK
Prompt O,L,R
L*((log(R/O))/(log(.5)))→T
Disp "Time="
Disp T
Disp "Unit HL is"
Goto F
Lbl TL
Goto TA
Lbl TD
Goto ZZ


Lbl F
Disp " "
