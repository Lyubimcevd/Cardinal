INTEGER*2 A(26,200),S(2,200),P(2,200),TQ(6,500),TQC(4,500),U(20,100),NXZ(10,4),U6(100),U2(100),EC(3,100),MET/'2 '/,MSIZ2,STZZ2                         
LOGICAL*1 MSIZ,STZZ,MSIZ1(2)/' ',' '/,STZZ1(2)/'0',' '/,MSIZ4     
INTEGER QZ1(500),QZ(4,500),UW(2,200),IW(100),XIFR(5000)           
REAL*8 SMAT                                                       
DATA LIMA,LIMS,LIMT/2*200,500/,L/72/                                                        
integer*1 JFM,NKMAX       
character*10 zakaz
integer*4 today(3)
equivalence (NXZ(1,1),zakaz)
200 FORMAT(I3,I2,4A2,I5,22A2,I5,I2,I1,I3,I8,I5,I8,F10.2)                                                                            
7 FORMAT(I8,I1,I2)                                                  
9 FORMAT(20X,'POTOKO‹ ”OPMˆPOBAHˆŸ OPT”E‹Ÿ O ˆ   ',2X,I8////10X,'B OPT”E‹E ›‹O-',I5/10X,'BBE„EHO ‡AKA‡OB-',I5/10X,'B OPT”E‹E CTA‹O-',I5////)                                                  
10 FORMAT(A1,170X)                                                   
20 FORMAT(//10X,'**** POBEPˆTœ „AHH›E „‹Ÿ ‡AKA‡A ',10A2,' (B OPT”E‹œ HE OME™EH).')      

integer*2 omoih_buf/1/,cmatin_buf/2/,kdz_buf/3/,bmp_buf/4/,u_buf/5/,portfel_buf/6/ 
OPEN(omoih_buf,FILE='D:\ASUIPW\tek_INF\omoih.dat')
OPEN(cmatin_buf,FILE='D:\ASUIPW\tek_INF\cmatin.dat',ACCESS='DIRECT',FORM='FORMATTED',RECL=81)
open(kdz_buf,file='D:\ASUIPW\tek_INF\kdz.dat')
open(bmp_buf,file='D:\ASUIPW\tek_INF\bpm.dat')
LIL=1                                                             
KI=1                                                              
MK=1                                                              
M=0
NSP=0                                                             
NMT=0
IDEP=0
IERL=50
call idate(today)
nm = 0
do while (.NOT.eof(cmatin_buf))
    nm = nm + 1
    READ(cmatin_buf,37,REC=i) XIFR(i),I2
37  FORMAT(I8,64X,I9)
    IF (I2>999990000) exit 
enddo

OPEN(u_buf,FILE='F:\ASUIPW\tek_INF\U.DAT',FORM='UNFORMATTED')
READ(u_buf) NW,NU                                                    
READ(u_buf) ((UW(I,J),I=1,2),J=1,NW),((U(I,J),I=1,20),J=1,NU)        
REWIND L1                                                         
NUU=NU-1                                                          
DO I=1,NW                                                     
    IF(UW(1,I) == 170501) then                                    
        K10=UW(2,I)  
        exit
    endif 
enddo                                                                                                                   
DO I=1,NU                                                    
    U6(I)=U(6,I)                                                      
    U2(I)=U(2,I)         
enddo
OPEN(portfel_buf,FILE='F:\ASUIPW\tek_INF\PORTFEL.DAT',FORM='UNFORMATTED')
kolvo_v_portfele = -1
DO while (.NOT.eof(portfel_buf))      
    kolvo_v_portfele = kolvo_v_portfele + 1
    READ(portfel_buf) ((NXZ(I,J),I=1,10),J=1,4),(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT                                                          
    IF(NA < 0) exit                                               
    READ(portfel_buf) ((A(I,J),I=1,26),J=1,NA)                                 
    READ(portfel_buf) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)          
    READ(portfel_buf) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)        
    READ(portfel_buf) ((TQC(I,J),I=1,4),J=1,NT)                                
enddo                                                           
REWIND portfel_buf
DO M = 1,kolvo_v_portfele  
    READ(portfel_buf) ((NXZ(I,J),I=1,10),J=1,4),(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,NA,NS,NT                                              
    READ(portfel_buf) ((A(I,J),I=1,26),J=1,NA)                                 
    READ(portfel_buf) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)          
    READ(portfel_buf) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)        
    READ(portfel_buf) ((TQC(I,J),I=1,4),J=1,NT)                                
enddo                                                            
do i = 1,26
    do j = 1,200
        A(i,j) = 0
    enddo
enddo
READ(omoih_buf,6) JFM,((NXZ(I,J),I=1,10),J=1,2),IP,IQ,((NXZ(I,J),I=1,10),J=3,4),ID,MP
6 FORMAT(I1,20A2,I2,I5,15A2,2X,5A2,I5,I8)
kolvo_add_in_portfel = kolvo_add_in_portfel + 1
NSP=NSP+1                                                         
CALL STRSSV(omoih_buf,NA,NS,A,S,LIMA,LIMS,NK,NKMAX)                      
IF(NA.LE.1) GOTO 4                                                
IRCZ=0                                                            
ITRZ=1                                                            
CALL TCLZKZ(NF,LIMT,NW,NU,NA,NT,UW,U,A,TQ,TQC,QZ,EC,NE,NK,NKMAX,IT
1RZ,IRCZ)                                                          
CALL STRPRM(LIMS,NA,NS,NXZ,A,S,P,IQ)                              
IND=IERL                                                          
CALL CHKPRT(LIMT,NA,NT,A,S,P,TQ,QZ,TQC,U,NU,IND)                  
CALL SGOKIZ(NA,NT,A,S,P,TQ,QZ,TQC,U)                              
CALL SVODCA(NT,TQ,QZ,NU,NW,IW,UW,IRAS,ITRZ,IRCZ)                  
DO 64 J=2,NT                                                      
64 QZ1(J)=TQ(4,J)*1000000+TQ(5,J)*1000+TQ(6,J)                       
IF(IP.GT.2) GOTO 88                                               
DO 500  J=1,NU                                                    
IF(IP.EQ.0.OR.IP.EQ.1) U(6,J)=0                                   
IF(IP.EQ.0.AND.U(2,J).EQ.820) U(2,J)=U(2,J)*2                     
500 CONTINUE                                                          
CALL PRIOR(A,TQ,QZ,NT,NA,S,U,IP)                                  
IF(IP.GT.0) CALL SGRLZE(LZ,NA,NT,NU,A,S,P,TQ,QZ,TQC,U,IP,IW)      
CALL MEHDET(NA,NT,A,S,P,TQ,TQC)                                   
IF(TQ(2,2)/100.EQ.5) TQ(4,2)=QZ1(2)/1000000                       
IF(TQ(2,2)/100.EQ.5) TQ(5,2)=QZ1(2)/1000-TQ(4,2)*1                
1000                                                               
IF(TQ(2,2)/100.EQ.15) TQ(6,2)=QZ1(2)-TQ(4,2)*100000               
10-TQ(5,2)*1000                                                    
88 CONTINUE                                                          
CALL MEHDET(NA,NT,A,S,P,TQ,TQC)                                   
IF(IP.GT.0) CALL SGRLZE(LZ,NA,NT,NU,A,S,P,TQ,QZ,TQC,U,IP,IW)      
I=NS+1                                                            
IF(NE.LE.0) GOTO 60                                               
DO 61 J=1,NE                                                      
K=EC(2,J)                                                         
DO 62 J1=2,NA                                                     
IF(A(9,J1).EQ.K) GOTO 63                                          
62 CONTINUE                                                          
GOTO 61                                                           
63 J1=A(12,J1)+EC(3,J)-1                                             
NS=NS+1                                                           
S(1,NS)=J1                                                        
S(2,NS)=EC(1,J)                                                   
61 CONTINUE                                                          
IF(NS.LT.I) GOTO 60                                               
NE=I                                                              
CALL  EXTCON(LZ,NA,NS,NT,NE,NU,A,S,P,TQ,QZ,TQC,U,IP,IW)           
60 NE=I                                                              
NS=NE-1                                                           
IF(TQ(2,2).NE.501) GOTO 400                                       
CALL ZAGOP(A,S,P,TQ,TQC,U,NXZ,IW,QZ,UW,K10,NA,NS,NT,LZ,ITRZ,IRCZ) 
400 CONTINUE                                                          
IF(IP3.EQ.1) IP=3                                                 
DO 501 J=1,NU                                                     
U(2,J)=U2(J)                                                      
501 U(6,J)=U6(J)                                                      
NZ=A(10,1)                                                        
NZ=S(1,NZ)                                                        
NZ=A(12,NZ)+A(13,NZ)-1                                            
70 LZ=0                                                              
IPPZ=0                                                            
DO 71 J=2,NT                                                      
IF(TQ(2,J).EQ.512.OR.TQ(2,J).EQ.513) IPPZ=1                       
IF(TQ(2,J).EQ.512.OR.TQ(2,J).EQ.513) GOTO 71                      
IF(TQ(4,J).GT.LZ) LZ=TQ(4,J)                                      
71 CONTINUE                                                          
IF(IPPZ.EQ.1) LZ=LZ+30                                            
CALL VEDMAT(A,NA,NXZ,P,L,XIFR,NM,ND,MK,SMAT)                      
NSTU=TQ(1,NZ)                                                     
nuhb=TQ(2,nz)/100
KW=0                                                              
DO 31 I=1,NUU                                                     
31  KW=KW+IW(I)                                                       
IF(NUHB.EQ.3.OR.NUHB.EQ.4) ID=(KW*105+500)/1000                   
IF(NUHB.EQ.1.OR.NUHB.EQ.2) ID=(KW*112+500)/1000
OPEN(3,FILE='F:\ASUIPW\tek_INF\SPTR.DAT')
DO 201 J=1,NU                                                     
IF(IW(J).EQ.0) GOTO 201                                           
IF(IW(J).GT.99999) GOTO 1983                                      
WRITE(3,200) U(1,NSTU),NUHB,(NXZ(I,1),I=1,4),IQ,(NXZ(I,4),I=1,5),
*(NXZ(I,2),I=1,9),(U(I,J),I=13,20),IW(J),U(8,J),IP,LZ,MP,ID,IRAS,
*SMAT                                                              
GOTO 201                                                          
1983 KOR=99999                                                         
KOP=IW(J)-99999                                                   
WRITE(3,200) U(1,NSTU),NUHB,(NXZ(I,1),I=1,4),IQ,(NXZ(I,4),I=1,5), 
*(NXZ(I,2),I=1,9),(U(I,J),I=13,20),KOR,U(8,J),IP,LZ,MP,ID,IRAS,
*SMAT                                                              
IF(KOP.LE.99999) GOTO 1984                                        
KOP=KOP-99999                                                     
WRITE(3,200) U(1,NSTU),NUHB,(NXZ(I,1),I=1,4),IQ,(NXZ(I,4),I=1,5), 
*(NXZ(I,2),I=1,9),(U(I,J),I=13,20),KOR,U(8,J),IP,LZ,MP,ID,IRAS,
*SMAT   
IF(KOP.LE.99999) GOTO 1984                                        
KOP=KOP-99999                                                     
WRITE(3,200) U(1,NSTU),NUHB,(NXZ(I,1),I=1,4),IQ,(NXZ(I,4),I=1,5), 
*(NXZ(I,2),I=1,9),(U(I,J),I=13,20),KOR,U(8,J),IP,LZ,MP,ID,IRAS,
*SMAT   
IF(KOP.LE.99999) GOTO 1984                                        
KOP=KOP-99999                                                     
WRITE(3,200) U(1,NSTU),NUHB,(NXZ(I,1),I=1,4),IQ,(NXZ(I,4),I=1,5), 
*(NXZ(I,2),I=1,9),(U(I,J),I=13,20),KOR,U(8,J),IP,LZ,MP,ID,IRAS,
*SMAT   
IF(KOP.LE.99999) GOTO 1984                                        
KOP=KOP-99999                                                     
WRITE(3,200) U(1,NSTU),NUHB,(NXZ(I,1),I=1,4),IQ,(NXZ(I,4),I=1,5), 
*(NXZ(I,2),I=1,9),(U(I,J),I=13,20),KOR,U(8,J),IP,LZ,MP,ID,IRAS,
*SMAT   
1984 WRITE(3,200) U(1,NSTU),NUHB,(NXZ(I,1),I=1,4),IQ,(NXZ(I,4),I=1,5), 
*(NXZ(I,2),I=1,9),(U(I,J),I=13,20),KOP,U(8,J),IP,LZ,MP,ID,IRAS,
*SMAT   
201 CONTINUE                                                          
IF(IDEP.EQ.0)GOTO 410                                             
410 CALL KDZ(A,S,P,TQ,TQC,U,NXZ,QZ,UW,IW,NW,NU,KI,LIL,NT,IQ)          
210 CONTINUE                                                          
DO 140 J=1,NA                                                     
IF(A(11,J).EQ.0) A(10,J)=0                                        
140 CONTINUE                                                          
IF(TQ(2,2).EQ.501) QZ(4,NT)=QZ(4,1)                               
NXZ(10,1)=MET                                                     
WRITE(L2) ((NXZ(I,J),I=1,10),J=1,4),(IW(I),I=1,NU),IQ,IP,ID,LZ,MP 
1,NA,NS,NT                                                         
WRITE(L2) ((A(I,J),I=1,26),J=1,NA)                                
WRITE(L2) ((S(I,J),I=1,2),J=1,NS),((P(I,J),I=1,2),J=1,NS)         
WRITE(L2) ((TQ(I,J),I=1,6),J=1,NT),((QZ(I,J),I=1,4),J=1,NT)       
WRITE(L2) ((TQC(I,J),I=1,4),J=1,NT)                               
N=N+1                                                             
GOTO 4                                                            
5 NA=-1000                                                          
WRITE(L2) ((NXZ(I,J),I=1,10),J=1,4),(IW(I),I=1,NU),IQ,IP,ID,LZ,MP,
*NA,NS,NT                                                          
PRINT 9,IDATA,M,NSP,N                                             
REWIND L2
pause                                                             
STOP
END                                                               
SUBROUTINE PRIOR(A,TQ,QZ,NT,NA,S,U,IP)                            
INTEGER*2 A(26,200),TQ(6,500),PRIZN(500)                          
INTEGER*2 U(20,100)                                               
INTEGER*2 UR(200),NU(10),KU(10)                                   
INTEGER*2 S(2,200),SB                                             
INTEGER QZ(4,500),TR                                              
INTEGER*2 SLES/'83'/                                              
90 CONTINUE                                                          
IF(IP.EQ.1) INOP=100                                              
IF(IP.EQ.2) INOP=200                                              
IF(IP.EQ.0) INOP=0                                                
DO 1 I=2,NT                                                       
IF(IP.EQ.1.OR.IP.EQ.0) GOTO 91                                    
GOTO 1                                                            
91 R=(QZ(1,I)+0.0)/U(2,TQ(1,I))                                      
J4=INT(R)                                                         
IF(R-J4.LE.0.0) J4=J4-1                                           
TQ(6,I)=TQ(5,I)-U(6,TQ(1,I))                                      
TQ(4,I)=TQ(5,I)+J4                                                
1 PRIZN(I)=0                                                        
DO 2 I=2,NA                                                       
NACH=A(12,I)                                                      
IPR=PRIZN(NACH)                                                   
IF(IPR.GT.0) GOTO 2                                               
KON=NACH+A(13,I)-1                                                
J=KON                                                             
TR=0                                                              
KRAN=0                                                            
5 IF(TQ(5,J).EQ.TQ(6,J)) GOTO 31                                    
IF(KRAN.EQ.0) GOTO 6                                              
K=J+1                                                             
GOTO 7                                                            
31 IF(TQ(4,J).EQ.TQ(5,J)) GOTO 32                                    
IF(KRAN.NE.0) K=J+1                                               
IF(KRAN.NE.0) GOTO 7                                              
N=J                                                               
KRAN=1                                                            
ID2=TQ(4,J)-TQ(5,J)                                               
TR=QZ(1,J)-U(2,TQ(1,J))*ID2                                       
TR=TR+INOP                                                        
IF(TR.LT.U(2,TQ(1,J))) GOTO 6                                     
KRAN=0                                                            
TR=0                                                              
GOTO 6                                                            
32 IF(KRAN.EQ.0) N=J                                                 
KRAN=1                                                            
TR=TR+QZ(1,J)+INOP                                                
I1=TQ(2,J)-TQ(2,J)/100*100                                        
IF(I1.EQ.SLES.AND.QZ(1,J).EQ.1) TR=TR-INOP                        
IF(TR.GE.U(2,TQ(1,J))) GOTO 4                                     
6 J=J-1                                                             
IF(J.GE.NACH) GOTO 5                                              
IF(KRAN.EQ.0) PRIZN(NACH)=1                                       
IF(KRAN.EQ.0) GOTO 2                                              
K=NACH                                                            
GOTO 7                                                            
4 IF(N.EQ.J) K=J                                                    
IF(N.EQ.J) J=J-1                                                  
IF(N.EQ.J) GOTO 7                                                 
K=J+1                                                             
7 IF(K.EQ.N) GOTO 33                                                
M=K                                                               
8 TQ(4,M)=TQ(4,N)                                                   
TQ(5,M)=TQ(4,N)                                                   
TQ(6,M)=TQ(4,N)                                                   
M=M+1                                                             
IF(M.LT.N) GOTO 8                                                 
IF(K.EQ.NACH) PRIZN(NACH)=1                                       
IF(K.EQ.NACH) GOTO 2                                              
M=K-1                                                             
9 ID1=TQ(5,M)-TQ(6,M)                                               
ID2=TQ(4,M)-TQ(5,M)                                               
TQ(6,M)=TQ(4,M+1)+1                                               
TQ(5,M)=TQ(6,M)+ID1                                               
TQ(4,M)=TQ(5,M)+ID2                                               
M=M-1                                                             
IF(M.GE.NACH) GOTO 9                                              
33 KRAN=0                                                            
TR=0                                                              
J=J+1                                                             
GOTO 6                                                            
2 CONTINUE                                                          
DO 50 K=1,NA                                                      
50 UR(K)=0                                                           
N=1                                                               
I=1                                                               
51 NU(I)=A(10,N)                                                     
KU(I)=NU(I)+A(11,N)-1                                             
M1=NU(I)                                                          
M2=KU(I)                                                          
DO 52 J=M1,M2                                                     
IF(I.GT.UR(S(1,J))) UR(S(1,J))=I                                  
52 CONTINUE                                                          
53 N=S(1,NU(I))                                                      
I=I+1                                                             
IF(A(11,N).GT.0) GOTO 51                                          
54 I=I-1                                                             
IF(I.LE.0) GOTO 60                                                
NU(I)=NU(I)+1                                                     
IF(NU(I).GT.KU(I)) GOTO 54                                        
GOTO 53                                                           
60 DO 61 I=1,NT                                                      
61 PRIZN(I)=0                                                        
MUR=0                                                             
DO 62 K=1,NA                                                      
IF(UR(K).GT.MUR) MUR=UR(K)                                        
62 CONTINUE                                                          
MU=0                                                              
68 MU=MU+1                                                           
IF(MU.GE.MUR) GOTO 70                                             
DO 63 I=1,NA                                                      
IF(UR(I).NE.MU) GOTO 63                                           
IF(A(11,I).LE.0) GOTO 63                                          
MX=TQ(4,A(12,I))                                                  
ND=A(10,I)                                                        
KD=ND+A(11,I)-1                                                   
MK=A(9,I)                                                         
64 IF(MK.EQ.A(9,S(1,ND))) GOTO 65                                    
NMK=A(12,S(1,ND))                                                 
KMK=NMK+A(13,S(1,ND))-1                                           
IF(PRIZN(A(12,S(1,ND))).EQ.0) GOTO 66                             
IF(TQ(6,KMK).GE.MX) GOTO 65                                       
66 ID1=TQ(5,KMK)-TQ(6,KMK)                                           
ID2=TQ(4,KMK)-TQ(5,KMK)                                           
TQ(6,KMK)=MX                                                      
67 TQ(5,KMK)=TQ(6,KMK)+ID1                                           
ID3=TQ(4,KMK)                                                     
TQ(4,KMK)=TQ(5,KMK)+ID2                                           
IF(KMK.LE.NMK) PRIZN(NMK)=1                                       
IF(KMK.LE.NMK) GOTO 65                                            
KMK=KMK-1                                                         
ID1=TQ(5,KMK)-TQ(6,KMK)                                           
ID2=TQ(4,KMK)-TQ(5,KMK)                                           
ID3=TQ(6,KMK)-ID3                                                 
TQ(6,KMK)=TQ(4,KMK+1)+ID3                                         
GOTO 67                                                           
65 ND=ND+1                                                           
IF(ND.LE.KD) GOTO 64                                              
63 CONTINUE                                                          
70 RETURN                                                            
80 RETURN                                                            
END                                                               
SUBROUTINE KDZ(A,S,P,TQ,TQC,U,NXZ,QZ,UW,IW,NW,NU,KI,LI,NT,IQ)     
INTEGER*2 A(26,200),S(2,200),P(2,200),TQ(6,500),TQC(4,500),U(20,10
*0),NXZ(10,4)                                                      
INTEGER*2 MET/'C'/                                               
INTEGER QZ(4,500),UW(2,200),IW(100)                               
INTEGER*2 DET(50),IMOP(8)                                         
1 FORMAT(4A2,5A2,7A2,I5,2I3,I5,I2,10I3,I5,I9,10A2)                  
N=A(10,1)                                                         
N=S(1,N)                                                          
N=A(12,N)+A(13,N)-1                                               
N=TQ(1,N)                                                         
N=U(1,N)                                                          
KI=KI+1                                                           
NK1=98                                                            
KO=1                                                              
DO 4 K=2,NT                                                       
N=TQ(3,K)                                                         
M=N/1000                                                          
N=N-N/1000*1000                                                   
N1=TQ(1,K)                                                        
DO I=1,8
IMOP(I)=U(I+12,N1)
ENDDO
C      IVO=U(8,N1)                                                       
DO 5 J=1,50                                                       
DET(J)=0                                                          
5 CONTINUE                                                          
I=1                                                               
M2=N+M                                                            
DO 6 J=N,M2                                                       
M1=A(1,J)                                                         
NK=A(9,M1)                                                        
DET(I)=A(2,M1)                                                    
I=I+1                                                             
6 CONTINUE                                                          
IF(NK.EQ.NK1) KO=KO+1                                             
IF(NK.EQ.NK1) GOTO 100                                            
KO=1                                                              
100 NZ=A(10,1)                                                        
NZ=S(1,NZ)                                                        
NZ=A(12,NZ)+A(13,NZ)-1                                            
NZ=TQ(1,NZ)                                                       
NZ=U(1,NZ)                                                        
WRITE(8,1) (NXZ(J,1),J=1,4),(NXZ(J,4),J=1,5),(IMOP(j),j=1,7),
1IQ,NK,KO,TQC(2,K),NZ,(DET(J),J=1,10),QZ(1,K),QZ(2,K),
2(NXZ(J,3),J=1,10)          
C     LI=LI+1                                                           
NK1=NK                                                            
4 CONTINUE                                                          
RETURN                                                            
END                                                               
SUBROUTINE ZAGOP(A,S,P,TQ,TQC,U,NXZ,IW,QZ,UW,K10,NA,NS,NT,LZ,ITRZ,
1IRCZ)                                                             
INTEGER*2 A(26,200),S(2,200),P(2,200),TQ(6,500),TQC(4,500),U(20,10
*0),NXZ(10,4),KDP(180),DET,OP                                      
INTEGER*2 S1(2,200)                                               
INTEGER IW(100),QZ(4,500),UW(2,200)                               
INTEGER*2 C/'  '/,H/'00'/                                         
DO 12 I=1,200                                                     
DO 13 J=1,2                                                       
S1(J,I)=S(J,I)                                                    
13 CONTINUE                                                          
12 CONTINUE                                                          
IF(NXZ(5,1).NE.C) GOTO 6                                          
N=A(10,1)                                                         
N=S(1,N)                                                          
N=A(12,N)+A(13,N)-1                                               
N=TQ(1,N)                                                         
NZ=U(1,N)                                                         
IF(NZ.EQ.10) GOTO 6                                               
DET=A(9,1)                                                        
DO 3 I=1,NA                                                       
IF(A(9,I).EQ.1) NMK1=A(12,I)                                      
IF(A(9,I).EQ.1) KMK1=A(13,I)                                      
IF(DET.GT.A(9,I)) GOTO 3                                          
DET=A(9,I)                                                        
3 CONTINUE                                                          
OP=TQ(4,1)                                                        
NZIK=NMK1+KMK1                                                    
DO 4 I=NZIK,NT                                                    
IF(OP.GT.TQ(4,I)) GOTO 4                                          
OP=TQ(4,I)                                                        
4 CONTINUE                                                          
N=NA+1                                                            
M=NT+1                                                            
DO 5 I=1,16                                                       
A(I,N)=0                                                          
5 CONTINUE                                                          
DO 7 I=17,26                                                      
A(I,N)=H                                                          
7 CONTINUE                                                          
A(1,N)=N                                                          
A(10,N)=0                                                         
A(12,N)=M                                                         
A(2,N)=1                                                          
A(9,N)=99                                                         
A(11,N)=0                                                         
A(13,N)=1                                                         
A(14,N)=NS+1                                                      
K=A(10,S(1,A(10,1)))                                              
JPRZ=S(1,A(10,1))                                                 
K=K+A(11,S(1,A(10,1)))-1                                          
K1=K+1                                                            
K2=N-2                                                            
DO 10 I=K1,K2                                                     
S(1,I+1)=S1(1,I)                                                  
S(2,I+1)=S1(2,I)                                                  
10 CONTINUE                                                          
S(1,K1)=N                                                         
S(2,K1)=1                                                         
K1=A(10,1)+1                                                      
K1=S(1,K1)                                                        
DO 11 I=K1,N                                                      
IF(A(11,I).GT.0) A(11,I)=A(11,I)+1                                
11 CONTINUE                                                          
A(10,1)=NS+1                                                      
A(15,N)=1                                                         
OP=OP+30                                                          
LZ=OP                                                             
TQ(1,M)=K10                                                       
TQ(3,M)=N                                                         
TQ(4,M)=OP+1                                                      
TQ(5,M)=OP+1                                                      
TQ(6,M)=OP                                                        
QZ(1,M)=ITRZ                                                      
QZ(2,M)=IRCZ                                                      
R=(QZ(1,M)+0.0)/U(2,TQ(1,M))                                      
J4=INT(R)                                                         
IF(R-J4.LE.0.0) J4=J4-1                                           
TQ(4,M)=TQ(5,M)+J4                                                
QZ(3,M)=0                                                         
QZ(4,M)=NT                                                        
TQ(2,M)=0501                                                      
TQC(2,M)=1                                                        
TQC(1,M)=-1000                                                    
TQC(3,M)=1                                                        
TQC(4,M)=0                                                        
DET=S(1,NS)                                                       
OP=S(2,NS)                                                        
P(2,NS+1)=1                                                       
P(1,NS+1)=JPRZ                                                    
NS=NS+1                                                           
NA=NA+1                                                           
DO 20 I=1,NA                                                      
IF(A(9,I).EQ.1) IPRK1=P(1,A(14,I))                                
IF(A(9,I).EQ.1) NSTK1=I                                           
IF(A(9,I).EQ.1) P(1,A(14,I))=NA                                   
20  CONTINUE                                                          
NC1=A(10,IPRK1)                                                   
NC2=A(10,IPRK1)+A(11,IPRK1)-1                                     
DO 30 I=NC1,NC2                                                   
IF(S(1,I).EQ.NSTK1) GOTO 31                                       
30  CONTINUE                                                          
31  SK2=S(2,I)                                                        
NSTSK1=I+1                                                        
NS1=NS-1                                                          
DO 32 I=NSTSK1,NS1                                                
S(1,I-1)=S(1,I)                                                   
32  S(2,I-1)=S(2,I)                                                   
S(1,NS1)=NSTK1                                                    
S(2,NS1)=SK2                                                      
A(10,NA)=NS1                                                      
A(11,NA)=1                                                        
A(11,IPRK1)=A(11,IPRK1)-1                                         
NA1=NA-1                                                          
DO 33 I=2,NA1                                                     
IF(A(10,I).GT.NC1) A(10,I)=A(10,I)-1                              
33  CONTINUE                                                          
NZIK1=NZIK-1                                                      
NZIK2=NMK1+1                                                      
DO 15 I=NZIK2,NZIK1                                               
TQ(4,I)=TQ(4,I)+J4+LZ+2-TQ(6,NZIK1)                               
TQ(5,I)=TQ(5,I)+J4+LZ+2-TQ(6,NZIK1)                               
15  TQ(6,I)=TQ(6,I)+J4+LZ+2-TQ(6,NZIK1)                               
TQ(4,NMK1)=TQ(4,NMK1+1)                                           
TQ(5,NMK1)=TQ(4,NMK1+1)                                           
TQ(6,NMK1)=TQ(4,NMK1+1)                                           
NT=NT+1                                                           
LZ=TQ(4,2)                                                        
6 RETURN                                                            
END                                                               
SUBROUTINE EXTCON(NI,NA,NS,NT,NE,NU,A,S,P,TQ,QZ,TQC,U,IP,IW)      
INTEGER IW(100)                                                   
INTEGER*2 A(26,1),S(2,1),P(2,1),TQ(6,1),TQC(4,1),U(20,1)          
INTEGER QZ(4,1)                                                   
NQ=NS-NE+1                                                        
IF(NQ.LE.0) GOTO 10                                               
DO 1 I=1,NQ                                                       
ID=0                                                              
DO 2 J=NE,NS                                                      
J1=S(1,J)                                                         
J2=S(2,J)                                                         
IF(TQ(6,J1).GT.TQ(4,J2)) GOTO 2                                   
ID=1                                                              
J2=TQ(4,J2)-TQ(6,J1)+1                                            
CALL SGCORR(J1,J2,A,S,TQ)                                         
2 CONTINUE                                                          
IF(ID.LT.1) GOTO10                                                
IF(IP.GT.0) CALL SGRLZE(NI,NA,NT,NU,A,S,P,TQ,QZ,TQC,U,IP,IW)      
1 CONTINUE                                                          
IF(ID.GT.0) NI=-NI                                                
10 RETURN                                                            
END                                                               
SUBROUTINE MEHDET(NA,NT,A,S,P,TQ,TQC)                             
INTEGER*2 A(26,1),S(2,1),P(2,1),TQ(6,1),TQC(4,1),W1(100)/100*0/,  
1W2(100)/100*0/,W3(50)/50*0/,W4(50)/50*0/                          
DATA NK/82/                                                       
DO 11 I=1,NA                                                      
11 A(16,I)=0                                                         
IT=0                                                              
DO 10 I=1,NT                                                      
TQC(4,I)=0                                                        
I1=TQ(2,I )-TQ(2,I )/100*100                                      
IF(I1.LE.NK) GO TO 10                                             
IF(TQ(4,I).GT.IT) IT=TQ(4,I)                                      
TQC(4,I)=1                                                        
10 CONTINUE                                                          
IF(IT.LE.0) GO TO 24                                              
IT=IT+1                                                           
K=0                                                               
DO 9 I=1,NA                                                       
IF(A(11,I).GT.0) GO TO 9                                          
CALL DERPOD(I,A,P,TQC,W1,W2,W3,TQ,K)                              
9 CONTINUE                                                          
15 CONTINUE                                                          
IF(K.LE.0) GO TO 12                                               
DO 14 I=1,K                                                       
14 W4(I)=W3(I)                                                       
K1=K                                                              
K=0                                                               
DO 13 I=1,K1                                                      
J1=W4(I)                                                          
CALL DERPOD(J1,A,P,TQC,W1,W2,W3,TQ,K)                             
13 CONTINUE                                                          
GO TO 15                                                          
12 J0=1                                                              
J=0                                                               
2 J=J+1                                                             
W1(J)=J0                                                          
W2(J)=A(11,J0)                                                    
I1=A(13,J0)                                                       
IF(I1.LE.0) GO TO 3                                               
IF(A(16,J0)-1) 5,3,6                                              
6 I1=A(16,J0)-1                                                     
5 I2=A(12,J0)+I1-1                                                  
I3=W1(J-1)                                                        
I3=A(12,I3)                                                       
IF(TQ(6,I2).GT.TQ(4,I3)) GO TO 4                                  
II=TQ(4,I3)-TQ(6,I2)+1                                            
GO TO 8                                                           
4 CONTINUE                                                          
IF(TQ(6,I2).GE.IT) GO TO 3                                        
II=IT-TQ(6,I2)                                                    
8 DO 7 I=1,I1                                                       
I2=A(12,J0)+I-1                                                   
TQ(4,I2)=TQ(4,I2)+II                                              
TQ(5,I2)=TQ(5,I2)+II                                              
7 TQ(6,I2)=TQ(6,I2)+II                                              
IF(A(16,J0).GT.0) GO TO 3                                         
A(16,J0)=-1                                                       
3 W2(J)=W2(J)-1                                                     
IF(W2(J).LT.0) GO TO 1                                            
J0=A(10,J0)+W2(J)                                                 
J0=S(1,J0)                                                        
GO TO 2                                                           
1 J=J-1                                                             
IF(J.LE.0) GO TO 24                                               
J0=W1(J)                                                          
GO TO 3                                                           
24 DO 17 I=1,NA                                                      
17 A(16,I)=-1                                                        
RETURN                                                            
END                                                               
SUBROUTINE DERPOD(I,A,P,TQC,W1,W2,W3,TQ,K)                        
INTEGER*2 A(26,1),P(2,1),TQC(4,1),W1(1),W2(1),W3(1),TQ(6,1)       
JJ=0                                                              
J=0                                                               
J0=I                                                              
12 J=J+1                                                             
IF(J.GT.100) STOP 100                                             
W1(J)=J0                                                          
W2(J)=A(15,J0)                                                    
IF(JJ.LE.0) GO TO 13                                              
2 A(16,J0)=1                                                        
L=A(12,J0)                                                        
TQC(4,L)=1                                                        
IF(TQ(3,L).LT.1000) GO TO 15                                      
I1=TQ(3,L)/1000                                                   
I2=TQ(3,L)-I1*1000                                                
I1=I1+1                                                           
DO 1 L=1,I1                                                       
J1=I2+L-1                                                         
J1=A(1,J1)                                                        
IF(A(16,J1).GT.0) GO TO 1                                         
K=K+1                                                             
W3(K)=J1                                                          
1 A(16,J1)=1                                                        
GO TO 15                                                          
13 I1=A(13,J0)                                                       
IF(I1.LE.0) GO TO 15                                              
DO 14 L=1,I1                                                      
I2=A(12,J0)+L-1                                                   
IF(TQC(4,I2).LE.0) GO TO 14                                       
A(16,J0)=L                                                        
JJ=J                                                              
GO TO 15                                                          
14 CONTINUE                                                          
15 W2(J)=W2(J)-1                                                     
IF(W2(J).LT.0) GO TO 16                                           
J0=W1(J)                                                          
J0=A(14,J0)+W2(J)                                                 
J0=P(1,J0)                                                        
IF(A(16,J0).LE.0) GO TO 12                                        
IF(A(16,J0).GT.1.AND.JJ.GT.0) GO TO 2                             
GO TO 15                                                          
16 J=J-1                                                             
IF(J.LE.0) RETURN                                                 
IF(JJ.GT.J) JJ=0                                                  
GO TO 15                                                          
END                                                               
SUBROUTINE CHKPRT(LIMT,NA,NT,A,S,P,TQ,QZ,TQC,UG,NUG,IP)           
INTEGER*2 A(26,1),S(2,1),TQ(6,1),TQC(4,1)                         
INTEGER*2 P(2,1),UG(20,1)                                         
INTEGER QZ(4,1),Q(40)                                             
NA1=NA+1                                                          
DO 1 J=2,NA                                                       
IF(A(13,J).GT.0) GOTO 1                                           
NT=NT+1                                                           
NA1=NA1-1                                                         
A(1,NA1)=J                                                        
A(12,J)=NT                                                        
A(13,J)=1                                                         
IF(NT.GT.LIMT) STOP 41                                            
IQ=0                                                              
J1=A(14,J)                                                        
J2=A(15,J)+J1-1                                                   
DO 2 J3=J1,J2                                                     
2 IQ=IQ+P(2,J3)                                                     
TQ(1,NT)=NUG                                                      
TQ(2,NT)=0                                                        
TQ(3,NT)=NA1                                                      
TQ(4,NT)=-1                                                       
TQ(5,NT)=-1                                                       
TQ(6,NT)=-1                                                       
QZ(1,NT)=1                                                        
QZ(2,NT)=0                                                        
TQC(1,NT)=-1000                                                   
TQC(2,NT)=IQ                                                      
TQC(3,NT)=0                                                       
TQC(4,NT)=-1                                                      
1 CONTINUE                                                          
J1=0                                                              
J2=0                                                              
J3=0                                                              
DO 3 J=2,NT                                                       
IF(QZ(1,J).LE.0) QZ(1,J)=1                                        
J1=J1+QZ(1,J)                                                     
IF(TQ(1,J).LT.NUG) GOTO 3                                         
J2=J2+QZ(1,J)                                                     
J3=J3+1                                                           
3 CONTINUE                                                          
RERL=IP/100.0                                                     
IP=1                                                              
IF((J3+0.0)/NT.GT.RERL.OR.(J2+0.0)/J1.GT.RERL) IP=-1              
IF(IP.GE.0) GOTO 4                                                
C     ***** PRINT FOR CHECK:                                            
PRINT 37                                                          
PRINT 100                                                         
DO 29 J=1,NA                                                      
J1=A(10,J)                                                        
J2=A(11,J)+J1-1                                                   
IF(J2.LT.J1) GOTO 29                                              
PRINT 105                                                         
PRINT 101,(A(I,J),I=17,26),A(2,J),A(9,J)                          
J5=0                                                              
DO 30 J3=J1,J2                                                    
J5=J5+1                                                           
J4=S(1,J3)                                                        
D1=A(6,J4)/10.                                                    
D2=A(7,J4)/10.                                                    
D3=A(8,J4)/10.                                                    
L=(A(3,J4)*1000+A(4,J4))*1000+A(5,J4)                             
PRINT 102,J5,(A(I,J4),I=17,26),S(2,J3),L,D1,D2,D3,               
1A(2,J4),A(9,J4)                                                   
30 CONTINUE                                                          
29 CONTINUE                                                          
PRINT 105                                                         
C     *****PRINT FOR CHECK:                                             
10 I1=0                                                              
PRINT 38                                                          
PRINT 107                                                         
DO 32 J=1,NT                                                      
IF(TQ(3,J).EQ.I1) GOTO 33                                         
PRINT 108                                                         
JJ=0                                                              
I1=TQ(3,J)                                                        
NK=I1/1000                                                        
NN=I1-NK*1000                                                     
NK=NK+NN                                                          
J1=0                                                              
DO 35 J2=NN,NK                                                    
J1=J1+1                                                           
J3=A(1,J2)                                                        
35 Q(J1)=A(2,J3)                                                     
36 PRINT 104,A(9,J3),(Q(J2),J2=1,J1)                                 
33 J1=TQ(1,J)                                                        
IO=UG(1,J1)*10000+TQ(2,J)                                         
R=QZ(2,J)/100.0                                                   
T=QZ(1,J)/100.0                                                   
JJ=JJ+1                                                           
PRINT 103,JJ,IO,TQC(2,J),T,R                                      
32 CONTINUE                                                          
PRINT 108                                                         
4 RETURN                                                            
37 FORMAT(//7X,'CE–ˆ”ˆKA–ˆŸ ‡AKA‡A:')                               
38 FORMAT(//7X,'MA4˜P“TH›E KAPT› ‡AKA‡A:')                           
100 FORMAT(//5X,92(1H-)/5X,8H:      :,20X,8H:      :,15X,41H: PA‡MEP› 
1‡AƒOTOBOK (MM)  :     :       :/5X,52H:HOMEP :    OO‡HA—EHˆE     
2:KO‹-BO:     ˜ˆ”P      :,25(1H-),15H:HOMEP: HOMEP :/5X,8H: /  :,
320X,64H:      :   MATEPˆA‹A   : „‹ˆHA : ˜ˆPˆHA  :TO‹™ˆHA: „ET.:M/K
4APT›:)                                                            
101 FORMAT(5X,8H:COPKA:,10A2,8H:      :,15X,27H:       :         :   
1    !,I5,3H!  ,I5,1H!)                                            
102 FORMAT(5X,2H! ,I4,2H !,10A2,2H! ,I5,1H!,I15,1H!,F7.1,1H!,F9.1,1H!,
1F7.1,1H!,I5,3H!  ,I5,1H!)                                         
103 FORMAT(5X,1H!,55X,2H! ,I3,3H ! ,I8,4H   !,I5,12H !      !   ,F6.2,
14H   !,F6.2,3H  !)                                                
104 FORMAT(5X,10H: M/KAPTA ,I4,10H „‹Ÿ „ET.:,5I6/(5X,1H:,23X,5I6))    
105 FORMAT(5X,8H!------!,20(1H-),8H!------!,15(1H-),41H!-------!------
1---!-------!-----!-------!)                                       
107 FORMAT(//61X,56(1H-)/61X,56H:HOMEP:    ˜ˆ”P    :KO‹-BO:PA‡PŸ„:TP“„
1OEMKOCTœ:PAC–EHKA:/61X,56H: / :OOP“„OBAHˆŸ:      :PAOT›:   (H/
2—.)   : (P“.) :)                                                 
108 FORMAT(5X,1H!,55(1H-),56H!-----!------------!------!------!-------
1-----!--------!)                                                  
END                                                               
SUBROUTINE SVODCA(NT,TQ,QZ,NU,NW,IW,UW,IRAS,ITRZ,IRCZ)            
INTEGER*2 TQ(6,1)                                                 
INTEGER QZ(4,1)  ,IW(1)                                           
INTEGER UW(2,1)                                                   
IRAS=0                                                            
DO 1 J=1,NU                                                       
1 IW(J)=0                                                           
DO 2 I=1,NT                                                       
J=TQ(1,I)                                                         
IRAS=IRAS+QZ(2,I)                                                 
2 IW(J)=IW(J)+QZ(1,I)                                               
DO 3 I=1,NW                                                       
IF(UW(1,I).EQ.170501) IW(UW(2,I))=IW(UW(2,I))+ITRZ                
3 CONTINUE                                                          
c      IRAS=(IRAS+IRCZ+50)/100                                           
iras=iras+ircz
RETURN                                                            
END                                                               
SUBROUTINE TCLZKZ(NF,LIMT,NU,NUG,NA,NT,U,UG,A,TQ,TQC,QZ,EC,NE,NK,N
1KMAX,ITRZ,IRCZ)                                                   
INTEGER*2 A(26,200),TQ(6,1),TQC(4,1)                                
INTEGER*2 UG(20,1)                                                
INTEGER*2 EC(3,1)                                                 
INTEGER U(2,1)                                                    
INTEGER QZ(4,1)                                                   
integer*1 F1/1/,NKMAX,JFM
1 FORMAT(I1,20X,I2,2I2,I2,I6,I5,I5,I9)                              
NE=0                                                              
A(1,1)=1                                                          
TQ(1,1)=NUG                                                       
TQ(2,1)=0                                                         
TQ(3,1)=1                                                         
TQ(4,1)=-1                                                        
TQ(5,1)=-1                                                        
TQ(6,1)=-1                                                        
QZ(1,1)=1                                                         
QZ(2,1)=0                                                         
TQC(1,1)=-1000                                                    
TQC(2,1)=1                                                        
TQC(3,1)=0                                                        
TQC(4,1)=-1                                                       
NT=1                                                              
NT1=1                                                             
IB=0                                                              
nk=nk+1
22  READ(NF,1,END=10,ERR=99) JFM,MK,MK1,JT1,JTM,IOB,IQ,ITR,IRC        
IF(JFM.NE.F1) GOTO 1000                                           
IF(JTM.NE.1)  GOTO 12                                             
C     *****ARRANGEMENT:                                                 
IF(MK.NE.1) GOTO 11                                               
IF(IOB.EQ.170501) ITRZ=ITR                                        
IF(IOB.EQ.170501) IRCZ=IRC                                        
IF(IOB.EQ.170501) ITR=1                                           
IF(IOB.EQ.170501) IRC=0                                           
11 NT2=NT1+1                                                         
DO 13 J=1,NA                                                      
IF(A(13,J).GT.0.OR.A(9,J).NE.MK) GOTO 13                          
14 NT1=NT1+1                                                         
A(1,NT1)=J                                                        
A(12,J)=NT+1                                                      
13 CONTINUE                                                          
IF(NT1.GE.NT2) GOTO 19                                            
17 PRINT 101,NK,MK                                                   
21 NK=NK+1                                                           
IF(NK.GT.NKMAX) GOTO 10                                           
READ(NF,1,ERR=10) JFM,MK,MK1,JT1,JTM,IOB,IQ,ITR,IRC               
IF(JFM.NE.F1) GOTO 10                                             
IF(JTM.EQ.1) GOTO 11                                              
GOTO 21                                                           
19 IB=(NT1-NT2)*1000+NT2                                             
12 IF(IB.LE.0) GOTO 17                                               
24 NT=NT+1                                                           
c      IF(NT.GT.LIMT) STOP 21                                            
28 DO 30 J1=NT2,NT1                                                  
J2=A(1,J1)                                                        
30 A(13,J2)=A(13,J2)+1                                               
29 TQ(1,NT)=NUG                                                      
TQ(2,NT)=IOB-IOB/10000*10000                                      
TQ(3,NT)=IB                                                       
TQ(4,NT)=-1                                                       
TQ(5,NT)=-1                                                       
TQ(6,NT)=-1                                                       
TQC(1,NT)=-1000                                                   
TQC(3,NT)=0                                                       
TQC(4,NT)=-1                                                      
IF(MK1*JT1.LE.0) GOTO32                                           
NE=NE+1                                                           
EC(1,NE)=NT                                                       
EC(2,NE)=MK1                                                      
EC(3,NE)=JT1                                                      
32 CONTINUE                                                          
DO 31 J1=1,NU                                                     
IF(IOB.EQ.U(1,J1)) TQ(1,NT)=U(2,J1)                               
31 CONTINUE                                                          
TQC(2,NT)=IQ                                                      
QZ(1,NT)=ITR                                                      
QZ(2,NT)=IRC                                                      
151 NK=NK+1                                                           
GOTO 22                                                           
99 WRITE(*,150)  MK,JTM
150 FORMAT(1X,'®è¨¡ª  çâ¥­¨ï',2I3)
goto 151
1000 NK=NK+1
10 RETURN                                                            
101 FORMAT(/5X,7H‹OK NO,I4,8H M/KAPTA,I4,15H HE OPAOTAHA.)         
END                                                               
SUBROUTINE STRPRM(LIMS,NA,NS,NXZ,A,S,P,IS)                        
INTEGER*2 A(26,1),S(2,1),P(2,1),W1(100),W2(100)                   
INTEGER*2 NXZ(10),D1/'AA'/,NOMA,A17(10)                           
INTEGER P4(200)                                                   
LOGICAL*1 ZAK(2),OBOZ(20),PROB/' '/                               
character*1 zakn(2),an/'€'/
EQUIVALENCE (NOMA,ZAKn(1)),(A17,OBOZ)                              
DO 14 J=2,26                                                      
A(J,1)=0                                                          
IF(J.GT.16) A(J,1)=NXZ(J-16)                                      
14 CONTINUE                                                          
A(10,1)=NS+1                                                      
A(12,1)=1                                                         
A(13,1)=1                                                         
NP=0                                                              
DO 11 JA=2,NA                                                     
A(15,JA)=0                                                        
IP=NP+1                                                           
DO 12 J=2,NA                                                      
J1=A(10,J)                                                        
J2=A(11,J)+J1-1                                                   
IF(J1.GT.J2) GOTO 12                                              
DO 13 JS=J1,J2                                                    
IF(S(1,JS).NE.JA) GOTO 13                                         
A(14,JA)=IP                                                       
A(15,JA)=A(15,JA)+1                                               
NP=NP+1                                                           
P(1,NP)=J                                                         
P(2,NP)=S(2,JS)                                                   
13 CONTINUE                                                          
12 CONTINUE                                                          
IF(A(15,JA).GT.0) GOTO 11                                         
NS=NS+1                                                           
C      IF(NS.GT.LIMS) STOP 31                                            
S(1,NS)=JA                                                        
15 S(2,NS)=IS                                                        
A(11,1)=A(11,1)+1                                                 
NP=NP+1                                                           
A(14,JA)=NP                                                       
A(15,JA)=1                                                        
P(1,NP)=1                                                         
P(2,NP)=IS                                                        
11 CONTINUE                                                          
DO 1 J1=1,NA                                                      
1 A(16,J1)=A(15,J1)                                                 
J0=1                                                              
J=0                                                               
2 J=J+1                                                             
W1(J)=J0                                                          
W2(J)=A(11,J0)                                                    
J1=A(14,J0)                                                       
J2=A(15,J0)+J1-1                                                  
JM=-1                                                             
IF(J2.LT.J1) GOTO 3                                               
JM=0                                                              
DO 4 J3=J1,J2                                                     
J4=P(1,J3)                                                        
P4(J3)=-P(2,J3)*A(16,J4)                                          
4 JM=JM-P4(J3)                                                      
3 A(16,J0)=JM                                                       
5 W2(J)=W2(J)-1                                                     
IF(W2(J).LT.0) GOTO 6                                             
J0=A(10,J0)+W2(J)                                                 
J0=S(1,J0)                                                        
A(16,J0)=A(16,J0)-1                                               
IF(A(16,J0).LE.0) GOTO 2                                          
7 J0=W1(J)                                                          
GOTO 5                                                            
6 J=J-1                                                             
IF(J.GT.0) GOTO 7                                                 
DO 25 K=2,NA                                                      
NOMA=A(17,K)                                                      
ZAK(2)=ZAK(1)                                                     
c     IF(NOMA.NE.D1) GOTO 29                                            
if(zakn(1).ne.an) goto 29
P4(A(14,K))=P4(A(14,K))/IS                                        
c      DO 26 I=1,10                                                      
c   26 A17(I)=A(16+I,K)                                                  
c      DO 27 I=1,19                                                      
c   27 OBOZ(I)=OBOZ(I+1)                                                 
c      OBOZ(20)=PROB                                                     
c      DO 28 I=1,10                                                      
c   28 A(16+I,K)=A17(I)                                                  
29 IF(P4(A(14,K)).GT.32000) P(2,A(14,K))=32000                       
IF(P4(A(14,K)).LE.32000) P(2,A(14,K))=P4(A(14,K))                 
25 CONTINUE                                                          
RETURN                                                            
END                                                               
SUBROUTINE SGOKIZ(N,NT,A,S,P,TQ,QZ,TQC,U)                         
INTEGER*2 A(26,1),S(2,1),P(2,1),TQ(6,1),TQC(4,1),W1(100),W2(100)  
INTEGER*2 U(20,1)                                                 
INTEGER QZ(4,1)                                                   
IND=1                                                             
C     *****LATE STARTS:                                                 
DO 1 J=1,N                                                        
1 A(16,J)=A(11,J)                                                   
J=0                                                               
J0=1                                                              
JT=1                                                              
2 J=J+1                                                             
NN=TQ(3,J0)                                                       
NK=NN/1000                                                        
W2(J)=NK+1                                                        
NN=NN-NK*1000                                                     
W1(J)=NN                                                          
NK=NN+NK                                                          
JN=0                                                              
JM=-1                                                             
IF(J.LE.1) GOTO 3                                                 
DO 21 J7=1,2                                                      
DO 4 J1=NN,NK                                                     
J4=A(1,J1)                                                        
J2=A(14,J4)                                                       
J3=A(15,J4)+J2-1                                                  
DO 5 J4=J2,J3                                                     
J5=P(1,J4)                                                        
J5=A(12,J5)                                                       
IF(J5.EQ.J0) GOTO 5                                               
J6=TQ(4,J5)                                                       
IF(J6.LT.0) GOTO 6                                                
IF(J7.GT.1) GOTO 42                                               
QZ(4,J5)=1                                                        
GOTO 5                                                            
42 IF(QZ(4,J5).LT.0) GOTO 5                                          
QZ(4,J5)=-1                                                       
IF(J6.GT.JM) JM=J6                                                
JN=JN+QZ(3,J5)                                                    
5 CONTINUE                                                          
4 CONTINUE                                                          
21 CONTINUE                                                          
3 J1=J0+JT                                                          
DO 7 J2=1,JT                                                      
J1=J1-1                                                           
QZ(3,J1)=JN+J2                                                    
JM=JM+1                                                           
TQ(6,J1)=JM                                                       
J3=TQ(1,J1)                                                       
JM=JM+U(6,J3)                                                     
TQ(5,J1)=JM                                                       
R=(QZ(1,J1)+0.0)/U(2,J3)                                          
J4=INT(R)                                                         
IF(R-J4.LE.0.0) J4=J4-1                                           
JM=JM+J4                                                          
7 TQ(4,J1)=JM                                                       
8 W2(J)=W2(J)-1                                                     
IF(W2(J).LT.0) GOTO 6                                             
11 J0=NN+W2(J)                                                       
J1=A(1,J0)                                                        
9 A(16,J1)=A(16,J1)-1                                               
IF(A(16,J1).LT.0) GOTO 8                                          
J0=A(10,J1)+A(16,J1)                                              
J0=S(1,J0)                                                        
JT=A(13,J0)                                                       
J0=A(12,J0)                                                       
IF(TQ(4,J0)) 2,9,9                                                
6 J=J-1                                                             
IF(J.LE.0) GOTO 10                                                
NN=W1(J)                                                          
GOTO 11                                                           
C     *****EMPTY BLOCKS CANCELLATION:                                   
10 J=J+1                                                             
31 IF(J.GT.N) GOTO 12                                                
J2=A(13,J)                                                        
J1=A(12,J)+J2-1                                                   
IF(TQ(6,J1).GE.0) GOTO 10                                         
IF(IND.GT.0) PRINT 100                                            
IND=-1                                                            
NN=TQ(3,J1)                                                       
NK=NN/1000                                                        
NN=NN-NK*1000                                                     
NK=NN+NK                                                          
J3=0                                                              
DO 13 J4=NN,NK                                                    
J3=J3+1                                                           
J5=A(1,J4)                                                        
W1(J3)=J5                                                         
13 W2(J3)=A(2,J5)                                                    
PRINT 101,A(9,J),(W2(J4),J4=1,J3)                                 
DO 14 J4=1,N                                                      
J5=0                                                              
J6=A(1,J4)                                                        
DO 15 J7=1,J3                                                     
IF(J6-W1(J7)) 16,16,15                                            
15 J5=J5+1                                                           
16 J7=J4                                                             
IF(J7.GT.NK) J7=J7-J3                                             
14 A(1,J7)=J6-J5                                                     
DO 17 J4=1,NT                                                     
J5=J4                                                             
IF(J4.LE.J1) GOTO 18                                              
J5=J4-J2                                                          
DO 19 J6=1,4                                                      
TQ(J6,J5)=TQ(J6,J4)                                               
QZ(J6,J5)=QZ(J6,J4)                                               
19 TQC(J6,J5)=TQC(J6,J4)                                             
TQ(5,J5)=TQ(5,J4)                                                 
TQ(6,J5)=TQ(6,J4)                                                 
18 J6=TQ(3,J5)                                                       
J7=J6-J6/1000*1000                                                
IF(J7.GT.NK) J6=J6-J3                                             
17 TQ(3,J5)=J6                                                       
J5=-1                                                             
J6=0                                                              
NK=J3+1                                                           
DO 20 J7=1,NK                                                     
J4=J5+2                                                           
J5=N                                                              
IF(J7.LT.NK) J5=W1(J7)-1                                          
IF(J4.GT.J5) GOTO 20                                              
DO 22 J8=J4,J5                                                    
J6=J6+1                                                           
IF(J6.GE.J8) GOTO 23                                              
DO 24 J9=2,26                                                     
24 A(J9,J6)=A(J9,J8)                                                 
23 IF(A(12,J6).GT.J1) A(12,J6)=A(12,J6)-J2                           
K1=A(10,J6)                                                       
K2=A(11,J6)+K1-1                                                  
IF(K1.GT.K2) GOTO 25                                              
K3=K1-1                                                           
DO 26 K4=K1,K2                                                    
K5=0                                                              
K6=S(1,K4)                                                        
DO 27 K7=1,J3                                                     
IF(W1(K7)-K6) 27,26,28                                            
27 K5=K5+1                                                           
28 K3=K3+1                                                           
S(1,K3)=K6-K5                                                     
S(2,K3)=S(2,K4)                                                   
26 CONTINUE                                                          
A(11,J6)=K3-K1+1                                                  
25 K1=A(14,J6)                                                       
K2=A(15,J6)+K1-1                                                  
IF(K1.GT.K2) GOTO 22                                              
DO 29 K4=K1,K2                                                    
K5=0                                                              
K6=P(1,K4)                                                        
DO 30 K7=1,J3                                                     
IF(K6-W1(K7)) 29,29,30                                            
30 K5=K5+1                                                           
29 P(1,K4)=K6-K5                                                     
22 CONTINUE                                                          
20 CONTINUE                                                          
N=N-J3                                                            
NT=NT-J2                                                          
GOTO 31                                                           
C     *****PARAMETERS OF TAILS:                                         
12 DO 32 J=1,N                                                       
32 A(16,J)=A(11,J)                                                   
J=0                                                               
J0=1                                                              
33 J=J+1                                                             
TQC(4,J0)=-2                                                      
NN=TQ(3,J0)                                                       
NK=NN/1000                                                        
W2(J)=NK+1                                                        
NN=NN-NK*1000                                                     
W1(J)=NN                                                          
34 W2(J)=W2(J)-1                                                     
IF(W2(J).LT.0) GOTO 35                                            
36 J0=NN+W2(J)                                                       
J1=A(1,J0)                                                        
37 A(16,J1)=A(16,J1)-1                                               
IF(A(16,J1).LT.0) GOTO 34                                         
J2=A(10,J1)+A(16,J1)                                              
J2=S(1,J2)                                                        
J0=A(12,J2)                                                       
IF(TQC(4,J0)+1) 37,33,33                                          
35 J1=A(1,NN)                                                        
J0=A(12,J1)                                                       
JT=A(13,J1)+J0-1                                                  
NK=NN+TQ(3,J0)/1000                                               
JN=0                                                              
JM=0                                                              
DO 43 J7=1,2                                                      
DO 38 J1=NN,NK                                                    
J3=A(1,J1)                                                        
J2=A(10,J3)                                                       
J3=A(11,J3)+J2-1                                                  
IF(J2.GT.J3) GOTO 38                                              
DO 39 J4=J2,J3                                                    
J5=S(1,J4)                                                        
J6=A(12,J5)                                                       
IF(J6.EQ.J0) GOTO 39                                              
IF(J7.GT.1) GOTO 44                                               
TQC(1,J6)=-2000                                                   
GOTO 39                                                           
44 IF(TQC(1,J6).GT.-1500) GOTO 39                                    
TQC(1,J6)=-1000                                                   
J6=J6+A(13,J5)-1                                                  
IF(TQC(3,J6).GT.JM) JM=TQC(3,J6)                                  
JN=JN+QZ(4,J6)                                                    
39 CONTINUE                                                          
38 CONTINUE                                                          
43 CONTINUE                                                          
DO 40 J1=J0,JT                                                    
JM=JM+TQ(4,J1)-TQ(6,J1)+1                                         
JN=JN+1                                                           
TQC(3,J1)=JM                                                      
40 QZ(4,J1)=JN                                                       
J=J-1                                                             
IF(J.LE.0) GOTO 41                                                
NN=W1(J)                                                          
GOTO 36                                                           
41 RETURN                                                            
100 FORMAT(/5X,31HB O—EPE„HOCTˆ MAP˜P“TOB- –ˆK‹›;/5X,34Hˆ‡'ŸT› C‹E„“ž™
1ˆE MAP˜P“TH›E KAPT›:)                                             
101 FORMAT(/5X,7HM/KAPTA,I3,18H, CO„EP†ˆT „ETA‹ˆ:,10I5/(33X,10I5))    
END                                                               
SUBROUTINE SGRLZE(NI,NA,NT,NU,A,S,P,TQ,QZ,TQC,U,IP,IW)            
INTEGER IW(100)                                                   
INTEGER*2 A(26,1),S(2,1),P(2,1),TQ(6,1),TQC(4,1),T1(500),T2(500)  
INTEGER*2 U(20,1)                                                 
INTEGER QZ(4,1)                                                   
DO 15 J=1,NA                                                      
15 A(16,J)=A(11,J)                                                   
NU1=NU-1                                                          
NI=0                                                              
1 NI=NI+1                                                           
IPRK=0                                                            
DO 2 JU=1,NU1                                                     
IF(IP.EQ.1.AND.IW(JU).LT.(U(4,JU)*U(3,JU)+50)/100) GOTO 2         
IF(IP.NE.1.AND.IW(JU).LT.U(2,JU)) GOTO 2                          
IPRK=1                                                            
IND=-1                                                            
M=0                                                               
DO 3 J=1,NT                                                       
J1=TQ(4,J)                                                        
IF(J1.LT.NI) GOTO 3                                               
IND=1                                                             
J2=TQ(5,J)                                                        
IF(J2.GT.NI.OR.TQ(1,J).NE.JU) GOTO 3                              
M=M+1                                                             
IF(M.GT.500) STOP 51                                              
T1(M)=J                                                           
T2(M)=0                                                           
3 CONTINUE                                                          
IF(IND.LT.0) GOTO 4                                               
IF(M.LE.0) GOTO 2                                                 
LT=U(2,JU)                                                        
LG=LT                                                             
IF(IP.LE.3.AND.LT.LE.(U(3,JU)*U(4,JU)+50)/100) LG=(U(3,JU)*U(4,JU)
1+50)/100                                                          
DO 6 J1=1,M                                                       
JQ=LT                                                             
J=T1(J1)                                                          
J2=TQ(5,J)                                                        
IF(J2.GE.NI) JQ=QZ(1,J)-(TQ(4,J)-J2)*LT                           
6 LG=LG-JQ                                                          
IF(LG.GE.0) GOTO 2                                                
DO 7 J0=1,M                                                       
DO 12 J1=1,M                                                      
IF(T2(J1)) 13,13,12                                               
12 CONTINUE                                                          
13 JM=J1                                                             
I=T1(J1)                                                          
DO 8 J2=J1,M                                                      
IF(T2(J2).GT.0) GOTO 8                                            
J=T1(J2)                                                          
IF(TQC(3,I)-TQC(3,J)) 8,9,10                                      
9 IF(QZ(4,I)-QZ(4,J)) 8,11,10                                       
11 IF(QZ(1,I)-QZ(1,J)) 10,8,8                                        
10 JM=J2                                                             
I=J                                                               
8 CONTINUE                                                          
J2=TQ(5,I)                                                        
T2(JM)=NI-J2+1                                                    
JQ=LT                                                             
IF(J2.GE.NI) JQ=QZ(1,I)-(TQ(4,I)-J2)*LT                           
LG=LG+JQ                                                          
IF(LG.GE.0) GOTO 14                                               
7 CONTINUE                                                          
14 DO 5 J=1,M                                                        
J2=T2(J)                                                          
IF(J2.LE.0) GOTO 5                                                
J1=T1(J)                                                          
CALL SGCORR(J1,J2,A,S,TQ)                                         
5 CONTINUE                                                          
2 CONTINUE                                                          
IF(IPRK.EQ.0) GOTO 4                                              
GOTO 1                                                            
4 DO 16 J=1,NA                                                      
16 A(16,J)=-1                                                        
RETURN                                                            
END                                                               
SUBROUTINE SGCORR(JT,ND,A,S,TQ )                                  
INTEGER*2 A(26,1),S(2,1),TQ(6,1),W1(100),W2(100)                  
I=TQ(6,JT)+ND                                                     
IPR=0                                                             
J=0                                                               
1 J=J+1                                                             
NN=TQ(3,JT)                                                       
NK=NN/1000                                                        
W2(J)=NK+1                                                        
NN=NN-NK*1000                                                     
W1(J)=NN                                                          
J1=A(1,NN)                                                        
J0=A(12,J1)                                                       
J2=JT+1                                                           
DO 2 J1=J0,JT                                                     
J2=J2-1                                                           
J3=I-TQ(6,J2)                                                     
IPR=0                                                             
IF(J3.LE.0) GOTO 3                                                
IF(TQ(6,J2-1).EQ.TQ(4,J2)) IPR=1                                  
DO 4 J4=4,6                                                       
4 TQ(J4,J2)=TQ(J4,J2)+J3                                            
IF(IPR.EQ.1) GOTO 2                                               
I=TQ(4,J2)+1                                                      
2 CONTINUE                                                          
5 W2(J)=W2(J)-1                                                     
IF(W2(J).LT.0) GOTO 7                                             
10 J1=NN+W2(J)                                                       
J1=A(1,J1)                                                        
6 A(16,J1)=A(16,J1)-1                                               
IF(A(16,J1).LT.0) GOTO 5                                          
J2=A(10,J1)+A(16,J1)                                              
J2=S(1,J2)                                                        
J3=A(12,J2)                                                       
IF(J3.EQ.J0) GOTO 6                                               
JT=A(13,J2)+J3-1                                                  
GOTO 1                                                            
7 NK=NN+NK                                                          
DO 8 J1=NN,NK                                                     
J2=A(1,J1)                                                        
8 A(16,J2)=A(11,J2)                                                 
3 J=J-1                                                             
IF(J.LE.0) GOTO 9                                                 
NN=W1(J)                                                          
J1=A(1,NN)                                                        
J0=A(12,J1)                                                       
I=TQ(4,J0)+1                                                      
NK=TQ(3,J0)/1000                                                  
GOTO 10                                                           
9 RETURN                                                            
END                                                               
