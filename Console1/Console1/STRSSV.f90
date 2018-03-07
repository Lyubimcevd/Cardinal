SUBROUTINE STRSSV(omoih_buf,NA,NS,A,S,LIMA,LIMS,NK,NKMAX)                
INTEGER*2 A(26,200),S(2,200),H(10),Q(9)                               
integer*1 NKMAX,JFM
NA=2  
NK = 1
NS=0                                                              
J0=1
JFM = 2
do while (JFM == 2)
    READ(NF,1,ERR=15)  JFM,H,Q 
    1 FORMAT(I1,40X,10A2,I3,I2,2I3,3I5,I3,I2)                                                                                                                                    
    !ƒÓ·‡‚ÎÂÌËÂ ÌÓ‚ÓÈ ‰ÂÚ‡ÎË
    A(2,NA)=Q(8)
    DO J=2,7                                                      
        A(J+1,NA)=Q(J)
    enddo
    A(9,NA)=Q(9)  
    A(13,NA)=0   
    DO J=1,10                                                     
        A(J+16,NA)=H(J)  
    enddo                                                                                                            
    A(11,NA)=0                                                         
    A(16,NA)=NK  
    IF(Q(1) > 0) then
        A(11,NA)=A(11,NA)+1 
        NS=NS+1
        S(1,NS)=NA                                                       
        S(2,NS)=Q(1)
        A(10,J0)=NS+1                                     
        NK=NK+1                                                                   
20 IF(A(2,J).EQ.Q(8).OR.A(2,J)*Q(8).LE.0) GOTO 32                    
21 PRINT 100,A(16,J),A(2,J),(A(J1,J),J1=17,26),NK,Q(8),H             
32 IF(A(2,J).LE.0) A(2,J)=Q(8)                                       
IF(A(3,J)+A(4,J)+A(5,J).GT.0) GOTO 25                             
DO 26 J1=2,7                                                      
26 A(J1+1,J)=Q(J1)                                                   
25 IF(A(9,J).LE.0) A(9,J)=Q(9)                                                                                    
J0=J                                                              
IF(A(11,J).GT.0) J0=1                                             
                                                     
A(11,J0)=0                                                        
GOTO 29                                                                                                        
NS=NS+1                                                                                             
S(1,NS)=J                                                         
S(2,NS)=Q(1)                                                      
29 NK=NK+1                                                           
GOTO 28                                                           
15 RETURN                                                            
100 FORMAT(/5X,61HHECOOTBETCTBàE:  ÅãOK NO  HOMEP ÑET.     :---OÅOáHAó
1EHàE----:,2(/25X,I4,7X,I5,5X,10A2))                               
END                                                               
