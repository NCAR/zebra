      FUNCTION CFI(I)                                                   
C *** McIDAS Revision History ***
C 1 CFI.FOR 27-Feb-90,9:52:44,`SSEC' PC-McIDAS ver 5.00
C 2 CFI.FOR 24-Sep-90,18:19:14,`SMG' First Release into COMmon
C *** McIDAS Revision History ***
C $ CFI(I)  (RJL)                                                       
C $ CONVERT INTEGER TO CHARACTER*12. RETURNED VALUE IS DIGITS RIGHT-    
C $   JUSTIFIED WITH LEADING BLANKS.                                    
C $ I = (I) INPUT  VALUE TO BE CONVERTED                                
C $$ CFI = CONVERT, INTEGER, CHARACTER                                  
C-----NOTE: (THIS ROUTINE DESIGNED FOR MACHINES WITH 36- OR FEWER       
C        BIT WORD.)                                                     
C                                                                       
      PARAMETER (LENGTH=12)                                             
      CHARACTER*12 CFI                                                  
C                                                                       
C
      IZERO=ICHAR('0')
      M=IABS(I)                                                         
C-----K ALWAYS POINTS AT THE NEXT POSITION IN THE OUTPUT FIELD,         
C-----   STARTING FROM THE RIGHT.                                       
      DO 2 K=LENGTH,2,-1                                                
         CFI(K:K)=CHAR(MOD(M,10)+IZERO)                                 
         M=M/10                                                         
         IF (M.LE.0) GOTO 3                                             
 2    CONTINUE                                                          
      K=2                                                               
C-----CHECK FOR MINUS SIGN NECESSARY                                    
 3    CONTINUE                                                          
      IF (I.LT.0) THEN                                                  
         K=K-1                                                          
         CFI(K:K)='-'                                                   
      ENDIF                                                             
C-----INSERT LEADING BLANKS                                             
      IF (K.GT.1) CFI(1:K-1)=' '                                        
      RETURN                                                            
      END                                                               
