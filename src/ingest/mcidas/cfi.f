C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.
C *** McIDAS Revision History ***
C 1 CFI.FOR 27-Feb-90,9:52:44,`SSEC' PC-McIDAS ver 5.00
C 2 CFI.FOR 24-Sep-90,18:19:14,`SMG' First Release into COMmon
C 3 CFI.FOR 1-Apr-94,1:02:46,`BARRYR' Add proprietary statement
C 4 CFI.FOR 2-May-94,16:25:08,`USER' Released
C 5 CFI.FOR 10-Sep-96,9:21:02,`BILLL' Added programmer documentation
C      (6920).
C 6 CFI.FOR 23-Sep-96,12:34:06,`USER' Released
C *** McIDAS Revision History ***
C $ CFI(I)  (RJL)                                                       
C $ CONVERT INTEGER TO CHARACTER*12. RETURNED VALUE IS DIGITS RIGHT-    
C $   JUSTIFIED WITH LEADING BLANKS.                                    
C $ I = (I) INPUT  VALUE TO BE CONVERTED                                
C $$ CFI = CONVERT, INTEGER, CHARACTER                                  
C-----NOTE: (THIS ROUTINE DESIGNED FOR MACHINES WITH 36- OR FEWER       
C        BIT WORD.)                                                     
C                                                                       

*$ Name:
*$      cfi     - Converts an integer to a character*12, right
*$                justified, with leading blanks.
*$
*$ Interface:
*$      character*12 function
*$      cfi(integer i)
*$
*$ Input:
*$      i       - Number to be converted.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      Converted number.
*$
*$ Remarks:
*$      none
*$
*$ Categories:
*$      text
*$      utility
*$      converter

      FUNCTION CFI(I)                                                   
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
