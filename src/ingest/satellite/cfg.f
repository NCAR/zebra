      FUNCTION CFG(XVALU)                                               
C *** McIDAS Revision History ***
C 1 CFG.FOR 23-Apr-90,20:39:44,`SSEC' PC-McIDAS ver 5.00
C 2 CFG.FOR 24-Sep-90,18:19:42,`SMG' First Release into COMmon
C *** McIDAS Revision History ***
C $ CFG(XVALUE)  (TMW)                                                  
C $ CONVERT REAL*4 TO CHARACTER*12 IN GENERAL FORMAT                    
C $ XVALU = (R*4) INPUT  REAL*4 TO CONVERT                              
C $$ CFG = CONVERT, REAL, CHARACTER                                     
      CHARACTER*12 CZEROS,CFD,CFI,CFG,CVAL,CVAL2
      REAL *8 DVAL
      DATA CZEROS/'.00000000000'/                                       
      X=ABS(XVALU)                                                      
      XMX=999999.                                                       
      IDEC=6                                                            
      DVAL=X                                                            
      IF (X.GT.XMX .OR. (X.LT.0.01.AND.X.NE.0.0)) THEN                  
         CVAL2=CFD(DVAL,IDEC)                                           
         CVAL=CVAL2(2:)                                                 
      ELSE                                                              
         CALL DPSINO(IDEC,DVAL,NCAL,IVAL,ISTAT)                         
C IF NCAL>0 THEN = DDD IN 'SSS.DDD'                                     
C IF NCAL<0 THEN =-DDD IN SSSSSSDDD.                                    
         IF (IVAL.EQ.0) THEN                                            
            CVAL2='0'                                                   
            GOTO 333                                                    
         ENDIF                                                          
         CVAL2=CFI(IVAL)                                                
         CVAL=' '                                                       
         IF (NCAL.GT.5) THEN                                            
            CVAL=CZEROS(1:NCAL-5)//CVAL2(7:12)                          
         ELSE IF (NCAL.LT.0) THEN                                       
            CVAL=CVAL2(7:12)//CZEROS(2:2-NCAL)//'.'                     
         ELSE IF (NCAL.EQ.0) THEN                                       
            CVAL=CVAL2(7:12)//'.'                                       
         ELSE                                                           
            CVAL=CVAL2(7:12-NCAL)//'.'//CVAL2(13-NCAL:12)               
         ENDIF                                                          
         CVAL2=CVAL                                                     
         DO 200 I=1,12                                                  
         IB=I                                                           
         IF (CVAL2(IB:IB).NE.'0'.AND.CVAL2(IB:IB).NE.' ') GOTO 300      
200      CONTINUE                                                       
300      DO 201 I=1,12                                                  
         IE=13-I                                                        
         IF (CVAL2(IE:IE).NE.'0'.AND.CVAL2(IE:IE).NE.' ') GOTO 301      
201      CONTINUE                                                       
301      IF (CVAL2(IE:IE).EQ.'.') IE=IE-1                               
         IF (IE.LT.IB) IB=IE                                            
         CVAL=CVAL2(IB:IE)                                              
      ENDIF                                                             
      CVAL2=CVAL                                                        
      IF (XVALU.LT.0.0) CVAL2='-'//CVAL                                 
333   CFG=CVAL2                                                         
      RETURN                                                            
      END                                                               
