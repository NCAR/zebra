C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.
      FUNCTION NVXINI(IFUNC,IPARMS)                                     
C *** McIDAS Revision History ***
C 1 NVXRECT.DLM 24-Apr-90,7:07:48,`SSEC' PC-McIDAS ver 5.00
C 2 NVXRECT.DLM 27-Sep-90,8:22:16,`SMG' First Release into COMmon
C 3 NVXRECT.DLM 12-Feb-93,8:54:52,`KENB' took out SPOS opt in NVXOPT (3362)
C 4 NVXRECT.DLM 12-Mar-93,11:02:28,`USER' Released
C 5 NVXRECT.DLM 1-Apr-94,1:33:26,`BARRYR' Add proprietary statement
C 6 NVXRECT.DLM 2-May-94,17:15:44,`USER' Released
C *** McIDAS Revision History ***
      DIMENSION IPARMS(*)                                               
      REAL*8 DRAD,DECC                                                  
      CHARACTER*4 CLIT                                                  
      COMMON/RCTCOM/XROW,XCOL,ZSLAT,ZSLON,ZDLAT,ZDLON,ITYPE,IWEST       
      IF (IFUNC.EQ.1) THEN                                              
         IF (IPARMS(1).NE.LIT('RECT')) GO TO 900                        
         ITYPE=1                                                        
         XROW=IPARMS(2)                                                 
         IPOWLAT=IPARMS(12)                ! <<<<< UPC add 951228 >>>>>
         IF (IPOWLAT .EQ. 0) IPOWLAT=4     ! <<<<< UPC add 951228 >>>>>
         ZSLAT=IPARMS(3)/10.**IPOWLAT      ! <<<<< UPC mod 951228 >>>>>
         XCOL=IPARMS(4)                                                 
         IPOWLON=IPARMS(13)                ! <<<<< UPC add 951228 >>>>>
         IF (IPOWLON .EQ. 0) IPOWLON=4     ! <<<<< UPC add 951228 >>>>>
         ZSLON=IPARMS(5)/10.**IPOWLON      ! <<<<< UPC mod 951228 >>>>>
         IPOWDLIN=IPARMS(14)               ! <<<<< UPC add 951228 >>>>>
         IF (IPOWDLIN .EQ. 0) IPOWDLIN=4   ! <<<<< UPC add 951228 >>>>>
         ZDLAT=IPARMS(6)/10.**IPOWDLIN     ! <<<<< UPC mod 951228 >>>>>
         IPOWDELE=IPARMS(15)               ! <<<<< UPC add 951228 >>>>>
         IF (IPOWDELE .EQ. 0) IPOWDELE=4   ! <<<<< UPC add 951228 >>>>>
         ZDLON=IPARMS(7)/10.**IPOWDELE     ! <<<<< UPC mod 951228 >>>>>
         IPOWRAD=IPARMS(16)                ! <<<<< UPC add 951228 >>>>>
         IF (IPOWRAD .EQ. 0) IPOWRAD=3     ! <<<<< UPC add 951228 >>>>>
         DRAD=IPARMS(8)/10.D0**IPOWRAD     ! <<<<< UPC mod 951228 >>>>>
         R=DRAD                                                         
         IPOWECC=IPARMS(17)                ! <<<<< UPC add 951228 >>>>>
         IF (IPOWECC .EQ. 0) IPOWECC=6     ! <<<<< UPC add 951228 >>>>>
         DECC=IPARMS(9)/10.D0**IPOWECC     ! <<<<< UPC mod 951228 >>>>>
         IWEST=IPARMS(11)                                               
         IF(IWEST.GE.0) IWEST=1                                         
         CALL LLOPT(DRAD,DECC,IWEST,IPARMS(10))                         
C-----------                                                            
C                                                                       
         XLIN=1                                                         
         XELE=1                                                         
         XLDIF=XROW-XLIN                                                
         XEDIF=IWEST*(XCOL-XELE)                                        
         XLON=ZSLON+XEDIF*ZDLON                                         
         XLAT=ZSLAT+XLDIF*ZDLAT                                         
CCC      IF(XLAT.GT.90. .OR. XLAT.LT.-90.) GO TO 900                    
CCC      IF(XLON.LT.-180.) THEN                                         
CCC         XLON=XLON+360.                                              
CCC         IF(XLON.LT.-180.) GO TO 900                                 
CCC      ENDIF                                                          
CCC      IF(XLON.GT.180.) THEN                                          
CCC         XLON=XLON-360.                                              
CCC         IF(XLON.GT.180.) GO TO 900                                  
CCC      ENDIF                                                          
         ZSLAT=XLAT                                                     
         ZSLON=XLON                                                     
         XROW=1                                                         
         XCOL=1                                                         
      ELSE IF (IFUNC.EQ.2) THEN                                         
         IF(INDEX(CLIT(IPARMS(1)),'XY').NE.0) ITYPE=1                   
         IF(INDEX(CLIT(IPARMS(1)),'LL').NE.0) ITYPE=2                   
      ENDIF                                                             
      NVXINI=0                                                          
      RETURN                                                            
900   CONTINUE                                                          
      NVXINI=-1                                                         
      RETURN                                                            
      END                                                               
      FUNCTION NVXSAE(XLIN,XELE,XDUM,XLAT,XLON,Z)                       
      COMMON/RCTCOM/XROW,XCOL,ZSLAT,ZSLON,ZDLAT,ZDLON,ITYPE,IWEST       
      XLDIF=XROW-XLIN                                                   
      XEDIF=IWEST*(XCOL-XELE)                                           
      XLON=ZSLON+XEDIF*ZDLON                                            
      XLAT=ZSLAT+XLDIF*ZDLAT                                            
      IF(XLAT.GT.90. .OR. XLAT.LT.-90.) GO TO 900                       
      IF(XLON.LT.-180.) THEN                                            
         XLON=XLON+360.                                                 
         IF(XLON.LT.-180.) GO TO 900                                    
      ENDIF                                                             
      IF(XLON.GT.180.) THEN                                             
         XLON=XLON-360.                                                 
         IF(XLON.GT.180.) GO TO 900                                     
      ENDIF                                                             
      IF(ITYPE.EQ.1) THEN                                               
         YLAT=XLAT                                                      
         YLON=XLON                                                      
         CALL LLCART(YLAT,YLON,XLAT,XLON,Z)                             
      ENDIF                                                             
      NVXSAE=0                                                          
      RETURN                                                            
900   CONTINUE                                                          
      NVXSAE=-1                                                         
      RETURN                                                            
      END                                                               
      FUNCTION NVXEAS(ZLAT,ZLON,Z,XLIN,XELE,XDUM)                       
      COMMON/RCTCOM/XROW,XCOL,ZSLAT,ZSLON,ZDLAT,ZDLON,ITYPE,IWEST       
      XLAT=ZLAT                                                         
      XLON=ZLON                                                         
      IF(ITYPE.EQ.1) THEN                                               
         X=XLAT                                                         
         Y=XLON                                                         
         CALL CARTLL(X,Y,Z,XLAT,XLON)                                   
      ENDIF                                                             
C                                                                       
C COMMENTED OUT PENDING BETTER INFO                                     
C                                                                       
C     IF(IWEST.EQ.1.AND.XLON.GT.ZSLON) XLON=XLON-360.                   
      IF(IWEST.EQ.-1.AND.XLON.LT.ZSLON) XLON=XLON+360.                  
      XLIN=XROW-(XLAT-ZSLAT)/ZDLAT                                      
      XELE=XCOL-(XLON-ZSLON)/(ZDLON*IWEST)                              
      NVXEAS=0                                                          
      RETURN                                                            
      END                                                               
      FUNCTION NVXOPT(IFUNC,XIN,XOUT)                                   
      COMMON/RCTCOM/XROW,XCOL,ZSLAT,ZSLON,ZDLAT,ZDLON,ITYPE,IWEST       
      REAL*4 XIN(*),XOUT(*)                                             
      CHARACTER*4 CLIT,CFUNC                                            
C                                                                       
C IFUNC= 'SPOS'    SUBSATELLITE LAT/LON                                 
C                                                                       
C        XIN - NOT USED                                                 
C        XOUT - 1. STANDARD LATITUDE                                    
C             - 2. NORMAL LONGITUDE                                     
C                                                                       
C                                                                       
C IFUNC= 'ORAD'  OBLATE RADIUS                                          
C                                                                       
C        XIN - LATITUDE                                                 
C        XOUT - RADIUS IN KM                                            
C                                                                       
      CFUNC=CLIT(IFUNC)                                                 
      NVXOPT=0                                                          
      IF(CFUNC.EQ.'SPOS') THEN                                          
ccc         XOUT(1)=ZSLAT
ccc         XOUT(2)=ZSLON
C
C--- NO SAT. POSITION FOR THIS PROJECTION
C
         NVXOPT = -1
C
      ELSE IF(CFUNC.EQ.'ORAD') THEN                                     
         CALL LLOBL(XIN,XOUT)                                           
      ELSE                                                              
         NVXOPT=1                                                       
      ENDIF                                                             
      RETURN                                                            
      END                                                               
