C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.
C *** McIDAS Revision History ***
C 1 NVXSIN.DLM 9-Nov-92,14:30:52,`KENB' Initial release (3357)
C 2 NVXSIN.DLM 23-Nov-92,8:44:20,`RICKK' First Release
C 3 NVXSIN.DLM 23-Nov-92,8:45:12,`USER' Released
C 4 NVXSIN.DLM 1-Apr-94,1:33:30,`BARRYR' Add proprietary statement
C 5 NVXSIN.DLM 2-May-94,17:15:52,`USER' Released
C 6 NVXSIN.DLM 16-Aug-94,22:01:16,`BETHA' Modified for Solaris (4731)
C 7 NVXSIN.DLM 22-Aug-94,6:50:24,`USER' Released
C 8 NVXSIN.DLM 11-Mar-96,8:14:04,`DAVES' Fix for X,Y,Z transform (5702)
C 9 NVXSIN.DLM 17-Apr-96,15:07:58,`USER' Released
C *** McIDAS Revision History ***

      FUNCTION NVXINI(IFUNC,IPARMS)
      DOUBLE PRECISION DRAD, DECC
      DIMENSION IPARMS(*)                                               
      CHARACTER*4 CLIT                                                  
      COMMON/SINOM/XROW,XCOL,XLAT,XLON,XBLAT,XBLON,ITYPE,IWEST
      INCLUDE 'hex80.inc'
      DATA RAD/.01745329/
      IF (IFUNC.EQ.1) THEN                                              
         IF (IPARMS(1).NE.LIT('SIN ')) THEN
            NVXINI=-1                                                   
            RETURN                                                      
         ENDIF                                                          
         ITYPE=1                                                        
         XROW=IPARMS(2)                                                 
         XCOL=IPARMS(3)                                                 
         XLAT=FLALO(IPARMS(4))                                          
         XLON=FLALO(IPARMS(5))
         XSPACE=IPARMS(6)/1000.                                         
         YSPACE=XSPACE                                                  
         DRAD=IPARMS(7)/1000.D0
         R=DRAD
         DECC=IPARMS(8)/1.D6
         IWEST=IPARMS(10)
         IF(IWEST.GE.0) IWEST=1
         CALL LLOPT(DRAD,DECC,IWEST,IPARMS(9))
         XBLAT=R*RAD/XSPACE                                             
         XBLON=RAD*R/YSPACE                                             
      ELSE IF (IFUNC.EQ.2) THEN                                         
         IF(INDEX(CLIT(IPARMS(1)),'XY').NE.0) ITYPE=1                   
         IF(INDEX(CLIT(IPARMS(1)),'LL').NE.0) ITYPE=2                   
      ENDIF                                                             
      NVXINI=0                                                          
      RETURN                                                            
      END                                                               
C
      FUNCTION NVXSAE(XLIN,XELE,XDUM,YLAT,YLON,Z)                       
      COMMON/SINOM/XROW,XCOL,XLAT,XLON,XBLAT,XBLON,ITYPE,IWEST
      DATA RAD/.01745329/
      XLDIF=XROW-XLIN
      XEDIF=XCOL-XELE
      XDIS=SQRT(XLDIF*XLDIF+XEDIF*XEDIF)
      IF(XDIS.GT. 0.001) THEN
         XANGL=ATAN2(XLDIF,XEDIF)-90.*RAD
         XANGE=ATAN2(XLDIF,XEDIF)+90.*RAD
         XLDIF=XDIS*COS(XANGL)
         XEDIF=XDIS*SIN(XANGE)
      ENDIF
C
C--- Compute latitude/longitude
C
      YLAT=XLAT+XLDIF/XBLAT                                             
      YLON=XLON+IWEST*XEDIF/XBLON/COS(YLAT*RAD)
C
C--- Gross error check
C
      IF((ABS(YLAT).GT.90.) .OR. (ABS(YLON).GT.180.)) THEN
         NVXSAE = -1
      ELSE
         NVXSAE=0
      ENDIF
C
C--- Convert to X,Y,Z if requested
C
      IF(ITYPE.EQ.1) THEN                                               
         ZLAT=YLAT                                                      
         ZLON=YLON                                                      
         CALL LLCART(ZLAT,ZLON,YLAT,YLON,Z)
      ENDIF
C
      RETURN                                                            
      END                                                               
C
      FUNCTION NVXEAS(YLAT,YLON,Z,XLIN,XELE,XDUM)                       
      COMMON/SINOM/XROW,XCOL,XLAT,XLON,XBLAT,XBLON,ITYPE,IWEST
      DATA RAD/.01745329/
      ZLAT=YLAT                                                         
      ZLON=YLON                                                         
      IF(ITYPE.EQ.1) THEN                                               
         X=YLAT                                                         
         Y=YLON                                                         
         CALL CARTLL(X,Y,Z,ZLAT,ZLON)
      ENDIF                                                             
      XRLON=IWEST*(ZLON-XLON)
      IF(XRLON .GT. 180.) XRLON = XRLON - 360.
      IF(XRLON .LT.-180.) XRLON = XRLON + 360.
      XRLAT=ZLAT-XLAT                                                   

      XLDIF=XBLAT*XRLAT
      XEDIF=XRLON*XBLON*COS(ZLAT*RAD)
      XDIS=SQRT(XLDIF*XLDIF+XEDIF*XEDIF)
      IF(XDIS.GT. .001) THEN
         XANGL=ATAN2(XLDIF,XEDIF)-90.*RAD
         XANGE=ATAN2(XLDIF,XEDIF)+90.*RAD
         XLDIF=XDIS*COS(XANGL)
         XEDIF=XDIS*SIN(XANGE)
      ENDIF
C
C--- CONVERT BACK TO TV COORD. FROM SINUSOIDAL EQUAL AREA PROJECTION
C
      XLIN=XROW-XLDIF
      XELE=XCOL-XEDIF
      NVXEAS=0                                                          
      RETURN                                                            
      END                                                               
C
      FUNCTION NVXOPT(IFUNC,XIN,XOUT)                                   
      COMMON/SINOM/XROW,XCOL,XLAT,XLON,XBLAT,XBLON,ITYPE,IWEST
      REAL*4 XIN(*),XOUT(*)                                             
      CHARACTER*4 CLIT,CFUNC                                            
C                                                                       
C IFUNC= 'SPOS'    SUBSATELLITE LAT/LON                                 
C                                                                       
C        XIN - NOT USED                                                 
C        XOUT - 1. STANDARD LATITUDE                                    
C             - 2. NORMAL LONGITUDE                                     
      CFUNC=CLIT(IFUNC)                                                 
      NVXOPT=0                                                          
      IF(CFUNC.EQ.'SPOS') THEN                                          
         XOUT(1)=XLAT                                                   
         XOUT(2)=XLON                                                   
      ELSE                                                              
         NVXOPT=1                                                       
      ENDIF                                                             
      RETURN                                                            
      END                                                               

