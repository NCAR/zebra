C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.
      FUNCTION NVXINI(IFUNC,IPARMS)                                     
C *** McIDAS Revision History ***
C 1 NVXRADR.DLM 19-Apr-90,7:54:52,`SSEC' PC-McIDAS ver 5.00
C 2 NVXRADR.DLM 26-Sep-90,12:58:22,`SMG' First Release into COMmon
C 3 NVXRADR.DLM 1-Apr-94,1:33:18,`BARRYR' Add proprietary statement
C 4 NVXRADR.DLM 2-May-94,17:15:38,`USER' Released
C *** McIDAS Revision History ***
      DIMENSION IPARMS(*)                                               
      CHARACTER*4 CLIT                                                  
      COMMON/RADROM/XROW,XCOL,XLAT,XLON,XROT,XBLAT,XBLON,ITYPE          
      INCLUDE 'hex80.inc'
      DATA RAD/.01745329/,R/6371.23/                                    
      DATA MISS/HEX80/
      IF (IFUNC.EQ.1) THEN                                              
         IF (IPARMS(1).NE.LIT('RADR')) THEN                             
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
         IF(IPARMS(8).NE.0.AND.IPARMS(8).NE.MISS)                       
     *         YSPACE=IPARMS(8)/1000.                                   
         XROT=-RAD*IPARMS(7)/1000.                                      
         XBLAT=R*RAD/XSPACE                                             
         XBLON=RAD*R/YSPACE                                             
      ELSE IF (IFUNC.EQ.2) THEN                                         
         IF(INDEX(CLIT(IPARMS(1)),'XY').NE.0) ITYPE=1                   
         IF(INDEX(CLIT(IPARMS(1)),'LL').NE.0) ITYPE=2                   
      ENDIF                                                             
      NVXINI=0                                                          
      RETURN                                                            
      END                                                               
      FUNCTION NVXSAE(XLIN,XELE,XDUM,YLAT,YLON,Z)                       
      COMMON/RADROM/XROW,XCOL,XLAT,XLON,XROT,XBLAT,XBLON,ITYPE          
      DATA RAD/.01745329/                                               
      XLDIF=XROW-XLIN                                                   
      XEDIF=XCOL-XELE                                                   
      XDIS=SQRT(XLDIF*XLDIF+XEDIF*XEDIF)                                
      IF(XDIS.GT. 0.001) THEN                                           
         XANGL=ATAN2(XLDIF,XEDIF)-90.*RAD                               
         XANGE=ATAN2(XLDIF,XEDIF)+90.*RAD                               
         XLDIF=XDIS*COS(XROT+XANGL)                                     
         XEDIF=XDIS*SIN(XROT+XANGE)                                     
      ENDIF                                                             
      YLAT=XLAT+XLDIF/XBLAT                                             
      YLON=XLON+XEDIF/XBLON/COS(YLAT*RAD)                               
CCC                                                                     
CCC   IF(XRLON.GT.180.) XRLON=XRLON-360.                                
CCC                                                                     
      IF(ITYPE.EQ.1) THEN                                               
         ZLAT=YLAT                                                      
         ZLON=YLON                                                      
         CALL NLLXYZ(ZLAT,ZLON,YLAT,YLON,Z)                             
      ENDIF                                                             
      NVXSAE=0                                                          
      RETURN                                                            
      END                                                               
      FUNCTION NVXEAS(YLAT,YLON,Z,XLIN,XELE,XDUM)                       
      COMMON/RADROM/XROW,XCOL,XLAT,XLON,XROT,XBLAT,XBLON,ITYPE          
      DATA RAD/.01745329/                                               
      ZLAT=YLAT                                                         
      ZLON=YLON                                                         
      IF(ITYPE.EQ.1) THEN                                               
         X=YLAT                                                         
         Y=YLON                                                         
         CALL NXYZLL(X,Y,Z,ZLAT,ZLON)                                   
      ENDIF                                                             
      XRLON=ZLON-XLON                                                   
      XRLAT=ZLAT-XLAT                                                   
CCC                                                                     
CCC   IF(XRLON.GT.180.) XRLON=XRLON-360.                                
CCC                                                                     
      XLDIF=XBLAT*XRLAT                                                 
      XEDIF=XRLON*XBLON*COS(ZLAT*RAD)                                   
      XDIS=SQRT(XLDIF*XLDIF+XEDIF*XEDIF)                                
      IF(XDIS.GT. .001) THEN                                            
         XANGL=ATAN2(XLDIF,XEDIF)-90.*RAD                               
         XANGE=ATAN2(XLDIF,XEDIF)+90.*RAD                               
         XLDIF=XDIS*COS(-XROT+XANGL)                                    
         XEDIF=XDIS*SIN(-XROT+XANGE)                                    
      ENDIF                                                             
      XLIN=XROW-XLDIF                                                   
      XELE=XCOL-XEDIF                                                   
      NVXEAS=0                                                          
      RETURN                                                            
      END                                                               
      FUNCTION NVXOPT(IFUNC,XIN,XOUT)                                   
      COMMON/RADROM/XROW,XCOL,XLAT,XLON,XROT,XBLAT,XBLON,ITYPE          
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

