C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.
      FUNCTION NVXINI(IFUNC,IPARMS)                                     
C *** McIDAS Revision History ***
C 1 NVXMERC.DLM 19-Mar-90,21:50:54,`SSEC' PC-McIDAS ver 5.00
C 2 NVXMERC.DLM 27-Sep-90,8:22:04,`SMG' First Release into COMmon
C 3 NVXMERC.DLM 22-Nov-91,9:19:34,`SUEG' remove bad line
C 4 NVXMERC.DLM 27-Feb-92,12:39:44,`USER' Released for McIDAS-X Only
C 5 NVXMERC.DLM 1-Apr-94,1:33:00,`BARRYR' Add proprietary statement
C 6 NVXMERC.DLM 2-May-94,17:15:16,`USER' Released
C *** McIDAS Revision History ***
      DIMENSION IPARMS(*)
      REAL*8 DRAD,DECC                                                  
      CHARACTER*4 CLIT                                                  
      COMMON/MERCOM/XROW,XCOL,XLAT1,XSPACE,XQLON,XBLAT,XBLON,ITYPE,IWEST
      DATA RAD/.01745329/                                               
      IF (IFUNC.EQ.1) THEN                                              
         IF (IPARMS(1).NE.LIT('MERC')) THEN                             
            NVXINI=-1                                                   
            RETURN                                                      
         ENDIF                                                          
         ITYPE=1                                                        
         XROW=IPARMS(2)                                                 
         XCOL=IPARMS(3)                                                 
         XLAT1=FLALO(IPARMS(4))                                         
         XSPACE=IPARMS(5)/1000.                                         
         XQLON=FLALO(IPARMS(6))                                         
         DRAD=IPARMS(7)/1000.D0                                         
         R=DRAD                                                         
         DECC=IPARMS(8)/1.D6                                            
         IWEST=IPARMS(10)                                               
         IF(IWEST.GE.0) IWEST=1                                         
         CALL LLOPT(DRAD,DECC,IWEST,IPARMS(9))                          
         XBLAT=R*COS(XLAT1*RAD)/XSPACE                                  
         XBLON=RAD*R/XSPACE                                             
      ELSE IF (IFUNC.EQ.2) THEN                                         
         IF(INDEX(CLIT(IPARMS(1)),'XY').NE.0) ITYPE=1                   
         IF(INDEX(CLIT(IPARMS(1)),'LL').NE.0) ITYPE=2                   
      ENDIF                                                             
      NVXINI=0                                                          
      RETURN                                                            
      END                                                               
      FUNCTION NVXSAE(XLIN,XELE,XDUM,XLAT,XLON,Z)                       
      COMMON/MERCOM/XROW,XCOL,XLAT1,XSPACE,XQLON,XBLAT,XBLON,ITYPE,IWEST
      DATA RAD/.01745329/                                               
      XLDIF=XROW-XLIN                                                   
      XEDIF=XCOL-XELE                                                   
      XRLON=IWEST*XEDIF/XBLON                                           
      XLON=XRLON+XQLON                                                  
      XRLAT=ATAN(EXP(XLDIF/XBLAT))                                      
      XLAT=(XRLAT/RAD-45.)*2.+XLAT1                                     
CCC                                                                     
      IF(XLON.GT.180.) XLON=XLON-360.                                   
      IF(XLON.LT.-180.) XLON=XLON+360.                                  
CCC                                                                     
      IF(ITYPE.EQ.1) THEN                                               
         YLAT=XLAT                                                      
         YLON=XLON                                                      
         CALL LLCART(YLAT,YLON,XLAT,XLON,Z)                             
      ENDIF                                                             
      NVXSAE=0                                                          
      RETURN                                                            
      END                                                               
      FUNCTION NVXEAS(ZLAT,ZLON,Z,XLIN,XELE,XDUM)                       
      COMMON/MERCOM/XROW,XCOL,XLAT1,XSPACE,XQLON,XBLAT,XBLON,ITYPE,IWEST
      DATA RAD/.01745329/                                               
      XLAT=ZLAT                                                         
      XLON=ZLON                                                         
      IF(ITYPE.EQ.1) THEN                                               
         X=XLAT                                                         
         Y=XLON                                                         
         CALL CARTLL(X,Y,Z,XLAT,XLON)                                   
      ENDIF                                                             
      XRLON=IWEST*(XLON-XQLON)                                          
CCC                                                                     
      IF(XRLON.GT.180.) XRLON=XRLON-360.                                
      IF(XRLON.LT.-180.) XRLON=XRLON+360.                               
CCC                                                                     
      IF(XLAT.GE.90.) XLAT=89.99                                        
      IF(XLAT.LE.-90.) XLAT=-89.99                                      
      XRLAT=((XLAT-XLAT1)/2.+45.)*RAD                                   
      XLIN=XROW-XBLAT*ALOG(TAN(XRLAT))                                  
      XELE=XCOL-XRLON*XBLON                                             
      NVXEAS=0                                                          
      RETURN                                                            
      END                                                               
      FUNCTION NVXOPT(IFUNC,XIN,XOUT)                                   
      COMMON/MERCOM/XROW,XCOL,XLAT1,XSPACE,XQLON,XBLAT,XBLON,ITYPE,IWEST
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
         XOUT(1)=XLAT1                                                  
         XOUT(2)=XQLON                                                  
      ELSE IF(CFUNC.EQ.'ORAD') THEN                                     
         CALL LLOBL(XIN,XOUT)                                           
      ELSE                                                              
         NVXOPT=1                                                       
      ENDIF                                                             
      RETURN                                                            
      END                                                               
