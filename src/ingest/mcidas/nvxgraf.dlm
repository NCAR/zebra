C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.

C *** McIDAS Revision History ***
C 1 NVXGRAF.DLM 19-Apr-90,7:47:56,`SSEC' PC-McIDAS ver 5.00
C 2 NVXGRAF.DLM 1-May-90,13:50:16,`DAS' Removed DDEST statement
C 3 NVXGRAF.DLM 27-Sep-90,8:22:10,`SMG' First Release into COMmon
C 4 NVXGRAF.DLM 11-Mar-91,9:34:30,`SMG' remove continuations from common blocks
C 5 NVXGRAF.DLM 11-Mar-91,9:34:30,`ERICN' Released
C 6 NVXGRAF.DLM 26-Mar-92,16:54:10,`JOHNP' fixed slon= for e.hemi,fixed many
C       other bugs in p.s. proj
C 7 NVXGRAF.DLM 23-Jul-92,12:31:22,`USER' Released
C 8 NVXGRAF.DLM 1-Apr-94,1:32:54,`BARRYR' Add proprietary statement
C 9 NVXGRAF.DLM 2-May-94,17:15:08,`USER' Released
C 10 NVXGRAF.DLM 3-Aug-94,16:31:06,`DWS' Modified for Solaris (4731)
C 11 NVXGRAF.DLM 22-Aug-94,6:44:26,`USER' Released for McIDAS-X Only
C *** McIDAS Revision History ***

      FUNCTION NVXINI(IFUNC,IPARMS)                                     
      DIMENSION IPARMS(*)
      CHARACTER*4 CLIT,CPRO
      COMMON/GRCOM/ITYPE,LIMS(4),XLL(4)                                 
      REAL*4 PARMS(4)                                                   
      NVXINI=-1
      IF (IFUNC.EQ.1) THEN                                              
         IF (IPARMS(1).NE.LIT('GRAF')) RETURN                           
         ITYPE=1                                                        
         CPRO=CLIT(IPARMS(21))                                          
         DO 1 I=1,4                                                     
         LIMS(I)=IPARMS(I+25)                                           
         XLL(I)=IPARMS(I+21)/10000.                                     
         IF(I.LT.4) PARMS(I)=IPARMS(I+29)/10000.                        
1        CONTINUE                                                       
         PARMS(4)=IPARMS(33)                                            
         IF(MAPDEF(XLL,LIMS,CPRO,PARMS).NE.0) RETURN                    
      ELSE IF (IFUNC.EQ.2) THEN                                         
         IF(INDEX(CLIT(IPARMS(1)),'XY').NE.0) ITYPE=1                   
         IF(INDEX(CLIT(IPARMS(1)),'LL').NE.0) ITYPE=2                   
C      CALL DDEST ('NVXINI - PROJ '//CLIT(IPARMS(21)),0)
      ENDIF                                                             
      NVXINI=0                                                          
      RETURN                                                            
      END                                                               
      FUNCTION NVXSAE(XLIN,XELE,XDUM,XLAT,XLON,Z)                       
      COMMON/GRCOM/ITYPE,LIMS(4),XLL(4)                                 
      IL=XLIN+LIMS(1)                                                   
      IE=XELE+LIMS(3)                                                   
      NVXSAE=IYXLL(IL,IE,XLAT,XLON)                                     
      IF(XLON.GT.180) XLON=XLON-360.                                    
      IF(XLON.LT.-180) XLON=XLON+360.                                   
      IF(ITYPE.EQ.1) THEN                                               
         YLAT=XLAT                                                      
         YLON=XLON                                                      
         CALL NLLXYZ(YLAT,YLON,XLAT,XLON,Z)                             
      ENDIF                                                             
      RETURN                                                            
      END                                                               
      FUNCTION NVXEAS(ZLAT,ZLON,Z,XLIN,XELE,XDUM)                       
      COMMON/GRCOM/ITYPE,LIMS(4),XLL(4)                                 
      XLAT=ZLAT                                                         
      XLON=ZLON                                                         
      IF(ITYPE.EQ.1) THEN                                               
         X=XLAT                                                         
         Y=XLON                                                         
         CALL NXYZLL(X,Y,Z,XLAT,XLON)                                   
      ENDIF                                                             
      NVXEAS=LLYX(XLAT,XLON,IL,IE)                                      
      XLIN=IL-LIMS(1)                                                   
      XELE=IE-LIMS(3)                                                   
      RETURN                                                            
      END                                                               
      FUNCTION NVXOPT(IFUNC,XIN,XOUT)                                   
      COMMON/GRCOM/ITYPE,LIMS(4),XLL(4)                                 
      REAL*4 XIN(*),XOUT(*)                                             
      CHARACTER*4 CLIT,CFUNC                                            
C                                                                       
C IFUNC= 'SPOS'    SUBSATELLITE LAT/LON                                 
C                                                                       
C        XIN - NOT USED                                                 
C        XOUT - 1. CENTER LATITUDE                                      
C             - 2. CENTER LONGITUDE                                     
      CFUNC=CLIT(IFUNC)                                                 
      NVXOPT=0                                                          
      IF(CFUNC.EQ.'SPOS') THEN                                          
         XOUT(1)=(XLL(1)+XLL(2))/2.                                     
         XOUT(2)=(XLL(3)+XLL(4))/2.                                     
      ELSE                                                              
         NVXOPT=1                                                       
      ENDIF                                                             
      RETURN                                                            
      END                                                               
      FUNCTION MAPDEF(XLL,LIMS,CTYP,PARMS)                              
      IMPLICIT INTEGER (A,B,E-W)                                        
      IMPLICIT CHARACTER*12 (C)                                         
      IMPLICIT REAL*8 (D)                                               
      REAL LA1,LA2,LO1,LO2,SLAT1,SLAT2,SLON,SCALE
      CHARACTER*(*) CTYP                                                
      DIMENSION XLL(4),LIMS(4)                                          
      REAL*4 PARMS(4)                                                   
      COMMON /LALO/   LA1,LA2,LO1,LO2                                   
      COMMON /SCRDAT/ BL,EL,BE,EE
      COMMON /FRMDAT/ BLUPL,BLUPE,BLDNL,BLDNE,
     &                REFIML,REFIME,REFTVL,REFTVE,JDAY,TIME
      COMMON /CONDAT/ SLAT1,SLAT2,SLON,SCALE
C  /LALO/    AT INPUT :  LAT/LON FOR LOWER LEFT (MN) & UPPER RIGHT (MX) 
C                        CORNER OF SCREEN                               
C            LATER :     MIN & MAX OF LAT/LON                           
C  /SCRDAT/  SCREEN DATA                                                
C  /FRMDAT/  IMAGE FRAME DATA                                           
C  /CONDAT/  INPUT DATA FOR CONFORMAL PROJECTION                        
      LA1=XLL(1)                                                        
      LA2=XLL(2)                                                        
      LO1=XLL(3)                                                        
      LO2=XLL(4)                                                        
      BL=LIMS(1)                                                        
      EL=LIMS(2)                                                        
      BE=LIMS(3)                                                        
      EE=LIMS(4)                                                        
      IF(LO2.LE.LO1) LO2=LO2+360.                                       
      SLAT1=PARMS(1)                                                    
      SLAT2=PARMS(2)                                                    
      SLON=PARMS(3)                                                     
      SCALE=PARMS(4)                                                    
      IF (CTYP.EQ.'SAT ') GO TO 110                                     
      MAPDEF=PROPRP(CTYP)
      SLON = PARMS(3)
      RETURN                                                            
110   CONTINUE                                                          
      MAPDEF=-1                                                         
      RETURN                                                            
      END                                                               
C                                                                       
      FUNCTION PROPRP (PROJ)                                            
C  PROJECTION PREPARATION. PROPRP=0  OK,  -1  WRONG PROJECTION          
                                                                        
      INTEGER PROPRP,BL,EL,BE,EE,IPROJ,TV0L,TV0E,TVL,TVE,
     &        TL1,TL2,TL3,TE1,TE2,TE3,IH,H
C  IH     HEMISPHERE INDEX (1 NORTH, -1 SOUTH)                          
C  IPROJ  1 SAT, 2 CONF, 3 MERC                                         
C  TV0L,TV0E   SCALED CARTESIAN COORDINATES OF CONFORMAL PROJECTION AT  
C              UPPER LEFT CORNER OF TV SCREEN (PANEL)                   
C  TL1,TL2,TL3,TE1,TE2,TE3 UNSCALED CARTESIAN COORDINATES               
      REAL LA1,LA2,LO1,LO2,SLAT1,SLAT2,SLON,SCALE,FAC,POW,DTR,RDIS,     
     &     SCLAT1,SCLAT2,SFL,SFE,SPIX,PRD,LA,LO,D,C,LAC,LOC
C  RDIS      REFERENCE DISTANCE ON EARTH ALONG THE STANDARD MERIDIAN AT 
C            STANDARD LATITUDE. EQUAL TO 2 DEG (IN RADIANS) MULTIPLIED  
C            BY AVERAGE EARTH RADIUS (6371.221E5 CM)                    
C  FAC       CONSTANT FACTOR IN THE CONFORMAL PROJECTION FORMULA        
C  POW       POWER IN THE CONFORMAL PROJECTION FORMULA                  
C  PRD       PROJECTED REFERENCE DISTANCE (RDIS)                        
C  DTR       DEGREES TO RADIANS CONVERSION FACTOR (PI/180)              
C  SCLAT1,SCLAT2   STANDARD COLATITUDES                                 
C  SFL,SFE   SCALE FACTORS FOR LINE & ELEMENT                           
C  SPIX      SIZE OF PIXEL IN CM                                        
      CHARACTER*4 PROJ
      COMMON /LALO/   LA1,LA2,LO1,LO2                                   
      COMMON /SCRDAT/ BL,EL,BE,EE
      COMMON /PRODAT/ FAC,POW,SFL,SFE,IPROJ,TV0L,TV0E,IH
      COMMON /CONDAT/ SLAT1,SLAT2,SLON,SCALE
      DATA DTR /.01745329/ SPIX /.05783/ RDIS /222.398E5/               
                                                                        
      PROPRP=0
      IF (PROJ.EQ.'CONF') THEN                                          
        IPROJ=2                                                         
        IH=SIGN(1.,SLAT1+SLAT2)
        IF (IH .EQ. 0)IH = 1
        IF ((IH.EQ.1.AND.LA1.GT.-89..AND.LA2.GT.-89.).OR.(IH.EQ.-1.AND.
     &      (LA1.LT.89..OR.LA1.EQ.361.).AND.LA2.LT.89.)) GOTO 5
          CALL EDEST ('PROJECTION NOT AVAILABLE',0)                     
          GOTO 130
 5      SCLAT1=(90.-IH*SLAT1)*DTR                                       
        SCLAT2=(90.-IH*SLAT2)*DTR                                       
        POW=1.                                                          
        IF (NINT(SLAT1-SLAT2).EQ.0) GOTO 10                             
        POW=ALOG(SIN(SCLAT1)/SIN(SCLAT2))                               
     &     /ALOG(TAN(.5*SCLAT1)/TAN(.5*SCLAT2))
10      CONTINUE
        IF (ABS(SCLAT1).LE.0.02) THEN
          FAC=2.                                                        
        ELSE                                                            
          FAC=SIN(SCLAT1)/POW/(TAN(.5*SCLAT1)**POW)                     
        ENDIF

C STANDARD LONGITUDE - DEFAULT COMPUTATION
        IF (SLON.EQ.361.) THEN
          LO1=AMOD(LO1+900.,360.)-180.
          LO2=AMOD(LO2+900.,360.)-180.
          IF (LO2.LT.LO1)LO2=LO2+360.
          D=LO2-LO1
          H=1
          IF (D.GT.180.) THEN
            D=360.-D
            H=-1
          ENDIF
          D=D*POW
          DO 20 C=.2,50.,.2
            VALUE=COTAN(C*DTR)-COTAN((D-C)*DTR)-2.
            IF (VALUE.LT.0.) GOTO 30
 20       CONTINUE
 30       CONTINUE
          SLON=LO1+IH*C/POW
          IF (H*IH.EQ.-1) SLON=LO2+IH*(C/POW-180.)
          SLON=AMOD(SLON,180.)
 
C              THIS LITTLE IF BLOCK IS A SPECIAL CASE FOR THE SOUTHERN
C              HEMISPHERE
 
          IF (IH.LT.0)THEN
              SUM=LO1+LO2
 
C              IF THE REGION IS TOTALLY IN THE EASTERN HEMISPHERE
 
              IF (SUM.LT.0)THEN
                 SLON=SLON-180.
 
C              IF THE REGION IS MOSTLY IN THE EASTERN HEMISPHERE AND
C              INCLUDES THE DATELINE
 
              ELSEIF (SUM.GT.0. .AND. LO2.GT. 180.)THEN
                 SLON=SLON-180.
              ENDIF
          ENDIF
 
        ENDIF
        SFL=100000.                                                     
        SFE=100000.                                                     
        TV0L=0                                                          
        TV0E=0                                                          
        II= LLYX (LA2,LO2,TL2,TE2)                                      
        IF (SCALE.NE.0.) GOTO 50                                        
        II= LLYX (LA1,LO1,TL1,TE1)                                      
        IF (TL1-TL2.EQ.0.OR.TE1-TE2.EQ.0) GOTO 40                       
        SFL=SFL*(EL-BL)/(TL1-TL2)                                       
        SFE=SFE*(EE-BE)/(TE1-TE2)                                       
        IF (SFL.GT.0.AND.SFE.GT.0.) GOTO 60                             
 40       CALL EDEST ('PROPRP:DATA INCONSISTENCY-CHECK LAT,LON,SLON'
     *                //',SLAT',0)
          GOTO 130
 50     LA=.5*(SLAT1+SLAT2)                                             
        IF (ABS(LA).GT.89.) LA=IH*89.                                   
        II= LLYX (LA+1.,SLON,TL1,TE1)                                   
        II= LLYX (LA-1.,SLON,TL3,TE3)                                   
        PRD=ABS(TL1-TL3)/100000.                                        
        SFL=RDIS/(SCALE*SPIX*PRD)                                       
        SFE=SFL                                                         
 60     TV0L=SFL*(TL2-BL)/100000.                                       
        TV0E=SFE*(TE2-BE)/100000.

C TEST FOR A POLE ON SCREEN
70      CONTINUE
        IOK = LLYX (IH*90.,0.,TVL,TVE)
        IF (TVL.LT.BL.OR.TVL.GT.EL.OR.TVE.LT.BE.OR.TVE.GT.EE) GOTO 80
           LO1=-180.
           LO2=180.
           IF (IH.EQ.1) LA2=90.
           IF (IH.EQ.-1) LA1=-90.
C EXPAND THE RANGE
80      CONTINUE

        DO 110 TVL=BL-10,EL+10,(EL-BL)/20
        DO 110 TVE=BE-10,EE+10,(EE-BE)/20
           IF (IYXLL (TVL,TVE,LA,LO).NE.0) GOTO 110
           IF (LO2.LE.180.) GOTO 90
           IF (LO.GT.LO1) GOTO 100
           IF (LO+360.-LO2.LT.LO1-LO) LO=LO+360.
 90        LO1=AMIN1(LO1,LO)
           LO2=AMAX1(LO2,LO)
 100       LA1=AMIN1(LA1,LA)
           LA2=AMAX1(LA2,LA)
 110    CONTINUE
C ADD 10% SAFETY MARGIN
        LAC=(LA2-LA1)*.1
        LOC=(LO2-LO1)*.1
        LA1=AMAX1(-90.,LA1-LAC)
        LA2=AMIN1(90.,LA2+LAC)
        IF (LO2-LO1+2.*LOC.LT.360.) GOTO 120
        LO1=-180.
        LO2=180.
        RETURN
120     LO1=LO1-LOC
        LO2=LO2+LOC
        IF (LO1.LT.-180.) THEN
          LO1=LO1+360.
          LO2=LO2+360.
        ENDIF
        IF (POW.LT.1.AND.LO2.GT.180.) THEN
          LO1=-180.
          LO2=180.
          RETURN
        ENDIF
        IF (LO2-LO1.LT.300.) RETURN
          LO2=LO1+360.
          RETURN
      ENDIF                                                             
      IF (PROJ.EQ.'MERC') THEN                                          
        IPROJ=3                                                         
        SFL=FLOAT(EL-BL)/(LA2-LA1)                                      
        SFE=FLOAT(EE-BE)/(LO2-LO1)                                      
        RETURN                                                          
      ENDIF                                                             
      CALL EDEST ('WRONG PROJECTION',0)                                 
 130  PROPRP=-1
      RETURN                                                            
      END                                                               
                                                                        
                                                                        
      FUNCTION IYXLL (TVL,TVE,LA,LO)                                    
C  TV LIN/ELE TO LAT/LON TRANSFORMATION FOR CONFORMAL PROJECTION        
C  LO RANGE -/+180                                                      
                                                                        
      INTEGER BL,EL,BE,EE,TV0L,TV0E,TVL,TVE,IPROJ,IH,IYXLL              
      REAL SLAT1,SLAT2,SLON,SCALE,FAC,POW,DTR,RADIUS,SFL,SFE,A,B,LA,LO, 
     &     PROANG,LA1,LA2,LO1,LO2                         
      COMMON /LALO/   LA1,LA2,LO1,LO2                                   
      COMMON /SCRDAT/ BL,EL,BE,EE
      COMMON /PRODAT/ FAC,POW,SFL,SFE,IPROJ,TV0L,TV0E,IH
      COMMON /CONDAT/ SLAT1,SLAT2,SLON,SCALE
      DATA DTR /.01745329/                                              
                                                                        
      IYXLL=0                                                           
      GOTO (10,20,30) IPROJ                                             
10    CONTINUE                                                          
C                                                                       
C SATELLITE PROJECTION                                                  
C                                                                       
      CALL EDEST('NOT IMPLEMENTED',0)                                   
      IYXLL=-1                                                          
      RETURN                                                            
20    CONTINUE                                                          
C                                                                       
C CONFORMAL PROJECTION                                                  
C                                                                       
      A=IH*(TVL-BL+TV0L)/SFL
      B=-IH*(TVE-BE+TV0E)/SFE                                           
      IF (A.NE.0..OR.B.NE.0.) GOTO 22                                   
        LA=IH*90.                                                       
        LO=0.                                                           
        RETURN                                                          
 22   RADIUS=SQRT(A*A+B*B)                                              
      LA=IH*(90.-2.*ATAN(EXP(ALOG(RADIUS/FAC)/POW))/DTR)                
      PROANG=ATAN2(B,A)/(IH*DTR)
      IF (POW.LT.1. .AND. ABS(PROANG).GT. 180.*POW)GOTO 26
      GOTO 28
 26     IYXLL=-1
        RETURN
 28   LO=PROANG/POW+SLON
      LO=AMOD(LO+900.,360.)-180.
      RETURN                                                            
30    CONTINUE                                                          
C                                                                       
C MERCATOR PROJECTION                                                   
C                                                                       
      LA=LA2-(TVL-BL)/SFL                                               
      LO=LO2-(TVE-BE)/SFE                                               
      RETURN                                                            
      END                                                               
                                                                        
      FUNCTION LLYX (LA,LO,TVL,TVE)                                     
C  LAT/LON TO TV LIN/ELE TRANSFORMATION FOR SAT,CONF & MERC PROJECTION  
                                                                        
      INTEGER LLYX,TVL,TVE,TV0L,TV0E,BL,BE,EL,EE,IH,
     &        REFIML,REFIME,REFTVL,REFTVE,BLUPL,BLUPE,BLDNL,BLDNE       
      REAL LA,LO,L,LA1,LA2,LO1,LO2,DTR,RELLON,
     &     PROANG,RADIUS,SFL,SFE,SLAT1,SLAT2,SLON,SCALE,POW,FAC         
C  IMLIN,IMELE   IMAGE LIN/ELE FOR A GIVEN LAT/LON                      
C  RELLON  RELATIVE LONGITUDE (=0 WHEN LO=SLON)                         
C  PROANG  PROJECTION ANGLE FOR A GIVEN LONGITUDE                       
C  RADIUS  PROJECTION RADIUS                                            
      COMMON /LALO/   LA1,LA2,LO1,LO2                                   
      COMMON /SCRDAT/ BL,EL,BE,EE
      COMMON /PRODAT/ FAC,POW,SFL,SFE,IPROJ,TV0L,TV0E,IH
      COMMON /FRMDAT/ BLUPL,BLUPE,BLDNL,BLDNE,
     &                REFIML,REFIME,REFTVL,REFTVE,JDAY,TIME             
      COMMON /CONDAT/ SLAT1,SLAT2,SLON,SCALE
      DATA DTR /.01745329/                                              
                                                                        
      LLYX=0                                                            
      L=LO                                                              
      IF (LO.GT.180.) L=LO-360.                                         
      GOTO (10,20,30) IPROJ                                             
C  SAT PROJECTION                                                       
 10   CALL EDEST('INVALID PROJECTION ',0)                               
      GOTO 100                                                          
C  CONF PROJECTION                                                      
 20   RELLON=LO-SLON                                                    
      IF (POW.EQ.1.) GOTO 25                                            
      RELLON=AMOD(RELLON+900.,360.)-180.
 25   PROANG=IH*RELLON*POW*DTR                                          
      RADIUS=FAC*TAN(.5*DTR*(90.-IH*LA))**POW                           
      TVL=BL+IH*NINT(SFL*RADIUS*COS(PROANG))-TV0L
      TVE=BE-IH*NINT(SFE*RADIUS*SIN(PROANG))-TV0E
      GO TO 80                                                          
C  MERC PROJECTION                                                      
 30   IF (LO.LT.LO1.AND.LO2.GT.180.) LO=LO+360.                         
      TVL=BL+NINT((LA2-LA)*SFL)                                         
      TVE=BE+NINT((LO2-LO)*SFE)                                         
      GO TO 80                                                          
 80   CONTINUE                                                          
      IF(TVL.LT.BL.OR.TVL.GT.EL) LLYX=-1                                
      IF(TVE.LT.BE.OR.TVE.GT.EE) LLYX=-1                                
      RETURN                                                            
 100  LLYX=-1                                                           
      RETURN                                                            
      END                                                               

