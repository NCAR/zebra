C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.
      FUNCTION NVXINI (IFUNC,KOD)                                       
C *** McIDAS Revision History ***
C 1 NVXTIRO.DLM 17-Apr-90,7:58:14,`SSEC' PC-McIDAS ver 5.00
C 2 NVXTIRO.DLM 27-Sep-90,9:51:40,`SMG' First Release into COMmon
C 3 NVXTIRO.DLM 11-Mar-91,11:42:30,`SMG' clean up some problems found with AIX
C 4 NVXTIRO.DLM 12-Mar-91,9:37:38,`DAVES' Replaced BLOCK DATA with assigns
C 5 NVXTIRO.DLM 12-Mar-91,9:37:38,`ERICN' Released
C 6 NVXTIRO.DLM 17-May-91,13:29:38,`SMG' fix arithmetic error
C 7 NVXTIRO.DLM 17-May-91,13:29:38,`ERICN' Released
C 8 NVXTIRO.DLM 30-Dec-93,10:47:40,`JMB' f2c compatibility
C 9 NVXTIRO.DLM 24-Jan-94,9:14:12,`USER' Released
C 10 NVXTIRO.DLM 9-Feb-94,12:58:24,`KENB' initialize the NVXOPT routine (4344)
C 11 NVXTIRO.DLM 10-Feb-94,14:11:22,`KENB' rechecked the correct version (4344
C 12 NVXTIRO.DLM 17-Feb-94,10:13:44,`USER' Released
C 13 NVXTIRO.DLM 21-Feb-94,9:08:54,`USER' Released
C 14 NVXTIRO.DLM 21-Feb-94,10:26:40,`DAVES' Fixed error near poles (2816)
C 15 NVXTIRO.DLM 7-Mar-94,9:13:24,`USER' Released
C 16 NVXTIRO.DLM 1-Apr-94,1:33:38,`BARRYR' Add proprietary statement
C 17 NVXTIRO.DLM 2-May-94,17:16:00,`USER' Released
C *** McIDAS Revision History ***
      IMPLICIT REAL*8 (D)                                               
      REAL*8 COSINC, RADDEG, COSINE
      DIMENSION KOD(*)
      REAL*4 OPV(3),TMPVEC(3),TMPRAD
      LOGICAL ASCEND                                                    
      COMMON/NH/ DEPOCH,DDINC,DTINC,COSINC,MODE,NAVMOD,NSPOT,IFLIP(3)   
      COMMON/NH2/DTSPT                                                  
      COMMON/VBL/DEPOBL,BLELEM(6)                                       
      PARAMETER (NSAMPS = 16)                                           
      COMMON/VEC/DSTART,TSTART,TEND,DTSMP,VECS(NSAMPS*3),RADS(NSAMPS),  
     &           ANGBTW(NSAMPS),CSCG(NSAMPS),SATVEC(3),TOTSCN           
      COMMON /VOP/OPV                                                   
      COMMON /LATDAT/ DEQTX, PERIOD, ASCEND

      REAL*8        TTO,R,AE,GM,BJ2,BJ3,BJ4,BJ5,FLTINV,XKE,ESQ
      COMMON/BLCNST/TTO,R,AE,GM,BJ2,BJ3,BJ4,BJ5,FLTINV,XKE,ESQ

      PARAMETER (ERAD = 6378.388)                                       
      PARAMETER (PI = 3.14159265)                                       
      PARAMETER (GK = 0.07436574)               
      PARAMETER (RADDEG = 1.745329252D-2)
      COSINE(ANGLE)=DCOS(RADDEG*ANGLE)                                 
      SINE(ANGLE)=DSIN(RADDEG*ANGLE)                                  
                                                                        
      NVXINI=0                                                          
      NAVMOD=0
C
C--- INITIALIZE BLCNST COMMON
C
      TTO=0.D0
      R=0.D0
      AE=6378.135D0
      GM=398600.8D0
      BJ2=-0.10826158D-02
      BJ3=0.25388100D-05
      BJ4=0.16559700D-05
      BJ5=0.21848266D-06
      FLTINV=298.25D0
      XKE=0.743669161D-01
      ESQ=0.6994317778266721D-02

      IF (IFUNC.EQ.2) GOTO 500                                          
C IMAGE START TIME                                                      
      JDAY=MOD(KOD(2),100000)                                           
      CALL DATCON (JDAY,NY,NM,ND,JYYDDD,AMON)                           
      DSTART=DABTIM (NY,NM,ND,0,0,.001*KOD(48))                         
C EPOCH TIME                                                            
      CALL DATCON (KOD(5),NY,NM,ND,JYYDDD,AMON)                         
      KHOUR=KOD(6)/10000                                                
      KMIN=MOD(KOD(6)/100,100)                                          
      KSEC=MOD(KOD(6),100)                                              
      DEPOCH=DABTIM (NY,NM,ND,KHOUR,KMIN,KSEC+KOD(15)/60000.)           
C ORBITAL PARAMETERS AS NEEDED BY THE BROUWER/LYDDANE MODEL             
C SEMI-MAJOR AXIS, ECCENTRICITY, INCLINATION, RIGHT ASCENSION, ARG OF   
C PERIGEE, MEAN ANOMALY.THE LAST FOUR MUST BE IN RADIANS                
      DEPOBL=DEPOCH                                                     
      BLELEM(1)=.01*KOD(7)                                              
      BLELEM(2)=.000001*KOD(8)                                          
      BLELEM(3)=RADDEG*.001*KOD(9)                                         
      BLELEM(4)=RADDEG*.001*KOD(12)                                        
      BLELEM(5)=RADDEG*.001*KOD(11)                                        
      BLELEM(6)=RADDEG*.001*KOD(10)                                        
C NSPOT= NUMBER OF SPOTS/SCAN                                           
      NSPOT=KOD(13)                                                     
C DDINC= ANGULAR INCREMENT/SPOT                                         
      DDINC=KOD(14)*1.D-6                                               
C DTINC= TIME OF ONE SCAN                                               
      DTINC=KOD(53)*1D-6                                                
      IF (KOD(53).EQ.0) DTINC=KOD(49)*1D-3                              
C DTSPT= TIME BETWEEN SPOTS                                             
      DTSPT=KOD(54)*1.D-8                                               
      IF(KOD(54).EQ.0) DTSPT=DTINC/NSPOT                                
      CALL MOVW (3,KOD(50),IFLIP)                                       
C NUMBER OF LINES ACTUALLY STORED IN THE AREA                           
      TOTSCN=KOD(51)                                                    
C TIME INTERVAL FROM IMAGE LINE 1 TO FIRST AND LAST LINE IN AREA        
      TSTART=DTINC*KOD(47)/86400.                                       
      TEND=DTINC*(TOTSCN+KOD(47))/86400.                                
                                                                        
C COLLECT NSAMPS SATELLITE POSITIONS, COVERING A FULL ORBIT OF 120 MIN  
      DTSMP = (120.0 / 1440.0) / (NSAMPS - 1)                           
      DT = DSTART                                                       
      J = 1                                                             
      FLATMN = 1000.0                                                   
      FLATMX = 0.0
      DO 100 I=1,NSAMPS                                                 
                                                                        
C  JOT DOWN SATELLITE POSITION, AND RADIUS FROM EARTH CENTER            
         CALL SATPSN (DT, VECS(J), FLAT, FLON, RADS(I))                 
C         WRITE (*, 1001) I, DT - DSTART, FLAT, FLON
C1001     FORMAT (I3, 3F9.3)                                            
         IF (FLAT.LT.-90.0) GOTO 900                                    
                                                                        
C  NORMALIZE THE POSITION VECTOR                                        
         CALL VECMUL (1.0 / VECMAG (VECS(J)), VECS(J))                  
                                                                        
C  COMPUTE ANGLE BETWEEN SUCCESSIVE POSITIONS, AND THE COSECANT         
         IF (I.GT.1) THEN                                               
            ANGBTW (I-1) = VECANG (VECS (J), VECS (J - 3))              
            CSCG (I-1) = 1.0 / SINE (ANGBTW (I-1))                      
         ENDIF                                                          
                                                                        
C  KEEP LOOKING FOR CLOSEST POINT TO EQUATOR                            
         IF (ABS (FLAT) .LT. ABS (FLATMN)) THEN                         
            FLATMN = FLAT                                               
            MN = I                                                      
         ENDIF    

C  KEEP LOOKING FOR CLOSEST POINT TO POLE
         IF (ABS (FLAT) .GT. FLATMX) THEN
            FLATMX = ABS(FLAT)
            MX = I
            DTPOLE = DT
         ENDIF
                                                                        
         DT=DT+DTSMP                                                    
         J=J+3                                                          
100   CONTINUE                                                          
                                                                        
C COSINE OF ORBITAL INCLINATION ANGLE                                   
      COSINC=COSINE (.001*KOD(9)) 

C Compute latitude extreme based on inclination angle
 
      FLATI = DABS( DACOS(-COSINC))/RADDEG
 
C Find the latitude extreme for this orbit and minimize the error
C from FLATI
 
      DT = DTPOLE

C---epislon (maximum allowable error)
      DEPSI = 1.D-4

C---threshold for time change
      DTMIN = 1.D-5

C---first guess delta time
      DTD =   1.D-3
 
      DO 200 N = 1, 16
 
         CALL SATPSN (DT, TMPVEC, FLAT, FLON, TMPRAD)

C--- compute error for extreme latitude 
         DIFLAT = DABS(DBLE(FLAT)) - DBLE(FLATI)

C--- note the maximum (note: we allow 16 iterations) 
         IF( ABS(FLAT) .GT. FLATMX ) THEN
            FLATMX = ABS(FLAT)
            DIFMAX = DABS(DIFLAT)
         ENDIF

C--- if error or time change meet criteria, branch out 
         IF(DABS(DIFLAT) .LT. DEPSI) GO TO 210
         IF(DABS(DTD) .LT. DTMIN) GO TO 210
 
         IF( N .EQ. 1) GO TO 190
 
         DERIV = (DIFLAT - DLAST)/DTD
         IF( DABS(DERIV) .LE. 0.D0) GO TO 210
 
         DTD = -DBLE(DIFLAT)/DERIV

C--- if time change is too large, reduce it 
 190     IF(DABS(DTD) .GT. .005D0) DTD = .005D0 * (DTD/DABS(DTD))
 
         DT = DT + DTD
 
 200  DLAST = DIFLAT
 
 210  CONTINUE

C---only use computed max latitude if the difference (as derived from the orbit
C---inclination angle) is small, < .1 deg.
 
        IF(DIFMAX .LT. 1.D-1) THEN
           FLATMX = AMIN1(ABS(FLATMX), FLATI)
        ELSE
           FLATMX = FLATI
        ENDIF
 
        COSINC = -COSINE(FLATMX)
                                                                        
C NORMAL VECTOR TO ORBITAL PLANE                                        
      CALL VECROS (VECS(1),VECS(4),OPV)                                 
      CALL VECMUL (1.0/VECMAG(OPV),OPV)                                 
                                                                        
C ESTIMATE THE PERIOD OF THE SATELLITE (IN DAYS PER DEGREE)             
      SEMIMA = (KOD (7) / 100.0) / ERAD                                 
      PERIOD = (2.0 * PI) * SQRT (SEMIMA * SEMIMA * SEMIMA) / GK        
      PERIOD = PERIOD / 1440.0                                          
      PERIOD = PERIOD / 360.0                                           
                                                                        
C WERE WE ASCENDING AT THE CLOSEST EQUATOR CROSSING?                    
      J = MN                                                            
      IF (J .EQ. 1) J = 2                                               
      J = J * 3                                                         
      ASCEND = (VECS (J) .GT. VECS (J - 3))                             
      IF (.NOT. ASCEND) PERIOD = -PERIOD                                
                                                                        
C REFINE ESTIMATE OF EQUATOR CROSSING                                   
      DEQTX = DSTART + ((MN - 1) * DTSMP) - (FLATMN * PERIOD)           
      DEQTX = DEQTX - (360.0 * ABS (PERIOD))                            
C     WRITE(*,*) 'PERIOD = ', PERIOD * 360.0 ,' DAYS'
C     WRITE(*,*) 'EQTR XING: ',DEQTX - DSTART
C     IF (ASCEND) THEN                                                  
C        WRITE(*,*)  'ASCENDING.'
C     ELSE                                                              
C        WRITE(*,*)  'DESCENDING'
C     ENDIF                                                             
                                                                        
      RETURN                                                            
                                                                        
 900  NVXINI=-1                                                         
      RETURN                                                            
                                                                        
C IFUNC=2, MODE SET                                                     
 500  IF (KOD(1).EQ.LIT('XYZ ')) NAVMOD=1                               
      END                                                               
                                                                        
      FUNCTION NVXSAE (SCAN,ELEM,XDUM,X,Y,Z)                            
C TO SOLVE THE 'FORWARD' NAVIGATION PROBLEM FOR NOAA                    
      IMPLICIT REAL*8 (D)                                               
      REAL*4 OPV(3),VV(3),VN(3)                                         
      REAL*8 RADDEG, SINH, SECANT, ARCSIN, COSINC
      PARAMETER (RADDEG = 1.745329252D-2)
      COMMON/NH/ DEPOCH,DDINC,DTINC,COSINC,MODE,NAVMOD,NSPOT,IFLIP(3)
      COMMON/NH2/DTSPT                                                  
      PARAMETER (NSAMPS = 16)                                           
      COMMON/VEC/DSTART,TSTART,TEND,DTSMP,VECS(NSAMPS*3),RADS(NSAMPS),  
     &           ANGBTW(NSAMPS),CSCG(NSAMPS),SATVEC(3),TOTSCN           
      COMMON /VOP/OPV                                                   
      DATA VN/0,0,1/                                                    

      SECANT(X)=1.D0/DCOS(RADDEG*X)
      COSINE(X)=DCOS(RADDEG*X)
      ARCSIN(DX)=DASIN(DX)/RADDEG
 
      SCANLO=SCAN                                                       
      ELEMLO=ELEM                                                       
      IF (ELEMLO.GT.NSPOT.OR.ELEMLO.LT.0) GOTO 900                      
      IF (IFLIP(1).EQ.0) GOTO 2                                         
C IMAGE IS INVERTED                                                     
      SCANLO=IFLIP(2)+1-SCANLO                                          
      ELEMLO=IFLIP(3)+1-ELEMLO                                          
C2    DINCR=(SCANLO-1.)*DTINC/86400.                                    
 2    DINCR = (SCANLO - 1.) * DTINC/86400. +                            
     *        (ELEMLO - 1.) * DTSPT/86400.                              
      DT=DSTART+DINCR                                                   
      CALL TINVER (DT,JY,JM,JD,KH,KM,KS,NTH)                            
      CALL VECSAT (DT,FLAT,FLONG,RAD,VE)                                
C MODE=1 ASCENDING ORBIT =-1 DESCENDING                                 
      CALL VECROS (OPV,SATVEC,VV)                                       
      RR=VECDOT (VV,VN)                                                 
      MODE=1                                                            
      IF (RR.LT.0) MODE=-1                                              
      SINH=COSINC*SECANT (FLAT)                                         
      IF( SINH .GT. 1.0D0) SINH = 1.0D0
      IF( SINH .LT.-1.0D0) SINH =-1.0D0
 
C SINE OF HEADING ANGLE, ALWAYS NEGATIVE SINCE COSINC LT 0.             
      ALT=(RAD-6371.)/1.853                                             
      ANGNAD=(.5*NSPOT+.5-ELEMLO)*DDINC                                 
      HEAD=ARCSIN(SINH)                                                 
      IF (MODE.LT.0) HEAD=180.- HEAD                                    
      AZIM=HEAD+90.                                                     
      IF (ANGNAD.LT.0.) AZIM=HEAD-90.                                   
      RIM=RIMARC (ALT,ANGNAD,ZENANG,SLANT)                              
      CALL COORDS (FLAT,FLONG,AZIM,RIM,YONLAT,YONLON)                   
      NVXSAE=0                                                          
      IF (NAVMOD.EQ.0) GOTO 100                                         
      CALL NLLXYZ (GEOLAT(YONLAT*.01745329,2)/.01745329,-YONLON,X,Y,Z)  
      RETURN                                                            
C 'NORMAL' MODE, LAT/LONG                                               
 100  CONTINUE                                                          
      X=GEOLAT(YONLAT*.01745329,2)/.01745329                            
      Y=-YONLON                                                         
      Z=RAD                                                             
      RETURN                                                            
 900  CONTINUE                                                          
      NVXSAE=-1                                                         
      END                                                               
                                                                        
      FUNCTION NVXEAS (LAT,LON,Z,SCAN,ELEM,DUM)                         
C THIS ROUTINE SOLVES THE "BACKWARD" NAVIGATION PROBLEM,                
C I.E.TO OBTAIN LINE AND ELEMENT, GIVEN LAT/LONG.                       
C MATHEMATICAL TECHNIQUE USED TO SOLVE THE 'BACKWARDS' NAVIGATION       
C PROBLEM:                                                              
C THE COORDINATE SYSTEM USED IN THIS ROUTINE IS THE CELESTIAL           
C CARTESIAN SYSTEM, WHEREIN THE ORIGIN IS THE CENTER OF THE EARTH,      
C THE X-AXIS IS DIRECTED TOWARD THE VERNAL EQUINOX, THE Z-AXIS          
C TOWARD THE NORTH POLE, AND THE Y-AXIS CHOSEN SO AS TO FORM A          
C DEXTRAL COORDINATE SYSTEM, I.E.90 DEG TO THE EAST OF THE              
C VERNAL EQUINOX IN THE EQUATORIAL PLANE.                               
C                                                                       
C THE UNIT OF TIME USED IN THIS ROUTINE IS THE JULIAN DAY               
C NUMBER (JDN), REPRESENTING THE NUMBER OF DAYS ELAPSED SINCE           
C 12Z 1 JAN 4713 BC.SEE A TEXTBOOK ON ASTRONOMY, OR DURANT'S            
C 'HISTORY OF CIVILIZATION' FOR AN EXPLANATION OF THIS BIZARRE          
C TIME UNIT IN ASTRONOMY.IT IS ALWAYS A REAL*8 VALUE.                   
C                                                                       
C THE ROUTINE 'VECSAT', HAVING ONCE BEEN INITIALIZED BY THE             
C FUNCTION 'NVXINI', RETURNS THE INSTANTANEOUS POSITION OF THE          
C ORBITING SATELLITE AT A GIVEN INSTANT OF TIME.UNFORTUNATELY,          
C WE DO NOT KNOW THE TIME AT WHICH THE SATELLITE VIEWS A GIVEN          
C LAT/LONG ON THE GROUND, CALLED GLAT/GLONG IN THIS ROUTINE.            
C WE MUST THEREFORE SUCCESSIVELY ESTIMATE THE MOMENT AT WHICH           
C THE SATELLITE VIEWS THE GROUND POINT, BECAUSE UNTIL WE KNOW THIS      
C TIME, WE DO NOT KNOW WHERE THE SATELLITE IS, AND HENCE WE CANNOT      
C APPLY SIMPLE TRIGONOMETRY TO OBTAIN THE SCAN ANGLE.                   
C THIS SUCCESSIVE ESTIMATION IS DONE USING A NEWTON-RAPHSON             
C APPROACH.                                                             
C                                                                       
C SINCE WE HAVE TWO TIMES AT WHICH THE SATELLITE'S POSITION IS          
C KNOWN, E.G.THE BEGINNING AND END OF AN NOAA IMAGE, WE HAVE            
C TWO POSITION VECTORS (A AND B IN COMMON BLOCK /VEC/)                  
C WHOSE CROSS (VECTOR) PRODUCT IS THE VECTOR ORBIT, DEFINING            
C THE ORBITAL PLANE OF THE SATELLITE DURING THE NOAA IMAGE.             
C OUR PROBLEM IS ESSENTIALLY SOLVED WHEN WE HAVE DETERMINED THAT        
C MOMENT OF TIME (DTX) AT WHICH THE CROSS PRODUCT (XP) OF THE           
C CELESTIAL SATELLITE VECTOR (CSV) AND THE CELESTIAL GROUND             
C VECTOR (CGV) HAS NO COMPONENT ALONG THE VECTOR ORBIT,I.E.             
C WHEN THE DOT PRODUCT XP*ORBIT VANISHES,                               
C OR WHEN SUCCESSIVE INCREMENTS OF TIME (DTD) ARE VERY SMALL.           
C                                                                       
C NOTE THAT ALTHOUGH THE GROUND POINT IS FIXED IN TERMS OF              
C LATITUDE AND LONGITUDE, ITS CELESTIAL COORDINATES CHANGE FROM         
C MOMENT TO MOMENT BECAUSE THE LONGITUDE OF THE VERNAL EQUINOX          
C (VEX) ALSO CHANGES, SO THAT CGV IS RECOMPUTED EACH TIME THROUGH       
C THE '100' LOOP.NOTE TOO THAT THE PROBLEM HAS TO BE SOLVED             
C IN CELESTIAL COORDINATES BECAUSE THE SATELLITE'S PATH ACROSS          
C THE SKY IS VERY NEARLY A GREAT CIRCLE, ALLOWING US TO USE             
C SPHERICAL TRIGONOMETRY IN OUR CALCULATION, WHEREAS THE SATELLITE'S    
C PATH ACROSS THE EARTH, EXPRESSED IN TERRESTRIAL COORDINATES,          
C IS ANYTHING BUT A GREAT CIRCLE.                                       
C F W NAGLE, NOAA/NESDIS                                                
C                                                                       
C TO COME UP WITH A GOOD GUESS AS TO THE TIME WHEN THE SATELLITE MIGHT  
C SEE THE POINT ON THE EARTH, A VERY CRUDE APPROXIMATION TO THE         
C LATITUDE IS STORED IN THE COMMON BLOCK "LATDAT". THE ASSUMPTION IS    
C MADE THAT THE FUNCTION OF LATITUDE VS TIME IS A SAW-TOOTH WAVE,       
C BEING LINEAR FROM -90 TO 90 AND THEN LINEAR FROM 90 TO -90. THIS IS   
C INEVITABLY A CRUDE ESTIMATION, SINCE THE SATELLITE SEES QUITE A       
C LARGE RANGE OF LATITUDE AT ANY TIME, ESPECIALLY NEAR THE POLES, AND   
C ITS TRAVEL IS NOT LINEAR, ESPECIALLY NEAR THE POLES.                  
C                                                                       
C ONCE AN ESTIMATE IS MADE, AND BEFORE STARTING THE ITERATION, THE      
C ANGLE BETWEEN THE SATELLITE VECTOR AND THE GROUND POINT VECTOR IS     
C CHECKED. IF GREATER THAN 60 DEGREES, WE MOVE ON TO THE NEXT LATITUDE  
C CROSSING OF THE SAWTOOTH WAVE.                                        
C JOHN SEYMOUR, SSEC                                                    
                                                                        
      IMPLICIT REAL*8 (D)                                               
      REAL LAT, LON                                                     
      REAL*8 COSINC 
      LOGICAL ASCEND                                                    
      COMMON/NH/ DEPOCH,DDINC,DTINC,COSINC,MODE,NAVMOD,NSPOT,IFLIP(3)   
      COMMON/NH2/DTSPT                                                  
      COMMON /LATDAT/ DEQTX, PERIOD, ASCEND                             
      DIMENSION CGV(3),XP(3),ORBIT(3),RDIFF(3),SLANT(3),AH(3)
      PARAMETER (NSAMPS = 16)                                           
      COMMON/VEC/DSTART,TSTART,TEND,DTSMP,VECS(NSAMPS*3),RADS(NSAMPS),  
     &           ANGBTW(NSAMPS),CSCG(NSAMPS),SATVEC(3),TOTSCN           
      COMMON /VOP/ORBIT                                                 
      DATA EPSI/.00001/ GLONGL/999999./ GLATL/999999./                  
     &     DTMIN /1D-5/ D0/0D0/ DTXL/0D0/                               
      SINE(X)=SIN(.0174533*X)                                           
      COSINE(X)=COS(.0174533*X)                                         
                                                                        
C  CONVERT TO GEOCENTRIC COORDINATES                                    
      IF (NAVMOD.NE.0) THEN                                             
C INPUTS ARE TERRESTRIAL CARTESIAN COORDINATES.                         
         CALL NXYZLL (LAT,LON,Z,GLAT,GLONG)                             
         GLAT=GEOLAT (GLAT*.01745329,1)/.01745329                       
         GLONG=-GLONG                                                   
      ELSE                                                              
C INPUTS ARE LAT/LONG                                                   
         GLONG =-LON                                                    
         GLAT=GEOLAT (LAT*.01745329,1)/.01745329                        
      ENDIF                                                             
                                                                        
      CGV(3)=SINE (GLAT)                                                
      EQRAD=COSINE (GLAT)                                               
      NC=1                                                              
      XPER = PERIOD                                                     
      XDEQTX = DEQTX - DSTART                                           
C     WRITE(*,*)  'TSTART = ',TSTART
C     WRITE(*,*)  'TEND = ',TEND
                                                                        
C TRY CONVERGING TO THE PROPER TIME FROM VARIOUS STARTING POINTS        
 7    CONTINUE                                                          
C     WRITE(*,*)  'TRYING FROM NEXT STARTING POINT ', NC
                                                                        
C COME UP WITH A GOOD GUESS - WHEN WILL THE SAT BE AT THE RIGHT LAT?    
 8    DTX = (XPER * GLAT) + XDEQTX                                      
C     WRITE(*,*) 'PERIOD = ',XPER * 360.0,' DAYS'
C     WRITE(*,*) 'EQTR XING: ',XDEQTX
C     WRITE(*,*) 'LAT CROSSING GUESS: ',DTX
                                                                        
C SET UP SAT POSITION ESTIMATORS TO FIND NEXT LAT CROSSING              
      XDEQTX = XDEQTX + (ABS (XPER) * 360.0 * 0.5)                      
      XPER = -XPER                                                      
                                                                        
C IS THIS GUESS WITHIN BOUNDS?                                          
      IF (DTX .LT. TSTART-0.004) GO TO 8                                
      IF (DTX .GT. TEND + 0.004) THEN                                   
         NVXEAS = -1                                                    
         RETURN                                                         
      ENDIF                                                             
      DTX = DTX + DSTART                                                
      DTD = .001                                                        
                                                                        
      DO 100 N=1,64                                                     
C       WRITE(*,*) 'DTX= ',DTX-DSTART,' N=', N
                                                                        
C GET THE CARTESIAN SATELLITE VECTOR, AND VERNAL EQUINOX                
        CALL VECSAT (-DTX,WB,WB,RADX,VEX)                               
                                                                        
C COMPUTE EARTH POINT VECTOR AT TIME DTX                                
        CGLON=GLONG-VEX                                                 
        CGV(1)=EQRAD*COSINE (CGLON)                                     
        CGV(2)=EQRAD*SINE (CGLON)                                       
                                                                        
C COMPUTE COSINE OF ANGLE BETWEEN SATELLITE AND EARTH POINT             
C IF THE ANGLE IS MORE THAN 60 DEGREES, DON'T BOTHER ITERATING          
        IF (N .EQ. 1) THEN                                              
           W = (SATVEC (1) * CGV (1)) + (SATVEC (2) * CGV (2)) +        
     *         (SATVEC (3) * CGV (3))                                   
           IF (W .LT. 0.5) GO TO 999                                    
        ENDIF                                                           
                                                                        
C COMPUTE VECTOR NORMAL TO SATELLITE-EARTH POINT PLANE                  
        XP(1)=SATVEC(2)*CGV(3)-CGV(2)*SATVEC(3)                         
        XP(2)=-SATVEC(1)*CGV(3)+CGV(1)*SATVEC(3)                        
        XP(3)=SATVEC(1)*CGV(2)-CGV(1)*SATVEC(2)                         
                                                                        
C COMPUTE COSINE OF THE ANGLE BETWEEN THIS VECTOR AND ORBIT NORMAL      
        VMAG=XP(1)*ORBIT(1)+XP(2)*ORBIT(2)+XP(3)*ORBIT(3)               
                                                                        
C CHECK IF WE ARE CLOSE ENOUGH, OR STOPPED IMPROVING                    
        IF (ABS(VMAG).LT.EPSI) GOTO 110                                 
        IF (DABS(DTD).LT.DTMIN) GOTO 110                                
                                                                        
C ESTIMATE THE DERIVATIVE AND CORRECTION (DTD), USING THE LAST VMAG     
        IF (N .GT. 1) THEN                                              
           DERIV=DBLE(VMAG-VLAST)/DTD                                   
           IF (DABS(DERIV).LE.D0) GOTO 102                              
           DTD=-DBLE(VMAG)/DERIV                                        
        ENDIF                                                           
                                                                        
C LIMIT THE CORRECTION TERM TO ABOUT 7 MINUTES                          
        IF (DTD .GT. 5D-3) DTD = 5D-3                                   
        IF (DTD .LT. -5D-3) DTD = -5D-3                                 
                                                                        
        DTX=DTX+DTD                                                     
        VLAST=VMAG                                                      
 100  CONTINUE                                                          
                                                                        
 102  CALL TINVER (DTX,KY,KM,KD,JH,JM,JS,NTH)                           
      WRITE(*,*) 'ALGORITHM FAILS TO CONVERGE IN NVXEAS'
      GOTO 999                                                          
                                                                        
C TEST TO EXCLUDE POINTS NOT ON IMAGE                                   
 110  CONTINUE                                                          
C MAKE SURE WE ARE STILL IN BOUNDS                                      
      IF ((DTX .LT. DSTART+TSTART).OR.(DTX .GT. DSTART+TEND)) GO TO 999 
C     WRITE(*,*) 'CHECKING ANGLE. ITERATIONS: ', N
      W = (SATVEC (1) * CGV (1)) + (SATVEC (2) * CGV (2)) +             
     *    (SATVEC (3) * CGV (3))                                        
      IF (W .LT. 0.707) GOTO 999                                        
C RDIFF IS SATELLITE MOVEMENT VECTOR BETWEEN DTX AND DTX+FEW MINUTES    
C     WRITE(*,*) 'COMPUTING SCAN ANGLE'
      AH (1) = SATVEC (1)                                               
      AH (2) = SATVEC (2)                                               
      AH (3) = SATVEC (3)                                               
      CALL VECSAT (-(DTX+0.003), WB, WB, RADX, VEX)                     
      CALL VECSUB (SATVEC, AH, RDIFF)                                   
      SATVEC (1) = AH (1)                                               
      SATVEC (2) = AH (2)                                               
      SATVEC (3) = AH (3)                                               
      CALL VECMUL (RADX/6371.,SATVEC)                                   
      CALL VECSUB (SATVEC,CGV,SLANT)                                    
      ANGNAD=VECANG (SLANT,SATVEC)                                      
      E=ANGNAD/DDINC                                                    
      CALL VECROS (SATVEC,SLANT,XP)                                     
      IF (VECDOT (XP,RDIFF).LT.0.) E=-E                                 
      E=.5*NSPOT+.5+E                                                   
      IF (E.LT.0.OR.E.GT.NSPOT) GOTO 999                                
      DTY=DTX-(E-1.)*DTSPT/86400.                                       
      S=((DTY-DSTART)*86400.0/DTINC)+1                                  
      ELEM=E                                                            
      SCAN=S                                                            
      NVXEAS=0                                                          
      GLATL=GLAT                                                        
      GLONGL=GLONG                                                      
      DTXL=DTX                                                          
      IF (IFLIP(1).EQ.0) GOTO 200                                       
C IMAGE IS INVERTED                                                     
      SCAN=IFLIP(2)+1-SCAN                                              
      ELEM=IFLIP(3)+1-ELEM                                              
200   CONTINUE                                                          
C     WRITE(*,*) 'LAT LON LINE ELEM  ',LAT,LON,SCAN,ELEM
      RETURN                                                            
 999  CONTINUE                                                          
      NC=NC+1                                                           
      IF (NC.LT.6) GOTO 7                                               
 1000 NVXEAS=-1                                                         
      END                                                               
                                                                        
      FUNCTION NVXOPT (IFUNC,XIN,XOUT)                                  
      IMPLICIT REAL*8 (D)                                               
      REAL*4 XIN(*),VEC(3),XOUT(*)                                      
      REAL*8 DTIME,DABTIM
      REAL*4 FLAT,FLONG,GCD
      CHARACTER*4 CLIT,CFUNC                                            
C IFUNC= 'SPOS' SUBSATELLITE LAT/LON                                    
C XIN  1  DAY (YYDDD)                                                   
C      2  TIME (HOURS)                                                  
C XOUT 1  SUB-SATELLITE LATITUDE                                        
C      2  SUB-SATELLITE LONGITUDE (WEST POSITIVE)                       
C      3  DISTANCE TO CENTER OF EARTH                                   
C      4,5,6  VECTOR TO EARTH CENTER                                    
      NVXOPT = 0
      CFUNC=CLIT(IFUNC)                                                 
      IF (CFUNC.EQ.'SPOS') THEN                                         
        JDAY=MOD(NINT(XIN(1)),100000)                                   
        CALL DATCON (JDAY,NY,NM,ND,JYYDDD,AMON)                         
        DTIME=DABTIM (NY,NM,ND,0,0,0)+(XIN(2)/2.4D+1)                   
        CALL SATPSN (DTIME,VEC,FLAT,FLONG,GCD)
        XOUT(1)=GEOLAT (FLAT*.01745329,2)/.01745329
        XOUT(2)=-FLONG                                                  
        XOUT(3)=GCD                                                     
        XOUT(4)=VEC(1)                                                  
        XOUT(5)=VEC(2)                                                  
        XOUT(6)=VEC(3)                                                  
      ELSE                                                              
        NVXOPT=1                                                        
      ENDIF                                                             
      END                                                               
                                                                        
      FUNCTION RIMARC (HEIGHT,ANGLE,ZENANG,SLANT)                       
C GET DISTANCE IN DEG OF LATITUDE FROM SUB-SAT POINT                    
C TO A POINT WHOSE NADIR ANGLE IN DEGREES IS 'ANGLE'.THE ZENITH         
C ANGLE IN DEGREES FROM THE POINT TO THE SATELLITE, AND THE             
C SLANT RANGE IN NAUT MILES ARE RETURNED AS BY-PRODUCTS.                
C 'HEIGHT' IS EXPRESSED IN NAUTICAL MILES.                              
      DATA RAD/3437.748/                                                
      SINE(X)=SIN(.01745329*X)                                          
      ARCSIN(X)=57.29578*ASIN(X)
      ANG=ABS(ANGLE)                                                    
      FAC=SINE(ANG)/RAD                                                 
      SINB=(RAD+HEIGHT)*FAC                                             
      IF (ABS(SINB).LT.1.0) THEN                                        
        ZENANG=ARCSIN(SINB)                                             
      ELSE                                                              
        ZENANG=90.*ABS(SINB)/SINB                                       
      ENDIF                                                             
      S=ZENANG-ANG                                                      
      SLANT=HEIGHT                                                      
      IF (FAC.GT.0.) SLANT=SINE(S)/FAC                                  
      RIMARC=S                                                          
      RETURN                                                            
      END                                                               
                                                                        
      SUBROUTINE VECADD (A,B,C)                                         
C VECADD, VECSUB, VECROS, VECDOT, VECMAG, VECPAK, VECMUL                
C TO PERFORM GENERAL VECTOR ARITHMETIC                                  
      DIMENSION A(3),B(3),C(3)                                          
      C(1)=A(1)+B(1)                                                    
      C(2)=A(2)+B(2)                                                    
      C(3)=A(3)+B(3)                                                    
      END                                                               
                                                                        
      SUBROUTINE VECSUB (A,B,C)                                         
      DIMENSION A(3),B(3),C(3)                                          
      C(1)=A(1)-B(1)                                                    
      C(2)=A(2)-B(2)                                                    
      C(3)=A(3)-B(3)                                                    
      END                                                               
                                                                        
      SUBROUTINE VECROS (A,B,C)                                         
      DIMENSION A(3),B(3),C(3)                                          
      C(1)=A(2)*B(3)-B(2)*A(3)                                          
      C(2)=B(1)*A(3)-A(1)*B(3)                                          
      C(3)=A(1)*B(2)-B(1)*A(2)                                          
      END                                                               
                                                                        
      FUNCTION VECDOT (A,B)                                             
      DIMENSION A(3),B(3)                                               
      VECDOT=A(1)*B(1)+A(2)*B(2)+A(3)*B(3)                              
      END                                                               
                                                                        
      FUNCTION VECMAG (A)                                               
      DIMENSION A(3)                                                    
      VECMAG=SQRT(A(1)*A(1)+A(2)*A(2)+A(3)*A(3))                        
      END                                                               
                                                                        
      FUNCTION VECANG (A,B)                                             
      DIMENSION A(3),B(3)                                               
      ARCCOS(X)=57.29578*ATAN2(SQRT(1.-X*X),X)
      VECDAT=VECMAG(A)
      COSANG=VECDOT(A,B)/(VECDAT*VECMAG(B))
      ANG=0.                                                            
      IF(ABS(COSANG).LT.1.) ANG=ARCCOS(COSANG)                          
      VECANG=ANG                                                        
      END                                                               
                                                                        
      SUBROUTINE VECMUL (SCALAR,VEC)                                    
C TO MULTIPLY A VECTOR BY A SCALAR                                      
      DIMENSION VEC(3)                                                  
      DO 10 J=1,3                                                       
 10   VEC(J)=SCALAR*VEC(J)                                              
      END                                                               
                                                                        
      SUBROUTINE COORDS (SATLAT,SATLNG,AZIM,DIST,YONLAT,YONLNG)         
C COMPUTE LAT, LONG OF YONDER POINT, GIVEN SATELLITE                    
C LAT, LONG, AND AZIMUTH AND DISTANCE TO YONDER POINT.ALL               
C QUANTITIES ARE EXPRESSED IN DEGREES. 
      IMPLICIT REAL*8 (A-Z)
      REAL*4 SATLAT, SATLNG, AZIM, DIST, YONLAT, YONLNG
 
      PARAMETER (RADDEG = 1.745329252D-2)
      PARAMETER (RAD90 = 90.D0 * RADDEG)
 
      CLAT = (90.D0 - SATLAT) * RADDEG
      CLTV = CLAT
      CLON = -SATLNG * RADDEG
      IF( SATLAT .LT. 0.0) THEN
         CLTV = -(90.D0 + SATLAT) * RADDEG
         CLON = (SATLNG - 180.D0) * RADDEG
      ENDIF
 
      ANG = AZIM * RADDEG
      CDIS = DIST * RADDEG
 
      QLAT = DACOS( DCOS(CLAT)*DCOS(CDIS) +
     &       DSIN(CLAT)*DSIN(CDIS)*DCOS(ANG))
 
      IF( DABS(QLAT) .LT. 1.D-7) THEN
         QLON = 0.0
      ELSE
         ARGU = DSIN(CDIS) * DSIN(ANG)/ DSIN(QLAT)
         IF( DABS(ARGU) .GT. 1.D0) ARGU = DSIGN(1.D0, ARGU)
         QLON = DASIN(ARGU)
         TV = DATAN( DSIN(RAD90 - ANG)/ DTAN(RAD90 - CDIS))
         IF( TV .GT. CLTV) QLON = 2 * RAD90 - QLON
      ENDIF
 
      QLON = CLON - QLON
      YONLAT = 90.D0 - QLAT/RADDEG
      YONLNG = DMOD(QLON/RADDEG, 360.D0)
      IF(YONLNG .GT. 180.0) YONLNG = YONLNG - 360.0
      IF(YONLNG .LT.-180.0) YONLNG = YONLNG + 360.0
 
      IF( SATLAT .GT. 0.0) YONLNG = -YONLNG
 
      RETURN
      END

                                                                        
      FUNCTION DABTIM (MYR,MON,MDAY,JHOUR,JMIN,JSEC)                    
C TO COMPUTE TIME IN JULIAN DAY NUMBER, GIVEN CONVENTIONAL              
C TIME UNITS.                                                           
C LAST 3 ARGUMENTS MAY BE EITHER REAL*4 OR INTEGER*4.                   
C IF THE DATE IS KNOWN AS YEAR AND DAY OF YEAR, THEN SET MON=0.         
C THE ROUTINE WILL FAIL FOR DATES EARLIER THAN 1 JAN 1901, BECAUSE      
C A.D.1900 WAS NOT A LEAP YEAR.IT WILL CONTINUE TO WORK UP              
C TO A.D.4000, WHEN IT WILL FAIL BECAUSE A.D.4000 IS NOT A              
C LEAP YEAR.I'LL MAKE THE NECESSAARY CHANGES AS WE APPROACH             
C A.D.4000 IF I HAVEN'T RETIRED BEFORE THEN.- NAGLE                     
      IMPLICIT REAL*8 (D)                                               
      REAL*8 DABTIM                                                     
      EQUIVALENCE(KHOUR,HOUR),(KMIN,FMIN),(KSEC,SEC)                    
      DATA IMAX/16777216/                                               
      KHOUR=JHOUR                                                       
      KMIN=JMIN                                                         
      KSEC=JSEC                                                         
      IF (KHOUR.LT.IMAX) HOUR=KHOUR                                     
      IF (KMIN.LT.IMAX) FMIN=KMIN                                       
      IF (KSEC.LT.IMAX) SEC=KSEC                                        
      KALDAY=MDAY                                                       
      JDAY=KALDAY                                                       
      MONTH=MON                                                         
      IF (MON.NE.0) GOTO 10                                             
      JCODE=1115212                                                     
      IF (MOD(MYR,4).EQ.0) JCODE=1115208                                
      DO 12 M=1,12                                                      
      MONTH=M                                                           
      NDM=31-MOD(JCODE,4)                                               
      JDAY=JDAY-NDM                                                     
      IF (JDAY.LE.0) GOTO 20                                            
 12   JCODE=JCODE/4                                                     
 20   KALDAY=JDAY+NDM                                                   
 10   DJUL=JULDAY (MYR,MONTH,KALDAY)                                    
      DHOUR=HOUR                                                        
      DMIN=FMIN                                                         
      DSEC=SEC                                                          
      DJUL=DJUL+(DHOUR+DMIN/60.D+0+DSEC/3600.D+0)/24.D+0                
      DABTIM=DJUL -.5D+0                                                
C BECAUSE A JULIAN DAY BEGINS AT NOON, NOT MIDNIGHT                     
      IF (DJUL.LE.0.D+0) DABTIM=0.D+0                                   
      END                                                               
                                                                        
      SUBROUTINE DATCON (JARG,NY,NM,ND,JYYDDD,AMON)                     
C TO INTERCONVERT AMONG VARIOUS FORMS OF DATE                           
C THE OUTPUT VARIABLE 'JYYDDD' IS YYDDD                                 
C POSSIBLE INPUT (JARG):                                                
C YYMMDD                                                                
C YYDDD                                                                 
C 'TODA'                                                                
C JULIAN DAY NUMBER                                                     
      REAL*8 DARG                                                       
      IF (JARG.EQ.0.OR.JARG.EQ.LIT('TODA')) GOTO 50                     
      IF (JARG.GT.100000) GOTO 20                                       
C ASSUME YYDDD IS GIVEN                                                 
      CALL CALDAY (JARG,NY,NM,ND,AMON)                                  
      JYYDDD=JARG                                                       
      RETURN                                                            
 20   IF (JARG.GT.2 400 000) GOTO 300                                   
C ASSUME YYMMDD                                                         
      NY=JARG/10000                                                     
      ND=MOD(JARG,100)                                                  
      NM=MOD(JARG/100,100)                                              
      JYYDDD=1000*NY+NTHDAY (NY,NM,ND)                                  
 44   CALL CALDAY (JYYDDD,NY,NM,ND,AMON)                                
      RETURN                                                            
 50   CALL GETDAY (JYYDDD)                                              
      CALL GETTIM (NOWTIM)                                              
      IF (NOWTIM.LT.60000) JYYDDD=JYYDDD+1                              
      CALL CALDAY (JYYDDD,NY,NM,ND,AMON)                                
      RETURN                                                            
C ASSUME JULIAN DAY NUMBER IS GIVEN                                     
 300  DARG=JARG                                                         
      CALL TINVER (DARG,NY,NM,ND,NH,MIN,NSEC,NTH)                       
      JYYDDD=1000*NY+NTH                                                
      GOTO 44                                                           
      END                                                               
                                                                        
      SUBROUTINE TINVER (DTIME,NYR,NMO,NDA,NHR,MIN,NSEC,NTHDAY)         
C CONVERT ABSOLUTE TIME IN JULIAN DAY TO TIME                           
C EXPRESSED IN THE USUAL VARIABLES.NTHDAY VARIES FROM 1 TO 366.         
      REAL*8 DTIME,DT,DFRAC
      INTEGER BMON
      IF (DTIME.LE.0D0) GOTO 800                                        
      DT=DTIME+.5D0                                                     
C.578704D-5=.5 SECS IN DAYS (ROUND-OFF)                                 
C ROUND-OFF CHANGED TO TRUNCATION 27 SEP 85                             
      JDT=DT                                                            
      CALL JULINV (JDT,NYR,NMO,NDA,BMON,NTHDAY)                         
      DFRAC=DT-DFLOAT(JDT)                                              
      KSECS=864D2*DFRAC                                                 
      NHR=KSECS/3600                                                    
      MIN=(KSECS-3600*NHR)/60                                           
      NSEC=KSECS-3600*NHR-60*MIN                                        
      RETURN                                                            
 800  NYR=0                                                             
      NMO=0                                                             
      NDA=0                                                             
      NHR=0                                                             
      MIN=0                                                             
      NSEC=0                                                            
      END                                                               
                                                                        
      SUBROUTINE VECSAT (DTIME,FLAT,FLONG,RAD,VELONG)                   
C ROUTINE FOR NOAA INGEST, TO MAKE A FAST                               
C COMPUTATION OF TIROS/NOAAX LAT, LONG, AND SEMI MAJOR AXIS.            
C THE ROUTINE ESTIMATES THE POSITION OF A SATELLITE AT TIME 'DT'.       
C                                                                       
C THE ROUTINE ASSUMES THAT NVXINI HAS INITIALIZED THE VECTOR "VECS"     
C TO HOLD 10 SETS OF SATELLITE POSITIONS DESCRIBING THE POSITION        
C AT THE START TIME OF THE IMAGE, AND THEREAFTER EVERY 10 MINUTES       
C OR SO. IT SHOULD ALSO HAVE INITIALIZED THE ARRAYS RADS (CONTAINING    
C THE SATELLITE DISTANCES FROM EARTH CENTER), ANGBTW (CONTAINING THE    
C ANGLES BETWEEN SUBSEQUENT SATELLITE POSITIONS), AND CSCG (CONTAIN-    
C ING THE COSECANTS OF GAMMAS - THE ANGLES BETWEEN).                    
C                                                                       
C THE APPROXIMATION OF RADIUS IS MADE BY STRAIGHT LINEAR                
C INTERPOLATION. THIS MAY NOT BE EXTREMELY ACCURATE, BUT HOPEFULLY      
C THIS PARAMETER IS NOT SENSITIVE. THE ACTUAL POSITION IS APPROXIMATED  
C UNDER THE ASSUMPTION THAT THE SATELLITE WILL HAVE CONSTANT ANGULAR    
C VELOCITY DURING THE 10 MINUTE INTERVAL CHOSEN.                        
C                                                                       
C IF DTIME < 0, ITS MAGNITUDE IS USED, AND THE ROUTINE RETURNS          
C AFTER COMPUTING ONLY THE CARTESIAN VECTOR SATVEC(3).                  
                                                                        
      IMPLICIT REAL*8 (D)                                               
      PARAMETER (NSAMPS = 16)                                           
      COMMON/VEC/DSTART,TSTART,TEND,DTSMP,VECS(NSAMPS*3),RADS(NSAMPS),  
     &           ANGBTW(NSAMPS),CSCG(NSAMPS),SATVEC(3),TOTSCN           
      SINE (X) = SIN (0.0174533 * X)                                    
      ARCTAN (X, Y) = 57.29578 * ATAN2 (Y, X)
      ARCSIN (X) = 57.29578 * ASIN (X)
                                                                        
      DT = DABS (DTIME)                                                 
      VELONG = VERNEQ (DT)                                              
      IF (DT .GT. DSTART + ((NSAMPS - 1) * DTSMP) .OR.                  
     -   (DT .LT. DSTART)) THEN                                         
C  OUTSIDE RANGE OF TABLE - USE THE LONG WAY                            
         CALL SATPSN (DT, SATVEC, FLAT, FLON, RAD)                      
         CALL VECMUL (1.0 / RAD, SATVEC)                                
      ELSE                                                              
         DT = DT - DSTART                                               
         I = AINT (DT / DTSMP) + 1                                      
         IF (I .GT. (NSAMPS - 1)) I = (NSAMPS - 1)                      
         IF (I .LT. 1) I = 1                                            
         FAC = (DT - ((I - 1) * DTSMP)) / DTSMP                         
         RAD = FAC * (RADS (I + 1) - RADS (I)) + RADS (I)               
         ALPHA = FAC * ANGBTW (I)                                       
         BETA = ANGBTW (I) - ALPHA                                      
         C = SINE (BETA) * CSCG (I)                                     
         D = SINE (ALPHA) * CSCG (I)                                    
         I = (3 * I) - 2                                                
         SATVEC (1) = (C * VECS (I)) + (D * VECS (I + 3))               
         SATVEC (2) = (C * VECS (I + 1)) + (D * VECS (I + 4))           
         SATVEC (3) = (C * VECS (I + 2)) + (D * VECS (I + 5))           
C THESE ARE CELESTIAL UNIT CARTESIAN COORDINATES.                       
C      WRITE (*, 1001) SATVEC
C1001   FORMAT (3F11.7)                                                 
      ENDIF                                                             
                                                                        
      IF (DTIME .LT. 0.D+0) RETURN                                      
      CELLON = ARCTAN (SATVEC(1), SATVEC(2))                            
C CELESTIAL LONGITUDE                                                   
      FLONG = CELLON + VELONG                                           
      IF (FLONG .GT. 180.) FLONG = FLONG - 360.                         
      IF (FLONG .LT. -180.) FLONG = FLONG + 360.                        
      FLAT = ARCSIN (SATVEC (3))                                        
C GEOCENTRIC LATITUDE                                                   
      END                                                               
                                                                        
      SUBROUTINE JULINV (JULIN,NYEAR,MONTH,KALDAY,KMONTH,NTHDAY)        
C TO GET CALENDAR INVERSE OF JULIAN DAY NUMBER.                         
C KMONTH IS A RIGHT-JUSTIFIED 3-LETTER MONTH.                           
C THIS ROUTINE IS THE INVERSE OF 'JULDAY', Q.V.                         
      DIMENSION MONTHS(12)                                              
      DATA MONTHS/4H JAN,4H FEB,4H MAR,4H APR,4H MAY,4H JUN,            
     & 4H JUL,4H AUG,4H SEP,4H OCT,4H NOV,4H DEC/                       
      IF (JULIN.GT.2600000) GOTO 900                                    
      ND=1                                                              
      MON=1                                                             
      NY=4                                                              
      JDAY=JULIN-2416481                                                
      IF (JDAY.LT.0) GOTO 900                                           
      KWADS=JDAY/1461                                                   
      NY=NY+4*KWADS                                                     
      JDAY=JDAY-1461*KWADS                                              
      KODE=1115212                                                      
      IF (JDAY.GE.366) GOTO 4                                           
      KODE=1115208                                                      
      GOTO 6                                                            
 4    NY=NY+1                                                           
      JDAY=JDAY-366                                                     
      NYS=JDAY/365                                                      
      NY=NY+NYS                                                         
      JDAY=JDAY-365*NYS                                                 
 6    NTHDAY=JDAY+1                                                     
      DO 10 I=1,12                                                      
      NDAYS=31-MOD(KODE,4)                                              
      JDAY=JDAY-NDAYS                                                   
      IF (JDAY.LT.0) GOTO 20                                            
      MON=MON+1                                                         
 10   KODE=KODE/4                                                       
 20   KALDAY=JDAY+NDAYS+1                                               
      NYEAR=NY                                                          
      MONTH=MON                                                         
      KMONTH=MONTHS(MON)                                                
      RETURN                                                            
 900  NYEAR=0                                                           
      MONTH=0                                                           
      KALDAY=0                                                          
      END                                                               
                                                                        
      FUNCTION TRUANG (ANG)                                             
C SET ANY ANGLE BETWEEN -180 AND +180.                                  
      INTEGER*2 IL                                                      
      EQUIVALENCE(J,IL)                                                 
      J=182.0444*ANG                                                    
C 182.0444=65536/360                                                    
      IL=0                                                              
      IF (J.GT.32768) J=J-65536                                         
      TRUANG=FLOAT(J)/182.0444                                          
      END                                                               
                                                                        
      SUBROUTINE SATPSN (DTJDN,VEC,GCLAT,GCLON,CD)                      
C THIS ROUTINE PROVIDES ENTRY TO 'BRLYNV' AND SERVES TO                 
C MAINTAIN CONTINUITY WITH TOVS SOFTWARE, WHICH USED                    
C A LESS RELIABLE ALGORITHM UNDER THE NAME 'SATPSN'.                    
      REAL*8 DTJDN                                                      
      REAL*4 VEC(3)                                                     
      CALL BRLYNV (DTJDN,GCLAT,GCLON,CD,VEC)                            
      END


      SUBROUTINE BRLYNV(DTIME, FLAT,FLON,CD,VEC)
C $ SUBROUTINE BRLYNV(DTIME,FLAT,FLON,CD,VEC) (RP)
C $ BRLYNV - obtain satellite position at given time
C $ Input:
C $   DTIME = (R*8) time in Julian day number
C $ COMMON/VBL/...
C $   DTEPOC = (R*8) epoch-time at which orbital elements are valid
C $   GIVENS = (R) array of orbital elements
C $ Output:
C $   FLAT = (R) geocentric latitude
C $   FLON = (R) geocentric longitude
C $   CD = (R) distance from satellite to center of the earth
C $   VEC = (R) satellite position vector (3 elements)
C $$ BRLYNV = NAVIGATION, TOVS
C
C  BROUWER-LYDDANE MODEL FOR SATELLITE NAVIGATION
C     'DTIME' IS THE TIME FOR WHICH A SATELLITE POSITION IS DESIRED,
C     EXPRESSED IN JULIAN DAY NUMBER, WHICH IS NOT, REPEAT, IS NOT
C     THE DAY OF THE YEAR.  'DTIME' MAY BE OBTAINED FROM ORDINARY
C     TIME UNITS (YEAR,MONTH,DAY,HOUR,MINUTE,SECOND) USING THE
C     REAL*8 FUNCTION 'DABTIM'.
C
C     THE RETURNED VALUES ARE GEOCENTRIC LATITUDE, LONGITUDE,
C     CENTRAL DISTANCE (I.E. FROM CENTER OF EARTH TO SATELLITE,
C     IN KILOMETERS), AND THE CELESTIAL POSITION VECTOR.
C
C     THE COMMON BLOCK /BLXTRA/ RETURNS THE B-L ELEMENTS UPDATED
C     TO THE GIVEN INPUT TIME.
C
C     THE ORBITAL PARAMETERS MUST ALREADY HAVE BEEN PUT INTO THE
C     COMMON BLOCK /VBL/ BY SOME OTHER ROUTINE, E.G. GETELS.
C
C     THIS ROUTINE USES THE SAME ORBITAL PREDICTION SOFTWARE AS
C     NOAA/NESDIS, AND HENCE CANNOT FAIL TO BE UTTERLY CORRECT.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (DTR = 6.283185307179586D0/360.D0, RTD=1.D0/DTR)
      REAL*4 CD,CLON,FLAT,FLON,GIVENS,VEC(3),VERNEQ,VLON
      DIMENSION BLLAST(6),OSCUL(6),AUX(5),VECPOS(3)
      COMMON/BLCNST/DTSECS,CENDIS,RADEAR,GRAV,DARGS(7)
      COMMON/BLXTRA/DTPREP,DBLPRE(6),VECVEL(3)
      COMMON/VBL/DTEPOC,GIVENS(6)
C
C     THE ANGLES APPEARING IN 'GIVENS' ARE RADIANS, NOT DEGREES.
C     'DTEPOC' IS THE EPOCH TIME IN JULIAN DAY NUMBER OR FRACTION
C     THEREOF.  THE 'GIVEN' PARAMETERS ARE RESPECTIVELY SEMI-MAJOR
C     AXIS IN KILOMETERS, ECCENTRICITY, INCLINATION, RIGHT ASCENSION
C     OF ASCENDING NODE, ARGUMENT OF PERIGEE, AND MEAN ANOMALY.
C
C     FOR A DEFINITION OF 'JULIAN DAY NUMBER' SEE BOWDITCH,
C    'AMERICAN PRACTICAL NAVIGATOR', PUBLICATION NO. 9,  VOL II,
C     P. 469, (DEFENSE MAPPING AGENCY)
C
      DATA  BLLAST/ 6*0.D+0/, ANOM/-99999./
C
      IF(DTEPOC .LT. 2.4D+6) THEN
         WRITE(*,'('' BRLYNV -- BAD EPOCH TIME:'',D20.12)') DTEPOC
         FLAT = 999.
         RETURN
      ENDIF
C
C     IF(DTIME .LT. DTEPOC) THEN
C        WRITE(*,'('' BRLYNV -- PREDICTION TIME'',D20.12)') DTIME
C        WRITE(*,'( ''    IS EARLIER THAN EPOCH'',D20.12)') DTEPOC
C        FLAT = 999.
C        RETURN
C     ENDIF
C
      DO 100 J = 1,6
  100 BLLAST(J) = GIVENS(J)
      DO 110 J = 3,6
      IF(ABS(GIVENS(J)) .LE. 6.283185) GO TO 110
      WRITE(*,'('' BRLYNV -- BAD PARAMETERS, MUST BE RADIANS:'',
     *  2E12.4,4F8.2)') GIVENS
      FLAT = 999.
      RETURN
  110 CONTINUE
C
      DTSECS = 86400.D+0 * (DTIME - DTEPOC)
      IF(GIVENS(6).NE.ANOM) CALL BROLYD(OSCUL,BLLAST,0,1,1,AUX)
C * RE-INITIALIZES PREDICTION SOFTWARE IF WE HAVE CHANGED ORBITAL
C   PARAMETERS SINCE THE PRECEDING CALL.
C
      ANOM = GIVENS(6)
      CALL BROLYD(OSCUL,BLLAST,2,2,1,AUX)
C * BROLYD COMPUTES OSCULATING KEPLERIAN VALUES VALID AT TIME
C   DTIME FROM B-L ELEMENTS VALID AT EPOCH 'DTEPOC'.
C
C * RETURN UPDATED B-L ELEMS THRU COMMON /BLXTRA/
      DO 120 J = 1,6
  120 DBLPRE(J) = BLLAST(J)
C * EPOCH OF PREDICTED PARAMETERS
      DTPREP = DTIME
C * CONVERT OSCULATING TO CELESTIAL
      CALL CELEM(OSCUL,GRAV,VECPOS,VECVEL)
C * STORE CD FOR RETURN
      CD = CENDIS
C * GET LONGITUDE OF VERNAL EQUINOX
      VLON = VERNEQ(DTIME)
C * CONVERT CELESTIAL POSITION VECTOR TO GEOCENTRIC LAT,LON
      CLON = DARCTAN(VECPOS(1),VECPOS(2))
      DEQR = DSQRT(VECPOS(1)**2 + VECPOS(2)**2)
      FLAT = DARCTAN(DEQR,VECPOS(3))
      FLON = CLON + VLON
      IF(FLON .LT. -180.) FLON = FLON + 360.
      IF(FLON .GT.  180.) FLON = FLON - 360.
C * COPY C-P-V TO SINGLE-PRECISION ARRAY FOR OUTPUT
      DO 130 J = 1,3
  130 VEC(J) = VECPOS(J)
      RETURN
      END
      DOUBLE PRECISION FUNCTION DKEPLR(M,E)
C $ DKEPLR - solve Kepler's equation
C $ Input:
C $   M = (R*8) mean anomaly
C $   E = (R*8) eccentricity
C $ Output (via FUNCTION name):
C $   DKEPLR = (R*8) eccentric anomaly
C $$ DKEPLR = NAVIGATION, TOVS
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 M,PI2,TOL             
      DATA     PI2/6.283185307179586D0/,TOL/0.5D-15/
C
C  TO SOLVE KEPLER'S EQUATION, WHICH
C     RELATES GEOMETRY OR POSITION IN ORBIT PLANE TO TIME.
C
C M - MEAN ANOMALY (0<M<2PI)
C E - ECCENTRICITY
C EA- ECCENTRIC ANOMALY
C
      EA=0.D0
      IF(M) 1,2,1
    1 EA=M+E*DSIN(M)
      DO 22 I=1,12
      OLDEA=EA
      FE=EA-E*DSIN(EA)-M
      EA=EA-FE/(1-E*DCOS(EA-0.5D0*FE))
C TEST FOR CONVERGENCE
      DELEA=DABS(EA-OLDEA)
      IF(DELEA.LE.TOL) GO TO 2
   22 CONTINUE
    2 EA=DMOD(EA,PI2)
      DKEPLR=EA
      RETURN
      END
      SUBROUTINE CELEM (ORBEL,GMC,PV,VV)
C $ CELEM - convert osculating orbital elements to Cartesian elements
C $ Input:
C $   ORBEL = (R*8) osculating elements
C $   GMC = (R*8) gravitational constant
C $ Output:
C $   PV = (R*8) Cartesian position vector
C $   VV = (R*8) Cartesian velocity vector
C $$ CELEM = NAVIGATION, TOVS
C
C   METHOD:
C       USES MILES STANDISH ITERATIVE SCHEME FOR SOLN TO KEPLERS EQN.
C   REFERENCES:
C       GTDS TASK SPEC FOR CELEM, C.E. VELEZ, 13 JANUARY 1971
C       DODS SYSTEM DESCRIPTION, SUBROUTINE KEPLR1
C       P. EXCOBAL-'METHODS OF ORBIT DETERMINATION'
C       X-552-67-421,'COMPARISON FO ITERATIVE TECHNIQUES FOR THE
C       SOLUTION OF KEPLERS EQUATION', I.COLE AND R. BORCHERS
C   PROGRAMMER:
C       CHARLES K. CAPPS,  CODE 553.2,  GSFC
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION PV(3),VV(3),ORBEL(6)
      DATA MAX /10/
      DATA TOL /+0.5D-16/
      ITER = 0
C     FIND IF THIS IS ELLIPTIC OR HYPERBOLIC ORBIT
      IF (ORBEL(1).LE.0.0D0.AND.ORBEL(2).GT.1.0D0) GO TO 50
C     ELLIPTIC ORBIT TAKES THIS ROUTE.
C     FIRST FIND ECCENTRIC ANOMALY VIA NEWTONS (MILES STANDISH VERSION)
      E1 = ORBEL(6)
  10  F = E1 - (ORBEL(2) * DSIN(E1)) - ORBEL(6)
      D = 1.0D0 - (ORBEL(2) * DCOS(E1 - 0.5D0 * F))
      E2 = E1 - (F / D)
      IF (DABS (E1-E2)-TOL )40,40,20
   20 ITER = ITER + 1
      E1 = E2
      IF(ITER - MAX) 10,10,30
C     SET UP ERROR CODE TO RETURN FROM SUBROUTINE
   30 NERR = 13
C     ECCENTRIC ANOMALY CONVERGED, NOW GET XO, YO, R
   40 COSE = DCOS(E2)
      SINE = DSIN (E2)
      TEMP = 1.0D0 - ORBEL(2) * ORBEL(2)
      XO = ORBEL(1) * (COSE - ORBEL(2))
      YO = ORBEL(1) * (DSQRT(TEMP)* SINE)
      R = ORBEL(1) * (1.0D0 - ORBEL(2) * COSE)
      XOD = (-DSQRT(GMC* ORBEL(1))* SINE)/R
      YOD = (DSQRT(GMC*ORBEL(1)*(TEMP))*COSE) / R
      GO TO 100
C     HYPERBOLIC ORBITS TAKE THIS ROUTE
   50 E1 = ORBEL(6) / 2.0D0
   60 F = ORBEL(2) * DSINH(E1) - E1 - ORBEL(6)
      D = ORBEL(2) * DCOSH(E1 - 0.5D0 * F ) - 1.0D0
      E2 = E1 - (F / D)
      IF (DABS (E1-E2)-TOL )90,90,70
   70 ITER = ITER + 1
      E1 = E2
      IF (ITER - MAX) 60,60,80
C      SET UP ERROR CODE FOR NON-CONVERGENCE PRIOR TO EXIT.
   80 NERR = 14
C     ECCENTRIC ANOMALY COMPUTED, NOW GET XO,YO,R
   90 COSE = DCOSH (E2)
      SINE = DSINH(E2)
      TEMP = ORBEL(2) * ORBEL(2) - 1.0D0
      XO = ORBEL(1)*(COSE- ORBEL(2))
      YO = - ORBEL (1)*DSQRT (TEMP) * SINE
      R = ORBEL(1)*(1.0D0 - ORBEL(2) * COSE)
      XOD = (-DSQRT(-GMC*ORBEL(1))*SINE)/R
      YOD = (DSQRT(-GMC*ORBEL(1)*TEMP)*COSE) / R
  100 COSO = DCOS(ORBEL(5))
      SINO = DSIN (ORBEL(5))
      COSOM = DCOS (ORBEL(4))
      SINOM = DSIN (ORBEL(4))
      COSI = DCOS(ORBEL(3))
      SINI = DSIN (ORBEL(3))
      B11 = COSO * COSOM - SINO * SINOM * COSI
      B21 = COSO * SINOM + SINO * COSOM * COSI
      B31 = SINO * SINI
      B12 = -SINO * COSOM - COSO * SINOM * COSI
      B22 = -SINO * SINOM + COSO * COSOM * COSI
      B32 = COSO * SINI
C     NOW MULTIPLY 3 X 2 MATRIX BY 2 X 1 VECTORS FOR POSITION, VELOCITY.
      PV(1) = B11 * XO + B12 * YO
      PV(2) = B21 * XO + B22 * YO
      PV(3) = B31 * XO + B32 * YO
      VV (1) = B11*XOD + B12 * YOD
      VV(2) = B21 * XOD + B22 * YOD
      VV(3) = B31 * XOD + B32 * YOD
  999 RETURN
      END
      SUBROUTINE BROLYD(OSCELE,DPELE,IPERT,IPASS,IDMEAN,ORBEL)
C $ BROLYD - compute osculating Keplerian elements
C $ Input:
C $   DPELE = (R*8) Brouwer elements at epoch time
C $ Output:
C $   OSCELE = (R*8) osculating elements
C $$ BROLYD = NAVIGATION, TOVS
C
C * THIS IS THE ORIGINAL NESDIS OFFICIAL VERSION EXCEPT FOR:
C     1. USE OF A PARAMETER STATEMENT RATHER THAN REPLACEMENTS IN THE
C        DEFINITION OF SEVERAL FRACTIONS;
C     2. ELIMINATION OF THE FUNCTION DATAN0.
C***********************************************************************
C   REF.   "BROUWER-LYDDANE ORBIT GENERATOR ROUTINE"
C                  (X-553-70-223)
C                BY E.A. GALBREATH 1970
C
C    MODIFIED 7/31/74 VIONA BROWN AND R.A. GORDON TO INTERFACE WITH GTDS
C***********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     THE FOLLOWING PARAMETERS WERE INSERTED BY F W NAGLE ON 23 NOV 88
C     TO REPLACE THE ARITHMETIC REPLACEMENTS FORMERLY CONTAINED IN THE
C     SECTION 'COMPUTE FRACTIONS'.
C
      PARAMETER(F3D8=3.0D0/8.0D0)
      PARAMETER(F1D2=1.0D0/2.0D0)
      PARAMETER(F3D2=3.0D0/2.0D0)
      PARAMETER(F1D4=1.0D0/4.0D0)
      PARAMETER(F5D4=5.0D0/4.0D0)
      PARAMETER(F1D8=1.0D0/8.0D0)
      PARAMETER(F5D12=5.0D0/12.0D0)
      PARAMETER(F1D16=1.0D0/16.0D0)
      PARAMETER(F15D16=15.0D0/16.0D0)
      PARAMETER(F5D24=5.0D0/24.0D0)
      PARAMETER(F3D32=3.0D0/32.0D0)
      PARAMETER(F15D32=15.0D0/32.0D0)
      PARAMETER(F5D64=5.0D0/64.0D0)
      PARAMETER(F35384=35.0D0/384.0D0)
      PARAMETER(F35576=35.0D0/576.0D0)
      PARAMETER(F35D52=35.0D0/1152.0D0)
      PARAMETER(F1D3=1.0D0/3.0D0)
      PARAMETER(F5D16=5.0D0/16.0D0)
C
      REAL*8 PI2                     
C
      DIMENSION OSCELE(6),DPELE(6),ORBEL(5)
      COMMON /BLCNST/ TTO,R,AE,GM,BJ2,BJ3,BJ4,BJ5,FLTINV,XKE,ESQ
      DATA BMU/1.0D0/,RE/1.0D0/,BKSUBC/0.01D0/
      DATA   PI2/6.283185307179586D0/
C
      EK=DSQRT(GM/AE**3)
      DELT=EK*TTO
      GO TO (10,111), IPASS
CI------------------------------I
CI EPOCH ELEMENTS AT EPOCH TIME I
CI------------------------------I
   10 ADP=DPELE(1)/AE
      EDP=DPELE(2)
      BIDP=DPELE(3)
      HDP=DPELE(4)
      GDP=DPELE(5)
      BLDP=DPELE(6)
      A0=ADP
      E0=EDP
      BI0=BIDP
      H0=HDP
      G0=GDP
      BL0=BLDP
      IFLG=0
C---------------------I
C COMPUTE MEAN MOTION I
C---------------------I
      ANU=DSQRT(BMU/A0**3)
C-------------------I
C COMPUTE FRACTIONS I
C-------------------I
C     REPLACED BY PARAMETER STATEMENT ABOVE
C
      BK2=-F1D2*(BJ2*RE*RE)
      BK3=BJ3*RE**3
      BK4=F3D8*(BJ4*RE**4)
      BK5=BJ5*RE**5
      GO TO 153
C
  111 IF(IPERT.EQ.0) GO TO 7
      IF(IDMEAN.NE.0) GO TO 202
      ADP=DPELE(1)/AE
      EDP=DPELE(2)
      BIDP=DPELE(3)
      HDP=DPELE(4)
      GDP=DPELE(5)
      BLDP=DPELE(6)
C
  153 EDP2=EDP*EDP
      CN2=1.0-EDP2
      CN=DSQRT(CN2)
      GM2=BK2/ADP**2
      GMP2=GM2/(CN2*CN2)
      GM4=BK4/ADP**4
      GMP4=GM4/CN**8
      THETA=DCOS(BIDP)
      THETA2=THETA*THETA
      THETA4=THETA2*THETA2
C
  202 IF(IDMEAN.EQ.0) GO TO 155
      IF(IPASS.EQ.2) GO TO 150
C------------------------I
C COMPUTE LDOT,GDOT,HDOT I
C------------------------I
  157 BLDOT=CN*ANU*(GMP2*(F3D2*(3.0*THETA2-1)+GMP2*F3D32*(THETA2
     1*(-96.0*CN+30.0-90.0*CN2)+(16.0*CN+25.0*CN2-15.0)+THETA4
     2*(144.0*CN+25.0*CN2+105.0)))+EDP2*GMP4*F15D16*(3.0+35.0*THETA4
     3-30.0*THETA2))
      GDOT=ANU*(F5D16*GMP4*((THETA2*(126.0*CN2-270.0)+THETA4*(385.0
     1-189.0*CN2))-9.0*CN2+21.0)+GMP2*(F3D32*GMP2*(THETA4*(45.0*CN2
     2+360.0*CN+385.0)+THETA2*(90.0-192.0*CN-126.0*CN2)+(24.0*CN
     3+25.0*CN2-35))+F3D2*(5*THETA2-1)))
      HDOT=ANU*(GMP4*F5D4*THETA*(3.0-7.0*THETA2)*(5.0-3.0*CN2)+GMP2
     1*(GMP2*F3D8*(THETA*(12.0*CN+9.0*CN2-5.0)-THETA*THETA2*(5.0*CN2
     2+36.0*CN+35.0))-3*THETA))
  155 IF(IFLG.EQ.1) GO TO 19
CI--------------------------------------------I
CI COMPUTE ISUBC TO TEST CRITICAL INCLINATION I
CI--------------------------------------------I
      BISUBC=((1.0-5.0*THETA2)**(-2))*((25.0*THETA4*THETA)*(GMP2*EDP2))
      IFLG=1
CI--------------------------------------I
CI FIRST CHECK FOR CRITICAL INCLINATION I
CI--------------------------------------I
      IF(BISUBC.GT.BKSUBC) GO TO 158
      ASSIGN 163 TO ID8
      GO TO 159
C-------------------------------I
C IS THERE CRITICAL INCLINATION I
C-------------------------------I
   19 IF(BISUBC.GT.BKSUBC) GO TO 150
  159 IF(IPERT.EQ.1) GO TO 150
      GM3=BK3/ADP**3
      GMP3=GM3/(CN2*CN2*CN2)
      GM5=BK5/ADP**5
      GMP5=GM5/CN**10
      G3DG2=GMP3/GMP2
      G4DG2=GMP4/GMP2
      G5DG2=GMP5/GMP2
CI---------------I
CI COMPUTE A1-A8 I
CI---------------I
      A1=(F1D8*GMP2*CN2)*(1.0-11.0*THETA2-((40.0*THETA4)/(1.0-5.0*THETA2
     1)))
      A2=(F5D12*G4DG2*CN2)*(1.0-((8.0*THETA4)/(1.0-5.0*THETA2))-3.0
     1*THETA2)
      A3=G5DG2*((3.0*EDP2)+4.0)
      A4=G5DG2*(1.0-(24.0*THETA4)/(1.0-5.0*THETA2)-9.0*THETA2)
      A5=(G5DG2*(3.0*EDP2+4.0))*(1.0-(24.0*THETA4)/(1.0-5.0*THETA2)-9.0
     1*THETA2)
      A6=G3DG2*F1D4
      SINI=DSIN(BIDP)
      A10=CN2*SINI
      A7=A6*A10
      A8P=G5DG2*EDP*(1.0-(16.0*THETA4)/(1.0-5.0*THETA2)-5.0*THETA2)
      A8=A8P*EDP
C
C    COMPUTE B13-B15
C
      B13=EDP*(A1-A2)
      B14=A7+F5D64*A5*A10
      B15=A8*A10*F35384
C
C   COMPUTE A11-A27
C
      A11=2.0+EDP2
      A12=3.0*EDP2+2.0
      A13=THETA2*A12
      A14=(5.0*EDP2+2.0)*(THETA4/(1.0-5.0*THETA2))
      A15=(EDP2*THETA4*THETA2)/((1.0-5.0*THETA2)*(1.0-5.0*THETA2))
      A16=THETA2/(1.0-5.0*THETA2)
      A17=THETA4/((1.0-5.0*THETA2)*(1.0-5.0*THETA2))
      A18=EDP*SINI
      A19=A18/(1.0+CN)
      A21=EDP*THETA
      A22=EDP2*THETA
      SINI2=DSIN(BIDP/2.0)
      COSI2=DCOS(BIDP/2.0)
      TANI2=DTAN(BIDP/2.0)
      A26=16.0*A16+40.0*A17+3.0
      A27=A22*F1D8*(11.0+200.0*A17+80.0*A16)
CI----------------I
CI COMPUTE B1-B12 I
CI----------------I
      B1=CN*(A1-A2)-((A11-400.0*A15-40.0*A14-11.0*A13)*F1D16+(11.0+200.0
     1*A17+80.0*A16)*A22*F1D8)*GMP2+((-80.0*A15-8.0*A14-3.0*A13+A11)
     2*F5D24+F5D12*A26*A22)*G4DG2
      B2=A6*A19*(2.0+CN-EDP2)+F5D64*A5*A19*CN2-F15D32*A4*A18*CN*CN2
     1+(F5D64*A5+A6)*A21*TANI2+(9.0*EDP2+26.0)*F5D64*A4*A18+F15D32*A3
     2*A21*A26*SINI*(1.0-THETA)
      B3=((80.0*A17+5.0+32.0*A16)*A22*SINI*(THETA-1.0)*F35576*G5DG2*EDP)
     1-((A22*TANI2+(2.0*EDP2+3.0*(1.0-CN2*CN))*SINI)*F35D52*A8P)
      B4=CN*EDP*(A1-A2)
      B5=((9.0*EDP2+4.0)*A10*A4*F5D64+A7)*CN
      B6=F35384*A8*CN2*CN*SINI
      B7=((CN2*A18)/(1.0-5.0*THETA2))*(F1D8*GMP2*(1.0-15.0*THETA2)+(1.0
     1-7.0*THETA2)*G4DG2*(-F5D12))
      B8=F5D64*(A3*CN2*(1.0-9.0*THETA2-(24.0*THETA4/(1.0-5.0*THETA2))))
     1+A6*CN2
      B9=A8*F35384*CN2
      B10=SINI*(A22*A26*G4DG2*F5D12-A27*GMP2)
      B11=A21*(A5*F5D64+A6+A3*A26*F15D32*SINI*SINI)
      B12=-((80.0*A17+32.0*A16+5.0)*(A22*EDP*SINI*SINI*F35576*G5DG2)+(A8
     1*A21*F35D52))
C
  150 IF(IPERT.EQ.0) GO TO 7
      IF(IDMEAN.EQ.0) GO TO 4
C-----------------------I
C COMPUTE SECULAR TERMS I
C-----------------------I
CI-----------------------I
CI ''MEAN'' MEAN ANOMALY I
CI-----------------------I
      BLDP=ANU*DELT+BLDOT*DELT+BL0
      BLDP=DMOD(BLDP,PI2)
      IF(BLDP.LT.0.0D0) BLDP=BLDP+PI2
CI---------------------------I
CI  MEAN ARGUMENT OF PERIGEE I
CI---------------------------I
      GDP=GDOT*DELT+G0
      GDP=DMOD(GDP,PI2)
      IF(GDP.LT.0.0D0) GDP=GDP+PI2
C   MEAN LONGITUDE OF ASCENDING NODE
      HDP=HDOT*DELT+H0
      HDP=DMOD(HDP,PI2)
      IF(HDP.LT.0.0D0) HDP=HDP+PI2
    4 DO 33 NN=1,6
   33 OSCELE(NN)=DPELE(NN)
C
      A=ADP
      E=EDP
      BI=BIDP
      H=HDP
      G=GDP
      BL=BLDP
CI-------------------------------------I
CI COMPUTE TRUE ANOMALY(DOUBLE PRIMED) I
CI-------------------------------------I
      EADP=DKEPLR(BLDP,EDP)
      SINDE=DSIN(EADP)
      COSDE=DCOS(EADP)
      SINFD=CN*SINDE
      COSFD=COSDE-EDP
      FDP=DATAN2(SINFD,COSFD)
      IF(FDP.LT.0.D+0) FDP=FDP+PI2
      IF(IPERT.EQ.1) GO TO 7
C
      DADR=(1.0-EDP*COSDE)**(-1)
      SINFD=SINFD*DADR
      COSFD=COSFD*DADR
      CS2GFD=DCOS(2.0*GDP+2.0*FDP)
      DADR2=DADR*DADR
      DADR3=DADR2*DADR
      COSFD2=COSFD*COSFD
CI----------------------------I
CI COMPUTE A(SEMI-MAJOR AXIS) I
CI----------------------------I
      A=ADP*(1.0+GM2*((3.0*THETA2-1.0)*(EDP2/(CN2*CN2*CN2))*(CN+(1.0/(1.
     1+CN)))+((3.0*THETA2-1.0)/(CN2*CN2*CN2))*(EDP*COSFD)*(3.0+3.0*EDP
     2*COSFD+EDP2*COSFD2)+3.0*(1.0-THETA2)*DADR3*CS2GFD))
      SN2GFD=DSIN(2.0*GDP+2.0*FDP)
      SNF2GD=DSIN(2.0*GDP+FDP)
      CSF2GD=DCOS(2.0*GDP+FDP)
      SN2GD=DSIN(2.0*GDP)
      CS2GD=DCOS(2.0*GDP)
      SN3GD=DSIN(3.0*GDP)
      CS3GD=DCOS(3.0*GDP)
      SN3FGD=DSIN(3.0*FDP+2.0*GDP)
      CS3FGD=DCOS(3.0*FDP+2.0*GDP)
      SINGD=DSIN(GDP)
      COSGD=DCOS(GDP)
      GO TO ID8,(163,164)
  163 DLT1E=B14*SINGD+B13*CS2GD-B15*SN3GD
CI------------------------I
CI COMPUTE (L+G+H) PRIMED I
CI------------------------I
      BLGHP=HDP+GDP+BLDP+B3*CS3GD+B1*SN2GD+B2*COSGD
      BLGHP=DMOD(BLGHP,PI2)
      IF(BLGHP.LT.0.0D0) BLGHP=BLGHP+PI2
      EDPDL=B4*SN2GD-B5*COSGD+B6*CS3GD-F1D4*CN2*CN*GMP2*(2.0*(3.0*THETA2
     1-1.0)*(DADR2*CN2+DADR+1.0)*SINFD+3.0*(1.0-THETA2)*((-DADR2*CN2
     2-DADR+1.0)*SNF2GD+(DADR2*CN2+DADR+F1D3)*SN3FGD))
      DLTI=F1D2*THETA*GMP2*SINI*(EDP*CS3FGD+3.0*(EDP*CSF2GD+CS2GFD))
     1-(A21/CN2)*(B8*SINGD+B7*CS2GD-B9*SN3GD)
      SINDH=(1.0/COSI2)*(F1D2*(B12*CS3GD+B11*COSGD+B10*SN2GD-(F1D2*GMP2
     1*THETA*SINI*(6.0*(EDP*SINFD-BLDP+FDP)-(3.0*(SN2GFD+EDP*SNF2GD)+EDP
     2*SN3FGD)))))
CI-----------------I
CI COMPUTE (L+G+H) I
CI-----------------I
  164 BLGH=BLGHP+((1.0/(CN+1.0))*F1D4*EDP*GMP2*CN2*(3.0*(1.0-THETA2)*
     1(SN3FGD*(F1D3+DADR2*CN2+DADR)+SNF2GD*(1.0-(DADR2*CN2+DADR)))+2.0*
     2SINFD*(3.0*THETA2-1.0)*(DADR2*CN2+DADR+1.0)))+GMP2*F3D2*((-2.0*
     3THETA-1.0+5.0*THETA2)*(EDP*SINFD+FDP-BLDP))+(3.0+2.0*THETA-5.0*
     4THETA2)*(GMP2*F1D4*(EDP*SN3FGD+3.0*(SN2GFD+EDP*SNF2GD)))
      BLGH=DMOD(BLGH,PI2)
      IF(BLGH.LT.0.0D0) BLGH=BLGH+PI2
      DLTE=DLT1E+(F1D2*CN2*((3.0*(1.0/(CN2*CN2*CN2))*GM2*(1.0-THETA2)
     1*CS2GFD*(3.0*EDP*COSFD2+3.0*COSFD+EDP2*COSFD*COSFD2+EDP))-(GMP2
     2*(1.0-THETA2)*(3.0*CSF2GD+CS3FGD))+(3.0*THETA2-1.0)*GM2*(1.0/
     3(CN2*CN2*CN2))*(EDP*CN+(EDP/(1.0+CN))+3.0*EDP*COSFD2+3.0*COSFD+
     4EDP2*COSFD*COSFD2)))
      EDPDL2=EDPDL*EDPDL
      EDPDE2=(EDP+DLTE)*(EDP+DLTE)
CI-------------------------I
CI COMPUTE E(ECCENTRICITY) I
CI-------------------------I
      E=DSQRT(EDPDL2+EDPDE2)
      SINDH2=SINDH*SINDH
      SQUAR=(DLTI*COSI2*F1D2+SINI2)*(DLTI*COSI2*F1D2+SINI2)
      SQRI=DSQRT(SINDH2+SQUAR)
CI--------------------------I
CI COMPUTE BI (INCLINATION) I
CI--------------------------I
      BI=DASIN(SQRI)
      BI=2.0*BI
      BI=DMOD(BI,PI2)
      IF(BI.LT.0.0D0) BI=BI+PI2
CI-----------------------------I
CI CHECK FOR E(ECCENTRICITY)=0 I
CI-----------------------------I
      IF(E.NE.0.0) GO TO 168
      BL=0.0
CI-----------------------------I
CI CHECK FOR BI(INCLINATION)=0 I
CI-----------------------------I
  145 IF(BI.NE.0.0) GO TO 169
      H=0.0
CI---------------------------------I
CI COMPUTE G(ARGUMENT  OF PERIGEE) I
CI---------------------------------I
  146 G=BLGH-BL-H
      G=DMOD(G,PI2)
      IF(G.LT.0.0D0) G=G+PI2
CI----------------------I
CI COMPUTE TRUE ANOMALY I
CI----------------------I
      EA  =DKEPLR(BL,E)
      ARG1=DSIN(EA) * DSQRT(1.0-E**2)
      ARG2=DCOS(EA)-E
      F=DATAN2(ARG1,ARG2)
      IF(F.LT.0.D+0) F=F+PI2
C
      OSCELE(1)=A*AE
      OSCELE(2)=E
      OSCELE(3)=BI
      OSCELE(4)=H
      OSCELE(5)=G
      OSCELE(6)=BL
    7 CONTINUE
      DPELE(1)=ADP*AE
      DPELE(2)=EDP
      DPELE(3)=BIDP
      DPELE(4)=HDP
      DPELE(5)=GDP
      DPELE(6)=BLDP
      IF(IPERT.EQ.0) BL=DMOD(ANU*DELT,PI2)
      ORBEL(1)=EADP
      ORBEL(2)=GDP+FDP
      ORBEL(3)=GDP
      ORBEL(4)=EK*(ANU+BLDOT)
      ORBEL(5)=FDP
      R=A*AE*(1.0D0-E*DCOS(EA))
      GO TO 45
CI----------------------------------------I
CI MODIFICATIONS FOR CRITICAL INCLINATION I
CI----------------------------------------I
  158 DLT1E=0.0
      BLGHP=0.0
      EDPDL=0.0
      DLTI=0.0
      SINDH=0.0
      ASSIGN 164 TO ID8
      GO TO 150
C
  168 SINLDP=DSIN(BLDP)
      COSLDP=DCOS(BLDP)
      SINHDP=DSIN(HDP)
      COSHDP=DCOS(HDP)
CI-------------------------I
CI COMPUTE L(MEAN ANOMALY) I
CI-------------------------I
      ARG1=EDPDL*COSLDP+(EDP+DLTE)*SINLDP
      ARG2=(EDP+DLTE)*COSLDP-(EDPDL*SINLDP)
      BL=DATAN2(ARG1,ARG2)
      BL=DMOD(BL,PI2)
      IF(BL.LT.0.0D0) BL=BL+PI2
      GO TO 145
CI----------------------------------------I
CI COMPUTE H(LONGITUDE OF ASCENDING NODE) I
CI----------------------------------------I
  169 ARG1=SINDH*COSHDP+SINHDP*(F1D2*DLTI*COSI2+SINI2)
      ARG2=COSHDP*(F1D2*DLTI*COSI2+SINI2)-(SINDH*SINHDP)
      H=DATAN2(ARG1,ARG2)
      H=DMOD(H,PI2)
      IF(H.LT.0.0D0) H=H+PI2
      GO TO 146
   45 CONTINUE
      RETURN
      END
      FUNCTION DARCTAN (X,Y)
      REAL*8 X,Y,DARCTAN
      DARCTAN=360.
      IF(ABS(X)+ABS(Y).LE.0) RETURN
      DARCTAN=57.29578*DATAN2(Y,X)
      RETURN
      END
