      FUNCTION NVXINI(IFUNC,IARR)
C *** McIDAS Revision History ***
C 1 NVXGOES.DLM 23-Mar-90,12:28:46,`SSEC' PC-McIDAS ver 5.00
C 2 NVXGOES.DLM 26-Sep-90,12:42:36,`SMG' First Release into COMmon
C 3 NVXGOES.DLM 11-Mar-91,11:42:34,`SMG' make angles, satpos to inline subs
C 4 NVXGOES.DLM 11-Mar-91,11:42:34,`ERICN' Released
C *** McIDAS Revision History ***
C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.
C ? NVXGOE  (DAS)
C
C THIS ROUTINE SETS UP COMMON BLOCKS NAVCOM AND NAVINI FOR USE BY THE
C NAVIGATION TRANSFORMATION ROUTINES NVXSAE AND NVXEAS.
C NVXINI SHOULD BE RECALLED EVERY TIME A TRANSFORMATION IS DESIRED
C FOR A PICTURE WITH A DIFFERENT TIME THAN THE PREVIOUS CALL.
C IFUNC IS 1 (INITIALIZE; SET UP COMMON BLOCKS)
C          2 (ACCEPT/PRODUCE ALL EARTH COORDINATES IN LAT/LON
C            FORM IF IARR IS 'LL  ' OR IN THE X,Y,Z COORDINATE FRAME
C            IF IARR IS 'XYZ '.
C            THIS AFFECTS ALL SUBSEQUENT NVXEAS OR NVXSAE CALLS.)
C IARR IS AN INTEGER ARRAY (DIM 128) IF IFUNC=1, CONTAINING NAV
C        PARAMETERS
C
      INTEGER IARR(*)
      CHARACTER*2 CLLSW
      COMMON/NAVCOM/NAVDAY,LINTOT,DEGLIN,IELTOT,DEGELE,SPINRA,IETIMY,IET
     1IMH,SEMIMA,OECCEN,ORBINC,PERHEL,ASNODE,NOPCLN,DECLIN,RASCEN,PICLIN
     2,PRERAT,PREDIR,PITCH,YAW,ROLL,SKEW
      COMMON /BETCOM/IAJUST,IBTCON,NEGBET,ISEANG
      COMMON /VASCOM/SCAN1,TIME1,SCAN2,TIME2
      COMMON /NAVINI/
     1  EMEGA,AB,ASQ,BSQ,R,RSQ,
     2  RDPDG,
     3  NUMSEN,TOTLIN,RADLIN,
     4  TOTELE,RADELE,PICELE,
     5  CPITCH,CYAW,CROLL,
     6  PSKEW,
     7  RFACT,ROASIN,TMPSCL,
     8  B11,B12,B13,B21,B22,B23,B31,B32,B33,
     9  GAMMA,GAMDOT,
     A  ROTM11,ROTM13,ROTM21,ROTM23,ROTM31,ROTM33,
     B  PICTIM,XREF
      COMMON /NVUNIT/ LLSW,IOLD
      INCLUDE 'hex80.inc'
      DATA MISVAL/ HEX80 /
      DATA JINIT/0/
C
C
      IF (JINIT.EQ.0) THEN
         JINIT=1
         LLSW=0
         JDAYSV=-1
         JTIMSV=-1
         IOLD=0
      ENDIF
      IF (IFUNC.EQ.2) THEN
         CALL MOVWC(IARR,CLLSW)
         IF (CLLSW.EQ.'LL') LLSW=0
         IF (CLLSW.EQ.'XY') LLSW=1
         NVXINI=0
         RETURN
      ENDIF
      JTYPE=IARR(1)
      IF (JTYPE.NE.LIT('GOES')) GOTO 90
      JDAY=IARR(2)
      JTIME=IARR(3)
      IF(JDAY.EQ.JDAYSV.AND.JTIME.EQ.JTIMSV) GO TO 10
C
C-----INITIALIZE NAVCOM
C
      NAVDAY=MOD(JDAY,100000)
      DO 20 N=7,12
      IF(IARR(N).GT.0) GO TO 25
   20 CONTINUE
      GO TO 90
   25 IETIMY=ICON1(IARR(5))
      IETIMH=100*(IARR(6)/100)+IROUND(.6*MOD(IARR(6),100))
      SEMIMA=FLOAT(IARR(7))/100.0
      OECCEN=FLOAT(IARR(8))/1000000.0
      ORBINC=FLOAT(IARR(9))/1000.0
      XMEANA=FLOAT(IARR(10))/1000.0
      PERHEL=FLOAT(IARR(11))/1000.0
      ASNODE=FLOAT(IARR(12))/1000.0
      CALL EPOCH(IETIMY,IETIMH,SEMIMA,OECCEN,XMEANA)
      IF (IARR(5).EQ.0) GOTO 90
CCC     IF(IARR(5).NE.0.AND.IARR(9).NE.0) IEXIST=IEXIST-10
      DECLIN=FLALO(IARR(13))
      RASCEN=FLALO(IARR(14))
      PICLIN=IARR(15)
      IF (IARR(15).GE.1000000) PICLIN=PICLIN/10000.
      IF (IARR(13).EQ.0.AND.IARR(14).EQ.0.AND.IARR(15).EQ.0)
     *   GOTO 90
C-----ADDED 9/83 TO SUPPORT FRACTIONAL VALUES FOR PICLIN
      SPINRA=IARR(16)/1000.0
      IF(IARR(16).NE.0.AND.SPINRA.LT.300.0) SPINRA=60000.0/SPINRA
      IF (IARR(16).EQ.0) GOTO 90
      DEGLIN=FLALO(IARR(17))
      LINTOT=IARR(18)
      DEGELE=FLALO(IARR(19))
      IELTOT=IARR(20)
      PITCH=FLALO(IARR(21))
      YAW=FLALO(IARR(22))
      ROLL=FLALO(IARR(23))
      SKEW=IARR(29)/100000.0
      IF (IARR(29).EQ.MISVAL) SKEW=0.
C
C-----INITIALIZE BETCOM
C
      IAJUST=IARR(25)
      ISEANG=IARR(28)
      IBTCON=6289920
      NEGBET=3144960
C
C
C-----INITIALIZE NAVINI COMMON BLOCK
C
C
      EMEGA=.26251617
      AB=40546851.22
      ASQ=40683833.48
      BSQ=40410330.18
      R=6371.221
      RSQ=R*R
      RDPDG=1.745329252E-02
      NUMSEN=MOD(LINTOT/100000,100)
      IF(NUMSEN.LT.1)NUMSEN=1
      TOTLIN=NUMSEN*MOD(LINTOT,100000)
      RADLIN=RDPDG*DEGLIN/(TOTLIN-1.0)
      TOTELE=IELTOT
      RADELE=RDPDG*DEGELE/(TOTELE-1.0)
      PICELE=(1.0+TOTELE)/2.0
      CPITCH=RDPDG*PITCH
      CYAW=RDPDG*YAW
      CROLL=RDPDG*ROLL
      PSKEW=ATAN2(SKEW,RADLIN/RADELE)
      STP=SIN(CPITCH)
      CTP=COS(CPITCH)
      STY=SIN(CYAW-PSKEW)
      CTY =COS(CYAW-PSKEW)
      STR=SIN(CROLL)
      CTR=COS(CROLL)
      ROTM11=CTR*CTP
      ROTM13=STY*STR*CTP+CTY*STP
      ROTM21=-STR
      ROTM23=STY*CTR
      ROTM31=-CTR*STP
      ROTM33=CTY*CTP-STY*STR*STP
      RFACT=ROTM31**2+ROTM33**2
      ROASIN=ATAN2(ROTM31,ROTM33)
      TMPSCL=SPINRA/3600000.0
      DEC=DECLIN*RDPDG
      SINDEC=SIN(DEC)
      COSDEC=COS(DEC)
      RAS=RASCEN*RDPDG
      SINRAS=SIN(RAS)
      COSRAS=COS(RAS)
      B11=-SINRAS
      B12=COSRAS
      B13=0.0
      B21=-SINDEC*COSRAS
      B22=-SINDEC*SINRAS
      B23=COSDEC
      B31=COSDEC*COSRAS
      B32=COSDEC*SINRAS
      B33=SINDEC
      XREF=RAERAC(NAVDAY,0,0.0)*RDPDG
C
C-----TIME-SPECIFIC PARAMETERS (INCL GAMMA)
C
      PICTIM=FLALO(JTIME)
      GAMMA=FLOAT(IARR(39))/100.
      GAMDOT=FLOAT(IARR(40))/100.
C
C-----INITIALIZE VASCOM
C
      ISS=JDAY/100000
      IF ((ISS.GT.25.OR.ISS.EQ.12).AND.IARR(31).GT.0) THEN
C        THIS SECTION DOES VAS BIRDS AND GMS
C        IT USES TIMES AND SCAN LINE FROM BETA RECORDS
         SCAN1=FLOAT(IARR(31))
         TIME1=FLALO(IARR(32))
         SCAN2=FLOAT(IARR(35))
         TIME2=FLALO(IARR(36))
      ELSE
C        THIS SECTION DOES THE OLD GOES BIRDS
         SCAN1=1.
         TIME1=FLALO(JTIME)
         SCAN2=FLOAT(MOD(LINTOT,100000))
         TIME2=TIME1+SCAN2*TMPSCL
      ENDIF
      IOLD=0
C
C-----ALL DONE. EVERYTHING OK
C
 10   CONTINUE
      JDAYSV=JDAY
      JTIMSV=JTIME
      NVXINI=0
      RETURN
 90   NVXINI=-1
      RETURN
      END
C
C
C
C
C
C
      FUNCTION NVXSAE(XLIN,XELE,XDUM,XPAR,YPAR,ZPAR)
C TRANSFORMS SAT COOR TO EARTH COOR.
C ALL PARAMETERS REAL*4
C INPUTS:
C XLIN,XELE ARE SATELLITE LINE AND ELEMENT (IMAGE COORDS.)
C XDUM IS DUMMY (IGNORE)
C OUTPUTS:
C XPAR,YPAR,ZPAR REPRESENT EITHER LAT,LON,(DUMMY) OR X,Y,Z DEPENDING
C ON THE OPTION SET IN PRIOR NVXINI CALL WITH IFUNC=2.
C
C FUNC VAL IS 0 (OK) OR -1 (CAN'T; E.G. OFF OF EARTH)
C
C
      COMMON/NAVCOM/NAVDAY,LINTOT,DEGLIN,IELTOT,DEGELE,SPINRA,IETIMY,IET
     1IMH,SEMIMA,OECCEN,ORBINC,PERHEL,ASNODE,NOPCLN,DECLIN,RASCEN,PICLIN
     2,PRERAT,PREDIR,PITCH,YAW,ROLL,SKEW
      COMMON/NAVINI/
     1  EMEGA,AB,ASQ,BSQ,R,RSQ,
     2  RDPDG,
     3  NUMSEN,TOTLIN,RADLIN,
     4  TOTELE,RADELE,PICELE,
     5  CPITCH,CYAW,CROLL,
     6  PSKEW,
     7  RFACT,ROASIN,TMPSCL,
     8  B11,B12,B13,B21,B22,B23,B31,B32,B33,
     9  GAMMA,GAMDOT,
     A  ROTM11,ROTM13,ROTM21,ROTM23,ROTM31,ROTM33,
     B  PICTIM,XREF
      COMMON /NVUNIT/ LLSW,IOLD
      DATA PI/3.14159265/
C
C
      ILIN=IROUND(XLIN)
      PARLIN=(ILIN-1)/NUMSEN+1
      FRAMET=TMPSCL*PARLIN
      SAMTIM=FRAMET+PICTIM
      CALL SATVEC(SAMTIM,XSAT,YSAT,ZSAT)
      YLIN=(XLIN-PICLIN)*RADLIN
      YELE=(XELE-PICELE+GAMMA+GAMDOT*SAMTIM)*RADELE
      XCOR=B11*XSAT+B12*YSAT+B13*ZSAT
      YCOR=B21*XSAT+B22*YSAT+B23*ZSAT
      ROT=ATAN2(YCOR,XCOR)+PI
      YELE=YELE-ROT
      COSLIN=COS(YLIN )
      SINLIN=SIN(YLIN)
      SINELE=SIN(YELE)
      COSELE=COS(YELE)
      ELI=ROTM11*COSLIN-ROTM13*SINLIN
      EMI=ROTM21*COSLIN-ROTM23*SINLIN
      ENI=ROTM31*COSLIN-ROTM33*SINLIN
      TEMP=ELI
      ELI=COSELE*ELI+SINELE*EMI
      EMI=-SINELE*TEMP+COSELE*EMI
      ELO=B11*ELI+B21*EMI+B31*ENI
      EMO=B12*ELI+B22*EMI+B32*ENI
      ENO=B13*ELI+B23*EMI+B33*ENI
      BASQ=BSQ/ASQ
      ONEMSQ=1.0-BASQ
      AQ=BASQ+ONEMSQ*ENO**2
      BQ=2.0*((ELO*XSAT+EMO*YSAT)*BASQ+ENO*ZSAT)
      CQ=(XSAT**2+YSAT**2)*BASQ+ZSAT**2-BSQ
      RAD=BQ**2-4.0*AQ*CQ
      IF(RAD.LT.1.0)GO TO 2
      S=-(BQ+SQRT(RAD))/(2.0*AQ)
      X=XSAT+ELO*S
      Y=YSAT+EMO*S
      Z=ZSAT+ENO*S
      CT=COS(EMEGA*SAMTIM+XREF)
      ST=SIN(EMEGA*SAMTIM+XREF)
      X1=CT*X+ST*Y
      Y1=-ST*X+CT*Y
      IF (LLSW.EQ.0) THEN
         CALL NXYZLL(X1,Y1,Z,XPAR,YPAR)
         ZPAR=0.
      ELSE
         XPAR=X1
         YPAR=Y1
         ZPAR=Z
      ENDIF
      NVXSAE=0
      RETURN
    2 NVXSAE=-1
      RETURN
      END
C
C
C
      FUNCTION NVXEAS(XPAR,YPAR,ZPAR,XLIN,XELE,XDUM)
C  FIXED 0Z PROBLEM (INIT. OF ORBTIM TO 0.0)
C 5/26/82;  TRANSFORM EARTH TO SATELLITE COORDS
C ALL PARAMETERS REAL*4
C INPUTS:
C XPAR,YPAR,ZPAR REPRESENT EITHER LAT,LON,(DUMMY) OR X,Y,Z DEPENDING
C ON THE OPTION SET IN PRIOR NVXINI CALL WITH IFUNC=2.
C OUTPUTS:
C XLIN,XELE ARE SATELLITE LINE AND ELEMENT (IMAGE COORDS.)
C XDUM IS DUMMY (IGNORE)
C
C FUNC VAL IS 0 (OK) OR -1 (CAN'T; E.G. BAD LAT/LON)
C
C
      COMMON/NAVINI/
     1  EMEGA,AB,ASQ,BSQ,R,RSQ,
     2  RDPDG,
     3  NUMSEN,TOTLIN,RADLIN,
     4  TOTELE,RADELE,PICELE,
     5  CPITCH,CYAW,CROLL,
     6  PSKEW,
     7  RFACT,ROASIN,TMPSCL,
     8  B11,B12,B13,B21,B22,B23,B31,B32,B33,
     9  GAMMA,GAMDOT,
     A  ROTM11,ROTM13,ROTM21,ROTM23,ROTM31,ROTM33,
     B  PICTIM,XREF
      COMMON/NAVCOM/NAVDAY,LINTOT,DEGLIN,IELTOT,DEGELE,SPINRA,IETIMY,IET
     1IMH,SEMIMA,OECCEN,ORBINC,PERHEL,ASNODE,NOPCLN,DECLIN,RASCEN,PICLIN
     2,PRERAT,PREDIR,PITCH,YAW,ROLL,SKEW
      COMMON/VASCOM/SCAN1,TIME1,SCAN2,TIME2
      COMMON /NVUNIT/ LLSW,IOLD
      DATA OLDLIN/910./,ORBTIM/-99999./
C
C
      NVXEAS=0
      IF (LLSW.EQ.0) THEN
         IF (ABS(XPAR).GT.90.) THEN
            NVXEAS=-1
            RETURN
         ENDIF
         CALL NLLXYZ(XPAR,YPAR,X1,Y1,Z)
      ELSE
         X1=XPAR
         Y1=YPAR
         Z=ZPAR
      ENDIF
      XDUM=0.0
      SAMTIM=TIME1
      DO 50 I=1,2
      IF(ABS(SAMTIM-ORBTIM).LT.0.0005) GO TO 10
      CALL SATVEC(SAMTIM,XSAT,YSAT,ZSAT)
      ORBTIM=SAMTIM
      XHT=SQRT(XSAT**2+YSAT**2+ZSAT**2)
   10 CT=COS(EMEGA*SAMTIM+XREF)
      ST=SIN(EMEGA*SAMTIM+XREF)
      X=CT*X1-ST*Y1
      Y= ST*X1+CT*Y1
      VCSTE1=X-XSAT
      VCSTE2=Y-YSAT
      VCSTE3=Z-ZSAT
      VCSES3=B31*VCSTE1+B32*VCSTE2+B33*VCSTE3
      ZNORM=SQRT(VCSTE1**2+VCSTE2**2+VCSTE3**2)
      X3=VCSES3/ZNORM
      UMV=ATAN2(X3,SQRT(RFACT-X3**2))-ROASIN
      XLIN=PICLIN-UMV/RADLIN
      PARLIN=IFIX(XLIN-1.0)/NUMSEN
      IF(I.EQ.2) GO TO 50
      SAMTIM=TIME2
      OLDLIN=XLIN
   50 CONTINUE
      SCNNUM=(IFIX(OLDLIN+XLIN)/2.0-1.0)/NUMSEN
      SCNFRC=(SCNNUM-SCAN1)/(SCAN2-SCAN1)
      XLIN=OLDLIN+SCNFRC*(XLIN-OLDLIN)
      SAMTIM=TIME1+TMPSCL*(SCNNUM-SCAN1)
      CALL SATVEC(SAMTIM,XSAT,YSAT,ZSAT)
      COSA=X*XSAT+Y*YSAT+Z*ZSAT
      CTST=0.0001*R*XHT+RSQ
      IF(COSA.LT.CTST) NVXEAS=-1
      XSATS1=B11*XSAT+B12*YSAT+B13*ZSAT
      YSATS2=B21*XSAT+B22*YSAT+B23*ZSAT
      CT=COS(EMEGA*SAMTIM+XREF)
      ST=SIN(EMEGA*SAMTIM+XREF)
      X=CT*X1-ST*Y1
      Y= ST*X1+CT*Y1
      VCSTE1=X-XSAT
      VCSTE2=Y-YSAT
      VCSTE3=Z-ZSAT
      VCSES1=B11*VCSTE1+B12*VCSTE2+B13*VCSTE3
      VCSES2=B21*VCSTE1+B22*VCSTE2+B23*VCSTE3
      VCSES3=B31*VCSTE1+B32*VCSTE2+B33*VCSTE3
      XNORM=SQRT(ZNORM**2-VCSES3**2)
      YNORM=SQRT(XSATS1**2+YSATS2**2)
      ZNORM=SQRT(VCSTE1**2+VCSTE2**2+VCSTE3**2)
      X3=VCSES3/ZNORM
      UMV=ATAN2(X3,SQRT(RFACT-X3**2))-ROASIN
      SLIN=SIN(UMV)
      CLIN=COS(UMV)
      U=ROTM11*CLIN+ROTM13*SLIN
      V=ROTM21*CLIN+ROTM23*SLIN
      XELE=PICELE+ASIN((XSATS1*VCSES2-YSATS2*VCSES1)/(XNORM*YNORM))/RADE
     1LE
      XELE=XELE+ATAN2(V,U)/RADELE
      XELE=XELE-GAMMA-GAMDOT*SAMTIM
      RETURN
      END
C
C
C
      FUNCTION NVXOPT(IFUNC,XIN,XOUT)
C
C IFUNC= 'SPOS'    SUBSATELLITE LAT/LON
C
C        XIN - NOT USED
C        XOUT - 1. SUB-SATELLITE LATITUDE
C             - 2. SUB-SATELLITE LONGITUDE
C
C
C IFUNC= 'ANG '   ANGLES
C
C        XIN - 1. SSYYDDD
C              2. TIME (HOURS)
C              3. LATITUDE
C              4. LONGITUDE (***** WEST NEGATIVE *****)
C        XOUT- 1. SATELLITE ANGLE
C              2. SUN ANGLE
C              3. RELATIVE ANGLE
C
C
C
C IFUNC= 'HGT '   INPUT HEIGHT FOR PARALLAX
C
C        XIN - 1. HEIGHT (KM)
C
      REAL*4 XIN(*),XOUT(*)
      CHARACTER*4 CLIT,CFUNC
      COMMON /NAVINI/
     1  EMEGA,AB,ASQ,BSQ,R,RSQ,
     2  RDPDG,
     3  NUMSEN,TOTLIN,RADLIN,
     4  TOTELE,RADELE,PICELE,
     5  CPITCH,CYAW,CROLL,
     6  PSKEW,
     7  RFACT,ROASIN,TMPSCL,
     8  B11,B12,B13,B21,B22,B23,B31,B32,B33,
     9  GAMMA,GAMDOT,
     A  ROTM11,ROTM13,ROTM21,ROTM23,ROTM31,ROTM33,
     B  PICTIM,XREF
      DATA LASDAY/-1/,LASTIM/-1/
      DATA A/6378.388/,B/6356.912/,RR/6371.221/
      CFUNC=CLIT(IFUNC)
CCC   CALL DDEST('IN NVXOPT '//CFUNC,0)
      NVXOPT=0
      IF(CFUNC.EQ.'SPOS') THEN
         INORB=0
         NTIME=ITIME(PICTIM)
         CALL SATPOS(INORB,NTIME,X,Y,Z)
         CALL NXYZLL(X,Y,Z,XOUT(1),XOUT(2))
      ELSE IF(CFUNC.EQ.'ANG ') THEN
         JDAY=IROUND(XIN(1))
         JTIME=ITIME(XIN(2))
         FLAT=XIN(3)
         FLON=XIN(4)
         IF(JDAY.NE.LASDAY.OR.JTIME.NE.LASTIM) THEN
            CALL SOLARP(JDAY,JTIME,GHA,DEC,XLAT,XLON)
            LASDAY=JDAY
            LASTIM=JTIME
         ENDIF
         CALL ANGLES(JDAY,JTIME,FLAT,FLON,GHA,DEC,ZENLOC,SZEN,RELANG)
         XOUT(1)=ZENLOC
         XOUT(2)=SZEN
         XOUT(3)=RELANG
      ELSE IF(CFUNC.EQ.'HGT ') THEN
         HGT=XIN(1)
         ASQ=(A+HGT)*(A+HGT)
         BSQ=(B+HGT)*(B+HGT)
         AB=(A+HGT)*(B+HGT)
         R=RR+HGT
         RSQ=R*R
      ELSE
         NVXOPT=1
      ENDIF
      RETURN
      END
C
C
C
C
C-----SUBSIDIARY SUBPROGRAMS
C
C
C
      FUNCTION ICON1(YYMMDD)
C
C     CONVERTS YYMMDD TO YYDDD
C
      IMPLICIT INTEGER(A-Z)
      DIMENSION NUM(12)
      DATA NUM/0,31,59,90,120,151,181,212,243,273,304,334/
C
      YEAR=MOD(YYMMDD/10000,100)
      MONTH=MOD(YYMMDD/100,100)
      DAY=MOD(YYMMDD,100)
      IF(MONTH.LT.0.OR.MONTH.GT.12)MONTH=1
      JULDAY=DAY+NUM(MONTH)
      IF(MOD(YEAR,4).EQ.0.AND.MONTH.GT.2) JULDAY=JULDAY+1
      ICON1=1000*YEAR+JULDAY
      RETURN
      END
C
C
      SUBROUTINE EPOCH(IETIMY,IETIMH,SEMIMA,OECCEN,XMEANA)
C EPOCH  PHILLI 0173 NAV: FINDS TIME OF PERIGEE FROM KEPLERIAN EPOCH
C
C
      PARAMETER (PI=3.14159265)
      PARAMETER (RDPDG=PI/180.0)
      PARAMETER (RE=6378.388)
      PARAMETER (GRACON=0.07436574)
      LEAPYR(IY)=366-(MOD(IY,4)+3)/4
C
C
      XMMC=GRACON*SQRT(RE/SEMIMA)**3
      XMANOM=RDPDG*XMEANA
      TIME=(XMANOM-OECCEN*SIN(XMANOM))/(60.0*XMMC)
      TIME1=FLALO(IETIMH)
      TIME=TIME1-TIME
      IDAY=0
      IF(TIME.GT.48.0)GO TO 8
      IF(TIME.GT.24.0)GO TO 1
      IF(TIME.LT.-24.0)GO TO 2
      IF(TIME.LT.0.0)GO TO 3
      GO TO 4
 8    TIME=TIME-48.0
      IDAY=2
      GO TO 4
 1    TIME=TIME-24.0
      IDAY=1
      GO TO 4
 2    TIME=TIME+48.0
      IDAY=-2
      GO TO 4
 3    TIME=TIME+24.0
      IDAY=-1
 4    IETIMH=ITIME(TIME)
      IF(IDAY.EQ.0)RETURN
      JYEAR=MOD(IETIMY/1000,100)
      JDAY=MOD(IETIMY,1000)
      JDAY=JDAY+IDAY
      IF(JDAY.LT.1)GO TO 5
      JTOT=LEAPYR(JYEAR)
      IF(JDAY.GT.JTOT)GO TO 6
      GO TO 7
 5    JYEAR=JYEAR-1
      JDAY=LEAPYR(JYEAR)+JDAY
      GO TO 7
 6    JYEAR=JYEAR+1
      JDAY=JDAY-JTOT
 7    IETIMY=1000*JYEAR+JDAY
      RETURN
      END
C
C
C SATVEC PHILLI 0880 NAVLIB  COMPUTES EARTH SATELLITE AS FUNCTION OF TIM
C VECTOR EARTH-CENTER-TO-SAT (FUNC OF TIME)
      SUBROUTINE SATVEC(SAMTIM,X,Y,Z)
      DOUBLE PRECISION TWOPI,PI720,DE,TE,DRA,TRA,DNAV,TDIFRA,TDIFE
      DOUBLE PRECISION PI,RDPDG,RE,GRACON,SOLSID,SHA
      DOUBLE PRECISION DIFTIM,ECANM1,ECANOM,XMANOM
      DOUBLE PRECISION DABS,DSQRT,DSIN,DCOS
      COMMON/NAVCOM/NAVDAY,LINTOT,DEGLIN,IELTOT,DEGELE,SPINRA,IETIMY,IET
     1IMH,SEMIMA,OECCEN,ORBINC,PERHEL,ASNODE,NOPCLN,DECLIN,RASCEN,PICLIN
     2,PRERAT,PREDIR,PITCH,YAW,ROLL,SKEW
      COMMON /NVUNIT/ LLSW,IOLD
      DATA NAVSAV/0/
      IF(IOLD.EQ.1) GO TO 10
      IOLD=1
      NAVSAV=NAVDAY
      PI=3.14159265D0
      TWOPI=2.0*PI
      PI720=PI/720.
      RDPDG=PI/180.0
      RE=6378.388
      GRACON=.07436574D0
      SOLSID=1.00273791D0
      SHA=100.26467D0
      SHA=RDPDG*SHA
      IRAYD=74001
      IRAHMS=0
      O=RDPDG*ORBINC
      P=RDPDG*PERHEL
      A=RDPDG*ASNODE
      SO=SIN(O)
      CO=COS(O)
      SP=SIN(P)*SEMIMA
      CP=COS(P)*SEMIMA
      SA=SIN(A)
      CA=COS(A)
      PX=CP*CA-SP*SA*CO
      PY=CP*SA+SP*CA*CO
      PZ=SP*SO
      QX=-SP*CA-CP*SA*CO
      QY=-SP*SA+CP*CA*CO
      QZ=CP*SO
      SROME2=SQRT(1.0-OECCEN)*SQRT(1.0+OECCEN)
      XMMC=GRACON*RE*DSQRT(RE/SEMIMA)/SEMIMA
      IEY=MOD(IETIMY/1000,100)
      IED=MOD(IETIMY,1000)
      IEFAC=(IEY-1)/4+1
      DE=365*(IEY-1)+IEFAC+IED-1
      TE=1440.0*DE+60.0*FLALO(IETIMH)
      IRAY=IRAYD/1000
      IRAD=MOD(IRAYD,1000)
      IRAFAC=(IRAY-1)/4+1
      DRA=365*(IRAY-1)+IRAFAC+IRAD-1
      TRA=1440.0*DRA+60.0*FLALO(IRAHMS)
      INAVY=MOD(NAVDAY/1000,100)
      INAVD=MOD(NAVDAY,1000)
      INFAC=(INAVY-1)/4+1
      DNAV=365*(INAVY-1)+INFAC+INAVD-1
      TDIFE=DNAV*1440.-TE
      TDIFRA=DNAV*1440.-TRA
      EPSILN=1.0E-8
   10 TIMSAM=SAMTIM*60.0
      DIFTIM=TDIFE+TIMSAM
      XMANOM=XMMC*DIFTIM
      ECANM1=XMANOM
      DO 2 I=1,20
      ECANOM=XMANOM+OECCEN*DSIN(ECANM1)
      IF(DABS(ECANOM-ECANM1).LT.EPSILN)GO TO 3
 2    ECANM1=ECANOM
 3    XOMEGA=DCOS(ECANOM)-OECCEN
      YOMEGA=SROME2*DSIN(ECANOM)
      Z =XOMEGA*PZ+YOMEGA*QZ
      Y =XOMEGA*PY+YOMEGA*QY
      X =XOMEGA*PX+YOMEGA*QX
      RETURN
      END
C
C
C
C MRNLLXYZ MB   09/15/83; BYPASS GEOLAT & MAKE WEST = +
C MRLLXYZ  MB    7/09/82;  CNVERT LAT/LON TO/FROM EARTH-CENTERED X,Y,Z
      SUBROUTINE NLLXYZ(XLAT,XLON,X,Y,Z)
C-----CONVERT LAT,LON TO EARTH CENTERED X,Y,Z
C     (DALY, 1978)
C-----XLAT,XLON ARE IN DEGREES, WITH NORTH AND WEST POSITIVE
C-----X,Y,Z ARE GIVEN IN KM. THEY ARE THE COORDINATES IN A RECTANGULAR
C        FRAME WITH ORIGIN AT THE EARTH CENTER, WHOSE POSITIVE
C        X-AXIS PIERCES THE EQUATOR AT LON 0 DEG, WHOSE POSITIVE Y-AXIS
C        PIERCES THE EQUATOR AT LON 90 DEG, AND WHOSE POSITIVE Z-AXIS
C        INTERSECTS THE NORTH POLE.
      COMMON /NAVINI/
     1  EMEGA,AB,ASQ,BSQ,RR,RSQ,
     2  RDPDG,
     3  NUMSEN,TOTLIN,RADLIN,
     4  TOTELE,RADELE,PICELE,
     5  CPITCH,CYAW,CROLL,
     6  PSKEW,
     7  RFACT,ROASIN,TMPSCL,
     8  B11,B12,B13,B21,B22,B23,B31,B32,B33,
     9  GAMMA,GAMDOT,
     A  ROTM11,ROTM13,ROTM21,ROTM23,ROTM31,ROTM33,
     B  PICTIM,XREF
      YLAT=RDPDG*XLAT
C-----CONVERT TO GEOCENTRIC (SPHERICAL) LATITUDE
CCC     YLAT=GEOLAT(YLAT,1)
      YLAT=ATAN2(BSQ*SIN(YLAT),ASQ*COS(YLAT))
      YLON=-RDPDG*XLON
      SNLT=SIN(YLAT)
      CSLT=COS(YLAT)
      CSLN=COS(YLON)
      SNLN=SIN(YLON)
      TNLT=(SNLT/CSLT)**2
      R=AB*SQRT((1.0+TNLT)/(BSQ+ASQ*TNLT))
      X=R*CSLT*CSLN
      Y=R*CSLT*SNLN
      Z=R*SNLT
      RETURN
      END
      SUBROUTINE NXYZLL(X,Y,Z,XLAT,XLON)
C-----CONVERT EARTH-CENTERED X,Y,Z TO LAT & LON
C-----X,Y,Z ARE GIVEN IN KM. THEY ARE THE COORDINATES IN A RECTANGULAR
C        COORDINATE SYSTEM WITH ORIGIN AT THE EARTH CENTER, WHOSE POS.
C        X-AXIS PIERCES THE EQUATOR AT LON 0 DEG, WHOSE POSITIVE Y-AXIS
C        PIERCES THE EQUATOR AT LON 90 DEG, AND WHOSE POSITIVE Z-AXIS
C        INTERSECTS THE NORTH POLE.
C-----XLAT,XLON ARE IN DEGREES, WITH NORTH AND WEST POSITIVE
C
      COMMON /NAVINI/
     1  EMEGA,AB,ASQ,BSQ,R,RSQ,
     2  RDPDG,
     3  NUMSEN,TOTLIN,RADLIN,
     4  TOTELE,RADELE,PICELE,
     5  CPITCH,CYAW,CROLL,
     6  PSKEW,
     7  RFACT,ROASIN,TMPSCL,
     8  B11,B12,B13,B21,B22,B23,B31,B32,B33,
     9  GAMMA,GAMDOT,
     A  ROTM11,ROTM13,ROTM21,ROTM23,ROTM31,ROTM33,
     B  PICTIM,XREF
C
      XLAT=100.0
      XLON=200.0
      IF(X.EQ.0..AND.Y.EQ.0..AND.Z.EQ.0.) GO TO 90
      A=ATAN(Z/SQRT(X*X+Y*Y))
C-----CONVERT TO GEODETIC LATITUDE
CCC     XLAT=GEOLAT(ATAN(Z/SQRT(X*X+Y*Y)),2)/RDPDG
      XLAT=ATAN2(ASQ*SIN(A),BSQ*COS(A))/RDPDG
      XLON=-ATAN2(Y,X)/RDPDG
   90 RETURN
      END
      SUBROUTINE ANGLES(JDAY,JTIME,XLAT,XLON,GHA,DEC,SATANG,SUNANG,
     * RELANG)
C $ SUBROUTINE ANGLES(JDAY,JTIME,XLAT,XLON,GHA,DEC,SATANG,SUNANG,RELANG)
C $ ANGLES - computes zenith angles of sun and satellite and relative
C $   azimuth angle              (DAS)
C $ INPUT:
C $   JDAY = (I) picture day (YYDDD)
C $   JTIME = (I) picture start time
C $   XLAT = (R) latitude of point
C $   XLON = (R) longitude of point
C $   GHA = (R) Greenwich hour angle of sun
C $   DEC = (R) declination of sun
C $ OUTPUT:
C $   SATANG = (R) zenith angle of satellite
C $   SUNANG = (R) zenith angle of sun
C $   RELANG = (R) relative angle
C $$ ANGLES = COMPUTATION, NAVIGATION
C ANGLES MOSHER 1074 WINLIB  ZENITH ANGLES TO SAT,SUN,AND REL AZIMUTH AN
C
      DATA IDAY/0/
      DATA PI/3.14159265/
      DATA R/6371.221/
      RDPDG=PI/180.0
      IF(IDAY.EQ.JDAY)GO TO 1
      IDAY=JDAY
      INORB=0
 1    PICTIM=FTIME(JTIME)
C
C   DETERMINE SATELLITE POSITION
C
      CALL SATPOS(INORB,JTIME,XSAT,YSAT,ZSAT)
      HEIGHT=SQRT(XSAT**2+YSAT**2+ZSAT**2)
      YLAT=RDPDG*XLAT
      YLAT=GEOLAT(YLAT,1)
      YLON=RDPDG*XLON
      SLAT=SIN(YLAT)
      CLAT=COS(YLAT)
      SLON=SIN(YLON)
      CLON=COS(YLON)
      XSAM=R*CLAT*CLON
      YSAM=R*CLAT*SLON
      ZSAM=R*SLAT
C
C   DETERMINE ZENITH ANGLE OF SUN
C
      SNLG=-PICTIM*PI/12.0-RDPDG*GHA
      SNDC=RDPDG*DEC
      COSDEC=COS(SNDC)
      US=COS(SNLG)*COSDEC
      VS=SIN(SNLG)*COSDEC
      WS=SIN(SNDC)
      SUNANG=ACOS((US*XSAM+VS*YSAM+WS*ZSAM)/R)/RDPDG
C
C   DETERMINE ZENITH ANGLE OF SATELLITE
C
      XVEC=XSAT-XSAM
      YVEC=YSAT-YSAM
      ZVEC=ZSAT-ZSAM
      XFACT=SQRT(XVEC**2+YVEC**2+ZVEC**2)
      SATANG=ACOS((XVEC*XSAM+YVEC*YSAM+ZVEC*ZSAM)/(R*XFACT))/RDPDG
C
C   DETERMINE RELATIVE ANGLE
C
      X1=CLAT*CLON
      Y1=CLAT*SLON
      Z1=SLAT
      X2=SLON
      Y2=-CLON
      X3=-SLAT*CLON
      Y3=-SLAT*SLON
      Z3=CLAT
      XC1=US-X1
      YC1=VS-Y1
      ZC1=WS-Z1
      XC2=XSAT/HEIGHT-X1
      YC2=YSAT/HEIGHT-Y1
      ZC2=ZSAT/HEIGHT-Z1
      XAN1=XC1*X3+YC1*Y3+ZC1*Z3
      XAN2=XC2*X3+YC2*Y3+ZC2*Z3
      YAN1=XC1*X2+YC1*Y2
      YAN2=XC2*X2+YC2*Y2
      XAN3=XAN1*XAN2+YAN1*YAN2
      YAN3=-YAN1*XAN2+XAN1*YAN2
      RELANG=ATAN2(YAN3,XAN3)/RDPDG
      RELANG=ABS(RELANG)
      RETURN
      END
      SUBROUTINE SATPOS(INORB,NTIME,X,Y,Z)
C SATPOS PHILLI 0174 NAVLIB  SAT POSITION VECTOR FROM EARTH CENTER
C $ SUBROUTINE SATPOS(INORB, NTIME, X, Y, Z)  (DAS)
C $ CALCULATE SATELLITE POSITION VECTOR FROM THE EARTH'S CENTER.
C $ INORB = (I) INPUT  INITIALIZATION FLAG - SHOULD = 0 ON FIRST CALL TO
C $   SATPOS, 1 ON ALL SUBSEQUENT CALLS.
C $ NTIME = (I) INPUT  TIME (HOURS, MINUTES, SECONDS) IN HHMMSS FORMAT
C $ X, Y, Z = (R) OUTPUT  COORDINATES OF POSITION VECTOR
C $$ SATPOS = NAVIGATION, COMPUTATION
C
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*4 DEGLIN,DEGELE,SPINRA,SEMIMA,OECCEN,ORBINC,PERHEL
      REAL*4 ASNODE,DECLIN,RASCEN,PICLIN,PRERAT,PREDIR,PITCH,YAW
      REAL*4 ROLL,SKEW
      REAL*4 X,Y,Z
CCC     REAL*8 PI,RDPDG,RE,GRACON,SOLSID,SHA
CCC     REAL*8 DIFTIM,ECANM1,ECANOM,RA,XMANOM,TIMDIF
CCC     REAL*8 DABS,DMOD,DSQRT,DSIN,DCOS,DATAN2
      COMMON/NAVCOM/NAVDAY,LINTOT,DEGLIN,IELTOT,DEGELE,SPINRA,IETIMY,IET
     1IMH,SEMIMA,OECCEN,ORBINC,PERHEL,ASNODE,NOPCLN,DECLIN,RASCEN,PICLIN
     2,PRERAT,PREDIR,PITCH,YAW,ROLL,SKEW
C
C
      IF(INORB.NE.0)GO TO 1
      INORB=1
      PI=3.14159265D0
      RDPDG=PI/180.0
      RE=6378.388
      GRACON=.07436574D0
      SOLSID=1.00273791D0
      SHA=100.26467D0
      SHA=RDPDG*SHA
      IRAYD=74001
      IRAHMS=0
      O=RDPDG*ORBINC
      P=RDPDG*PERHEL
      A=RDPDG*ASNODE
      SO=SIN(O)
      CO=COS(O)
      SP=SIN(P)*SEMIMA
      CP=COS(P)*SEMIMA
      SA=SIN(A)
      CA=COS(A)
      PX=CP*CA-SP*SA*CO
      PY=CP*SA+SP*CA*CO
      PZ=SP*SO
      QX=-SP*CA-CP*SA*CO
      QY=-SP*SA+CP*CA*CO
      QZ=CP*SO
      SROME2=SQRT(1.0-OECCEN)*SQRT(1.0+OECCEN)
      XMMC=GRACON*RE*DSQRT(RE/SEMIMA)/SEMIMA
 1    DIFTIM=TIMDIF(IETIMY,IETIMH,NAVDAY,NTIME)
      XMANOM=XMMC*DIFTIM
      ECANM1=XMANOM
      EPSILN=1.0E-8
      DO 2 I=1,20
      ECANOM=XMANOM+OECCEN*DSIN(ECANM1)
      IF(DABS(ECANOM-ECANM1).LT.EPSILN)GO TO 3
 2    ECANM1=ECANOM
 3    XOMEGA=DCOS(ECANOM)-OECCEN
      YOMEGA=SROME2*DSIN(ECANOM)
      XS=XOMEGA*PX+YOMEGA*QX
      YS=XOMEGA*PY+YOMEGA*QY
      ZS=XOMEGA*PZ+YOMEGA*QZ
      DIFTIM=TIMDIF(IRAYD,IRAHMS,NAVDAY,NTIME)
      RA=DIFTIM*SOLSID*PI/720.0D0+SHA
      RAS=DMOD(RA,2.0*PI)
      CRA=COS(RAS)
      SRA=SIN(RAS)
      X=CRA*XS+SRA*YS
      Y=-SRA*XS+CRA*YS
      Z=ZS
      RETURN
      END
