C *** McIDAS Revision History ***
C 1 IFTOK.FOR 27-Feb-90,9:34:44,`SSEC' PC-McIDAS ver 5.00
C 2 IFTOK.FOR 24-Sep-90,18:19:20,`SMG' First Release into COMmon
C 3 IFTOK.FOR 1-Jul-94,9:20:00,`USER' Released
C 4 IFTOK.FOR 24-Sep-96,12:36:02,`BILLL' Added programmer documentation
C      (6993).
C 5 IFTOK.FOR 4-Oct-96,10:22:00,`USER' Released
C *** McIDAS Revision History ***
C $ FUNCTION IFTOK(CTOK)  (TMW)
C $ CONVERT NUMERIC CHARACTER TOKEN TO INTERNAL FORM. FN VAL IS INTEGER
C $   (ROUNDED) PART OF NUMERIC  VALUE.
C $ CTOK = (C) INPUT  CHARACTER TOKEN TO CONVERT
C $$ IFTOK = CONVERT, UTILITY, SCANNER
C
C-----CONTAINS ENTRY POINTS IFTOK,DFTOK
C-----CONVERT NUMERIC TOKEN TO BINARY
C-----TOKEN TO CONVERT IS IN CTOK
C-----FUNCTION VALUE:
C        IFTOK: INTEGER (ROUNDED) PART OF NUMERIC VALUE
C        DFTOK: REAL*8
C-----FUNCTION COMPUTED BY CALCULATING IVAL & ISCAL, WHERE TRUE
C        VALUE = IVAL * (10**ISCAL). ISCAL IS ARBITRARILY RESTRICTED TO
C        BETWEEN -30 AND 30.
C-----IF SYNTAX ERROR OR OUT-OF-RANGE CONDITION DETECTED, 0 IS
C        RETURNED
C
C-----NUMBER IN CTOK CAN APPEAR AS: HEX (E.G. $7FFF) DECIMAL INTEGER
C        (E.G. -123) FLOATING POINT (E.G. -123. , -123.E-6 , -123.D15)
C        BABYLONIAN (E.G. 12:30 , -179:59:59)
C        DATE (E.G. 83/1/31 , 2883/1/31 (28 IS SS) , 28/// )
C
C
C
*$ Name:
*$      iftok  - Provides conversions from tokens to integers and
*$               dates/lats/lons in various integer formats.
*$
*$      NOTE   - This code has been superseded by a group of routines
*$               in daytime.c and in argconv.c.  People doing
*$               development should use these utilities.
*$
*$ Categories:
*$      utility
*$      converter
*$      day/time

      FUNCTION IFTOK(CTOK)
      PARAMETER (IMAXL=214748364,IMAXR=7)
      IMPLICIT CHARACTER*12 (C)
      IMPLICIT REAL*8 (D)
      CHARACTER CTOK*(*)
      CHARACTER*33 CSTR
      CHARACTER*2 C2
      CHARACTER*1 C
      INTEGER IPART(4)
      INTEGER MAXVAL(3)
      INTEGER *2 ITYPE,IZERO,ICHA,J,ISIGN,ICH,IDIG,K,IC,INDEX,NP
      INTEGER *2 JJ
      DATA MAXVAL/9999,59,59/
C
C
C
C----(FUNCTION IFTOK(CTOK))
         ITYPE=1
         GOTO 1
      ENTRY DFTOK(CTOK)



*$ Name:
*$      dftok  - Provides conversions from tokens to double precision
*$               reals and dates/lats/lons in various formats.
*$
*$      NOTE   - This code has been superseded by a group of routines
*$               in daytime.c and in argconv.c.
*$               People doing development should use these utilities.
*$
*$ Categories:
*$      utility
*$      converter
*$      day/time

C $ ENTRY DFTOK(CTOK)  (TMW)
C $ CONVERT NUMERIC CHARACTER TOKEN TO INTERNAL FORM. FN VAL IS REAL*8.
C $ CTOK = (C) INPUT  CHARACTER TOKEN TO CONVERT
C $$ DFTOK = CONVERT, UTILITY, SCANNER
         ITYPE=2
         GOTO 1
C
C
C
 1    IZERO=ICHAR('0')
       ICHA=ICHAR('A')
      LENC=MIN0(32,LEN(CTOK))
C-----LOCATE FIRST NON-BLANK
      DO 2 J=1,LENC
         C=CTOK(J:J)
         IF (C.NE.' ') GOTO 3
 2    CONTINUE
      GOTO 89
 3    CSTR=CTOK(J:LENC)
      IVAL=0
      ISCAL=0
      ISIGN=1
      C=CSTR(1:1)
      K=1
      IF (C.EQ.'+') THEN
         K=2
      ELSE IF (C.EQ.'-') THEN
         K=2
         ISIGN=-1
      ENDIF
      C2=CSTR(1:2)
C
C
      IF (C.EQ.'$') THEN
C
C-----CONVERT HEX NUMBER
      IF (C2.EQ.'$ '.OR.CSTR(10:10).NE.' ') GOTO 89
      DO 5 J=2,9
         C=CSTR(J:J)
         IF (C.EQ.' ') GOTO 90
         ICH=ICHAR(C)
         IDIG=ICH-IZERO
         IF (IDIG.GE. 0 .AND.IDIG.LE. 9 ) GOTO 6
         IDIG=ICH-ICHA+ 10
         IF (IDIG.GE. 10 .AND.IDIG.LE. 15 ) GOTO 6
         GOTO 89
 6       CONTINUE
         IF (J.LT. 9 ) THEN
            IVAL=IVAL*16+IDIG
         ELSE
            K=IC(IVAL,0)
            CALL STC(0,IVAL,0)
            IVAL=IVAL*16+IDIG
            CALL STC(K*16+IC(IVAL,0),IVAL,0)
         ENDIF
 5    CONTINUE
C
C
      ELSE IF (INDEX(CSTR(1:16),':').GT.0) THEN
C
C
C-----CONVERT BABYLONIAN TOKEN (E.G. TIME-- 12:45:00) TO INTERNAL
C        BINARY (SCALED INTEGER) FORM. IVAL IS HHDDDDD, WHERE HH IS
C        HOURS AND DDDDD IS FRACTIONS OF AN HOUR
C
C
      IVAL=0
      ISCAL=-5
      IF (C2.EQ.': ') THEN
         CALL GETTIM(L)
         LSECS=MOD(L,10000)/100*60+MOD(L,100)
         IVAL=L/10000*100000+(LSECS*1000+18)/36
         GOTO 90
      ENDIF
      IPART(1)=0
      IPART(2)=0
      IPART(3)=0
C-----NP COUNTS # OF ':' SEEN (PLUS 1)
      NP=1
C
C-----GET NEXT CHARACTER
C
      DO 22 J=K,16
         C=CSTR(J:J)
         IF (C.EQ.' ') THEN
            L=((IPART(2)*60+IPART(3))*1000+18)/36
            IVAL=IPART(1)*100000+L
            IVAL=IVAL*ISIGN
            GOTO 90
         ELSE IF (C.EQ.':') THEN
            NP=NP+ 1
            IF (NP.GT. 3 ) GOTO 89
         ELSE IF (C.GE.'0'.AND.C.LE.'9') THEN
            IPART(NP)=IPART(NP)*10+ICHAR(C)-IZERO
            IF (IPART(NP).GT.MAXVAL(NP)) GOTO 89
         ELSE
            GOTO 89
         ENDIF
 22   CONTINUE
C
C
      ELSE IF (INDEX(CSTR(1:16),'/').GT.0) THEN
C
C
C-----CONVERT DATE IN (GENERAL) FORM SS/YY/MM/DD OR SSYY/MM/DD
      CALL GETDAY(IYYDDD)
      DO 31 J=1,4
 31   IPART(J)=0
      NP=1
      DO 35 J=K,16
         C=CSTR(J:J)
         IF (C.EQ.' ') THEN
C-----      REVERSE IPART ARRAY SO THAT DAY IS IN IPART(1), MON
C           IN IPART(2), ETC.
            I=IPART(1)
            IPART(1)=IPART(NP)
            IPART(NP)=I
            IF (NP.GT. 3 ) THEN
               I=IPART(2)
               IPART(2)=IPART(3)
               IPART(3)=I
            ENDIF
            IF (IPART(3).EQ.0) IPART(3)=IYYDDD/1000
            IF (IPART(1).EQ.0.AND.IPART(2).EQ.0) THEN
               IVAL=MOD(IYYDDD,1000)+IPART(3)*1000
            ELSE IF (IPART(1).EQ.0.OR.IPART(2).EQ.0) THEN
               GOTO 89
            ELSE
               IF (IPART(1).GT.31) GOTO 89
               IF (IPART(2).GT.12) GOTO 89
               IVAL=IDMYYD(IPART(1),IPART(2),IPART(3))
            ENDIF
            IF (IPART(4).GT.9999) GOTO 89
            IVAL=IVAL+IPART(4)*100000
            IVAL=ISIGN*IVAL
            GOTO 90
         ELSE IF (C.EQ.'/') THEN
            NP=NP+ 1
            IF (NP.GT. 4 ) GOTO 89
         ELSE IF (C.GE.'0'.AND.C.LE.'9') THEN
            IPART(NP)=IPART(NP)*10+ICHAR(C)-IZERO
         ELSE
            GOTO 89
         ENDIF
 35   CONTINUE
C
C
      ELSE
C
C
C-----CONVERT NUMERIC (INTEGER OR FLOATING POINT) VALUE
C-----SAMPLE FORMS: 0, +12, 12 , 12., -12.3, +12.3@09, .004@-11
C
C
      IF (C2.EQ.'. '.OR.C2.EQ.'+ '.OR.C2.EQ.'- ') GOTO 89
C-----NDEC COUNTS # OF PLACES AFTER DECIMAL POINT
      NDEC=-1
C-----IEXPSN IS SIGN OF EXPONENT
      IEXPSN=1
C
C-----NEXT-CHARACTER LOOP
C
      DO 42 J=K,16
         C=CSTR(J:J)
         II=ICHAR(C)
         IF (C.EQ.' ') GOTO 45
         IF (C.EQ.'.') THEN
            IF (NDEC.GE.0) GOTO 89
            NDEC=0
         ELSE IF (C.GE.'0'.AND.C.LE.'9') THEN
            IDIG=ICHAR(C)-IZERO
           IF(IVAL.GT.IMAXL.OR.(IVAL.EQ.IMAXL.AND.IDIG.GT.IMAXR))GOTO 891
            IVAL=IVAL*10+IDIG
            IF (NDEC.GE.0) NDEC=NDEC+ 1
         ELSE IF (C.EQ.'E'.OR.C.EQ.'D') THEN
            JJ=J+ 1
            C=CSTR(JJ:JJ)
            JJ=JJ+ 1
            IF (C.EQ.'+') THEN
               C=CSTR(JJ:JJ)
               JJ=JJ+ 1
            ELSE IF (C.EQ.'-') THEN
               IEXPSN=-1
               C=CSTR(JJ:JJ)
               JJ=JJ+ 1
            ENDIF
            IF (C.LT.'0'.OR.C.GT.'9') GOTO 89
            ISCAL=ICHAR(C)-IZERO
            C=CSTR(JJ:JJ)
            IF (C.EQ.' ') GOTO 45
            IF (C.LT.'0'.OR.C.GT.'9') GOTO 89
            ISCAL=ISCAL*10+ICHAR(C)-IZERO
            GOTO 45
         ELSE
            GOTO 89
         ENDIF
 42   CONTINUE
 45   CONTINUE
      IVAL=IVAL*ISIGN
      ISCAL=IEXPSN*ISCAL-MAX0(0,NDEC)
      IF (ISCAL.LT.-30.OR.ISCAL.GT.30) GOTO 89
      GOTO 90
C
C
      ENDIF
C
C-----DONE
C
C-----ERROR CONDITION COMES HERE
 89   CONTINUE
      IVAL=0
      ISCAL=0
C-----ALL ORDINARY RETURNS COME HERE
 90   CONTINUE
C-----CONVERT TO CORRECT OUTPUT TYPE
      IF (ITYPE.EQ. 1 ) THEN
C-----   INTEGER TYPE. IF IABS(INTEGER) .GE. 1D10, 0 IS RETURNED
         IF (IVAL.EQ.0.OR.ISCAL.EQ.0) THEN
            IFTOK=IVAL
            RETURN
         ELSE
            D=DFLOAT(IABS(IVAL))*(10.D0**ISCAL)
            IF (D.LE.1.D9) THEN
               IFTOK=IDINT(D+.5D0)
               IF (IVAL.LT.0) IFTOK=-IFTOK
            ELSE
               IFTOK=0
            ENDIF
         ENDIF
      ELSE
         DFTOK=DFLOAT(IVAL)*(10.D0**ISCAL)
      ENDIF
      RETURN
      END
