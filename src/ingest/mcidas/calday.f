C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.

C *** McIDAS Revision History ***
C 1 CALDAY.FOR 17-Apr-90,8:01:52,`SSEC' PC-McIDAS ver 5.00
C 2 CALDAY.FOR 24-Sep-90,18:19:46,`SMG' First Release into COMmon
C 3 CALDAY.FOR 1-Apr-94,1:02:06,`BARRYR' Add proprietary statement
C 4 CALDAY.FOR 2-May-94,16:24:06,`USER' Released
C 5 CALDAY.FOR 22-Mar-96,11:46:46,`DWS' reglue: modified file
C 6 CALDAY.FOR 25-Mar-96,13:50:20,`USER' Released
C 7 CALDAY.FOR 5-Sep-96,12:27:24,`BILLL' Added programmer documentation
C      (6920).
C 8 CALDAY.FOR 23-Sep-96,12:33:48,`USER' Released
C *** McIDAS Revision History ***

C $ SUBROUTINE CALDAY(SYD, YR, MON, DAY, AMON )       (JOE)
C $ CONVERT SSYYDDD TO DAY, MONTH, YEAR & CHARACTER MONTH
C $ SYD = (I) INPUT  SATELLITE DAY (SSSYYDDD FORMAT)
C $ YR = (I) OUTPUT  YEAR
C $ MON = (I) OUTPUT  MONTH
C $ DAY = (I) OUTPUT  DAY
C $ AMON = (I) OUTPUT CHARACTER MONTH
C $$ CALDAY = CONVERT, DATE
C-----NOTE: OLD HARRIS VERSION RETURNED YR AS 19YY; THIS VERSION
C-----   RETURNS IT AS JUST YY.
C

*$ Name:
*$      calday  - Converts SSYYDDD to day, month, year and character month.
*$
*$ Interface:
*$      subroutine
*$      mcname(integer syd, integer yr, integer mon, integer day,
*$             integer amon)
*$
*$ Input:
*$      syd     - Satellite day formatted as SSSYYDDD.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      yy      - Year (as yy).
*$      mon     - Month.
*$      day     - Day.
*$      amon    - Month name, as a character string.
*$
*$ Return values:
*$      none
*$
*$ Remarks:
*$      none
*$
*$ Categories:
*$      utility
*$      converter
*$      day/time

C
      SUBROUTINE CALDAY(SYD,YR,MON,DAY,AMON)
      IMPLICIT INTEGER (A,B,D-Z)
      CHARACTER CMON(12)*4
      DIMENSION DN(12,2)
      DATA CMON/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     &  'SEP','OCT','NOV','DEC'/
      DATA DN/31,59,90,120,151,181,212,243,273,304,334,365,
     &        31,60,91,121,152,182,213,244,274,305,335,366/
C
C
      YY=MOD(SYD/1000,100)
      DDD=MOD(SYD,1000)
      ILY=1
      IF (MOD(YY,4).EQ.0) ILY=2
      DO 20 ID=1,12
      IF (DDD.LE.DN(ID,ILY)) GOTO 21
 20   CONTINUE
 21   MON=ID
      DAY=DDD
      IF (MON.GT.1) DAY=DDD-DN(MON-1,ILY)
      YR=YY
      CALL MOVCW(CMON(MON),AMON)
      RETURN
      END
