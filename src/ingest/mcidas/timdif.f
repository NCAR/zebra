C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.
      FUNCTION TIMDIF(IYRDA1,IHMS1,IYRDA2,IHMS2)
C *** McIDAS Revision History ***
C 1 TIMDIF.FOR 23-Mar-90,12:38:46,`SSEC' PC-McIDAS ver 5.00
C 2 TIMDIF.FOR 25-Sep-90,7:28:46,`SMG' First Release into COMmon
C 3 TIMDIF.FOR 1-Apr-94,1:43:24,`BARRYR' Add proprietary statement
C 4 TIMDIF.FOR 2-May-94,17:33:56,`USER' Released
C 5 TIMDIF.FOR 4-Oct-96,9:46:52,`BILLL' Added programmer documentation (7055)
C 6 TIMDIF.FOR 21-Oct-96,16:29:08,`USER' Released
C *** McIDAS Revision History ***

*$ Name:
*$      timdif - Returns difference between two times in minutes.
*$
*$ Interface:
*$      double precision function
*$      timdif(integer iyrda1, integer ihms1, integer iyrda2,
*$             integer ihms2)
*$
*$ Input:
*$      iyrda1  - Year/day of first time (yyddd).
*$      ihms1   - Hours/minutes/seconds of first time (hhmmss).
*$      iyrda2  - Year/day of second time (yyddd).
*$      ihms2   - Hours/minutes/seconds of second time (hhmmss).
*$
*$ Input and Output:
*$      none
*$      param2  - description of it
*$
*$ Output:
*$      none
*$      param3  - description of it
*$
*$ Return values:
*$      The difference between the two times, in minutes.
*$      If the first time is greater than the second, the result
*$      will be negative.
*$
*$ Remarks:
*$      none
*$
*$ Categories:
*$      utility
*$      converter
*$      day/time

      DOUBLE PRECISION TIMDIF,D1,D2,T1,T2
      IY1=MOD(IYRDA1/1000,100)
      ID1=MOD(IYRDA1,1000)
      IFAC1=(IY1-1)/4+1
      D1=365*(IY1-1)+IFAC1+ID1-1
      IY2=MOD(IYRDA2/1000,100)
      ID2=MOD(IYRDA2,1000)
      IFAC2=(IY2-1)/4+1
      D2=365*(IY2-1)+IFAC2+ID2-1
      T1=1440.D0*D1+60.D0*FLALO(IHMS1)
      T2=1440.D0*D2+60.D0*FLALO(IHMS2)
      TIMDIF=T2-T1
      RETURN
      END
