C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.

C *** McIDAS Revision History ***
C 1 FTIME.FOR 23-Mar-90,12:38:58,`SSEC' PC-McIDAS ver 5.00
C 2 FTIME.FOR 25-Sep-90,7:28:44,`SMG' First Release into COMmon
C 3 FTIME.FOR 1-Apr-94,1:12:36,`BARRYR' Add proprietary statement
C 4 FTIME.FOR 2-May-94,16:40:44,`USER' Released
C 5 FTIME.FOR 22-Mar-96,11:48:54,`DWS' reglue: modified file
C 6 FTIME.FOR 25-Mar-96,13:52:26,`USER' Released
C 7 FTIME.FOR 11-Sep-96,11:56:56,`BILLL' Added programmer documentation
C      (6926).
C 8 FTIME.FOR 23-Sep-96,12:37:44,`USER' Released
C *** McIDAS Revision History ***

C $ FUNCTION FTIME(M)  (BL)
C $ CONVERT PACKED INTEGER (SIGN HH MM SS) TIME TO REAL
C $ M = (I) INPUT  PACKED INTEGER (SIGN HH MM SS) TIME
C $$ FTIME = CONVERT, INTEGER, TIME, REAL
C

*$ Name:
*$      ftime   - Converts a packed integer time value to a real.
*$
*$ Interface:
*$      real function
*$      ftime(integer m)
*$
*$ Input:
*$      m       - Time, formatted as:
*$                 sign hhmmss.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      The reformatted time.
*$
*$ Remarks:
*$      The reformatted time is in terms of hours and fractions of
*$      hours.  93500 is returned as 9.5833.
*$
*$      The sign can be negative.
*$
*$ Categories:
*$      utility
*$      converter
*$      day/time

      REAL FUNCTION FTIME(M)
      IF(M.LT.0)GO TO 1
      N=M
      X=1.0
      GO TO 2
 1    N=-M
      X=-1.0
 2    FTIME=FLOAT(N/10000)+FLOAT(MOD(N/100,100))/60.0+FLOAT(MOD(N,100))/
     &3600.0
      FTIME=X*FTIME
      RETURN
      END
