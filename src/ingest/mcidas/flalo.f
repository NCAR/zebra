C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.

C *** McIDAS Revision History ***
C 1 FLALO.FOR 19-Mar-90,21:49:24,`SSEC' PC-McIDAS ver 5.00
C 2 FLALO.FOR 25-Sep-90,7:33:56,`SMG' First Release into COMmon
C 3 FLALO.FOR 1-Apr-94,1:11:14,`BARRYR' Add proprietary statement
C 4 FLALO.FOR 2-May-94,16:38:40,`USER' Released
C 5 FLALO.FOR 22-Mar-96,11:48:34,`DWS' reglue: modified file
C 6 FLALO.FOR 25-Mar-96,13:52:08,`USER' Released
C 7 FLALO.FOR 12-Jun-96,12:58:26,`BILLL' Added programmer documentation
C      (6553).
C 8 FLALO.FOR 6-Sep-96,10:17:08,`USER' Released
C *** McIDAS Revision History ***

*$ Name:
*$      flalo - Converts a packed integer (SIGN DDD MM SS) latitude/longitude
*$              to real.
*$
*$ Interface:
*$      real function
*$      flalo(integer m)
*$
*$ Input:
*$      m     - Integer containing the packed data.
*$
*$ Input and Output:
*$	none
*$
*$ Output:
*$	none
*$
*$ Return values:
*$            - Converted representation of a lat/lon.
*$
*$
*$ Remarks:
*$      No error checking is performed. If the input is not formatted
*$      correctly the output will be inaccurate.
*$
*$ Categories: 
*$	utility 
*$	converter 

C
      REAL FUNCTION FLALO(M)
      IF(M.LT.0)GO TO 1
      N=M
      X=1.0
      GO TO 2
 1    N=-M
      X=-1.0
 2    FLALO=FLOAT(N/10000)+FLOAT(MOD(N/100,100))/60.0+FLOAT(MOD(N,100))/
     &3600.0
      FLALO=X*FLALO
      RETURN
      END
