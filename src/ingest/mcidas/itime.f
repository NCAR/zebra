C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.

C *** McIDAS Revision History ***
C 1 ITIME.FOR 23-Mar-90,12:32:54,`SSEC' PC-McIDAS ver 5.00
C 2 ITIME.FOR 25-Sep-90,7:28:54,`SMG' First Release into COMmon
C 3 ITIME.FOR 1-Apr-94,1:18:20,`BARRYR' Add proprietary statement
C 4 ITIME.FOR 2-May-94,16:50:18,`USER' Released
C 5 ITIME.FOR 19-Feb-96,15:54:10,`DWS' reglue: modified file
C 6 ITIME.FOR 20-Feb-96,11:54:58,`USER' Released
C 7 ITIME.FOR 18-Sep-96,7:23:12,`BILLL' Added programmer documentation (6993)
C 8 ITIME.FOR 4-Oct-96,10:22:26,`USER' Released
C *** McIDAS Revision History ***

C --- All calls to this function should be changed to calls to M0ITIME()
C --- because some systems provide an ITIME() function with different
C --- semantics.

*$ Name:
*$      itime  - Converts a time in real format to packed integer,
*$               then returns it.
*$
*$ Interface:
*$      integer function
*$      itime(real x)
*$
*$ Input:
*$      x       - Time, in real format.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      Time is returned as (Sign hh mm ss)
*$
*$ Remarks:
*$      The time can be negative.
*$
*$      12.5 is returned as 123000.
*$
*$ Categories:
*$      utility
*$      converter
*$      day/time

      INTEGER FUNCTION ITIME(X)
      IMPLICIT NONE
      REAL X

C --- external functions

      INTEGER M0ITIME

      ITIME=M0ITIME(X)
      RETURN
      END
