C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.
C *** McIDAS Revision History ***
C 1 CFZ.FOR 27-Feb-90,21:47:28,`SSEC' PC-McIDAS ver 5.00
C 2 CFZ.FOR 25-Sep-90,7:35:58,`SMG' First Release into COMmon
C 3 CFZ.FOR 1-Apr-94,1:03:14,`BARRYR' Add proprietary statement
C 4 CFZ.FOR 2-May-94,16:25:54,`USER' Released
C 5 CFZ.FOR 6-Sep-96,9:18:56,`BILLL' Added programmer documentation (6920).
C 6 CFZ.FOR 23-Sep-96,12:34:16,`USER' Released
C *** McIDAS Revision History ***

*$ Name:
*$      cfz     - Returns a hexadecimal representation of the data in an
*$                integer.
*$
*$ Interface:
*$      character*12 function
*$      cfz(integer l)
*$
*$ Input:
*$      l       - Field whose data is to be converted.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      Character string representation of the data field as hexadecimal,
*$      right justified, with blank fill.
*$
*$ Remarks:
*$      none
*$
*$ Categories:
*$      text
*$      utility
*$      converter

       CHARACTER*12 FUNCTION CFZ(L)
       IMPLICIT CHARACTER*12 (C)
       WRITE(C,1)L
 1     FORMAT(4X,Z8)
       CFZ=C
       RETURN
       END
