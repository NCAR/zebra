C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.
C *** McIDAS Revision History ***
C 1 LIT.FOR 27-Feb-90,9:04:00,`SSEC' PC-McIDAS ver 5.00
C 2 LIT.FOR 24-Sep-90,18:19:26,`SMG' First Release into COMmon
C 3 LIT.FOR 1-Apr-94,1:22:02,`BARRYR' Add proprietary statement
C 4 LIT.FOR 2-May-94,16:57:00,`USER' Released
C 5 LIT.FOR 12-Jun-96,14:22:00,`BILLL' Added programmer documentation
C      (6653).
C 6 LIT.FOR 6-Sep-96,10:18:02,`USER' Released
C *** McIDAS Revision History ***

*$ Name:
*$      lit     - Returns the four bytes of a character string as an integer.
*$
*$ Interface:
*$	character*4 function
*$      lit(integer c)
*$
*$ Input:
*$      c       - Character string that is copied to an integer.
*$
*$ Input and Output:
*$	none
*$
*$ Output:
*$	none
*$
*$ Return values:
*$      The four bytes of the character string.
*$
*$ Remarks:
*$      This function is designed to move the four bytes of a character string
*$      to an integer variable.  It enables programmers to avoid conversions
*$      implicit in replacement statements in which variables of different
*$      types are on opposite sides of the equal sign.
*$
*$ Categories: 
*$      converter

       FUNCTION LIT(C)
       CHARACTER*(*) C
       CHARACTER*4 C1
       C1=C
       CALL MOVCW(C1,L1)
       LIT=L1
       RETURN
       END
