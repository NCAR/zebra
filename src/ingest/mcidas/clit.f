C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.

C *** McIDAS Revision History ***
C 1 CLIT.FOR 27-Feb-90,8:52:06,`SSEC' PC-McIDAS ver 5.00
C 2 CLIT.FOR 24-Sep-90,18:19:28,`SMG' First Release into COMmon
C 3 CLIT.FOR 1-Apr-94,1:03:38,`BARRYR' Add proprietary statement
C 4 CLIT.FOR 2-May-94,16:26:28,`USER' Released
C 5 CLIT.FOR 13-Mar-95,15:37:16,`BETHA' api documentation
C 6 CLIT.FOR 14-Mar-95,10:17:06,`USER' Released
C 7 CLIT.FOR 12-Jun-96,13:52:58,`BILLL' Added programmer documentation
C      (6653).
C 8 CLIT.FOR 6-Sep-96,10:16:36,`USER' Released
C *** McIDAS Revision History ***

*$ Name:
*$      clit    - Returns the four bytes of an integer as a character string.
*$
*$ Interface:
*$	character*4 function
*$	clit(integer l)
*$
*$ Input:
*$      l       - Integer that is copied to a character variable.
*$
*$ Input and Output:
*$	none
*$
*$ Output:
*$	none
*$
*$ Return values:
*$      The four bytes of the input integer.
*$
*$ Remarks:
*$      This function is designed to move the four bytes of an integer to a
*$      character variable.  It enables programmers to avoid conversions
*$      implicit in replacement statements in which variables of different
*$      types are on opposite sides of the equal sign.
*$
*$ Categories: 
*$      converter

       CHARACTER*4 FUNCTION CLIT(L)
       IMPLICIT NONE
       INTEGER L

C --- local variables

       CHARACTER*4 C

       CALL MOVWC(L,C)
       CLIT=C

       RETURN
       END
