C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.
C *** McIDAS Revision History ***
C 1 MOVCW.FOR 27-Feb-90,9:34:30,`SSEC' PC-McIDAS ver 5.00
C 2 MOVCW.FOR 24-Sep-90,18:19:22,`SMG' First Release into COMmon
C 3 MOVCW.FOR 1-Apr-94,1:30:32,`BARRYR' Add proprietary statement
C 4 MOVCW.FOR 2-May-94,17:10:36,`USER' Released
C 5 MOVCW.FOR 17-Jul-96,11:20:44,`BILLL' Added programmer documentation.
C 6 MOVCW.FOR 6-Sep-96,10:18:50,`USER' Released
C *** McIDAS Revision History ***

*$ Name:
*$      movcw - Moves characters in a string to an integer array.
*$
*$ Interface:
*$      subroutine
*$      movcw(character*(*)cbuf, integer ibuf(*))
*$
*$ Input:
*$      cbuf  - String of characters, starting at first location.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      ibuf  - Integer array receiving characters.
*$
*$ Return values:
*$      none
*$
*$ Remarks:
*$      The number of characters transferred depends on the length of
*$      the input character array.
*$
*$ Categories: 
*$      utility 


      SUBROUTINE MOVCW(CBUF,IBUF)

      INTEGER IBUF(*)
      CHARACTER*(*) CBUF
      LENC=LEN(CBUF)
      CALL MOVB(LENC,CBUF,IBUF,0)
      RETURN
      END
