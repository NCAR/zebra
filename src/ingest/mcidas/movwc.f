C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.
C *** McIDAS Revision History ***
C 1 MOVWC.FOR 26-Feb-90,16:19:10,`SSEC' PC-McIDAS ver 5.00
C 2 MOVWC.FOR 24-Sep-90,18:19:32,`SMG' First Release into COMmon
C 3 MOVWC.FOR 1-Apr-94,1:30:52,`BARRYR' Add proprietary statement
C 4 MOVWC.FOR 2-May-94,17:11:20,`USER' Released
C 5 MOVWC.FOR 17-Jul-96,11:20:52,`BILLL' Added programmer documentation
C      (6653).
C 6 MOVWC.FOR 6-Sep-96,10:19:10,`USER' Released
C *** McIDAS Revision History ***

*$ Name:
*$      movwc  - Moves bytes in an integer array to a character array.
*$
*$ Interface:
*$      subroutine
*$      movwc(integer ibuf, character*(*) cbuf))
*$
*$ Input:
*$      ibuf   - Array containing the bytes to be transferred.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      cbuf   - Array that will contain the transferred bytes.
*$
*$ Return values:
*$      none
*$
*$ Remarks:
*$      The number of bytes transferred depends on the length of 
*$      the character array.
*$
*$ Categories: 
*$      utility 
      
      SUBROUTINE MOVWC(IBUF,CBUF)
      INTEGER IBUF(*)
      CHARACTER*(*) CBUF
      LENC=LEN(CBUF)
      CALL MOVB(LENC,IBUF,CBUF,0)
      RETURN
      END
