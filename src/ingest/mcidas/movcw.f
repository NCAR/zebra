      SUBROUTINE MOVCW(CBUF,IBUF)
C *** McIDAS Revision History ***
C 1 MOVCW.FOR 27-Feb-90,9:34:30,`SSEC' PC-McIDAS ver 5.00
C 2 MOVCW.FOR 24-Sep-90,18:19:22,`SMG' First Release into COMmon
C *** McIDAS Revision History ***
      INTEGER IBUF(*)
      CHARACTER*(*) CBUF
      LENC=LEN(CBUF)
      CALL MOVB(LENC,CBUF,IBUF,0)
      RETURN
      END
