      SUBROUTINE MOVWC(IBUF,CBUF)
C *** McIDAS Revision History ***
C 1 MOVWC.FOR 26-Feb-90,16:19:10,`SSEC' PC-McIDAS ver 5.00
C 2 MOVWC.FOR 24-Sep-90,18:19:32,`SMG' First Release into COMmon
C *** McIDAS Revision History ***
C     MOVE BYTES IN INTEGER (IBUF) TO CHARACTER (CBUF)
      INTEGER IBUF(*)
      CHARACTER*(*) CBUF
      LENC=LEN(CBUF)
      CALL MOVB(LENC,IBUF,CBUF,0)
      RETURN
      END
