      FUNCTION LEN_TRIM(CTEXT)
C *** McIDAS-AIX Revision History ***      
C 1 LEN_TRIM.FOR 24-Oct-90,9:17:04,`DAVES' First release
C 2 LEN_TRIM.FOR 16-Feb-92,14:49:28,`USER' Released for McIDAS-X Only
C *** McIDAS-AIX Revision History ***      
      CHARACTER*(*) CTEXT
      LENC=LEN(CTEXT)
      DO 100 I=LENC,1,-1
      IF(CTEXT(I:I).NE.' ') THEN
         LEN_TRIM=I
         RETURN
      ENDIF
  100 CONTINUE
      LEN_TRIM=0
      RETURN
      END


