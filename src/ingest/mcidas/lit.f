       FUNCTION LIT(C)
C *** McIDAS Revision History ***
C 1 LIT.FOR 27-Feb-90,9:04:00,`SSEC' PC-McIDAS ver 5.00
C 2 LIT.FOR 24-Sep-90,18:19:26,`SMG' First Release into COMmon
C *** McIDAS Revision History ***
C $    FUNCTION LIT(CC)   (JMB)
C $    LIT  --  CHANGE CHARACTER*4 INTO INTEGER*4
C $    INPUT:
C $        CC  (C)  CHARACTER STRING
C $    FUNCTION VALUE:
C $        THE SAME BYTES, CHANGED TO TYPE INTEGER
       CHARACTER*(*) C
       CHARACTER*4 C1
       C1=C
       CALL MOVCW(C1,L1)
       LIT=L1
       RETURN
       END
