       SUBROUTINE SDEST(X,L)
C *** McIDAS Revision History ***
C 1 SDEST.FOR 27-Feb-90,9:34:28,`SSEC' PC-McIDAS ver 5.00
C 2 SDEST.FOR 24-Sep-90,18:19:32,`SMG' First Release into COMmon
C 3 SDEST.FOR 14-Aug-91,9:01:52,`DAVES' Check neg uc for quick return
C 4 SDEST.FOR 14-Aug-91,9:01:52,`USER' Released
C *** McIDAS Revision History ***
C $    SUBROUTINE SDEST( CTEXT, LVAL)      (JMB)
C $    SDEST  -  DISPLAY TEXT AND AN OPTIONAL INTEGER ON STANDARD OUTPUT
C $    INPUT:
C $        CTEXT  (C)  TEXT TO DISPLAY
C $        LVAL   (I)  INTEGER DISPLAYED AFTER TEXT IF NOT 0
       CHARACTER*(*) X
       CHARACTER*80 A
       CHARACTER*12 CFR,CCC
       IF(LUC(-31).EQ.0) RETURN
       A=X
       LL=LEN(X)
       CCC=CFR(L)
       IF(LL.GT.78) GO TO 1
       L1=LL+2
       L2=L1+11
       IF(L2.GT.79) L2=79
       IF(L.NE.0) A(L1:L2)=CCC
 1     CALL SPOUT(A)
       RETURN
       END
