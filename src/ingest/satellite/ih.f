       INTEGER FUNCTION IH*2(C)
C *** McIDAS Revision History ***
C 1 IH.FOR 27-Feb-90,9:47:36,`SSEC' PC-McIDAS ver 5.00
C 2 IH.FOR 1-Oct-90,13:47:46,`RCD' Added file re-direction code
C 3 IH.FOR 12-Dec-90,8:00:26,`SMG' change function statement to be ok by AIX
C 4 IH.FOR 25-Feb-93,14:20:46,`SUEG' increase path length for redirect 3587
C 5 IH.FOR 12-Mar-93,11:01:12,`USER' Released
C *** McIDAS Revision History ***
C $    INTERNAL USE BY LWI/LWO
C      RETURNS FILE HANDLE FOR FILE C, OPENNING FILE IF NECESSARY
       IMPLICIT INTEGER*2 (I), CHARACTER*32 (A)
       CHARACTER*80 AP
       COMMON/ANAME/A(32)
       COMMON/HNAME/I(32)
       CHARACTER*(*) C
       AA=C
       DO 10 JA=1,32
       IF(A(JA).EQ.AA) THEN
           IH=I(JA)
           RETURN
       ENDIF
 10    CONTINUE
 11    DO 20 JA=1,32
       IF(A(JA).EQ.' ') THEN
           A(JA)=AA
           CALL VOLNAM(AA,AP)
           I(JA)=IOPN(AP)
           IH=I(JA)
           RETURN
       ENDIF
 20    CONTINUE
       CALL LWMOP
       GOTO 11
       END
