       SUBROUTINE LWMOP
C *** McIDAS Revision History ***
C 1 LWMOP.FOR 27-Feb-90,9:04:00,`SSEC' PC-McIDAS ver 5.00
C 2 LWMOP.FOR 26-Sep-90,15:07:28,`SMG' First Release into COMmon
C 3 LWMOP.FOR 21-Mar-91,13:59:16,`RUSSD' Added call to VOLNAM
C 4 LWMOP.FOR 21-Mar-91,13:59:16,`ERICN' Released
C 5 LWMOP.FOR 31-Dec-92,10:46:06,`TOMW' Fix problem with COM3 (3493)
C 6 LWMOP.FOR 11-Jan-93,8:13:00,`USER' Released
C 7 LWMOP.FOR 3-Mar-93,10:55:58,`SUEG' make path 72 characters for volnam 3587
C 8 LWMOP.FOR 12-Mar-93,10:30:24,`USER' Released
C *** McIDAS Revision History ***
C $    LWMOP    (JMB)
C $    CALLED TO FORCE CLOSURE OF OPEN LW FILES
       IMPLICIT CHARACTER*32 (A), INTEGER*2 (I), INTEGER*4 (L)
       CHARACTER*72 AA
       INTEGER*2 DELFIL
       COMMON/ANAME/A(32)
       COMMON/HNAME/I(32)
       DO 10 JA=1,32
C      DONT CLOSE COM PORTS
       if (a(ja)(1:3).eq.'COM' .and. len_trim(a(ja)).eq.4 .and.
     *    (a(ja)(4:4).ge.'1' .and. a(ja)(4:4).le.'9') ) goto 10
       IF(I(JA).LE.0) GOTO 10
       IY=I(JA)
       L=LEOF(IY)
       CALL DC(IY)
       I(JA)=-1
C      DELETE FILES THAT ARE STILL EMPTY
       CALL VOLNAM(A(JA),AA)
       IF(L.LT.4) ISTAT=DELFIL(AA)
       A(JA)=' '
 10    CONTINUE
       RETURN
       END
