       SUBROUTINE EDEST(C,N)
C *** McIDAS Revision History ***
C 1 EDEST.FOR 27-Feb-90,9:03:56,`SSEC' PC-McIDAS ver 5.00
C 2 EDEST.FOR 20-Sep-90,14:11:46,`SMG' First Common release
C 3 EDEST.FOR 14-Aug-91,9:00:46,`DAVES' Use neg uc instead of DEV= parameter
C 4 EDEST.FOR 14-Aug-91,9:00:46,`USER' Released
C *** McIDAS Revision History ***
       CHARACTER*(*) C
       CHARACTER*80  A
       CHARACTER*12  CTOK
       COMMON/PARSTO/CTOK(64)
       COMMON/TWIN/LWIN,LCOL
       ISAVE=LUC(-31)
       IP=LUC(-32)
       IF(IP.EQ.0) RETURN
       CALL PUC(IP,-31)
       LS=LCOL
       LCOL=14
       IF(INDEX(C,'done').NE.0) LCOL=7
       IF(INDEX(C,'Done').NE.0) LCOL=7
       IF(INDEX(C,'DONE').NE.0) LCOL=7
       A=CTOK(1)
       I=INDEX(A(2:80),' ')
       A(I+1:I+2)=': '
       A(I+3:80)=C
       L=LEN(C)
       L=L+I+3
       CALL SDEST(A(1:L),N)
       LCOL=LS
       CALL PUC(ISAVE,-31)
       RETURN
       END
