       SUBROUTINE DDEST(C,N)
C *** McIDAS Revision History ***
C 1 DDEST.FOR 16-Mar-90,14:43:38,`SSEC' PC-McIDAS ver 5.00
C 2 DDEST.FOR 25-Sep-90,7:35:24,`SMG' First Release into COMmon
C 3 DDEST.FOR 14-Aug-91,9:00:52,`DAVES' Use neg uc instead of DEV= parameter
C 4 DDEST.FOR 14-Aug-91,9:00:52,`USER' Released
C *** McIDAS Revision History ***
       CHARACTER*(*) C
       CHARACTER*80  A
       CHARACTER*12  CTOK
       COMMON/PARSTO/CTOK(64)
       COMMON/TWIN/LWIN,LCOL
       ISAVE=LUC(-31)
       IP=LUC(-33)
       IF (IP.EQ.0) RETURN
       CALL PUC(IP,-31)
       LS=LCOL
       LCOL=14
       A=CTOK(1)
       I=INDEX(A(2:80),' ')
       A(I+1:I+2)='* '
       A(I+3:80)=C
       L=LEN(C)
       L=L+I+3
       CALL SDEST(A(1:L),N)
       LCOL=LS
       CALL PUC(ISAVE,-31)
       RETURN
       END
