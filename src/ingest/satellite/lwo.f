       FUNCTION LWO(CFILE, LB, LW, LA)
C *** McIDAS-AIX Revision History ***
C 1 LWO.FOR 18-Sep-90,12:41:44,`SMG' initial release
C 2 LWO.FOR 26-Sep-90,10:50:36,`SMG' fix int*2 variables
C 3 LWO.FOR 11-Feb-91,14:55:26,`SMG' clean up integer declarations
C 4 LWO.FOR 18-Oct-91,12:51:30,`SUEG' fix parameter redefinition problem
C 5 LWO.FOR 16-Feb-92,14:35:56,`USER' Released for McIDAS-X Only
C 6 LWO.FOR 18-May-92,14:59:52,`SUEG' change error message to say LWO not DOSWRITE
C 7 LWO.FOR 29-May-92,9:05:38,`DAVES' Changed SQUAK message to: file write
C 8 LWO.FOR 29-May-92,16:03:38,`USER' Released for McIDAS-X Only
C 9 LWO.FOR 15-Jan-93,17:01:56,`JOHNP' added file name to squak output
C 10 LWO.FOR 8-Feb-93,8:11:46,`USER' Released for McIDAS-X Only
C *** McIDAS-AIX Revision History ***
C $    FUNCTION LWO (CFILE, LB, LW, LA)    (JMB)
C $    LWO - WRITE TO LW FILE
C $    INPUT:
C $	   CFILE  (C)  NAME OF FILE
C $	   LB	  (I)  FIRST WORD IN FILE FOR TRANSFER (0-BASED)
C $	   LW	  (I)  NUMBER OF 4-BYTE WORDS TO TRANSFER
C $	   LA	  (*)  SOURCE OF THE TRANSFER
C $    FUNCTION VALUES:
C $        ALWAYS 0
       IMPLICIT INTEGER*2 (I), INTEGER*4 (L-M)
       INTEGER*2 DW
       DIMENSION LA(*)
       CHARACTER*(*) CFILE
       CHARACTER*20 COUT
       DIMENSION LL(128)
       COUT = CFILE
       LWO=0
       II=IH(CFILE)
       LE=LEOF(II)
       LF=LB*4
       IF(LF.LE.LE) GOTO 10
       CALL FILBUF(512, '80'X , LL, 0)
       LX=LF-LE
       CALL SCRA(II,LE)
       L1=LX/512
       L2=MOD(LX,512)
       NA=512
       DO 1 JA=1,L1
       IRC=DW(II, LL, NA, LR)
       CALL SQUAK('file write:'//COUT,IRC)
 1     CONTINUE
       MB=L2
       IRC=DW(II, LL, MB, LR)
 10    CALL SCRA(II,LF)
       NC=4*LW
       IRC=DW(II, LA, NC, LR)
       CALL SQUAK('file write:'//COUT,IRC)
       LWO=IRC
       RETURN
       END
