       SUBROUTINE SPOUT(B)
C *** McIDAS-AIX Revision History ***
C 1 SPOUT.FOR 2-Jan-91,12:56:28,`DAVES' First release
C 2 SPOUT.FOR 8-Jan-91,12:37:20,`SMG' send correct line length to SENDTEXT
C 3 SPOUT.FOR 18-Apr-91,14:24:40,`DAVES' Added printer support
C 4 SPOUT.FOR 18-Sep-91,19:20:42,`DAVES' Negative UC changes
C 5 SPOUT.FOR 19-Sep-91,18:39:34,`DAVES' Fix up pickup of screen
C 6 SPOUT.FOR 16-Feb-92,14:55:32,`USER' Released for McIDAS-X Only
C 7 SPOUT.FOR 2-Nov-92,14:41:40,`RAYL' Blank pad text buffer and allow for
C      up to 132 characters per text window line (2998)
C 8 SPOUT.FOR 23-Nov-92,8:30:26,`USER' Released for McIDAS-X Only
C 9 SPOUT.FOR 21-Jan-93,15:35:10,`RAYL' Allow for up to 160 characters per
C      text window line (3525)
C 10 SPOUT.FOR 15-Feb-93,7:57:30,`USER' Released for McIDAS-X Only
C 11 SPOUT.FOR 20-Oct-93,12:03:26,`BETHA' blank line problem (3592)
C 12 SPOUT.FOR 27-Oct-93,10:33:38,`USER' Released for McIDAS-X Only
C *** McIDAS-AIX Revision History ***

C $    SPOUT(CC)      (JMB)
C $    SPOUT - OUTPUT STRING TO SCROLLING TEXT WINDOW
C $    INPUT:
C $        CC  (C)  ANY LENGTH FORM 1 TO 80
C------OUTPUT TO NORMAL SCROLLING TEXT WINDOW       J.BENSON  05/88.
       CHARACTER*(*) B
       CHARACTER*162 A
       INTEGER L(41)
       CHARACTER*4 CLIT
       CHARACTER*12 CFILE
       COMMON/TWIN/LWIN,LCOL
       ICH=LUC(-31)
       IF(ICH.EQ.0) RETURN
       IL1=LEN_TRIM(B)
       IL2=MIN0(IL1,160)
       A=B(1:IL2)
       ICHARS=NCHARS(A,IB,IE)
       IF(ICHARS.EQ.0) THEN
         IE=1
         A(1:1)=' '
       ENDIF
       IF(ICH.EQ.4) THEN
          LASTC=80
          IF(IE.LT.80) LASTC=IE
          A(LASTC+1:160)=' '
       ELSE
C *** add a LF to the line of text to be written
          IEE=IE+1
          A(IE+1:IE+1)=CHAR(10)
       ENDIF
C   OUTPUT TO PRINTER
       IF(ICH.EQ.2.OR.ICH.EQ.3) THEN
           CALL SENDTEXT(A(1:IEE), -1, 0)
C   OUTPUT TO FILE
       ELSE IF(ICH.EQ.4) THEN
           CFILE(1:4) =CLIT(LUC(-35))
           CFILE(5:8) =CLIT(LUC(-36))
           CFILE(9:12)=CLIT(LUC(-37))
           IWORD=(LWENDW(CFILE)+1)/20*20
           CALL MOVCW(A(1:80),L)
           CALL LWO(CFILE,IWORD,20,L)
C   OUTPUT TO TEXT WINDOW
       ELSE
           CALL SENDTEXT(A(1:IEE), LWIN, LCOL)
       ENDIF
       RETURN
       END

