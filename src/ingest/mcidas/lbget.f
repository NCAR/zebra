C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.
      FUNCTION LBGET(CNAME,COUT)
C *** McIDAS Revision History ***
C 1 LBGET.FOR 6-Mar-90,13:37:42,`SSEC' PC-McIDAS ver 5.00
C 2 LBGET.FOR 25-Sep-90,7:35:46,`SMG' First Release into COMmon
C 3 LBGET.FOR 1-Apr-94,1:20:46,`BARRYR' Add proprietary statement
C 4 LBGET.FOR 2-May-94,16:55:00,`USER' Released
C 5 LBGET.FOR 23-Jul-96,8:26:46,`BILLL' Added programmer documentation
C      (6653)
C 6 LBGET.FOR 6-Sep-96,10:17:50,`USER' Released
C *** McIDAS Revision History ***

*$ Name:
*$      lbget  - Retrieves a string from the current string table.
*$
*$ Interface:
*$      integer function
*$      lbget(character*(*) cname, character*(*) cout)
*$
*$ Input:
*$      cname  - Name of string to be retrieved.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      cout   - String to be retrieved.
*$
*$ Return values:
*$      >0     - Length of string that is returned.
*$      -1     - String was not found.
*$
*$ Remarks:
*$      The table will be read into common if necessary.
*$
*$ Categories: 
*$      text 

C $ FUNCTION LBGET(CNAME, COUT)  (RCD)
C $ GET A STRING FROM CURRENT STRING TABLE.  TABLE READ INTO COMMON IF
C $   NECESSARY.
C $ CNAME =(C) INPUT  STRING NAME, 12 CHARS OR FEWER
C $ COUT = (C) OUTPUT  WILL RECEIVE STRING VALUE
C $$ LBGET = STRING-TABLE
      IMPLICIT CHARACTER*12 (C)
      PARAMETER (NTOT=15*1024)
C
C-----ARGUMENTS
      CHARACTER CNAME*(*)
      CHARACTER COUT*(*)
C
       EQUIVALENCE (ICSTR,CSTR)
      CHARACTER*8 CFILE
       CHARACTER CSTLIS(10)*160
      CHARACTER*160 CSTR
       INTEGER LISTPT(10)
      COMMON/LBCOM1/LBTERM,LBBEG,LBN,LENTBL(256)
      COMMON/LBCOM2/CSTR,CLBH,CLBY,CTBL(256)
      DATA LEVEL/-1/,CFILE/'STRTABLE'/, CHOLD/' '/,IPOINT/1/
      CHOLD=CNAME
C
       IF (LEVEL.NE.LUC(184)) THEN
       LBBEG=-999999
       DO 200 I=1,10
200    LISTPT(I)=0
C
       ELSE
C
       DO 201 I=1,10
       M=LISTPT(I)
       IF (M.EQ.0) GOTO 201
         IF (CHOLD.EQ.CTBL(M).AND.LENTBL(M).GT.0) THEN
          COUT=CSTLIS(I)
          LBGET=LENTBL(M)
          RETURN
       ENDIF
201    CONTINUE
C
       ENDIF
C
      CALL LBCHEK
       LEVEL=LUC(184)
C
      DO 3 J=1,LBN
C
      IF (LENTBL(J).GT.0) THEN
         IF (CHOLD.EQ.CTBL(J)) THEN
            IF (CHOLD.EQ.'H       ') THEN
               COUT=CLBH
            ELSE IF (CHOLD.EQ.'Y       ') THEN
               COUT=CLBY
            ELSE
               LWRETN=LWI(CFILE,LBBEG+3072+40*(J-1),40,ICSTR)
               COUT=CSTR
               IPOINT=IPOINT+1
               IF (IPOINT.GT.10) IPOINT=1
               CSTLIS(IPOINT)=CSTR
               LISTPT(IPOINT)=J
            ENDIF
            LENSAV=LENTBL(J)
            LBGET=LENSAV
            RETURN
         ENDIF
      ENDIF
C
 3    CONTINUE
C
      LBGET=-1
      CSTR=' '
       CHOLD=' '
      RETURN
      END
