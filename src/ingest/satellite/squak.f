       SUBROUTINE SQUAK(C,I)
C *** McIDAS-AIX Revision History ***
C 1 SQUAK.FOR 27-Feb-90,9:04:02,`SSEC' PC-McIDAS ver 5.00
C 2 SQUAK.FOR 18-Jun-90,8:41:26,`SUEG' change stop to call abort(0)
C 3 SQUAK.FOR 24-Sep-90,18:19:24,`SMG' First Release into COMmon
C 4 SQUAK.FOR 16-Feb-92,14:40:04,`USER' Released for McIDAS-X Only
C 5 SQUAK.FOR 15-Jan-93,17:01:06,`JOHNP' expanded error string to 32 chars
C 6 SQUAK.FOR 19-Jan-93,11:53:26,`RICKK' Updated Revision History Cards
C 7 SQUAK.FOR 15-Feb-93,9:45:16,`USER' Released for McIDAS-X Only
C *** McIDAS-AIX Revision History ***
C      USED TO OUTPUT ERRORS FROM SYSTEM CALLS
C      C IS THE NAME OF THE SYSTEM CALL
C      I IS THE RETURN CODE
       CHARACTER*(*) C
       CHARACTER*32 C1
       INTEGER*2 I
       IF(I.EQ.0) RETURN
       L=I
       C1=C
       CALL EDEST('Error in '//C1//' system return code is ',L)
       IF(LUC(181).EQ.2) CALL XONOFF('ON')
       CALL ABORT(0)
       END
