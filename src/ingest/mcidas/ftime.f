      FUNCTION FTIME(M)
C *** McIDAS Revision History ***
C 1 FTIME.FOR 23-Mar-90,12:38:58,`SSEC' PC-McIDAS ver 5.00
C 2 FTIME.FOR 25-Sep-90,7:28:44,`SMG' First Release into COMmon
C *** McIDAS Revision History ***
C $ FUNCTION FTIME(M)  (BL)
C $ CONVERT PACKED INTEGER (SIGN HH MM SS) TIME TO REAL*4
C $ M = (I) INPUT  PACKED INTEGER (SIGN HH MM SS) TIME
C $$ FTIME = CONVERT, INTEGER, TIME, REAL
C
      IF(M.LT.0)GO TO 1
      N=M
      X=1.0
      GO TO 2
 1    N=-M
      X=-1.0
 2    FTIME=FLOAT(N/10000)+FLOAT(MOD(N/100,100))/60.0+FLOAT(MOD(N,100))/
     13600.0
      FTIME=X*FTIME
      RETURN
      END
