      FUNCTION ITIME(X)
C *** McIDAS Revision History ***
C 1 ITIME.FOR 23-Mar-90,12:32:54,`SSEC' PC-McIDAS ver 5.00
C 2 ITIME.FOR 25-Sep-90,7:28:54,`SMG' First Release into COMmon
C *** McIDAS Revision History ***
C $ FUNCTION ITIME(X)  (JMB)
C $ FLOATING POINT TIME TO PACKED INTEGER ( SIGN HH MM SS )
C $ X = (R) INPUT  FLOATING POINT TIME
C $$ ITIME = TIME, CONVERT, INTEGER, REAL
C
      IF(X.LT.0.0)GO TO 1
      Y=X
      I=1
      GO TO 2
 1    Y=-X
      I=-1
 2    J=3600.0*Y+0.5
      ITIME=10000*(J/3600)+100*MOD(J/60,60)+MOD(J,60)
      ITIME=I*ITIME
      RETURN
      END
