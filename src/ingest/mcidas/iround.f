      FUNCTION IROUND(X)
C *** McIDAS Revision History ***
C 1 IROUND.FOR 16-Mar-90,16:38:50,`SSEC' PC-McIDAS ver 5.00
C 2 IROUND.FOR 25-Sep-90,7:34:30,`SMG' First Release into COMmon
C *** McIDAS Revision History ***
C $ FUNCTION IROUND(X)  (JMB)
C $ ROUNDS A FLOATING POINT VALUE
C $ X = (R) INPUT  FLOATING POINT VALUE
C $$ IROUND = REAL
C
      IROUND=NINT(X)
      RETURN
      END
