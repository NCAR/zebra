C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.

C *** McIDAS Revision History ***
C 1 COTAN.FOR 26-Dec-90,9:15:14,`SMG' initial release for AIX
C 2 COTAN.FOR 24-Apr-91,16:09:02,`DAVES' minor fis
C 3 COTAN.FOR 16-Feb-92,15:05:58,`USER' Released for McIDAS-X Only
C 4 COTAN.FOR 1-Apr-94,0:47:08,`BARRYR' Add proprietary statement
C 5 COTAN.FOR 2-May-94,15:55:44,`USER' Released for McIDAS-X Only
C 6 COTAN.FOR 19-Feb-96,15:02:42,`DWS' reglue: moved from AIX to COM
C 7 COTAN.FOR 20-Feb-96,12:41:42,`USER' Released for McIDAS-X Only
C *** McIDAS Revision History ***

C old McIDAS-AIX revision history ***
C 1 COTAN.FOR 26-Dec-90,9:15:14,`SMG' initial release for AIX
C 2 COTAN.FOR 24-Apr-91,16:09:02,`DAVES' minor fis
C 3 COTAN.FOR 16-Feb-92,15:05:58,`USER' Released for McIDAS-X Only
C 4 COTAN.FOR 1-Apr-94,0:47:08,`BARRYR' Add proprietary statement
C 5 COTAN.FOR 2-May-94,15:55:44,`USER' Released for McIDAS-X Only
C old McIDAS-AIX revision history ***

C  Some Fortran compilers do not supply this function.

      REAL FUNCTION COTAN(X)
      IMPLICIT NONE
      REAL X

C --- initialized variables

      REAL PI_OVER_2
      DATA PI_OVER_2 /-1./

      IF (PI_OVER_2 .LT. 0.) THEN
	  PI_OVER_2 = 2. * ATAN(1.)
      ENDIF

      COTAN = TAN(PI_OVER_2 - X)
      RETURN
      END
