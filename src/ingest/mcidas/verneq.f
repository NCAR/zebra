C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.
      FUNCTION VERNEQ(DT)
C *** McIDAS Revision History ***
C 1 VERNEQ.FOR 17-Apr-90,8:02:30,`SSEC' PC-McIDAS ver 5.00
C 2 VERNEQ.FOR 24-Sep-90,18:19:46,`SMG' First Release into COMmon
C 3 VERNEQ.FOR 1-Apr-94,1:44:42,`BARRYR' Add proprietary statement
C 4 VERNEQ.FOR 2-May-94,17:35:42,`USER' Released
C *** McIDAS Revision History ***
C $ FUNCTION VERNEQ(DT)  (BTR)
C $ VERNEQ - compute longitude of vernal equinox as a function of time.
C $ Input:
C $     DT = (D) time of vernal equinox
C $ Output description:
C $    longitude of vernal equinox
C $$ VERNEQ = COMPUTAT, SOUNDER, TOVS
C     COMPUTE LONGITUDE OF VERNAL EQUINOX AS A FUNCTION OF TIME
      IMPLICIT REAL*8 (D)
      DATA D0/ 0.D+0/
C
      DV = -215.465D+0 - 360.9856473D+0 * (DT - 2442348.D+0)
      DCIRC = DV/360.D+0
C     DCIRC WILL TYPICALLY BE NEGATIVE.
C
10    IF(DCIRC .GT. D0) GO TO 20
      DCIRC = DCIRC + 365.D+0
      GO TO 10
C
20    DCIRC = DCIRC - DINT(DCIRC)
      V = 360.D+0 * DCIRC
      IF(V .GT. 180.) V = V - 360.
      VERNEQ = V
      RETURN
      END
