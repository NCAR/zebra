C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.
      FUNCTION GEOLAT(XLAT,IDIR)
C *** McIDAS Revision History ***
C 1 GEOLAT.FOR 23-Mar-90,12:40:32,`SSEC' PC-McIDAS ver 5.00
C 2 GEOLAT.FOR 25-Sep-90,7:28:48,`SMG' First Release into COMmon
C 3 GEOLAT.FOR 1-Apr-94,1:12:42,`BARRYR' Add proprietary statement
C 4 GEOLAT.FOR 2-May-94,16:40:54,`USER' Released
C *** McIDAS Revision History ***
C $ FUNCTION GEOLAT(XLAT, IDIR)  (DAS)
C $ GEOCENTRIC/GEODETIC LATITUDE CONVERSION.  FN VAL IN RADIANS.
C $ XLAT = (R) INPUT  LATITUDE (RADIANS)
C $ IDIR = (I) 1 FOR GEODEDIC TO GEOCENTRIC CONVERSION, 2 FOR GEOCENTRIC
C $   TO GEODEDIC CONVERSION
C $$ GEOLAT = CONVERT, LATITUDE, NAVIGATION
C
C-----XLAT, FN VALUE EXPRESSED IN RADIANS AS PER HARRIS SYSTEM
C
      DATA A/6378.388/,B/6356.912/
      ASQ=A**2
      BSQ=B**2
      CX=COS(XLAT)
      SX=SIN(XLAT)
      IF(IDIR.EQ.2)GOTO 1
      GEOLAT=ATAN2(BSQ*SX,ASQ*CX)
      RETURN
    1 GEOLAT=ATAN2(ASQ*SX,BSQ*CX)
      RETURN
      END
