C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.
      SUBROUTINE LLCART(XLAT,XLON,X,Y,Z)                                
C *** McIDAS Revision History ***
C 1 LLCART.FOR 19-Mar-90,21:50:54,`SSEC' PC-McIDAS ver 5.00
C 2 LLCART.FOR 25-Sep-90,7:33:58,`SMG' First Release into COMmon
C 3 LLCART.FOR 1-Apr-94,1:22:16,`BARRYR' Add proprietary statement
C 4 LLCART.FOR 2-May-94,16:57:16,`USER' Released
C *** McIDAS Revision History ***
C $ SUBROUTINE LLCART(XLAT, XLON, X,Y,Z)  (DAS)                         
C $ CONVERT LAT, LON TO CARTESIAN CENTERED COORDS  (X, Y, Z).           
C $ XLAT = (R) INPUT  LATITUDE IN DEGREES, NORTH +                      
C $ XLON = (R) INPUT  LONGITUDE IN DEGREES, WEST +                      
C $ X, Y, Z = (R) OUTPUT  COORDS IN SYSTEM WITH ORIGIN AT CENTER OF     
C $  PLANET. POS X-AXIS PIERCES EQUATOR AT LON 0 DEG, POS Y-AXIS PIERCES
C $  EQUATOR AT LON 90 DEG, & POS Z-AXIS INTERSECTS THE NORTH POLE.     
C $  (IN KM).                                                           
C $$ LLCART = COORDINATES,LATITUDE,LONGITUDE,NAVIGATION                 
      IMPLICIT REAL*8 (D)                                               
      REAL*8 ASQ,BSQ,AB,RDPDG,ECC,ECCSQR
      DATA ASQ/40683833.48/,BSQ/40410330.18/,AB/40546851.22/            
      DATA ECC/.081992D0/,ECCSQR/6.72265D-3/                            
      DATA RDPDG/1.74532925199D-02/
      DATA KWEST/-1/,KCORD/0/                                           
      GO TO 55                                                          
      ENTRY LLOPT(DRAD,DECC,IWEST,ICORD)                                
C $ SUBROUTINE LLOPT(DRAD,DECC,IWEST,ICORD)                             
C $ LLOPT IS USED TO INPUT RADIUS & ECCENTRICITY OTHER THAN EARTH       
C $ AND DETERMINE + LONGITUDE CONVETION & PLANETOCENTRIC/DETIC          
C $ DRAD = (R*8) INPUT  EQUATORIAL RADIUS                               
C $ DECC = (R*8) INPUT  ECCENTRICITY                                    
C $ IWEST = (I) INPUT  >= 0    WEST POSITIVE, < 0 WEST NEGATIVE         
C $ ICORD = (I) INPUT  >= 0 PLANETODETIC, < 0 PLANETOCENTRIC            
C $$ LLOPT = COORDINATES,LATITUDE,LONGITUDE,NAVIGATION                  
      ASQ=DRAD*DRAD                                                     
      ECC=DECC                                                          
      ECCSQR=ECC*ECC                                                    
      DPOLE=SQRT(ASQ*(1.D0-ECCSQR))                                     
      BSQ=DPOLE*DPOLE                                                   
      AB=DRAD*DPOLE                                                     
      IF(IWEST.LT.0) KWEST=1                                            
      IF(ICORD.LT.0) KCORD=-1                                           
      RETURN                                                            
      ENTRY LLOBL(TLAT,RAD)                                             
C $ SUBROUTINE LLOBL(TLAT,RAD) (DAS)                                    
C $ CALCULATE RADIUS AT LATITUDE TLAT                                   
C $ TLAT = (R) INPUT  LATITUDE                                          
C $ RAD = (R) OUTPUT  RADIUS IN KM                                      
C $$ LLOBL = NAVIGATION,COORDINATES                                     
      TCOS=COS(TLAT*RDPDG)                                              
      DDRAD=((1.D0-ECCSQR)*ASQ)/(1.D0-ECCSQR*TCOS*TCOS)                 
      RAD=SQRT(DDRAD)                                                   
      RETURN                                                            
55    CONTINUE                                                          
      YLAT=RDPDG*XLAT                                                   
C-----CONVERT TO GEOCENTRIC (SPHERICAL) LATITUDE IF KCORD >= 0          
CCC     YLAT=GEOLAT(YLAT,1)                                             
      IF(KCORD.GE.0) YLAT=ATAN2(BSQ*SIN(YLAT),ASQ*COS(YLAT))            
      YLON=KWEST*RDPDG*XLON                                             
      SNLT=SIN(YLAT)                                                    
      CSLT=COS(YLAT)                                                    
      CSLN=COS(YLON)                                                    
      SNLN=SIN(YLON)                                                    
      TNLT=(SNLT/CSLT)**2                                               
      R=AB*SQRT((1.0+TNLT)/(BSQ+ASQ*TNLT))                              
      X=R*CSLT*CSLN                                                     
      Y=R*CSLT*SNLN                                                     
      Z=R*SNLT                                                          
      RETURN                                                            
      ENTRY CARTLL(X,Y,Z,XLAT,XLON)                                     
C $ SUBROUTINE CARTLL(X, Y, Z, XLAT, XLON)  (DALY 1978)                 
C $ CONVERT CARTESIAN CENTERED COORD (X, Y, Z) TO LAT, LON.             
C $ X, Y, Z = (R) OUTPUT  COORDS IN SYSTEM WITH ORIGIN AT CENTER OF     
C $  PLANET. POS X-AXIS PIERCES EQUATOR AT LON 0 DEG, POS Y-AXIS PIERCES
C $  EQUATOR AT LON 90 DEG, & POS Z-AXIS INTERSECTS THE NORTH POLE.     
C $  IN KM.                                                             
C $ XLAT = (R) INPUT  LATITUDE IN DEGREES, NORTH +                      
C $ XLON = (R) INPUT  LONGITUDE IN DEGREES, WEST +                      
C $$ CARTLL = COORDINATES,LATITUDE,LONGITUDE,NAVIGATION                 
C                                                                       
C                                                                       
      XLAT=100.0                                                        
      XLON=200.0                                                        
      IF(X.EQ.0..AND.Y.EQ.0..AND.Z.EQ.0.) GO TO 90                      
      A=ATAN(Z/SQRT(X*X+Y*Y))                                           
C-----CONVERT TO GEODETIC LATITUDE IF KCORD > 0                         
CCC     XLAT=GEOLAT(ATAN(Z/SQRT(X*X+Y*Y)),2)/RDPDG                      
      IF(KCORD.GE.0) THEN                                               
         XLAT=ATAN2(ASQ*SIN(A),BSQ*COS(A))/RDPDG                        
      ELSE                                                              
         XLAT=A/RDPDG                                                   
      ENDIF                                                             
      XLON=KWEST*ATAN2(Y,X)/RDPDG                                       
   90 RETURN                                                            
      END                                                               
