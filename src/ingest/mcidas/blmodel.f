C THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.

C *** McIDAS Revision History ***
C 1 BLMODEL.FOR 27-Feb-96,13:17:08,`ROBERTM' initial checkin of DMSP nav
C 2 BLMODEL.FOR 17-Apr-96,14:47:30,`USER' Released
C *** McIDAS Revision History ***

C Brouwer-Lyddane orbital prediction model

*| Name:
*|	m0blpred - Predict satellite position and velocity
*|
*| Interface:
*|	subroutine
*|	m0blpred(integer handle, double precision dtime,
*|	  double precision posx, double precision posy,
*|        double precision posz, double precision velx,
*|        double precision vely, double precision velz )
*|
*| Input:
*|	dtime	- prediction time
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	posx	- X coordinate of satellite position
*|	posy	- Y coordinate of satellite position
*|	posz	- Z coordinate of satellite position
*|	velx	- X component  of satellite velocity
*|	vely	- Y coordinate of satellite velocity
*|	velz	- Z coordinate of satellite velocity
*|
*| Return values:
*|	none
*|
*| Remarks:
*|	    Navigation subsystems needing an orbital prediction
*|	should call this routine. A single call to m0blset() must be
*|	made prior to any calls to m0blpred() in order to set
*|	or change the orbital elements.
*|	    The 'dtime' is a Julian Date (elapsed days and fraction
*|	since 12 UTC 4713 BC). Positions are in km in a celestial
*|	coordinate system. Velocity components are in km per second.
*|	    This routine is based upon the Brouwer-Lyddane orbital
*|	prediction model used by NOAA/NESDIS. It has been modified
*|	to conform to core McIDAS standards and to provide an interface
*|	compatible with the POES/DMSP 'generic navigation' subsystem.
*|
*| Categories: 
*|	navigation  


	subroutine m0blpred(handle, dtime, posx, posy, posz,
     &    velx, vely, velz)
      
	implicit		NONE

C	// Interface

	integer			handle	! instance to use
	double precision	dtime	! Julian Date of prediction
	double precision	posx	! X coordinate of position
	double precision	posy	! Y coordinate of position
	double precision	posz	! Z coordinate of position
	double precision	velx	! U component of velocity
	double precision	vely	! V component of velocity
	double precision	velz	! W component of velocity


C	// Local variables

	double precision	ae		! Earth radius
	double precision	aux(5)		!
	double precision	bj2		! Gravity harmonics?
	double precision	bj3		!    "        "
	double precision	bj4		!    "        "
	double precision	bj5		!    "        "
	double precision	blelems(6)	! Brouwer elements
C						! at 'blepoch'
	double precision	blepoch		! Epoch time
	double precision	blpred(6)	! predicted Brouwer
C						! elements at 'dtime'
	double precision	dtsecs		! time since epoch
	double precision	gm		! Gravity constant
	double precision	oscele(6)	! osculating elements
C						! at 'dtime'
	double precision	vecpos(3)	! position vector
	double precision	vecvel(3)	! velocity vector

	integer			i		! loop index
	integer			idmean		! Unknown Brouwer model
C						! switch
	integer			ipass		! Unknown Brouwer model
C						! switch
	integer			ipert		! compute perturbations?


C	// Initialized variables

	double precision	anom	! previous value of mean
C					! anomaly (initialization test)
	double precision	DTR	! convert degree to radian
	double precision	RTD	! convert radian to degree
	double precision	SPD	! seconds per day

	save	anom
	

	parameter (DTR = 6.283185307179586D0/360.D0, RTD=1.D0/DTR)
	parameter (SPD = 86400.D0)
	data anom/-99999.D0/


C	// Get Brouwer constants and Brouwer elements and compute
C	// elapsed time in (seconds) from epoch time to prediction
C	// time

	call m0blgetcons(ae, gm, bj2, bj3, bj4, bj5)
	call m0blgetels(handle, blepoch, blelems)

	dtsecs = SPD * (dtime-blepoch)

 
C	// Make copy of Brouwer elements. These will be modified
C	// by the orbital element propagator

	do i = 1,6
	    blpred(i) = blelems(i)
	end do

C	// If orbital elements at epoch have changed, re-
C	// initialize the orbital element propagator

	if( blelems(6).ne.anom) then
	    ipert  = 0
	    ipass  = 1
	    idmean = 1
	    call m0brolyd(dtsecs, ipert, ipass, idmean, blpred,
     &	      oscele, aux)
	    anom = blelems(6)
	end if

C	// Compute osculating Keplerian values valid at time
C	// 'dtime' from Brouwer-Lyddane elements valid at
C	// epoch time.

	ipert  = 2
	ipass  = 2
	idmean = 1
	call m0brolyd(dtsecs, ipert, ipass, idmean, blpred,
     &	  oscele, aux)


C	// Convert osculating elements to a position in celestial
C	// coordinates

	call m0celem(oscele, gm, vecpos, vecvel)

C	// Capture position and vector components for return to caller

	posx = vecpos(1)
	posy = vecpos(2)
	posz = vecpos(3)

	velx = vecvel(1)
	vely = vecvel(2)
	velz = vecvel(3)

	return
	end

*| Name:
*|	m0blgetcons - Retrieve constants used by Brouwer-
*|		      Lyddane orbit prediction package
*|
*| Interface:
*|	subroutine
*|	m0blgetcons(double precision ae,
*|	  double precision gm,  double precision bj2,
*|	  double precision bj3, double precision bj4,
*|	  double precision bj5)
*|
*| Input:
*|	none
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	ae		- Earth radius
*|	gm		- Gravitational constant
*|	bj2		- gravitational harmonics?
*|	bj3		-      "           "
*|	bj4		-      "           "
*|	bj5		-      "           "
*|
*| Return values:
*|	none
*|
*| Remarks:
*|	    This function is intended for internal use by the
*|	Brouwer orbit package only. The constants must have
*|	been set with a call to m0blset() before calling
*|	this function.
*|
*| Categories: 
*|	navigation  

	subroutine m0blgetcons(ae, gm, bj2, bj3, bj4, bj5)

C	// Interface 

	double precision	ae		! Earth radius
	double precision	bj2		! gravity harmonics
	double precision	bj3		! (I think)
	double precision	bj4
	double precision	bj5
	double precision	gm		! gravity constant


	ae	=   6378.135D0
      	gm	= 398600.8D0
      	bj2	=     -0.10826158D-02
     	bj3	=      0.25388100D-05
	bj4	=      0.16559700D-05
	bj5	=      0.21848266D-06

	return
	end




*| Name:
*|	m0dkeplr - Solve Kepler's equation
*|
*| Interface:
*|	double precision function
*|	m0dkeplr(double precision M, double precision E)
*|
*| Input:
*|	M	- mean anomaly
*|	E	- eccentricity
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	eccentric anomaly
*|
*| Remarks:
*|	    Kepler's equation relates geometry or position in the
*|	orbit plane to time. This version is based on DKEPLR from
*|	the NOAA/NESDIS Brouwer-Lyddane package, modified to conform
*|	to core McIDAS standards.
*|
*| Categories: 
*|	navigation  

	double precision function m0dkeplr(M,E)

	implicit	NONE


C	// Interface variables

	double precision	E	! eccentricity
	double precision	M	! mean anomaly


C	// Local variables

	double precision	DELEA	! change in estimate of
C					! eccentric anomaly
	double precision	EA	! eccentric anomaly
	double precision	FE	! ??
	double precision	OLDEA	! previous estimate of
C					! eccentric anomaly
	integer			I	! loop index


C	// Initialized data

	double precision	PI2
	double precision	TOL

	parameter( PI2=6.283185307179586D0 )
	data TOL/0.5D-15/



C	// (implementation is unchanged from original NOAA/NESDIS
C	// version; commentary and indentation added)


	EA=0.D0

        IF(M) 1,2,1

C	// Mean anomaly is not zero; compute eccentric anomaly
C	// iteratively

    1		EA = M + E*DSIN(M)
		DO 22 I=1,12

C			// Update estimate of eccentric anomaly

			OLDEA=EA
			FE=EA-E*DSIN(EA)-M
			EA=EA-FE/(1-E*DCOS(EA-0.5D0*FE))

C 			// test for convergence

			DELEA=DABS(EA-OLDEA)
			IF(DELEA.LE.TOL) GO TO 2

   22		CONTINUE

C		// Mean anomaly is zero, or value of eccentric anomaly
C		// has been computed

    2		EA=DMOD(EA,PI2)

	M0DKEPLR=EA

	RETURN
	END


*| Name:
*|	m0celem - Compute satellite position and velocity given
*|		  osculating orbital elements
*|
*| Interface:
*|	subroutine
*|	m0celem(double precision oscele(6), double precision gm,
*|	  double precision pv(3), double precision vv(3))
*|
*| Input:
*|	oscele	- Osculating orbital elements
*|	gm	- gravitational constant
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	pv	- Celestial cartesian position vector
*|	vv	- Celestial cartesian velocity vector
*|
*| Return values:
*|	none
*|
*| Remarks:
*|	    This routine is the NOAA/NESDIS 'CELEM' modified to
*|	conform to core McIDAS standards.
*|	    Kepler's equation relates geometry or position in the
*|	orbit plane to time. This version is based on DKEPLR from
*|	the NOAA/NESDIS Brouwer-Lyddane package, modified to conform
*|	to core McIDAS standards.
*|
*| Categories: 
*|	navigation  

	subroutine m0celem(oscele, gm, pv, vv)
C
C	// (original comments)
C   METHOD:
C       USES MILES STANDISH ITERATIVE SCHEME FOR SOLN TO KEPLERS EQN.
C   REFERENCES:
C       GTDS TASK SPEC FOR M0CELEM, C.E. VELEZ, 13 JANUARY 1971
C       DODS SYSTEM DESCRIPTION, SUBROUTINE KEPLR1
C       P. EXCOBAL-'METHODS OF ORBIT DETERMINATION'
C       X-552-67-421,'COMPARISON FO ITERATIVE TECHNIQUES FOR THE
C       SOLUTION OF KEPLERS EQUATION', I.COLE AND R. BORCHERS
C   PROGRAMMER:
C       CHARLES K. CAPPS,  CODE 553.2,  GSFC
C
	implicit		NONE

C	// Interface variables

	double precision	gm		! gravity constant
	double precision	oscele(6)	! osculating elements
	double precision	pv(3)		! position vector
	double precision	vv(3)		! velocity vector


C	// Local variables

	double precision	COSE		! cosine of eccentric
C						! anomaly
	double precision	B11		! convert orbit plane
	double precision	B12		! to Celestial Cartesian
	double precision	B21		! coordinates with these
	double precision	B22		! B.. coefficients
	double precision	B31		! 
	double precision	B32		!
	double precision	COSI		! cosine of inclination
	double precision	COSO		! cosine of arg of perigee
	double precision	COSOM		! cosine of ascending node
	double precision	D		! ??
	double precision	E1		! ecc. anom. estimate
	double precision	E2		! ecc. anom. estimate
	double precision	F		! ??
	double precision	ITER		! iteration counter
	double precision	R		! ??
	double precision	SINE		! sine of eccentric
C						! anomaly
	double precision	SINI		! sine   of inclination
	double precision	SINO		! sine   of arg of perigee
	double precision	SINOM		! sine   of ascending node
	double precision	TEMP		! ??
	double precision	XO		! position in orbit plane
	double precision	XOD		! motion in orbit plane
	double precision	YO		! position in orbit plane
	double precision	YOD		! motion in orbit plane

	integer			NERR		! error code


C	// Initialized variables

	integer			MAX		!
	double precision	TOL		!

	data MAX/10/, TOL/+0.5D-16/

	ITER = 0

C	// (implementation unchanged from original; commentary
C	// indentation, and white space added for clarity).

C	// Determine if orbit is elliptic or hyperbolic

	IF (oscele(1).LE.0.0D0.AND.oscele(2).GT.1.0D0) GO TO 50

C		// ELLIPTIC orbit. Find eccentric anomaly via
C		// Newton's methoc (Miles Standish version)

		E1 = oscele(6)
		
   10 		F  = E1 -    (oscele(2) * DSIN(E1)) - oscele(6)
      		D  = 1.0D0 - (oscele(2) * DCOS(E1-0.5D0*F))
		E2 = E1 - (F/D)

C		// Test for convergence

		IF (DABS (E1-E2)-TOL )40,40,20
   20		ITER = ITER + 1
		E1 = E2

C		// Test iteration count; if limit exceeded, set
C		// error code, otherwise take another try

		IF(ITER - MAX) 10,10,30
   30			NERR = 13

C		// Eccentric anomaly has converged. Now get XO, YO, R


   40		COSE = DCOS(E2)
		SINE = DSIN (E2)
		TEMP = 1.0D0 - oscele(2) * oscele(2)
		XO   = oscele(1) * (COSE - oscele(2))
		YO   = oscele(1) * (DSQRT(TEMP)* SINE)
		R    = oscele(1) * (1.0D0 - oscele(2) * COSE)
		XOD  = (-DSQRT(gm* oscele(1))* SINE)/R
		YOD  = (DSQRT(gm*oscele(1)*(TEMP))*COSE) / R

		GO TO 100

C	// HYPERBOLIC orbit

   50		E1 = oscele(6) / 2.0D0
   60		F  = oscele(2) * DSINH(E1) - E1 - oscele(6)
		D  = oscele(2) * DCOSH(E1 - 0.5D0 * F ) - 1.0D0
		E2 = E1 - (F / D)

		IF (DABS (E1-E2)-TOL )90,90,70
   70		ITER = ITER + 1
		E1 = E2

C		// Test iteration count; if limit exceeded, set
C		// error code, otherwise take another try

		IF (ITER - MAX) 60,60,80
   80			NERR = 14

C		// Eccentric anomaly converged. Now get XO, YO, and R

   90		COSE = DCOSH (E2)
		SINE = DSINH(E2)
		TEMP =  oscele(2) * oscele(2) - 1.0D0

		XO   =  oscele(1) *(COSE  - oscele(2))
		YO   = -oscele(1) * DSQRT(TEMP) * SINE
		R    =  oscele(1) *(1.0D0 - oscele(2) * COSE)

		XOD  = (-DSQRT(-gm*oscele(1))*SINE)/R
		YOD  = ( DSQRT(-gm*oscele(1)*TEMP)*COSE) / R

C	// Now finish up -- all orbit types done the same from here


  100	COSO  = DCOS(oscele(5))
	SINO  = DSIN(oscele(5))
	COSOM = DCOS(oscele(4))
	SINOM = DSIN(oscele(4))
	COSI  = DCOS(oscele(3))
	SINI  = DSIN(oscele(3))

	B11 =  COSO * COSOM - SINO * SINOM * COSI
	B21 =  COSO * SINOM + SINO * COSOM * COSI
	B31 =  SINO * SINI
	B12 = -SINO * COSOM - COSO * SINOM * COSI
	B22 = -SINO * SINOM + COSO * COSOM * COSI
	B32 =  COSO * SINI

C	// Matrix multiplies of 3x2 matrix by 2x1 vectors (XO,YO)
C	// and (XOD,YOD) give position and velocity vectors in
C	// Celestial cartesian space

	pv(1) = B11 * XO + B12 * YO
	pv(2) = B21 * XO + B22 * YO
	pv(3) = B31 * XO + B32 * YO

	vv(1) = B11 * XOD + B12 * YOD
	vv(2) = B21 * XOD + B22 * YOD
	vv(3) = B31 * XOD + B32 * YOD

  999	RETURN
	END


*| Name:
*|	m0brolyd - Brouwer-Lyddane orbital element propagator
*|
*| Interface:
*|	subroutine
*|	m0brolyd(dtsecs, ipert, ipass, idmean, blpred, oscele, aux)
*|
*| Input:
*|	dtsecs	- time since epoch for prediction
*|	ipert	- unknown switch
*|	ipass	- unknown switch
*|	idmean	- unknown switch
*|
*| Input and Output:
*|	blpred	- Brouwer elements at epoch time, updated to
*|		  prediction time
*|
*| Output:
*|	oscele	- osculating elements
*|	aux	- (another form of elements; not sure what)
*|
*| Return values:
*|	none
*|
*| Remarks:
*|	    This routine is the NOAA/NESDIS 'BROLYD' modified to
*|	conform to core McIDAS standards. The exact purpose of
*|	the switches is unknown. NESDIS usage is settings of
*|	0, 1, 1 for initialization and 2, 2, 1 for prediction.
*|
*| Categories: 
*|	navigation  


	subroutine m0brolyd(dtsecs, ipert, ipass, idmean, blpred,
     &    oscele, aux)


C  (original comment block)
C***********************************************************************
C   REF.   "BROUWER-LYDDANE ORBIT GENERATOR ROUTINE"
C                  (X-553-70-223)
C                BY E.A. GALBREATH 1970
C
C    MODIFIED 7/31/74 VIONA BROWN AND R.A. GORDON TO INTERFACE WITH GTDS
C***********************************************************************

	implicit		NONE


C	// Interface variables

	double precision	blpred(6)	! mean elements at 
C						! prediction time
	double precision	dtsecs
	double precision	oscele(6)	! osculating elements
C						! at prediction time
	double precision	aux(5)		! alternate elements ??
	integer			ipert		!
	integer			ipass		!
	integer			idmean		!


C	// External functions

	double precision	m0dkeplr	! Kepler equation solver


C	// Local variables
		
	double precision	A	!
	double precision	A0	!
	double precision	A1	!
	double precision	A10	!
	double precision	A11	!
	double precision	A12	!
	double precision	A13	!
	double precision	A14	!
	double precision	A15	!
	double precision	A16	!
	double precision	A17	!
	double precision	A18	!
	double precision	A19	!
	double precision	A2	!
	double precision	A21	!
	double precision	A22	!
	double precision	A26	!
	double precision	A27	!
	double precision	A3	!
	double precision	A4	!
	double precision	A5	!
	double precision	A6	!
	double precision	A7	!
	double precision	A8	!
	double precision	A8P	!
	double precision	ADP	!
	double precision	AE	! Earth radius
	double precision	ANU	!
	double precision	ARG1	!
	double precision	ARG2	!
	double precision	B1	!
	double precision	B10	!
	double precision	B11	!
	double precision	B12	!
	double precision	B13	!
	double precision	B14	!
	double precision	B15	!
	double precision	B2	!
	double precision	B3	!
	double precision	B4	!
	double precision	B5	!
	double precision	B6	!
	double precision	B7	!
	double precision	B8	!
	double precision	B9	!
	double precision	BI	!
	double precision	BI0	!
	double precision	BIDP	!
	double precision	BISUBC	!
	double precision	BJ2	! 
	double precision	BJ3	!
	double precision	BJ4	!
	double precision	BJ5	!
	double precision	BK2	!
	double precision	BK3	!
	double precision	BK4	!
	double precision	BK5	!
	double precision	BL	!
	double precision	BL0	!
	double precision	BLDOT	!
	double precision	BLDP	!
	double precision	BLGH	!
	double precision	BLGHP	!
	double precision	CN	!
	double precision	CN2	!
	double precision	COSDE	!
	double precision	COSFD	!
	double precision	COSFD2	!
	double precision	COSGD	!
	double precision	COSHDP	!
	double precision	COSI2	!
	double precision	COSLDP	!
	double precision	CS2GD	!
	double precision	CS2GFD	!
	double precision	CS3FGD	!
	double precision	CS3GD	!
	double precision	CSF2GD	!
	double precision	DADR	!
	double precision	DADR2	!
	double precision	DADR3	!
	double precision	DELT	!
	double precision	DLT1E	!
	double precision	DLTE	!
	double precision	DLTI	!
	double precision	E	!
	double precision	E0	!
	double precision	EA	!
	double precision	EADP	!
	double precision	EDP	!
	double precision	EDP2	!
	double precision	EDPDE2	!
	double precision	EDPDL	!
	double precision	EDPDL2	!
	double precision	EK	!
	double precision	F	!
	double precision	FDP	!
	double precision	G	!
	double precision	G0	!
	double precision	G3DG2	!
	double precision	G4DG2	!
	double precision	G5DG2	!
	double precision	GDOT	!
	double precision	GDP	!
	double precision	GM	! Gravity constant
	double precision	GM2	!
	double precision	GM3	!
	double precision	GM4	!
	double precision	GM5	!
	double precision	GMP2	!
	double precision	GMP3	!
	double precision	GMP4	!
	double precision	GMP5	!
	double precision	H	!
	double precision	H0	!
	double precision	HDOT	!
	double precision	HDP	!
	double precision	R
	double precision	SINDE	!
	double precision	SINDH	!
	double precision	SINDH2	!
	double precision	SINFD	!
	double precision	SINGD	!
	double precision	SINHDP	!
	double precision	SINI	!
	double precision	SINI2	!
	double precision	SINLDP	!
	double precision	SN2GD	!
	double precision	SN2GFD	!
	double precision	SN3FGD	!
	double precision	SN3GD	!
	double precision	SNF2GD	!
	double precision	SQRI 	!
	double precision	SQUAR 	!
	double precision	TANI2 	!
	double precision	THETA	!
	double precision	THETA2	!
	double precision	THETA4	!
	integer			ID8	!
	integer			IFLG	!
	integer			NN	!

C	// Initialized variables

	double precision	BKSUBC
	double precision	BMU
	double precision	F15D16	!  15 / 16
	double precision	F15D32	!  15 / 32
	double precision	F1D16	!   1 / 16
	double precision	F1D2	!   1 /  2
	double precision	F1D3	!   1 /  3
	double precision	F1D4	!   1 /  4
	double precision	F1D8	!   1 /  8
	double precision	F35384	!  35 /384
	double precision	F35576	!  35 /576
	double precision	F35D52	!  35 / 52
	double precision	F3D2	!   3 /  2
	double precision	F3D32	!   3 / 32
	double precision	F3D8	!   3 /  8
	double precision	F5D12	!   5 / 12
	double precision	F5D16	!   5 / 16
	double precision	F5D24	!   5 / 24
	double precision	F5D4	!   5 /  4
	double precision	F5D64	!   5 / 64
	double precision	PI2
	double precision	RE


	parameter( BKSUBC = 0.01D0 )
	parameter( BMU    = 1.0D0 )
	parameter( F15D16 = 15.0D0/  16.0D0)
	parameter( F15D32 = 15.0D0/  32.0D0)
	parameter( F1D16  =  1.0D0/  16.0D0)
	parameter( F1D2   =  1.0D0/   2.0D0)
	parameter( F1D3   =  1.0D0/   3.0D0)
	parameter( F1D4   =  1.0D0/   4.0D0)
	parameter( F1D8   =  1.0D0/   8.0D0)
	parameter( F35384 = 35.0D0/ 384.0D0)
	parameter( F35576 = 35.0D0/ 576.0D0)
	parameter( F35D52 = 35.0D0/1152.0D0)
	parameter( F3D2	  =  3.0D0/   2.0D0)
	parameter( F3D32  =  3.0D0/  32.0D0)
	parameter( F3D8   =  3.0D0/   8.0D0)
	parameter( F5D12  =  5.0D0/  12.0D0)
	parameter( F5D16  =  5.0D0/  16.0D0)
	parameter( F5D24  =  5.0D0/  24.0D0)
	parameter( F5D4   =  5.0D0/   4.0D0)
	parameter( F5D64  =  5.0D0/  64.0D0)
	parameter( PI2    = 6.283185307179586D0 )
	parameter( RE     = 1.0D0 )


C	// Common blocks have been removed. Flow control, however, is
C	// unchanged from original; complete with assigned GOTOs :(
C	// All I did was add indentation, white space, and commentary
C	// where I understood what was going on. (RTM)

	call m0blgetcons(AE, GM, BJ2, BJ3, BJ4, BJ5)

	EK	= DSQRT(GM/AE**3)
	DELT	= EK*dtsecs

	GO TO (10,111), ipass

CI------------------------------I
CI EPOCH ELEMENTS AT EPOCH TIME I
CI------------------------------I
	
C	// (comes here on ipass=0)

   10	ADP  = blpred(1)/AE
	EDP  = blpred(2)
	BIDP = blpred(3)
	HDP  = blpred(4)
	GDP  = blpred(5)
	BLDP = blpred(6)

	A0   = ADP
	E0   = EDP
	BI0  = BIDP
	H0   = HDP
	G0   = GDP
	BL0  = BLDP
	IFLG = 0

C---------------------I
C COMPUTE MEAN MOTION I
C---------------------I

	ANU=DSQRT(BMU/A0**3)

C-------------------I
C COMPUTE FRACTIONS I
C-------------------I
C     REPLACED BY PARAMETER STATEMENT ABOVE
C
	BK2 = -F1D2*(BJ2*RE*RE)
	BK3 =  BJ3 *RE**3
	BK4 =  F3D8*(BJ4*RE**4)
	BK5 =  BJ5 *RE**5

	GO TO 153


C	// (comes here on ipass=1)

  111	IF(ipert.EQ.0) GO TO 7
	IF(idmean.NE.0) GO TO 202

	ADP  = blpred(1)/AE
	EDP  = blpred(2)
	BIDP = blpred(3)
	HDP  = blpred(4)
	GDP  = blpred(5)
	BLDP = blpred(6)


  153	EDP2   = EDP*EDP
	CN2    = 1.0-EDP2
	CN     = DSQRT(CN2)
	GM2    = BK2/ADP**2
	GMP2   = GM2/(CN2*CN2)
	GM4    = BK4/ADP**4
	GMP4   = GM4/CN**8
	THETA  = DCOS(BIDP)
	THETA2 = THETA*THETA
	THETA4 = THETA2*THETA2
C
  202	IF( idmean.EQ.0 ) GO TO 155
	IF( ipass .EQ.2 ) GO TO 150

C------------------------I
C COMPUTE LDOT,GDOT,HDOT I
C------------------------I

  157	BLDOT=CN*ANU*(GMP2*(F3D2*(3.0*THETA2-1)+GMP2*F3D32*(THETA2
     &    *(-96.0*CN+30.0-90.0*CN2)+(16.0*CN+25.0*CN2-15.0)+THETA4
     &    *(144.0*CN+25.0*CN2+105.0)))+EDP2*GMP4*F15D16*(3.0+35.0*THETA4
     &    -30.0*THETA2))
	GDOT=ANU*(F5D16*GMP4*((THETA2*(126.0*CN2-270.0)+THETA4*(385.0
     &    -189.0*CN2))-9.0*CN2+21.0)+GMP2*(F3D32*GMP2*(THETA4*(45.0*CN2
     &    +360.0*CN+385.0)+THETA2*(90.0-192.0*CN-126.0*CN2)+(24.0*CN
     &    +25.0*CN2-35))+F3D2*(5*THETA2-1)))
	HDOT=ANU*(GMP4*F5D4*THETA*(3.0-7.0*THETA2)*(5.0-3.0*CN2)+GMP2
     &    *(GMP2*F3D8*(THETA*(12.0*CN+9.0*CN2-5.0)-THETA*THETA2*(5.0*CN2
     &    +36.0*CN+35.0))-3*THETA))


  155	IF(IFLG.EQ.1) GO TO 19


CI--------------------------------------------I
CI COMPUTE ISUBC TO TEST CRITICAL INCLINATION I
CI--------------------------------------------I

	BISUBC = ((1.0-5.0*THETA2)**(-2))
     &         * ((25.0*THETA4*THETA)*(GMP2*EDP2))
	IFLG   = 1

CI--------------------------------------I
CI FIRST CHECK FOR CRITICAL INCLINATION I
CI--------------------------------------I

	IF(BISUBC.GT.BKSUBC) GO TO 158
	ASSIGN 163 TO ID8
	GO TO 159

C-------------------------------I
C IS THERE CRITICAL INCLINATION I
C-------------------------------I

   19	IF( BISUBC.GT.BKSUBC) GO TO 150
  159	IF(  ipert.EQ.1)      GO TO 150
	GM3   = BK3/ADP**3
	GMP3  = GM3/(CN2*CN2*CN2)
	GM5   = BK5/ADP**5
	GMP5  = GM5/CN**10
	G3DG2 = GMP3/GMP2
	G4DG2 = GMP4/GMP2
	G5DG2 = GMP5/GMP2

CI---------------I
CI COMPUTE A1-A8 I
CI---------------I

	A1 = (F1D8*GMP2*CN2)*(1.0-11.0*THETA2
     &     - ((40.0*THETA4)/(1.0-5.0*THETA2)))
	A2 = (F5D12*G4DG2*CN2)*(1.0-((8.0*THETA4)/(1.0-5.0*THETA2))
     &     - 3.0*THETA2)
	A3 = G5DG2*((3.0*EDP2)+4.0)
	A4 = G5DG2*(1.0-(24.0*THETA4)/(1.0-5.0*THETA2)-9.0*THETA2)
	A5 = (G5DG2*(3.0*EDP2+4.0))*(1.0-(24.0*THETA4)/(1.0-5.0*THETA2)
     &     - 9.0*THETA2)
	A6 =G3DG2*F1D4

	SINI = DSIN(BIDP)

	A10 = CN2*SINI
	A7  = A6*A10
	A8P = G5DG2*EDP*(1.0-(16.0*THETA4)/(1.0-5.0*THETA2)-5.0*THETA2)
	A8  = A8P*EDP

C    COMPUTE B13-B15
C
	B13 = EDP*(A1-A2)
	B14 = A7+F5D64*A5*A10
	B15 = A8*A10*F35384
C
C   COMPUTE A11-A27
C
	A11   = 2.0+EDP2
	A12   = 3.0*EDP2+2.0
	A13   = THETA2*A12
	A14   = (5.0*EDP2+2.0)*(THETA4/(1.0-5.0*THETA2))
	A15   = (EDP2*THETA4*THETA2)/((1.0-5.0*THETA2)*(1.0-5.0*THETA2))
	A16   = THETA2/(1.0-5.0*THETA2)
	A17   = THETA4/((1.0-5.0*THETA2)*(1.0-5.0*THETA2))
	A18   = EDP*SINI
	A19   = A18/(1.0+CN)
	A21   = EDP*THETA
	A22   = EDP2*THETA
	SINI2 = DSIN(BIDP/2.0)
	COSI2 = DCOS(BIDP/2.0)
	TANI2 = DTAN(BIDP/2.0)
	A26   = 16.0*A16+40.0*A17+3.0
	A27   = A22*F1D8*(11.0+200.0*A17+80.0*A16)

CI----------------I
CI COMPUTE B1-B12 I
CI----------------I

	B1  = CN*(A1-A2)-((A11-400.0*A15-40.0*A14-11.0*A13)
     &      * F1D16+(11.0+200.0*A17+80.0*A16)*A22*F1D8)*GMP2
     &      + ((-80.0*A15-8.0*A14-3.0*A13+A11)*F5D24+F5D12*A26*A22)
     &      * G4DG2
	B2  = A6*A19*(2.0+CN-EDP2)+F5D64*A5*A19*CN2
     &      - F15D32*A4*A18*CN*CN2+(F5D64*A5+A6)*A21*TANI2
     &      + (9.0*EDP2+26.0)*F5D64*A4*A18+F15D32*A3*A21
     &      * A26*SINI*(1.0-THETA)
	B3  = ((80.0*A17+5.0+32.0*A16)*A22*SINI*(THETA-1.0)
     &      * F35576*G5DG2*EDP)-((A22*TANI2+(2.0*EDP2+3.0
     &      * (1.0-CN2*CN))*SINI)*F35D52*A8P)
	B4  = CN*EDP*(A1-A2)
	B5  = ((9.0*EDP2+4.0)*A10*A4*F5D64+A7)*CN
	B6  = F35384*A8*CN2*CN*SINI
	B7  = ((CN2*A18)/(1.0-5.0*THETA2))*(F1D8*GMP2*(1.0-15.0*THETA2)
     &      + (1.0-7.0*THETA2)*G4DG2*(-F5D12))
	B8  = F5D64*(A3*CN2*(1.0-9.0*THETA2-(24.0*THETA4/(1.0
     &      - 5.0*THETA2))))+A6*CN2
	B9  = A8*F35384*CN2
	B10 = SINI*(A22*A26*G4DG2*F5D12-A27*GMP2)
	B11 = A21*(A5*F5D64+A6+A3*A26*F15D32*SINI*SINI)
	B12 =-((80.0*A17+32.0*A16+5.0)*(A22*EDP*SINI*SINI*F35576*G5DG2)
     &      + (A8*A21*F35D52))


  150	IF( ipert.EQ.0) GO TO 7
	IF(idmean.EQ.0) GO TO 4


C-----------------------I
C COMPUTE SECULAR TERMS I
C-----------------------I


CI-----------------------I
CI ''MEAN'' MEAN ANOMALY I
CI-----------------------I

	BLDP = ANU*DELT+BLDOT*DELT+BL0
	BLDP = DMOD(BLDP,PI2)
	IF(BLDP.LT.0.0D0) BLDP=BLDP+PI2

CI---------------------------I
CI  MEAN ARGUMENT OF PERIGEE I
CI---------------------------I

	GDP = GDOT*DELT+G0
	GDP = DMOD(GDP,PI2)
	IF(GDP.LT.0.0D0) GDP=GDP+PI2

CI-----------------------------------I
CI  MEAN LONGITUDE OF ASCENDING NODE |
CI-----------------------------------I

	HDP = HDOT*DELT+H0
	HDP = DMOD(HDP,PI2)
	IF(HDP.LT.0.0D0) HDP=HDP+PI2




   4	DO 33 NN=1,6
   33	oscele(NN)=blpred(NN)


	A  = ADP
	E  = EDP
	BI = BIDP
	H  = HDP
	G  = GDP
	BL = BLDP

CI-------------------------------------I
CI COMPUTE TRUE ANOMALY(DOUBLE PRIMED) I
CI-------------------------------------I

	EADP  = M0DKEPLR(BLDP,EDP)
	SINDE = DSIN(EADP)
	COSDE = DCOS(EADP)
	SINFD = CN*SINDE
	COSFD = COSDE-EDP
	FDP   = DATAN2(SINFD,COSFD)

	IF(FDP.LT.0.D+0) FDP=FDP+PI2
	IF(ipert.EQ.1) GO TO 7


	DADR   = (1.0-EDP*COSDE)**(-1)
	SINFD  = SINFD*DADR
	COSFD  = COSFD*DADR
	CS2GFD = DCOS(2.0*GDP+2.0*FDP)
	DADR2  = DADR*DADR
	DADR3  = DADR2*DADR
	COSFD2 = COSFD*COSFD

CI----------------------------I
CI COMPUTE A(SEMI-MAJOR AXIS) I
CI----------------------------I

	A = ADP*(1.0+GM2*((3.0*THETA2-1.0)*(EDP2/(CN2*CN2*CN2))
     &    * (CN+(1.0/(1.+CN)))+((3.0*THETA2-1.0)/(CN2*CN2*CN2))
     &    * (EDP*COSFD)*(3.0+3.0*EDP*COSFD+EDP2*COSFD2)+3.0
     &    * (1.0-THETA2)*DADR3*CS2GFD))

	SN2GFD = DSIN(2.0*GDP+2.0*FDP)
	SNF2GD = DSIN(2.0*GDP+FDP)
	CSF2GD = DCOS(2.0*GDP+FDP)
	SN2GD  = DSIN(2.0*GDP)
	CS2GD  = DCOS(2.0*GDP)
	SN3GD  = DSIN(3.0*GDP)
	CS3GD  = DCOS(3.0*GDP)
	SN3FGD = DSIN(3.0*FDP+2.0*GDP)
	CS3FGD = DCOS(3.0*FDP+2.0*GDP)
	SINGD  = DSIN(GDP)
	COSGD  = DCOS(GDP)



	GO TO ID8,(163,164)

  163	DLT1E  = B14*SINGD+B13*CS2GD-B15*SN3GD

CI------------------------I
CI COMPUTE (L+G+H) PRIMED I
CI------------------------I

	BLGHP=HDP+GDP+BLDP+B3*CS3GD+B1*SN2GD+B2*COSGD
	BLGHP=DMOD(BLGHP,PI2)

	IF(BLGHP.LT.0.0D0) BLGHP=BLGHP+PI2

	EDPDL = B4*SN2GD-B5*COSGD+B6*CS3GD-F1D4*CN2*CN*GMP2
     &        * (2.0*(3.0*THETA2-1.0)*(DADR2*CN2+DADR+1.0)
     &        * SINFD+3.0*(1.0-THETA2)*((-DADR2*CN2-DADR+1.0)
     &        * SNF2GD+(DADR2*CN2+DADR+F1D3)*SN3FGD))

	DLTI  = F1D2*THETA*GMP2*SINI*(EDP*CS3FGD+3.0
     &        * (EDP*CSF2GD+CS2GFD))-(A21/CN2)*(B8*SINGD+B7*CS2GD
     &        - B9*SN3GD)
	SINDH = (1.0/COSI2)*(F1D2*(B12*CS3GD+B11*COSGD+B10*SN2GD
     &        - (F1D2*GMP2*THETA*SINI*(6.0*(EDP*SINFD-BLDP+FDP)
     &        - (3.0*(SN2GFD+EDP*SNF2GD)+EDP*SN3FGD)))))

CI-----------------I
CI COMPUTE (L+G+H) I
CI-----------------I

  164	BLGH = BLGHP+((1.0/(CN+1.0))*F1D4*EDP*GMP2*CN2
     &       * (3.0*(1.0-THETA2)*(SN3FGD*(F1D3+DADR2*CN2+DADR)
     &       + SNF2GD*(1.0-(DADR2*CN2+DADR)))+2.0*SINFD
     &       * (3.0*THETA2-1.0)*(DADR2*CN2+DADR+1.0)))+GMP2*F3D2
     &       * ((-2.0*THETA-1.0+5.0*THETA2)*(EDP*SINFD+FDP-BLDP))
     &       + (3.0+2.0*THETA-5.0*THETA2)*(GMP2*F1D4*(EDP*SN3FGD
     &       + 3.0*(SN2GFD+EDP*SNF2GD)))
	BLGH = DMOD(BLGH,PI2)

	IF(BLGH.LT.0.0D0) BLGH=BLGH+PI2

	DLTE = DLT1E+(F1D2*CN2*((3.0*(1.0/(CN2*CN2*CN2))*GM2
     &       * (1.0-THETA2)*CS2GFD*(3.0*EDP*COSFD2+3.0*COSFD+EDP2
     &       * COSFD*COSFD2+EDP))-(GMP2*(1.0-THETA2)*(3.0*CSF2GD
     &       + CS3FGD))+(3.0*THETA2-1.0)*GM2*(1.0/(CN2*CN2*CN2))
     &       * (EDP*CN+(EDP/(1.0+CN))+3.0*EDP*COSFD2+3.0*COSFD
     &       + EDP2*COSFD*COSFD2)))

	EDPDL2 = EDPDL*EDPDL
	EDPDE2 = (EDP+DLTE)*(EDP+DLTE)

CI-------------------------I
CI COMPUTE E(ECCENTRICITY) I
CI-------------------------I

	E      = DSQRT(EDPDL2+EDPDE2)
	SINDH2 = SINDH*SINDH
	SQUAR  = (DLTI*COSI2*F1D2+SINI2)*(DLTI*COSI2*F1D2+SINI2)
	SQRI   = DSQRT(SINDH2+SQUAR)

CI--------------------------I
CI COMPUTE BI (INCLINATION) I
CI--------------------------I

	BI = DASIN(SQRI)
	BI = 2.0*BI
	BI = DMOD(BI,PI2)
	IF(BI.LT.0.0D0) BI=BI+PI2

CI-----------------------------I
CI CHECK FOR E(ECCENTRICITY)=0 I
CI-----------------------------I

	IF(E.NE.0.0) GO TO 168
	BL = 0.0

CI-----------------------------I
CI CHECK FOR BI(INCLINATION)=0 I
CI-----------------------------I

  145	IF(BI.NE.0.0) GO TO 169
	H = 0.0

CI---------------------------------I
CI COMPUTE G(ARGUMENT  OF PERIGEE) I
CI---------------------------------I

  146	G = BLGH-BL-H
	G = DMOD(G,PI2)
	IF(G.LT.0.0D0) G=G+PI2

CI----------------------I
CI COMPUTE TRUE ANOMALY I
CI----------------------I

	EA   = M0DKEPLR(BL,E)
	ARG1 = DSIN(EA) * DSQRT(1.0-E**2)
	ARG2 = DCOS(EA)-E
	F    = DATAN2(ARG1,ARG2)
	IF(F.LT.0.D+0) F=F+PI2


	oscele(1) = A*AE
	oscele(2) = E
	oscele(3) = BI
	oscele(4) = H
	oscele(5) = G
	oscele(6) = BL

    7	CONTINUE

	blpred(1) = ADP*AE
	blpred(2) = EDP
	blpred(3) = BIDP
	blpred(4) = HDP
	blpred(5) = GDP
	blpred(6) = BLDP

	IF(ipert.EQ.0) BL=DMOD(ANU*DELT,PI2)

	aux(1) = EADP
	aux(2) = GDP+FDP
	aux(3) = GDP
	aux(4) = EK*(ANU+BLDOT)
	aux(5) = FDP

	R=A*AE*(1.0D0-E*DCOS(EA))

	GO TO 45

CI----------------------------------------I
CI MODIFICATIONS FOR CRITICAL INCLINATION I
CI----------------------------------------I

  158	DLT1E = 0.0
	BLGHP = 0.0
	EDPDL = 0.0
	DLTI  = 0.0
	SINDH = 0.0

	ASSIGN 164 TO ID8

	GO TO 150
C
  168	SINLDP = DSIN(BLDP)
	COSLDP = DCOS(BLDP)
	SINHDP = DSIN(HDP)
	COSHDP = DCOS(HDP)

CI-------------------------I
CI COMPUTE L(MEAN ANOMALY) I
CI-------------------------I

	ARG1 = EDPDL*COSLDP+(EDP+DLTE)*SINLDP
	ARG2 = (EDP+DLTE)*COSLDP-(EDPDL*SINLDP)
	BL   = DATAN2(ARG1,ARG2)
	BL   = DMOD(BL,PI2)
	IF(BL.LT.0.0D0) BL=BL+PI2
	GO TO 145

CI----------------------------------------I
CI COMPUTE H(LONGITUDE OF ASCENDING NODE) I
CI----------------------------------------I

  169	ARG1 = SINDH*COSHDP+SINHDP*(F1D2*DLTI*COSI2+SINI2)
	ARG2 = COSHDP*(F1D2*DLTI*COSI2+SINI2)-(SINDH*SINHDP)
	H    = DATAN2(ARG1,ARG2)
	H    = DMOD(H,PI2)

	IF(H.LT.0.0D0) H=H+PI2

	GO TO 146

   45   CONTINUE

	RETURN
	END




*| Name:
*|	m0atomm - compute mean motion from semi-major axis
*|
*| Interface:
*|	double precision function
*|	m0atomm(double precision sma, double precision ecc,
*|        double precision fincl)
*|
*| Input:
*|	sma	- semi-major axis
*|	ecc	- eccentricity
*|	fincl	- inclination
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return value:
*|	mean motion
*|
*| Remarks:
*|	Semi-major axis is in km, eccentricity is unitless, and
*|	inclination is in degrees. The return value of mean motion
*|	is in revolutions per day. See 'remarks' for m0mmtoa for
*|	a discussion of the algorithm.
*|
*| Categories: 
*|	navigation  


	double precision function m0atomm ( SMA, ECC, FINC )

	implicit NONE


C	// Interface variables

	double precision	ECC	! eccentricity
	double precision	FINC	! inclination
	double precision	SMA	! semi-major axis

C	// Local variables

	double precision	A0	! ??
	double precision	A1	! ??
	double precision	A1OLD	! ??
	double precision	AB	! semi-major axis (non-
C					! dimensional)
	double precision	B1	!
	double precision	B2	!
	double precision	B3	!
	double precision	COSINC	! cos(finc)
	double precision	D	! 
	double precision	DE2RA	! 
	double precision	F	!
	double precision	FPRIME	!
	double precision	Q	!
	double precision	R	!
	double precision	T	!
	double precision	TWOPI	! 


C	// Initialized variables

	double precision	CK2	!
	double precision	XKE	!
	double precision	XMNPDA	! Minutes per day
	double precision	XKMPER	! Earth radius

	data	CK2	/  0.000541308D0 /
	data 	XKE	/  0.743669161D-1/
	data	XMNPDA	/  1440.D0	 /
	data	XKMPER	/  6378.135D0	 /



	TWOPI	= 2.D0  * DACOS(-1.D0)
	DE2RA	= TWOPI / 360.D0

	AB	= SMA / XKMPER
	COSINC	= DCOS(DE2RA*FINC)
	D	= 1.5D0*CK2*(3.D0*COSINC*COSINC-1.D0)
     &          * (1.D0-ECC*ECC)**(-1.5D0)

C	// The following code section solves the cubic
C	//
C	// A0^3 - AB*A0^2 + D*AB = 0
C	//
C	// using the algorithm described in Section 5.5 of
C	// of Numerical Recipes (Press et al.)

	B1	= -AB
	B2	=   0.D0
	B3	=  D*AB
	Q	= (B1*B1-3.D0*B2)/9.D0
	R	= (2.D0*B1*B1*B1-9.D0*B1*B2+27.D0*B3)/54.D0
	T	= (DSQRT(R*R-Q*Q*Q)+DABS(R))**(1.D0/3.D0)
	A0	= -DSIGN(T+Q/T,R) - B1/3.D0


C	// The following code section solves
C	//
C	// A1^6 - A0*A1^5 - (D/3)*A1^4 - D^2*A1^2 - 134*D^3/81 = 0
C	//
C	// using Newton's method; the initial guess for A1 is A0.

	A1 = A0
  40	CONTINUE 
	    A1OLD  = A1
            F      = 134.D0*D*D*D/81.D0 + A1*A1*(-D*D + A1*A1*(-D/3.D0
     &             + A1*(-A0+A1)))
	    FPRIME = A1*(-2.D0*D*D + A1*A1*(-4.D0*D/3.D0
     &             + A1*(-5.D0*A0+6.D0*A1)))
	    A1     = A1OLD - F/FPRIME
	if ( DABS(A1-A1OLD).gt.1.D-7) go to 40

C	// Now apply Kepler's third law to get the mean motion
C	// in radians per minute; convert to revs per day

	m0atomm	= XKE*A1**(-1.5D0)
	m0atomm = m0atomm*XMNPDA/TWOPI

	return
	end


*| Name:
*|	m0mmtoa - compute semi-major axis from mean motion 
*|
*| Interface:
*|	double precision function
*|	m0mmtoa(double precision mm, double precision ecc,
*|        double precision fincl)
*|
*| Input:
*|	mm	- mean motion
*|	ecc	- eccentricity
*|	fincl	- inclination
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return value:
*|	semi-major axis
*|
*| Remarks:
*|	This algorithm is based on code used in	both NESDIS
*|	routine AMMSMA and NORAD SGP4 to convert a two-line
*|	mean motion to a Brouwer mean semi-major axis. Mean motion
*|	is in revolutions per day, semi-major axis is in km,
*|	eccentricity is unitless, and inclination is in degrees.
*|	For a more thorough treatment of the problem of converting
*|	orbital elements from one orbit predicition model to
*|	another, read 
*|	  Walter, H. G. 1967: Conversion of osculating orbital
*|	     elements into mean elements. The Astronomical Journal,
*|	     72(8), 994-997.
*|	I do not know whether the algorithm in AMMSMA was based on
*|	Walter's work or not. The NESDIS package contains a routine
*|	OSMEAN that purports to be an implementation of Walter's
*|	algorithm and it is NOT called by AMMSMA. In conclusion,
*|	the algorithm implemented in m0mmtoa is of indeterminate
*|	theoretical ancestry, but is in use by both NORAD and
*|	NESDIS for the particular problem of converting	a NORAD
*|	two-line mean motion into a Brouwer-mean semi-major
*|	for sun-synchronous orbits. "Your Mileage May Vary"
*|	(RTM 95.05.02)
*|
*| Categories: 
*|	navigation  


	double precision function m0mmtoa ( MM, ECC, FINC )

	implicit NONE


C	// Interface variables

	double precision	ECC	! eccentricity (unitless)
	double precision	FINC	! inclination (degrees)
	double precision	MM	! mean motion (km)


C	// Local variables

	double precision	A0	!
	double precision	A1	!
	double precision	AB	! non-dimensional
C					! semi-major axis
	double precision	B0	!
	double precision	B1	!
	double precision	COSINC	! cos(inclination)
	double precision	DE2RA	! degrees to radians
	double precision	ICCON	! 
	double precision	N0	!
	double precision	NB	!
	double precision	TWOPI	!


C	// Initialized variables

	double precision	CK2	! 
	double precision	XKE	! 
	double precision	XMNPDA	! minutes per day
	double precision	XKMPER	! Earth radius (km)

	data	CK2	/  0.000541308D0 /
	data 	XKE	/  0.743669161D-1/
	data	XMNPDA	/  1440.D0	 /
	data	XKMPER	/  6378.135D0	 /

	TWOPI	= 2.D0  * DACOS(-1.D0)
	DE2RA	= TWOPI / 360.D0

	COSINC	= DCOS(FINC*DE2RA)
	ICCON	= (3.D0*COSINC*COSINC-1.D0)*(1.D0-ECC*ECC)**(-1.5D0)
      
	N0	= MM * TWOPI / XMNPDA
	A1	= (XKE/N0)**(2.D0/3.D0)
	B1	= 1.5D0*CK2*ICCON/A1/A1
	A0	= A1*(1.D0-B1*(1.D0/3.D0+B1*(1.D0+134D0*B1/81.D0)))
	B0	= 1.5D0*CK2*ICCON/A0/A0

	NB	= N0 / (1.D0+B0)
	AB	= A0 / (1.D0-B0)

	m0mmtoa	= AB*XKMPER

	return
	end
