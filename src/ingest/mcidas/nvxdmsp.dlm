C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

C *** McIDAS Revision History ***
C 1 NVXDMSP.DLM 27-Feb-96,13:17:46,`ROBERTM' initial checkin of DMSP nav
C 2 NVXDMSP.DLM 29-Mar-96,16:02:30,`ROBERTM' upper case entry pts for convdlm
C 3 NVXDMSP.DLM 17-Apr-96,14:53:22,`USER' Released
C *** McIDAS Revision History ***

C$ Name:
C$	NVXINI - initialize DMSP navigation
C$
C$ Interface:
C$	INTEGER FUNCTION
C$	NVXINI(integer option, integer navparms)
C$
C$ Input:
C$	option	- 1 to initialize navigation module
C$		  2 to set Earth coordinate form
C$	navparms- navigation parameter array for option=1
C$		  lit('LL  ') or lit('XYZ ') for option=2
C$
C$ Input and Output:
C$	none
C$
C$ Output:
C$	none
C$
C$ Return values:
C$	  0	- navigation module initialized
C$	 -1	- could not initialize re-entrant data collection
C$	 -2	- could not initialize planet module
C$	 -3	- could not initialize inverse nav engine (locus)
C$	 -4	- navigation module could not be initialized
C$		  with the parameters given (option=1)
C$	 -5	- unrecognized Earth coordinate form (option=2)
C$	 -6	- could not save re-entrant data
C$	-10	- unrecognized 'option'
C$
C$ Remarks:
C$	    This routine is actually called as nv1ini(),
C$	nv2ini(), or nv3ini() according to the 'slot' (instance)
C$	of navigation desired.
C$	    The contents of the navigation parameter array are
C$	satellite and sensor-specific. 
C$
C$ Categories: 
C$	navigation  

	INTEGER FUNCTION NVXINI(option, codicil)
	
	integer		codicil(*)
	integer		handle
	integer		option
	integer		rc

	COMMON /NAVHDL/ handle

	rc = m0nchdl(handle)
	if( rc.lt.0) call ddest('m0nchdl() return code=',rc)

	rc = mcgpnini(handle, option, codicil)
	if( rc.lt.0) call ddest('mcgpnini() return code=',rc)

	NVXINI = rc

	return
	end



C$ Name:
C$	NVXSAE - DMSP forward (image->earth) navigation
C$
C$ Interface:
C$	INTEGER FUNCTION
C$	NVXSAE(real line, real elem, real dummy,
C$	  real e1, real e2, real e3)
C$
C$ Input:
C$	line		- image line (scan) coordinate
C$	elem		- image element coordinate
C$	dummy		- (presently unused)
C$
C$ Input and Output:
C$	none
C$
C$ Output:
C$	e1		- first Earth coordinate
C$	e2		- second Earth coordinate
C$	e3		- third Earth coordinate
C$
C$ Return values:
C$	 0		- success
C$	-1		- line or element out of range for sensor
C$	-2		- point not on Earth surface
C$
C$ Remarks:
C$	    This routine is actually called as nv1sae(),
C$	nv2sae(), or nv3sae() according to the 'slot' (instance)
C$	of navigation desired.
C$	    The values of e1, e2, and e3 depend upon the I/O option
C$	specified by calling mcgpnini() with option = 2. For LLT,
C$	the values are geodetic latitude, geodetic longitude, and
C$	height above the geoid (presently always 0). Angles are
C$	in degrees, West positive as per McIDAS convention. For CT,
C$	the values are the X, Y, and Z coordinates in a Cartesian
C$	terrestrial system.
C$
C$ Categories: 
C$	navigation  

	INTEGER FUNCTION NVXSAE(line, elem, dummy, x, y, z)

	integer		handle

	real		dummy
	real		dtct
	real		elem
	real		line
	real		x
	real		y
	real		z

	COMMON /NAVHDL/ handle

C	// The use of a separate value 'dtct' for the constant
C	// detector number is necessary because assignment of
C	// dummy to 1. results in a segmentation violation on
C	// a Sun.

	dtct = 1.
	NVXSAE = mcgpnfwd(handle, line, elem, dtct, x, y, z)
	if( NVXSAE.lt.0) call ddest('mcgpnfwd() return code=',NVXSAE)

	return
	end


C$ Name:
C$	NVXEAS - DMSP inverse (Earth->image) navigation
C$
C$ Interface:
C$	INTEGER FUNCTION
C$	NVXEAS(real e1, real e2, real e3,
C$	  real line, real elem, real detector)
C$
C$ Input:
C$	e1		- first Earth coordinate
C$	e2		- second Earth coordinate
C$	e3		- third Earth coordinate
C$
C$ Input and Output:
C$	none
C$
C$ Output:
C$	line		- image line (scan) coordinate
C$	elem		- image element coordinate
C$	detector	- (presently unused)
C$
C$ Return values:
C$	 0		- success
C$	-1		- could not create re-entrant data collection
C$	-2		- could not save locus state
C$	-3		- could not save intermediate solution
C$	-4		- point not on image
C$
C$ Remarks:
C$	    This routine is actually called as nv1eas(),
C$	nv2eas(), or nv3eas() according to the 'slot' (instance)
C$	of navigation desired.
C$	    The values of e1, e2, and e3 depend upon the I/O option
C$	specified by calling mcgpnini() with option = 2. For LLT,
C$	the values are geodetic latitude, geodetic longitude, and
C$	height above the geoid (presently always 0). For CT, the
C$	values are the X, Y, and Z coordinates in a Cartesian
C$	terrestrial system.
C$
C$ Categories: 
C$	navigation  

	INTEGER FUNCTION NVXEAS(x, y, z, line, elem, dtct)

	integer		handle

	real		dtct
	real		elem
	real		line
	real		x
	real		y
	real		z

	COMMON /NAVHDL/ handle

	NVXEAS = mcgpninv(handle, x, y, z, line, elem, dtct)
	if( NVXEAS.lt.0) call ddest('mcgpninv() return code=',NVXEAS)

	return
	end

C$ Name:
C$	NVXOPT - generic polar navigation special services
C$
C$ Interface:
C$	INTEGER FUNCTION
C$	NVXOPT(integer option, real xin, real xout)
C$
C$ Input:
C$	option		- special service request (see 'Remarks')
C$	xin		- input data vector; contents depend on
C$			  service (see 'Remarks')
C$
C$ Input and Output:
C$	none
C$
C$ Output:
C$	xout		- output data vector; contents depend on
C$			  service (see 'Remarks')
C$
C$ Return values:
C$	 0		- success
C$	-1		- instance not initialized (call mcgpini()
C$			  first)
C$
C$ Remarks:
C$	    This routine is actually called as nv1opt(),
C$	nv2opt(), or nv3opt() according to the 'slot' (instance)
C$	of navigation desired.
C$
C$	    The following services are available:
C$ option	input			output
C$ ------	-----			------
C$ 'SPOS'	date (yyddd)		geodetic latitdude (degrees)
C$		time (hours)		geodetic longitude (w>0)
C$					radius from Earth center (km)
C$					x (km)    Cartesian celestial
C$					y (km)	  position (X axis
C$					z (km)	  through vernal
C$						  equinox)
C$
C$ 'ORBT'	(none)			orbital period (seconds)
C$					at epoch (valid time of
C$					orbital elements)
C$
C$ Categories: 
C$	navigation  

	INTEGER FUNCTION NVXOPT(option, input, output)

	integer		handle
	integer		option

	real		input(*)
	real		output(*)

	COMMON /NAVHDL/ handle

	NVXOPT = mcgpnopt(handle, option, input, output)
	if( NVXOPT.lt.0) call ddest('mcgpnfwd() return code=',NVXOPT)

	return
	end
