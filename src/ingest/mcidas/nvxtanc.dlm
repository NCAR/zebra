C   THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.

C *** McIDAS Revision History ***
C *** McIDAS Revision History ***

*$ Name:
*$	nvxini - Initialize navigation for tangent cone projection
*$
*$ Interface:
*$	integer function
*$	nvxint(integer option, integer param(*))
*$
*$ Input:
*$	option	- 1 to set or change projection parameters
*$      option  - 2 set output option
*$	param	- For option 1:
*$		  param( 1) = 'TANC'
*$		  param( 2) = image line    of pole*10000
*$		  param( 3) = image element of pole*10000
*$		  param( 4) = km per pixel         *10000
*$		  param( 6) = standard latitude    *10000
*$		  param( 7) = standard longitude   *10000
*$                for option 2:
*$                param( 1) = 'LL' or 'XYZ'
*$
*$ Input and Output:
*$	none
*$
*$ Output:
*$	none
*$
*$ Return values:
*$	 0	- success
*$      -3	- invalid or inconsistent navigation parameters
*$	-4	- invalid navigation parameter type
*$	-5	- invalid nvxini() option
*$
*$ Remarks:
*$	    Latitudes and longitudes are in degrees, West positive.
*$	Projection parameters must be in the following ranges:
*$	      -90. <  standard latitude  <  90.
*$	     -180. <= standard longitude < 180.
*$		0. <  scale
*$	Accuracy may suffer near the standard latitude limits.
*$
*$          The projection algorithm is adapted from that in
*$	Saucier, W. J. 1989: Principles of meteorological analysis.
*$	  Dover Publications, Inc. 433 pp.
*$
*$ Categories: 
*$	navigation  

C     // CODING CONVENTION note: function declarations and common
C     // block declarations are all capitalized to be recognizeable
C     // to script 'convdlm;' this is necessary for a correct build
C     // in MCIDAS-X. For the same reason, one must avoid referring
C     // to function or common block names in upper case elsewhere

      INTEGER FUNCTION NVXINI(option,param)


      implicit		NONE


C     // Interface variables (formal arguments)

      integer		option		! initialization option
      integer		param(*)	! navigation parameters or
C					! output coordinate type

C     // Local variable definitions

      character*4	navtyp		! codicil type
      character*4	outcoord	! output coordinate type
      character*80	cbuf		! text output buffer



C     //////////////////////////////////////////////////////////////////
C     Common block variables and declaration.

C     ALL CODE BETWEEN THE '//////' SEPARATORS MUST BE 
C     DUPLICATED EXACTLY IN EACH NAVIGATION ROUTINE

C     (A more maintenance-safe version would use ENTRY points
C     rather than separate functions for the navigation API
C     but entry points cannot be processed by 'convdlm.')

C     // Common block contents: projection parameters

      real		Lin0		! image line of pole
      real		Ele0		! image element of pole
      real		Scale		! km per unit image coordinate
C					! (pixel)
      real		Lon0		! standard longitude
      real		Lat0		! standard latitude
      real		Colat0		! standard colatitude

C     // Common block contents: pre-computed intermediate values

      real		Coscl		! cosine(Colat0)
      real		Tancl		! tangent(Colat0)
      real		Tancl2		! tangent(Colat0/2)
      real		Mxtheta		! limit of angle from std. lon
C					! on projection surface

C     // Common block contents: constants

      real		D2R		! degrees to radians factor
      real		Pi
      real		Badreal		! returned when navigation
C					! cannot be done
      real		Erad		! Earth radius
      logical		Init		! initialized flag
      logical		Latlon		! .TRUE. for lat/lon I/O


      COMMON/TANC/ Lin0, Ele0, Scale, Lon0, Colat0, Lat0,
     &  Coscl, Tancl, Tancl2, Mxtheta,
     &  D2R, Pi, Badreal, Erad, Init, Latlon
      
C     End of common block variables and declaration.
C     //////////////////////////////////////////////////////////////////




C     // Begin initialization process by setting values of constants.

      Erad = 6370.			! This value of Erad is ok 
C					! for "low-precision" nav
C					! where a spherical Earth is
C					! adequate (Saucier, p. 32)
      Pi	= acos(-1.)
      D2R	= Pi / 180.

      Badreal	= -1.E10		! obvious unreasonable value
C					! for nav transform result

C     // Process initialization options. Only one option, initialize
C     // navigation parameters, is supported in this demo version,
C     // but a 'hook' is left for an additional option to set the
C     // output coordinate to something other than lat/lon

      if( option.eq.1 ) then
          
         call DDEST('nvxini(tanc) option=1',0)

         call movwc(param(1),navtyp)
         if( navtyp.eq.'TANC') then

C           // Unpack tangent cone projection parameters

            Lin0	= param(2) / 10000.
            Ele0	= param(3) / 10000.
            Scale	= param(4) / 10000.
            Lat0	= param(5) / 10000.
            Lon0	= param(6) / 10000.
           

            call ddest('Lin0= param(2)',param(2))
            call ddest('Ele0= param(3)',param(3))
            call ddest('Scale= param(4)',param(4))
            call ddest('Lat0= param(5)',param(5))
            call ddest('Lon0= param(6)',param(6))
            write(cbuf,'('' nvxini: Lat0, Lon0 '',2F12.4)')
     *        Lat0, Lon0
            call DDEST(cbuf,0)

C           // apply range checking

            if(Scale.le.0. ) then
               call DDEST('nvxini(tanc) scale is negative',0)
               Init	= .FALSE.
               NVXINI	= -3
               return
            end if

            if(Lat0.le.-90. .or. Lat0.ge.90. ) then
               call DDEST('nvxini(tanc) standard lat out of range',0)
               Init	= .FALSE.
               NVXINI	= -3
               return
            end if

            if(Lat0 .eq. 0.)then
               call DDEST('nvxini(tanc) standard lat equal to 0'//
     &                    ' not allowed',0)
               Init	= .FALSE.
               NVXINI	= -3
               return
            end if

            if(Lon0.le.-180. .or. Lon0.gt.180. ) then
               call DDEST('nvxini(tanc) standard lon out of range',0)
               Init	= .FALSE.
               NVXINI	= -3
               return
            end if

C           // convert degrees to radians and latitude to colatitude.
C           // Account for McIDAS longitude convention

            Lon0	= -Lon0 * D2R

c  ** check for northern or southern hemisphere projection cone.
c  ** force colatitude to positive definite.

            if(Lat0 .lt. 0)then
              Colat0      =  Pi/2.  + D2R*Lat0
            else 
              Colat0      =  Pi/2.  - D2R*Lat0
            endif

            write(cbuf,'('' nvxini:  Colat0, Lon_0 '',2F12.4)')
     *        Colat0, Lon0
            call DDEST(cbuf,0)

C           // Compute intermediate quantities

            Coscl	= cos(Colat0)
	    Tancl	= tan(Colat0)
	    Tancl2	= tan(Colat0/2.)
	    Mxtheta	= Pi*Coscl

            write(cbuf,'('' nvxini: Coscl,   Tancl'', 2F7.4)')
     *        Coscl, Tancl
            call DDEST(cbuf,0)

            write(cbuf,'('' nvxini: Tancl2, Mxtheta '', 2F7.4)')
     *        tancl2, Mxtheta
            call DDEST(cbuf,0)

	    Latlon	= .TRUE.


         else		! option=1 but type not 'TANC'

            call DDEST('nvxini(tanc) parameter type bad',0)
            Init	= .FALSE.
            NVXINI	= -4
	    return

         end if

      else if ( option .eq. 2) then

         call movwc(param(1),outcoord)
	 if( outcoord.eq.'LL' ) then
	     Latlon = .TRUE.
	 else if( outcoord.eq.'XYZ') then
	     Latlon = .FALSE.
         else
             call DDEST('option=2 coord '//outcoord//' not supported',0)
             Init   = .FALSE.
             NVXINI = -5
         end if

      else		! option not 1 or 2

         call DDEST('nvxini(tanc) unrecognized output option ',option)
         NVXINI =  -4
         return

      end if

      NVXINI	= 0
      Init	= .TRUE.

            call ddest('Lin0= param(2)',param(2))
            call ddest('Ele0= param(3)',param(3))
            call ddest('Scale= param(4)',param(4))
            call ddest('Lat0= param(5)',param(5))
            call ddest('Lon0= param(6)',param(6))
      return
      end
    



*$ Name:
*$	nvxsae - Compute earth coordinates from image coordinates
*$
*$ Interface:
*$	integer function
*$	nvxsae( real lin, real ele, real dummy,
*$              real e1,  real  e2, real    e3 )
*$
*$ Input:
*$	lin	- image line
*$	ele	- image element
*$	dummy	- (unused)
*$
*$ Input and Output:
*$	none
*$
*$ Output:
*$	e1	- latitude  or x
*$	e2	- longitude or y
*$	e3	- height    or z
*$
*$ Return values:
*$	 0	- success
*$	-1	- input data physically valid but not navigable
*$		  given the specified projection
*$	-6	- module not initialized
*$
*$ Remarks:
*$          The navigation module must first be initialized with
*$	a call to nvxini(). The output form (lat,lon) or (x,y,z)
*$	depends on the last call to nvxini() with option 2.
*$
*$ Categories: 
*$	navigation  

      INTEGER FUNCTION NVXSAE( lin, ele, dummy, e1, e2, e3 )

      implicit		NONE

C     // Interface variables (formal arguments)

      real		lin	! image line to navigate
      real		ele	! image element to navigate
      real		dummy	! (unused argument)
      real		e1	! Earth coordinate 1
      real		e2	! Earth coordinate 2
      real		e3	! Earth coordinate 3

C     // Local variables

      real		lat	! latitude  (McIDAS convention)
      real		lon	! longitude (McIDAS convention)
      real		hgt	! height
      real		dx	! zonal displacement from pole
C				! on projection surface
      real		dy	! meridional displacement from pole
      real		radius	! distance from pole on projection
      real		theta	! angle from standard longitude on
C				! projection surface
      real           theta_rh	! right handed angle measure
      real		colat	! colatitude of navigated point


C     //////////////////////////////////////////////////////////////////
C     Common block variables and declaration.

C     ALL CODE BETWEEN THE '//////' SEPARATORS MUST BE 
C     DUPLICATED EXACTLY IN EACH NAVIGATION ROUTINE

C     (A more maintenance-safe version would use ENTRY points
C     rather than separate functions for the navigation API
C     but entry points cannot be processed by 'convdlm.')

C     // Common block contents: projection parameters

      real		Lin0		! image line of pole
      real		Ele0		! image element of pole
      real		Scale		! km per unit image coordinate
C					! (pixel)
      real		Lon0		! standard longitude
      real		Colat0		! standard colatitude
      real		Lat0		! standard latitude

C     // Common block contents: pre-computed intermediate values

      real		Coscl		! cosine(Colat0)
      real		Tancl		! tangent(Colat0)
      real		Tancl2		! tangent(Colat0/2)
      real		Mxtheta		! limit of angle from std. lon
C					! on projection surface

C     // Common block contents: constants

      real		D2R		! degrees to radians factor
      real		Pi
      real		Badreal		! returned when navigation
C					! cannot be done
      real		Erad		! Earth radius
      logical		Init		! initialized flag
      logical		Latlon		! .TRUE. for lat/lon I/O


      COMMON/TANC/ Lin0, Ele0, Scale, Lon0, Colat0, Lat0,
     &  Coscl, Tancl, Tancl2, Mxtheta,
     &  D2R, Pi, Badreal, Erad, Init, Latlon
      
C     End of common block variables and declaration.
C     //////////////////////////////////////////////////////////////////

      e1	= Badreal
      e2	= Badreal
      e3	= Badreal


C     // verify initialized module

      if(.not.Init) then
          NVXSAE = -6
          return
      end if


C     // Compute radius and bearing from pole

      dx	= Scale*(lin-Lin0)
      dy	= Scale*(ele-Ele0)

      radius	= sqrt(dx*dx+dy*dy)
      theta_rh	= atan2(dy,dx)

c ** convert theta_rh to angle FROM standard longitude (theta)
c ** maintaining theta positive from positive x-axis.

      if(Lat0 .lt. 0.)then
         if(theta_rh .le. 0.)then
            theta  =  (Pi - abs(theta_rh))
         else if(theta_rh .gt. 0)then
            theta  = -1.*(Pi - abs(theta_rh))
         endif
      else
            theta = theta_rh
      endif

C     // Apply range checking on theta to determine if point is navigable

      if ( theta.le.-Mxtheta .or. theta.gt.Mxtheta ) then
         NVXSAE = -1
         return
      end if

C     // Forward navigation: compute longitude and colatitude
C     // from radius and theta.


         lon = Lon0 + theta/Coscl

         if(lon.le.-Pi) lon = lon + 2.d0*Pi
         if(lon.gt. Pi) lon = lon - 2.d0*Pi

         colat	= 2.*atan( Tancl2 * (radius/(Erad*Tancl))**(1./Coscl))

C     // Rescale to McIDAS convention (degrees, West positive).
C     // Apply conversion to Cartesian coordinates if 'XYZ' set
C     // as output form. Set return code for success.

      lon	= -lon/D2R
      lat	= 90. - colat/D2R
      hgt	= 0.

c ** Check for northern or southern hemisphere projection cone.

      if(.not.Latlon) then
          if(Lat0 .lt. 0.)lat = -1.*lat
          call nllxyz(lat,lon,e1,e2,e3)
      else
          if(Lat0 .lt. 0.)then
            e1 = -1.*lat
          else
            e1 = lat
          endif
          e2 = lon
	  e3 = 0.
      end if

      NVXSAE	= 0

      return
      end




*$ Name:
*$	nvxeas - Compute image coordinates from earth coordinates 
*$
*$ Interface:
*$	integer function
*$	nvxeas( real e1, real e2, real e3,
*$	        real lin, real ele, real dummy)
*$
*$ Input:
*$	e1	- latitude  or x
*$	e2	- longitude or y
*$	e3	- height    or z
*$
*$ Input and Output:
*$	none
*$
*$ Output:
*$	lin	- image line
*$	ele	- image element
*$	dummy	- (unused)
*$
*$ Return values:
*$	 0	- success
*$	-1	- input data physically valid but not navigable
*$		  given the specified projection
*$	-2	- input data exceed physical limits
*$	-6	- module not initialized
*$
*$ Remarks:
*$          The navigation module must first be initialized with
*$	a call to nvxini(). The input form (lat,lon) or (x,y,z)
*$	depends on the last call to nvxini() with option 2.
*$	    Input longitude may be in the range -360 to +360;
*$	values outside this range will not be denavigated.
*$	Height (hgt) is ignored.
*$
*$ Categories: 
*$	navigation  

      INTEGER FUNCTION NVXEAS( e1, e2, e3, lin, ele, dummy)

      implicit		NONE

C     // Interface variables (formal arguments)

      real		e1	! Earth coordinate 1
      real		e2	! Earth coordinate 2
      real		e3	! Earth coordinate 3
      real		lin	! image line to navigate
      real		ele	! image element to navigate
      real		dummy	! (unused argument)

C     // Local variables

      real		lat	! latitude  (McIDAS convention)
      real		lon	! longitude (McIDAS convention)
      real		hgt	! height
      real		in_lon	! input longitude (radians,
C				! East positive)
      real		colat	! colatitude
      real		radius	! distance from pole on projection
      real		theta	! angle from standard longitude on
C				! projection surface

C     //////////////////////////////////////////////////////////////////
C     Common block variables and declaration.

C     ALL CODE BETWEEN THE '//////' SEPARATORS MUST BE 
C     DUPLICATED EXACTLY IN EACH NAVIGATION ROUTINE

C     (A more maintenance-safe version would use ENTRY points
C     rather than separate functions for the navigation API
C     but entry points cannot be processed by 'convdlm.')

C     // Common block contents: projection parameters

      real		Lin0		! image line of pole
      real		Ele0		! image element of pole
      real		Scale		! km per unit image coordinate
C					! (pixel)
      real		Lon0		! standard longitude
      real		Colat0		! standard colatitude
      real		Lat0		! standard latitude

C     // Common block contents: pre-computed intermediate values

      real		Coscl		! cosine(Colat0)
      real		Tancl		! tangent(Colat0)
      real		Tancl2		! tangent(Colat0/2)
      real		Mxtheta		! limit of angle from std. lon
C					! on projection surface

C     // Common block contents: constants

      real		D2R		! degrees to radians factor
      real		Pi
      real		Badreal		! returned when navigation
C					! cannot be done
      real		Erad		! Earth radius
      logical		Init		! initialized flag
      logical		Latlon		! .TRUE. for lat/lon I/O


      COMMON/TANC/ Lin0, Ele0, Scale, Lon0, Colat0, Lat0,
     &  Coscl, Tancl, Tancl2, Mxtheta,
     &  D2R, Pi, Badreal, Erad, Init, Latlon
      
C     End of common block variables and declaration.
C     //////////////////////////////////////////////////////////////////


      lin	= Badreal
      ele	= Badreal
      dummy	= Badreal

C     // verify that module is initialized

      if(.not.init) then
          NVXEAS = -6
          return
      end if

C     // Preprocess input values. If mode is 'XYZ' first convert
C     // from Cartesian to lat/lon. If mode is 'LL' just transcribe
C     // from arguments.

      if(Latlon) then
          lat = e1
          lon = e2
          hgt = e3
      else
          call nxyzll( e1, e2, e3, lat, lon)
	  hgt = 0.
      end if

C     // check that input values are physically possible and
C     // then convert to radians and East positive

      if ( lat.lt.-90. .or. lat.gt.90. ) then
         NVXEAS	= -2
         return
      end if

      if( lon.le.-360..or.lon.gt.360.) then
          NVXEAS = -2
          return
      end if

      if( lat.eq.-90. .or. lat.eq.90. ) then
          NVXEAS = -1
          return
      end if

c     // Check for northern or southern hemisphere projection cone.

      if(Lat0 .lt. 0)then
           colat     = Pi/2. + D2R*lat
      else
           colat     = Pi/2. - D2R*lat
      endif
      in_lon	= -D2R*lon

C     // map longitude into range -Pi to Pi

      if(in_lon.le.-Pi) in_lon = in_lon + 2.*Pi
      if(in_lon.gt. Pi) in_lon = in_lon - 2.*Pi


C     // Now trap opposite Pole. Though a physically possible latitude,
C     // tan(colat/2) -> infinity there so it is not navigable

      if ( colat.eq.Pi ) then
        NVXEAS	= -1
        return
      end if


C     // Compute radius and theta of point on projection surface.
C     // Theta is tricky; you have to compute offset relative
C     // to standard longitude, force that into -pi to +pi range,
C     // and THEN scale by cos(Colat0)

      radius	= Erad * Tancl *( tan(colat/2.)/Tancl2 ) ** Coscl
      theta	= in_lon-Lon0

      if(theta.le.-Pi) theta = theta + 2.*Pi
      if(theta.gt. Pi) theta = theta - 2.*Pi

      theta     = Coscl * theta


c ** Compute line and element, check for northern or southern
c ** hemisphere projection cone.  Put north pole on top of frame,
c ** south pole on bottom.  Maintain right-handed coordinate system
c ** by measuring theta positive from the positive x-axis.

      if(Lat0 .lt. 0.)then
         theta = Pi + theta
      endif

         lin	= Lin0 + radius*cos(theta)/Scale
         ele	= Ele0 + radius*sin(theta)/Scale

      dummy	= 0.

      NVXEAS 	= 0

      return
      end



*$ Name:
*$	nvxopt - Perform supplemental navigation operations
*$
*$ Interface:
*$	integer function
*$	nvxopt(integer option, real xin(*),
*$	  real xout(*) )
*$ Input:
*$	option	- 'SCAL' compute projection scale
*$	xin(1)	- latitude
*$
*$ Input and Output:
*$	none
*$
*$ Output:
*$	xout(1)	- km per pixel at given latitude
*$
*$ Return values:
*$	 0	- success
*$	-1	- input latitude physically valid, but projection
*$		  undefined or scale infinite there
*$	-2	- input latitude exceeds physical limits
*$	-5	- unrecognized option
*$	-6	- module not initialized
*$
*$ Remarks:
*$	    The navigation module must first be initialized by
*$	a call to nvxini().  Latitude is in degrees, north positive,
*$	and must lie between -90. and +90.
*$
*$ Categories: 
*$	navigation  

      INTEGER FUNCTION NVXOPT( option, xin, xout)

      implicit		NONE

C     // Interface variables (formal arguments)

      integer		option	! special service name (character
C				! stored as integer)
      real		xin(*)	! input vector
      real		xout(*) ! output vector

C     // Local variables

      character*4	copt	! special service (character form)
      real		colat	! input colatitude


C     //////////////////////////////////////////////////////////////////
C     Common block variables and declaration.

C     ALL CODE BETWEEN THE '//////' SEPARATORS MUST BE 
C     DUPLICATED EXACTLY IN EACH NAVIGATION ROUTINE

C     (A more maintenance-safe version would use ENTRY points
C     rather than separate functions for the navigation API
C     but entry points cannot be processed by 'convdlm.')

C     // Common block contents: projection parameters

      real		Lin0		! image line of pole
      real		Ele0		! image element of pole
      real		Scale		! km per unit image coordinate
C					! (pixel)
      real		Lon0		! standard longitude
      real		Colat0		! standard colatitude
      real		Lat0		! standard latitude

C     // Common block contents: pre-computed intermediate values

      real		Coscl		! cosine(Colat0)
      real		Tancl		! tangent(Colat0)
      real		Tancl2		! tangent(Colat0/2)
      real		Mxtheta		! limit of angle from std. lon
C					! on projection surface

C     // Common block contents: constants

      real		D2R		! degrees to radians factor
      real		Pi
      real		Badreal		! returned when navigation
C					! cannot be done
      real		Erad		! Earth radius
      logical		Init		! initialized flag
      logical		Latlon		! .TRUE. for lat/lon I/O


      COMMON/TANC/ Lin0, Ele0, Scale, Lon0, Colat0, Lat0,
     &  Coscl, Tancl, Tancl2, Mxtheta,
     &  D2R, Pi, Badreal, Erad, Init, Latlon
      
C     End of common block variables and declaration.
C     //////////////////////////////////////////////////////////////////


      xout(1) 	= Badreal

C     // verify initialized module

      if(.not.init) then
          NVXOPT = -6
          return
      end if

C     // Extract and interpret the option

      call movwc(option,copt)

      if(copt.eq.'SCAL') then

C        //  Compute colatitude and make sure it is
C        //  physically possible and navigable

         if ( xin(1).gt.90. .or. xin(1).lt.-90. ) then
             NVXOPT = -2
             return
	 else if ( xin(1).eq.90. .or. xin(1).eq.-90. ) then
	     NVXOPT = -1	
	     return
         end if

c ** check for northern or southern hemisphere projection cone.

         if(Lat0 .lt. 0.)then
            colat = Pi/2. + D2R*xin(1)
         else
            colat = Pi/2. - D2R*xin(1)
         endif

C        // Now compute actual scale for this colatitude

         xout(1) = scale
     *           *(sin(Colat0)*(tan(colat/2.)/Tancl2)**Coscl)/sin(colat)

C     else if(copt.eq.'????') 
C        // Add code for additional options here

      else
         NVXOPT = -5
         return
      end if

      NVXOPT = 0

      return
      end
