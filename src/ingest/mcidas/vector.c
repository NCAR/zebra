/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 VECTOR.C 27-Feb-96,13:16:12,`ROBERTM' initial checkin of DMSP nav       */
/* 2 VECTOR.C 17-Apr-96,14:54:52,`USER' Released                             */
/* 3 VECTOR.C 27-Sep-96,16:29:40,`ROBERTM' Include mcidasp.h; upgrade docs   */
/* 4 VECTOR.C 22-Oct-96,19:43:34,`USER' Released                             */
/**** McIDAS Revision History *** */

/* vector.c
 *
 * For unit test, compile with UNIT_TEST defined
 *
 * Change log:
 * 95.10.19 rtm Convert naming conventions to Core McIDAS
 *		standards
 * 95.07.21 rtm	Add posFromVector() and vecDiff() to support
 *		Chebyshev polynomial debugging.
 * 95.03.01 rtm Remove vecNLX and posNLX to nvlog module.
 *		Rename vecConvert, posConvert, posExtract,
 *		llh_to_adr and adr_to_llh and move to
 *		planet module.
 * 95.02.22 rtm Add PLNX prefix to all 'planet' module
 *		calls. Add NLX to nav log calls.
 * 95.02.07 rtm Removed subroutine calls in vecNorm
 *		(all now done in-line) and optimize
 *		vecCopy.
 * 95.01.11 rtm Added posOffset
 * 95.01.05 rtm	Created
 *
 */


/* COORDINATE SYSTEM ORIENTATIONS:
 * The two supported coordinate systems at present are
 * CELESTIAL and TERRESTRIAL. Both of these systems have their
 * origin at the Earth's center.
 *
 * A 'celestial' system is one in which the principal axis (x) passes
 * through the vernal equinox; the principal axis can alternatively
 * be viewed as the intersection of the equatorial plane and the
 * ecliptic (plane of the Earth's orbit). It is therefore
 * nearly inertial (not exactly so because of Earth axis precession
 * and nutation).
 *
 * A 'terrestrial' system is one in which the principal axis (x) passes
 * through 0 longitude. Such a system therefore rotates with the Earth.
 *
 * SYSTEM REPRESENTATIONS:
 *
 * The representations are Cartesian, Right Ascension/Declination,
 * and Latitude/Longitude.
 *
 * Declination is the angle between the object and the normal to the
 * z-axis (like a latitude). Right Ascension is the angle between
 * the plane of the x and z axes and the object (like a longitude).
 *
 * The latitude is a GEODETIC latitude, defined as the angle
 * between the equatorial plane and local vertical relative to
 * the geoid. Note that geodetic latitude is always greater
 * than Declination.
 *
 *
 * COORDINATE TYPES
 * The coordinate system and representation of a position is
 * specified by an enumerated type with one of the following
 * values:
 *	LLT	spherical (lat-lon) position relative to geoid
 *		(geodetic latititude)
 *	CT	cartesian position in terrestrial system
 *	ADT	spherical (lat-lon) position in terrestrial system
 *		(geocentric latitude)
 *	ADC	spherical (lat-lon) position in celestial system
 *	CC	cartesian position in celestial system
 */


/* WHAT POSITION ELEMENTS (3) MEAN
 * The position specification always consists of three words, but
 * they mean different things depending upon the system and
 * representation in use.
 *
 *	type	word 1		word 2		word 3
 *	----	------		------		------
 *	LLT	longitude	latitude	height above geoid
 *	CT	X		Y		Z
 *	ADT	right ascen.	declination	radius
 *	ADC	right ascen.	declination	radius
 *	CC	X		Y		Z
 * 
 * X, Y, and Z are the dot-products of the vector from origin
 * to position with each of the axis unit vectors (i,j,k).
 * Radius is the distance from the coordinate center (all of these
 * are Earth-centered) and the position.
 */

/* A DESIGN NOTE: 
 * A model where each coordinate system has
 * its own type was considered but rejected because it would have
 * meant a lot of wasted space and awkward code. An example is NVXSAE
 * where the result can be in any coordinate system set by the NVXINI
 * call; for strong typing the code would have had to define one
 * instance of EACH position type just to do the conversions, and
 * would have required modification each time a new coordinate
 * was added. */

/*
 * GEOCENTRIC and GEODETIC LATITUDES
 * Note that the actual coordinate transform between geocentric
 * latitude (declination) and geodetic latitude depends on the
 * oblateness (flattening or eccentricity) of the planet. The
 * other transforms are purely 'algorithmic.' This results in a
 * somewhat unfortunate split of the code between vector.c and 
 * planet.c. Many routines in vector.c are used only by planet.c
 * but must be made public anyway. Were it not for the name mangling
 * need, it would be better to move them to planet.c. These
 * routines are marked in their doc blocks in preparation of such
 * a move.
 */

/* WHY DO VECTORS LOOK LIKE POSITIONS?
 * There are no differences as stored.
 * There is a semantic difference; vectors must have a Cartesian type
 * CT or CC. Many of the M0VC and M0PT routines check for this
 * explicitly when compiled with M0DEBUG defined.
 * There is also a major logical difference, reflected in how M0PT
 * and M0VC are used. Vectors specify only an orientation in space,
 * positions refer to an offset from the origin of the coordinate
 * system (always the Earth center for the coordinate types
 * presently defined -- CC, ADC, CT, ADT, LLT). */

/* ZERO-LENGTH VECTORS
 * For applications involving the angle between vectors,
 * zero length vectors can lead to division of 0/0. It is 
 * therefore necessary to trap vectors that are 'approximately'
 * zero. The routine M0VCzero() can detect these. */

/* Philosophical question: Is a zero-length vector in
 * M0VTnorm or M0VTangle ever a legitimate occurrence that
 * should be trapped and handled or does it always represent
 * a bug in the calling routine?  I would hate to have to
 * introduce error code and error handlers unless I have to */


/* DEFINITION OF "COORDINATE SYSTEM"
 * is a collection of three orthogonal unit vectors making up a
 * 'basis'. In M0DEBUG mode, coordinate systems should be
 * validated at creation. */

/* REFERENCE COORDINATE SYSTEM
 * All coordinate systems have to be defined relative to something.
 * The 'something' is here the celestial system (CC), for which
 * the 'M0CR' struct contains
 *	M0PT { CC, 0., 0., 0. }
 *	i   { CC, 1., 0., 0. }
 *	j   { CC, 0., 1., 0. }
 *	k   { CC, 0., 0., 1. }
 *
 * The 'M0CT' for the position and each vector is redundant,
 * but this allows the components of a coordinate system
 * (especially the 'scanner' coordinate) to be referenced and
 * used just like the position and vectors they really are.
 */


/* SATELLITE COORDINATES:
 * The 'assumed' coordinate system is centered at the Earth origin.
 * Another frequently occurring coordinate system is a 'satellite'
 * system, centered on the satellite and oriented relative to the
 * orbit plane and local vertical. The scanner view directions
 * are computed relative to this system. Such a coordinate system
 * is stored in an M0CT. Each unit vector is stored either in
 * CC or CT form (Cartesian Celestial or Terrestrial) and provides
 * a way to go from direction cosines relative to the satellite
 * to an orientation in space.  Vectors (M0VC) and positions (M0PT)
 * in systems other than Earth centered (CC or CT) never really
 * exist, hence the lack of a 'coordinate system' element in
 * M0PT or M0VC. */


#include <ctype.h>	/* for posGet and posPut only	*/
#include <float.h>	/* DBL_EPSILON in here		*/	
#include <math.h>
#include <string.h>
#include "mcidasp.h"
#include "m0frame.h"


/*
*| Name:
*|      M0PTdiff - Computes the vector between two points.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	M0VC
*|	M0PTdiff(M0PT PTs, M0PT PTd)
*|
*| Input:
*|      PTs     - Starting    point.
*|      PTd     - Destination point.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	Vector from PTs to PTd.
*|
*| Remarks:
*|      Both positons are assumed to be in the same fundamental
*|      coordinate system, either Celestial or Terrestrial.  The
*|      coordinate system type must be Cartesian.  M0PLpt_convert
*|	can be used to convert PT object coordinates.
*|      The above assumption is validated if compiled with
*|	M0DEBUG defined.
*|
*| Categories: 
*|      utility
*/

M0VC
M0PTdiff(M0PT PTs, M0PT PTd)
{
	M0VC	VCdiff;		/* vector from PTs to PTd	*/

	/* Validate the types. Use M0VCvalid_type even though these
	 * are positions because Cartesian coordinates in particular
	 * are needed. Types must also agree */

	M0ASSERT ( M0VCvalid_type(PTs.type) );
	M0ASSERT ( M0VCvalid_type(PTd.type) );
	M0ASSERT ( PTs.type == PTd.type );

	VCdiff.type	= PTs.type;

	VCdiff.c[0] = PTd.c[0] - PTs.c[0];
	VCdiff.c[1] = PTd.c[1] - PTs.c[1];
	VCdiff.c[2] = PTd.c[2] - PTs.c[2];

	return(VCdiff);
}



/*
*| Name:
*|      M0PTidentical - Compares two positions.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	M0flag
*|	M0PTidentical(M0PT PTa, M0PT PTb)
*|
*| Input:
*|      PTa     - First point.
*|      PTb     - Second point.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|      M0TRUE if points are identical.
*|
*| Remarks:
*|      This is a very strict comparison.  It detects only
*|      exact copies; there is no tolerance for slight differences
*|      due to roundoff during computation.
*|
*| Categories: 
*|      utility
*/


M0flag
M0PTidentical(M0PT PTa, M0PT PTb)

/* A lotta guys might convert this to use
 * memcmp(&PTa, &PTb, sizeof(M0PT)); */
{
	return ( PTa.type == PTb.type &&
		 PTa.c[0] == PTb.c[0] &&
		 PTa.c[1] == PTb.c[1] &&
		 PTa.c[2] == PTb.c[2] );
}



/*
*| Name:
*|      M0PTmake - Initializes a position (M0PT) object.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	M0PT
*|	M0PTmake(double c1, double c2, double c3, M0CT type)
*|
*| Input:
*|      c1      - Value of  first coordinate.
*|      c2      - Value of second coordinate.
*|      c3      - Value of third  coordinate.
*|      type    - Coordinate type.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	M0PT object with specified properties.
*|
*| Remarks:
*|      When compiled with M0DEBUG defined, this routine
*|	validates the type.
*|
*| Categories: 
*|      utility
*/


M0PT
M0PTmake(double c1, double c2, double c3, M0CT type)
{
	M0PT	PTnew;	/* newly created position (M0PT type)	*/

	M0ASSERT ( M0PTvalid_type(type) );

	PTnew.c[0] = c1;
	PTnew.c[1] = c2;
	PTnew.c[2] = c3;

	PTnew.type = type;

	return(PTnew);
}



/*
*| Name:
*|      M0PToffset - Displaces a position by a given vector.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	M0PT
*|	M0PToffset(M0PT PTs, M0VC VCoffset)
*|
*| Input:
*|      PTs        - Starting position.
*|      VCoffset   - Offset vector.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|      Original position located VCoffset away from PTs.
*|
*| Remarks:
*|      PTs must be in a Cartesian coordinate system
*|      and PTs and VCoffset must have the same type.  When compiled
*|	with M0DEBUG defined, these assumptions will be validated.
*|
*| Categories: 
*|      utility
*/


M0PT
M0PToffset(M0PT PTs, M0VC VCoffset)
{
	M0PT	PTd;	/* offset (destination) position	*/

	M0ASSERT( M0VCvalid_type(VCoffset.type) );	/* not Cartesian */
	M0ASSERT( VCoffset.type == PTs.type );		/* no type match */

	PTd.type = PTs.type;

	PTd.c[0] = PTs.c[0] + VCoffset.c[0];
	PTd.c[1] = PTs.c[1] + VCoffset.c[1];
	PTd.c[2] = PTs.c[2] + VCoffset.c[2];

	return(PTd);
}


/*
*| Name:
*|      M0PTvc - Converts position to position vector (M0VC).
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	M0VC
*|	M0PTvc(M0PT PT))
*|
*| Input:
*|      PT      - Input position (type M0PT).
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|      "Position vector" (M0VC)
*|
*| Remarks:
*|      The input position must be in Cartesian form
*|      (type CC or CT).  This assumption is validated with
*|	an assertion when compiled with M0DEBUG defined.
*|
*| Categories: 
*|      utility
*/


M0VC
M0PTvc(M0PT PT)
{
	M0VC	VCpos;	/* position vector	*/

	M0ASSERT ( M0VCvalid_type(PT.type) );

	VCpos.type = PT.type;
	VCpos.c[0] = PT.c[0];
	VCpos.c[1] = PT.c[1];
	VCpos.c[2] = PT.c[2];

	return(VCpos);
}


/*
*| Name:
*|      M0VCangle - Computes the angle between two vectors.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	double
*|	M0VCangle(M0VC VCa, M0VC VCb)
*|
*| Input:
*|      VCa     - The first vector.
*|      VCb     - The second vector.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|      The angle between the vectors, in radians.
*|
*| Remarks:
*|      The angle is computed using
*|	VCa (dot) VCb = mag(VCa)*mag(VCb) * cos(angle)
*|      to get at the angle.  The coordinate systems must
*|	be the same, and neither vector can be of zero
*|      magnitude.  These assumptions are validated with
*|	assertions when compiled with M0DEBUG defined.
*|
*| Categories: 
*|      utility
*/

double M0VCangle(M0VC VCa, M0VC VCb)
{
	double	 angle;		/* angle between vectors	*/

	M0ASSERT( M0VCvalid_type(VCa.type) );
	M0ASSERT( VCa.type == VCb.type );
	M0ASSERT(!M0VCzero(VCa) );
	M0ASSERT(!M0VCzero(VCb) );

	angle = M0VCinner(VCa,VCb) / ( M0VCmag(VCa) * M0VCmag(VCb) );
	if( fabs(angle) > 1. ) {
#ifdef UNDEF	
		fprintf(stderr,"M0VCangle cos(angle) = %15.12f\n",
		  angle);
#endif
		angle = angle < 0. ? -1. : 1.;
	}
	M0ASSERT(fabs(angle) <= 1.);
	angle = acos(angle);

	return(angle);
}



/*
*| Name:
*|      M0VCcross - Computes cross product of two vectors.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	M0VC
*|	M0VCcross(M0VC VCa, M0VC VCb)
*|
*| Input:
*|      VCa     - The first vector.
*|      VCb     - The second vector.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|      The cross product of the two vectors.
*|
*| Remarks:
*|          The vector coordinate types must be the same.  This
*|	is validated with an assertion when compiled with
*|	M0DEBUG defined.
*|
*| Categories: 
*|      utility
*/

M0VC
M0VCcross(M0VC VCa, M0VC VCb)
{
	M0VC	VCcross;	/* cross product	*/

	M0ASSERT( M0VCvalid_type(VCa.type) );	/* corrupted type	*/
	M0ASSERT( VCa.type == VCb.type );	/* type mismatch	*/

	VCcross.c[0] =   VCa.c[1]*VCb.c[2] - VCa.c[2]*VCb.c[1];
	VCcross.c[1] = - VCa.c[0]*VCb.c[2] + VCa.c[2]*VCb.c[0];
	VCcross.c[2] =   VCa.c[0]*VCb.c[1] - VCa.c[1]*VCb.c[0];

	VCcross.type = VCa.type;

	return(VCcross);
}


/*
*| Name:
*|      M0VCdiff - Computes the difference between two vectors.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	M0VC
*|	M0VCdiff(M0VC VCa, M0VC VCb)
*|
*| Input:
*|      VCa     - The first vector.
*|      VCb     - The second vector.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	VCb-VCa
*|
*| Remarks:
*|      The vector coordinates must be the same type.  This
*|	is validated with an assertion when compiled with
*|	M0DEBUG defined.
*|
*| Categories: 
*|      utility
*/


M0VC
M0VCdiff(M0VC VCa, M0VC VCb)
{
	M0VC	VCdiff;

	M0ASSERT( M0VCvalid_type(VCa.type) );	/* corrupted type	*/
	M0ASSERT( VCa.type == VCb.type );	/* type mismatch	*/

	VCdiff.type = VCa.type;
	VCdiff.c[0] = VCb.c[0] - VCa.c[0];
	VCdiff.c[1] = VCb.c[1] - VCa.c[1];
	VCdiff.c[2] = VCb.c[2] - VCa.c[2];

	return(VCdiff);
}



/*
*| Name:
*|      M0VCinner - Computes the inner (dot) product of two vectors.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	double
*|	M0VCinner(M0VC VCa, VCb)
*|
*| Input:
*|      VCa     - First vector.
*|      VCb     - Second vector.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|      Inner product of the two vectors (a scalar).
*|
*| Remarks:
*|      The vector coordinate types must be the same.
*|	This assumption is validated with an assertion when
*|	compiled with M0DEBUG defined.
*|
*| Categories: 
*|      utility
*/

double
M0VCinner(M0VC VCa, M0VC VCb)
{
	double	inner;	/* inner product	*/

	M0ASSERT( M0VCvalid_type(VCa.type) );	/* corrupted type	*/
	M0ASSERT( VCa.type == VCb.type );	/* mismatched types	*/

	inner = VCa.c[0]*VCb.c[0] + VCa.c[1]*VCb.c[1] + VCa.c[2]*VCb.c[2];
	return(inner);
}


/*
*| Name:
*|      M0VCmag - Computes the magnitude (length) of a vector.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	double
*|	M0VCmag(M0VC VC)
*|
*| Input:
*|      VC      - A vector
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|      Magnitude (length) of the vector.
*|
*| Remarks:
*|      This routine just computes the inner product
*|	(M0VCinner() ) and takes the square root of that.
*|
*| Categories: 
*|      utility
*/

double
M0VCmag(M0VC VC)
{
	return( sqrt ( M0VCinner(VC, VC) ) );
}




/*
*| Name:
*|      M0VCnorm - Normalize a vector (compute unit vector).
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	M0VC
*|	M0VCnorm(M0VC VC)
*|
*| Input:
*|      VC      - Vector of arbitrary length.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|      Unit vector.
*|
*| Remarks:
*|      none
*|
*| Categories: 
*|      utility
*/


M0VC
M0VCnorm(M0VC VC)
{
	double	length;	/* vector length (magnitude)	*/
	double	len2;	/* vector length squared	*/
	M0VC	VCunit;	/* unit vector			*/

	M0ASSERT(M0VCvalid_type(VC.type) );
	VCunit.type = VC.type;

	/* for efficiency, do not bother to modify vectors
	 * that are 'almost' unit vectors already. Use 
	 * M0VCinner() to compute the square of the
	 * length and test against 1; if 'close,'
	 * bypass the (unnecessary) square root and three
	 * divisions */

	len2	= M0VCinner(VC, VC);

	if ( fabs(len2-1.) > DBL_EPSILON ) {
		length  = sqrt(len2);
		VCunit.c[0] = VC.c[0] / length;
		VCunit.c[1] = VC.c[1] / length;
		VCunit.c[2] = VC.c[2] / length;
	}
	else {
		VCunit.c[0] = VC.c[0];
		VCunit.c[1] = VC.c[1];
		VCunit.c[2] = VC.c[2];
	}

	return(VCunit);
}


/*
*| Name:
*|      M0VCproject - Computes the projection of a vector onto a plane.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	M0VC
*|	M0VCproject(M0VC VCnorm, VC)
*|
*| Input:
*|      VCnorm  - Vector normal to plane.
*|      VC      - Vector to project.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|      Projection of VC onto plane VCnorm.
*|
*| Remarks:
*|      The projection is accomplished by rotating VCnorm
*|	and VC so that VCnorm is coincident with the coordinate
*|	Z axis, dropping the Z coordinate of VC, and reversing
*|      the rotations.  The vector coordinates must be the same
*|	and the projection vector must be of non-zero length;
*|	these assumptions are validated with assertions if
*|	compiled with M0DEBUG defined. 
*|
*| Categories: 
*|      utility
*/

M0VC
M0VCproject(M0VC VCnorm, M0VC VC)
{
	M0VC	VCproj;		/* projection of VC into plane
				 * normal to VCnorm */

	M0ASSERT(M0VCvalid_type(VC.type) );
	M0ASSERT( VC.type == VCnorm.type);

	VCproj.type = VC.type;
	m0vcpro_( VCnorm.c, VC.c, VCproj.c );

	return(VCproj);
}





/*
*| Name:
*|      M0VCpt  - Converts a position vector to position.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	M0VC
*|	M0VCpt(M0VC VCpos)
*|
*| Input:
*|      VCpos   - A position vector.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|      PT      - Position.
*|
*| Remarks:
*|      Use with care.  The conversion of a vector (an
*|	orientation in space) to a position (offset from a
*|	coordinate system origin) is valid only if the vector
*|	was computed relative to the origin of the vector's
*|      coordinate system.  There is no way to automatically
*|      validate this assumption.  For example, suppose a
*|	vector Vpoint is computed that describes the view
*|      vector of a satellite sensor.  It could be converted
*|	to a 'position' with this routine, but the result would
*|	be meaningless, because the view vector's origin is
*|      the satellite, not the center of the Earth.  (There
*|	are other reasons why it is probably meaningless,
*|	but this is the main one).
*|
*| Categories: 
*|      utility
*/

M0PT
M0VCpt(M0VC VCpos)
{
	M0PT	PT;	/* position	*/

	PT.type = VCpos.type;
	PT.c[0] = VCpos.c[0];
	PT.c[1] = VCpos.c[1];
	PT.c[2] = VCpos.c[2];

	return(PT);
}





/*
*| Name:
*|      M0VCrotate - Rotates a vector about an arbitrary axis in
*|                   space.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	M0VC
*|	M0VCrotate(M0VC VCaxis, double angle, M0VC VC)
*|
*| Input:
*|      VCaxis  - Vector axis of rotation.
*|      angle   - Angle of rotation.
*|      VC      - Vector to rotate.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	Rotated vector
*|
*| Remarks:
*|      The rotation angle is in radians, counter-clockwise
*|	positive.
*|
*|      The rotation is carried out in 4-d (homogeneous)
*|      form.  Both VC and VCaxis are rotated about the X axis to
*|	put 'VCaxis' in the X-Z plane, then about the Y axis
*|	to put 'VCaxis' parallel with Z. 'VC' is then rotated
*|      about the Z axis through 'angle,' and then VC and VCaxis are
*|	rotated back to its original state relative to the
*|      Y and X axes.  Such a use of homogeneous coordinate
*|	transforms is described in Chapter 5 of Foley, van Dam,
*|	Feiner, and Hughes, "Computer Graphics: Principles and
*|	Practice."
*|
*| Categories: 
*|      utility
*/

M0VC
M0VCrotate(M0VC VCaxis, double angle, M0VC VC)
{
	M0VC	VCrotate;	/* Rotated vector	*/

	M0ASSERT( M0VCvalid_type(VCaxis.type) );
	M0ASSERT( VC.type == VCaxis.type );

	VCrotate.type = VC.type;
	m0vcrot_( VCaxis.c, &angle, VC.c, VCrotate.c);

	return(VCrotate);
}

/*
*| Name:
*|      M0VCscale - Multiply vector by scalar.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	M0VC
*|	M0VCscale(double scale, M0VC VC)
*|
*| Input:
*|      scale   - Scalar to multiply with vector.
*|      VC      - Vector to rescale.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	Scaled vector
*|
*| Remarks:
*|	None.
*|
*| Categories: 
*|      utility
*/

M0VC
M0VCscale(double scale, M0VC VC)
{
	M0VC	VCscale;	/* Scaled vector	*/

	M0ASSERT(M0VCvalid_type(VC.type));	/* corrupt
						 * coordinate
						 * type ! */
	VCscale.type = VC.type;
	VCscale.c[0] = scale * VC.c[0];
	VCscale.c[1] = scale * VC.c[1];
	VCscale.c[2] = scale * VC.c[2];

	return(VCscale);
}
/*
*| Name:
*|      M0zrot - Rotates a vector (array form) about the Z axis.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	void
*|	M0zrot(double a[3], double angle, double b[3])
*|
*| Input:
*|  a[3]    - Initial vector.
*|  angle   - Rotation angle.
*|  b[3]    - Final vector.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	none
*|
*| Remarks:
*|      This routine works directly with 3-d vectors in
*|      array form to perform a simple Z-axis rotation.  It is
*|	used for quick conversions by M0PLvc_convert() and
*|	M0PLpt_convert() from Celestial to Terrestrial
*|	coordinates.
*|
*| Categories: 
*|      utility
*/


void
M0zrot(double a[], double angle, double b[])
{
	double		cosg;	/* cosine(angle)	*/
	double		sing;	/* sine(angle)		*/

	cosg = cos(angle);
	sing = sin(angle);

	b[0] =  a[0]*cosg - a[1]*sing;
	b[1] =  a[0]*sing + a[1]*cosg;
	b[2] =  a[2];

	return;
}




/*
*| Name:
*|      M0adr_xyz - Converts Spherical coordinates to Cartesian.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	void
*|	M0adr_xyz(double sph[3], double crt[3] )
*|
*| Input:
*|      adr[3]    - Array of Spherical coordinates
*|
*| Input and Output:
*|	none
*|
*| Output:
*|      xyz[3]    - Array of Cartesian coordinates.
*|
*| Return values:
*|	none
*|
*| Remarks:
*|      The Spherical coordinates are Right Ascension
*|	(radians east of Prime Meridian), Declination (radians
*|	north of Equator), and Radius from origin.
*|
*|      The Cartesian coordinates are x, y, and z, where the
*|	Z axis pierces the North Pole, the X axis pierces the
*|	Equator at the Prime Meridian, and the Y axis completes
*|	a right-handed system.
*|
*| Categories: 
*|      utility
*/

void
M0adr_xyz(double adr[3], double xyz[3])
{
	double	rz;		/* distance from  Z axis to point */

	xyz[2]	= adr[2] * sin ( adr[1] );
	rz	= adr[2] * cos ( adr[1] );
	xyz[1]	= rz	 * sin ( adr[0] );
	xyz[0]	= rz	 * cos ( adr[0] );

	return;
}



/*
*| Name:
*|      M0xyz_adr - Converts Cartesian to Spherical form.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	void
*|	M0xyz_adr(double crt[3], double sph[3] )
*|
*| Input:
*|      xyz[3]    - Array of Cartesian coordinates.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|      adr[3]    - Array of Spherical coordinates.
*|
*| Return values:
*|	none
*|
*| Remarks:
*|      The Cartesian coordinates are x, y, and z, where the
*|	Z axis pierces the North Pole, the X axis pierces the
*|	Equator at the Prime Meridian, and the Y axis completes
*|	a right-handed system.
*|      The Spherical coordinates are Right Ascension
*|	(radians east of Prime Meridian), Declination (radians
*|	north of Equator), and Radius from origin.
*|
*| Categories: 
*|      utility
*/


void
M0xyz_adr(double xyz[3], double adr[3] )
{
	adr[2]	= sqrt  ( xyz[0]*xyz[0]
		+ 	  xyz[1]*xyz[1]
		+         xyz[2]*xyz[2] );
	adr[1]	= asin  ( xyz[2]/adr[2] );
	adr[0]	= atan2 ( xyz[1],xyz[0] );

	return;
}

 
/*
*| Name:
*|      M0VCzero - Tests vector for zero length.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	M0flag
*|	M0VCzero(M0VC VC)
*|
*| Input:
*|      VC      - Input vector for length test.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	M0TRUE if vector length is zero, M0FALSE otherwise
*|
*| Remarks:
*|      M0VCzero() returns M0TRUE if the length of 'VC' is
*|	'close' to zero, i.e. its magnitude is less than
*|      DBL_EPSILON.  This scaling is really only valid for
*|	vectors of approximately the same length as a unit
*|      vector.  This is intended to be a very strict test.
*|	It will probably flag as 'zero length' some otherwise
*|	useable vectors.
*|
*| Categories: 
*|      utility
*/

M0flag
M0VCzero(M0VC VC)
{
	double	len2;	/* square of vector length	*/

	len2 = M0VCinner(VC, VC);

	return ( sqrt(len2) < DBL_EPSILON );
}



/*345678901234567890123456789012345678901234567890123456789012345678901234567890
 *       1         2         3         4         5         6         7         8
 */

/*
*| Name:
*|      M0PTrange - Maps coordinate components into standard ranges.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	void
*|	M0PTrange(M0PT *PT)
*|
*| Input:
*|	none
*|
*| Input and Output:
*|      *PT       - Pointer to position (MOPT) to re-range.
*|
*| Output:
*|	none
*|
*| Return values:
*|	none
*|
*| Remarks:
*|      The longitude (or right ascension if appropriate)
*|      is remapped into the range -PI to +PI.  If M0DEBUG defined
*|	when compiled, the latitude (or declination, if appropriate)
*|	is verified to be in the range -PI/2 to +PI/2.
*|
*| Categories: 
*|      utility
*/

void
M0PTrange(M0PT *pPT)
{

	/* Validate type	*/

	M0ASSERT(M0PTvalid_type(pPT->type) );


	/* Apply re-ranging and validation only to spherical
	 * coordinate, not Cartesian, types	*/

	if ( pPT->type == LLT ||
	     pPT->type == ADT ||
	     pPT->type == ADC ) {

		/* Remap longitude to be less than or 
		 * equal to +PI... */

		while ( pPT->c[0] > M0PI ) {
			pPT->c[0] -= 2.*M0PI;
		}	

		/* ...and greater than -PI */

		while ( pPT->c[0] <= -M0PI ) {
			pPT->c[0] += 2.*M0PI;
		}

		/* Expand validation range for latitude
		 * slightly. Input of +90 degrees and
		 * roundoff in conversion caused the 
		 * assertion to fail for this 'valid' 
		 * input */

		M0ASSERT ( pPT->c[1] <=  M0PI/2+FLT_EPSILON );
		M0ASSERT ( pPT->c[1] >= -M0PI/2-FLT_EPSILON );

		/* For right-ascension/declination systems
		 * (Celestial or Terrestrial) the third element,
		 * radius, should never be negative */

		if ( pPT->type != LLT )
			M0ASSERT ( pPT->c[2] >= 0.     );
	}
	return;
}


/*
*| Name:
*|      M0VCvalid_type - Validates coordinate type for vector.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	M0flag
*|	M0VCvalid_type(M0CT coord_type)
*|
*| Input:
*|      coord_type     - Coordinate system type.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	M0TRUE if coord_type is a valid type for a vector (M0VC).
*|
*| Remarks:
*|      Valid coordinate types for vectors are CT and CC.
*|
*| Categories: 
*|      utility
*/


M0flag
M0VCvalid_type(M0CT coord_type)
{
	if ( coord_type == CT || coord_type == CC ) 
		return(M0TRUE);
	else
		return(M0FALSE);
}


/*
*| Name:
*|      M0PTvalid_type - Validates coordinate type for position.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	M0flag
*|	M0PTvalid_type(M0CT coord_type)
*|
*| Input:
*|      coord_type     - Coordinate system type.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	M0TRUE if coord_type is a valid type for a position (M0PT).
*|
*| Remarks:
*|      Valid coordinate types for positions are LLT, CT, ADT,
*|	CC, and ADC.
*|
*| Categories: 
*|      utility
*/

M0flag
M0PTvalid_type(M0CT coord_type)
{
	if ( coord_type == LLT ||
	     coord_type == CT  ||
	     coord_type == ADT ||
	     coord_type == ADC ||
	     coord_type == CC ) 
		return(M0TRUE);
	else
		return(M0FALSE);
}


#if defined(M0DEBUG)

/*
*| Name:
*|      M0VCput - Writes vector (M0VC) to stdout.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	void
*|	M0VCput(char *szDescription, M0VC VC)
*|
*| Input:
*|      szDescription   - Description of vector.
*|      VC              - Vector.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	none
*|
*| Remarks:
*|      This routine is intended for debugging and unit tests
*|      only, as it does NOT conform to McIDAS I/O standards.  It
*|	is available only when compiled with M0DEBUG defined.
*|
*| Categories: 
*|      utility
*/

void
M0VCput(char *szDescription, M0VC VC)
{
	char szType[] = "   ";

	if      ( VC.type == CC )
		strcpy ( szType, "CC" );
	else if ( VC.type == CT )
		strcpy ( szType, "CT" );
	else
		strcpy ( szType, "NIL" );
	
	printf("%s: %s %13.6lf %13.6lf %13.6lf\n",
	  szDescription, szType, VC.c[0], VC.c[1], VC.c[2] );
	
	return;
}


/*
*| Name:
*|      M0VCget - Reads vector (M0VC) from stdin.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	M0VC
*|	M0VCget(char *szDescription)
*|
*| Input:
*|      szDescription   - Description of vector.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	Vector (M0VC)
*|
*| Remarks:
*|      This routine is intended for debugging and unit tests
*|      only, as it does NOT conform to McIDAS I/O standards.  It
*|	is available only when compiled with M0DEBUG defined.
*|
*| Categories: 
*|      utility
*/

M0VC
M0VCget(char *szPrompt)
{
	char	szBuf[BUFSIZ];		/* input text buffer	*/
	char	*pch;			/* text buffer pointer	*/
	char	*pchType;		/* type pointer		*/
	M0VC	VC;			/* vector to return	*/
	int	i;			/* position element	*/

	VC.type 	= NIL;

	/* This 'while' loop will keep prompting the user for a
	 * vector until a valid result is obtained */

	while ( VC.type == NIL ) {

		/* write the prompt and read the response into
		 * the buffer */

		fprintf ( stdout, "Input %s (crd_type c1 c2 c3): ",
		  szPrompt );
		fgets ( szBuf, BUFSIZ, stdin );


		/* get first token, which is coordinate type.
		 * Convert to all uppercase and try to
		 * recognize it. If unrecognized, prompt for
		 * another try */

		pch	= strtok ( szBuf, " ," );
		for ( pchType = pch; *pchType != '\0'; pchType++ )
			*pchType = (unsigned char)toupper((int)(*pchType));

		if      ( strcmp ( pch, "CT" ) == 0 )
			VC.type = CT;
		else if ( strcmp ( pch, "CC" ) == 0 )
			VC.type = CC;
		else  {
			fprintf ( stderr, "valid coord. types "
			  "are CT and CC\n" );
			continue;
		}

		/* now read the vector components. If there
		 * aren't enough, or if they cannot be decoded,
		 * reset coordinate type to NIL, print message
		 * and ask user to re-enter the vector */

		for ( i = 0; i < 3; i++ ) {
	
			pch = strtok ( NULL, " ," );
			if ( pch == NULL ) {
				fprintf ( stderr, "must enter "
				  "three components\n" );
				VC.type = NIL;
				break;
			}
			else {
				if ( sscanf ( pch, "%lf", &(VC.c[i]) )
				 != 1 )  {
					fprintf ( stderr, "could not"
					  " decode component %d\n",i);
					VC.type = NIL;
					break;
				}
			}
		}
	}
	return (VC);
}


/*
*| Name:
*|      M0PTput - Writes position (M0PT) to stdout.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	void
*|	M0PTput(char *szDescription, M0PT PT)
*|
*| Input:
*|      szDescription   - Description of vector.
*|      PT              - Position.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	none
*|
*| Remarks:
*|      This routine is intended for debugging and unit tests
*|      only, as it does NOT conform to McIDAS I/O standards.  It
*|	is available only when compiled with M0DEBUG defined.
*|
*| Categories: 
*|      utility
*/

void
M0PTput(char *szDescription, M0PT PT)
{
	char *aszFormat[] = {
	 "pos %s LLT lon,lat,hgt = %9.4lf %9.4lf %9.2lf\n",
	 "pos %s  CT  x,  y,  z  = %9.1lf %9.1lf %9.1lf\n",
	 "pos %s ADT lon,lat,rad = %9.4lf %9.4lf %9.2lf\n",
	 "pos %s ADC lon,lat,rad = %9.4lf %9.4lf %9.2lf\n",
	 "pos %s  CC  x,  y,  z  = %9.1lf %9.1lf %9.1lf\n" };
	switch(PT.type) {
		case LLT:
		case ADT:
		case ADC:
			printf ( aszFormat[PT.type],
	  		  szDescription,
			  M0R2D*PT.c[0],
			  M0R2D*PT.c[1],
			      PT.c[2] );
			break;
		case CT:
		case CC:
			printf ( aszFormat[PT.type],
	  		  szDescription,
			      PT.c[0],
			      PT.c[1],
			      PT.c[2] );
			break;
		default:
			M0ASSERT(M0FALSE);	/* trap NIL type */
	}
	
	return;
}



/*
*| Name:
*|      M0PTget - Reads position (M0PT) from stdin.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	M0PT
*|	M0PTget(char *szDescription)
*|
*| Input:
*|      szDescription   - Description of position.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	Position (M0PT)
*|
*| Remarks:
*|      This routine is intended for debugging and unit tests
*|      only, as it does NOT conform to McIDAS I/O standards.  It
*|	is available only when compiled with M0DEBUG defined.
*|
*| Categories: 
*|      utility
*/


M0PT
M0PTget(char *szPrompt)
{
	int	i;			/* position element	*/
	char	*pch;			/* text buffer pointer	*/
	char	*pchType;		/* type pointer		*/
	M0PT	PT;			/* position to return	*/
	char	szBuf[BUFSIZ];		/* Text input buffer	*/

	PT.type 	= NIL;

	while ( PT.type == NIL ) {

		fprintf(stdout,"Input %s (crd_type c1 c2 c3): ",
		  szPrompt );

		fgets ( szBuf, BUFSIZ, stdin );
		if ( strcmp ( szBuf, "quit\n" ) == 0 ) 
			return(PT);

		/* get first token, which is coordinate type.
		 * Convert to all uppercase and try to
		 * recognize it */

		pch	= strtok ( szBuf, " ," );
		for ( pchType = pch; *pchType != '\0'; pchType++ )
			*pchType = (unsigned char)toupper((int)(*pchType));

		if      ( strcmp ( pch, "LLT" ) == 0 )
			PT.type = LLT;
		else if ( strcmp ( pch, "CT" ) == 0 )
			PT.type = CT;
		else if ( strcmp ( pch, "ADT" ) == 0 )
			PT.type = ADT;
		else if ( strcmp ( pch, "ADC" ) == 0 )
			PT.type = ADC;
		else if ( strcmp ( pch, "CC" ) == 0 )
			PT.type = CC;
		else  {
			fprintf ( stderr, "valid coord. types "
			  "are LLT CT ADT ADC CC\n" );
			continue;
		}

		/* now read the position components. If there
		 * aren't enough, or if they cannot be decoded,
		 * reset coordinate type to NIL, print message
		 * and ask user to re-enter the vector */

		for ( i = 0; i < 3; i++ ) {
	
			pch = strtok ( NULL, " ," );
			if ( pch == NULL ) {
				fprintf ( stderr, "must enter "
				  "three components\n" );
				PT.type = NIL;
				break;
			}
			else {
				if ( sscanf ( pch, "%lf", &(PT.c[i]) )
				 != 1 )  {
					fprintf ( stderr, "could not"
					  " decode component %d\n",i);
					PT.type = NIL;
					break;
				}
			}
		}

		/* Rescale from degrees to radians if the
		 * position is of type LLT, ADT, or ADC */

		if ( PT.type == LLT ||
		     PT.type == ADT ||
		     PT.type == ADC ) {

			PT.c[0] *= M0D2R;
			PT.c[1] *= M0D2R;
		}	
	}
	M0PTrange(&PT);
	return (PT);
}

#endif

#if defined (UNIT_TEST)


/* Unit test ideas:
 * 	Drive test with M0VCrot(). For each angle step, (and for 
 *	   angle since start) use M0VCangle() to verify the angle.
 *	   Also use mag(cross)/mag()mag() to get the sine of the
 *	   angle, and test cross dot both terms against zero.
 *	   Cross product can only locate angle in first quadrant,
 *	   though.
 */

#include <stdio.h>
#include "m0frame.h"
#include "m0gpnav.h"


enum {MAGENTA=1, CYAN, YELLOW, GREEN, RED, BLUE, WHITE};


/* local prototypes	*/

void
log_angle(double axis_lat, double axis_lon,
  double ptg_lat, double ptg_lon,
  double rot, double angle, double cross);

void
rottest(int handle, M0DT DTcurrent, double axis_lat, double axis_lon,
  double ptg_lat, double ptg_lon, double incRot, 
  M0flag graphics, M0flag listing, M0ST *pSTangle,
  M0ST *pSTcross);

void
usage(char *szExename);

/* vector.c unit test
 * 
 * This unit test is mainly directed at M0VCrotate() and
 * M0VCproject() as these are by far the most complicated
 * routines (invoking as they do the entire vecrot.f homogeneous
 * vector package.
 *
 * The unit test consists of two parts. In the first part,
 * two latitude-longitude pairs are read in off the command line.
 * The first is used to fix the head of a rotation axis and the
 * second, a pointing vector. The pointing vector is then rotated
 * about the axis at angles between -180 and +180 degrees, at
 * an increment also read in from the command line. If the
 * unit test is compiled with M0NVLOG defined, output will be
 * written to a file called 'vectest.log' as well; this file
 * can be converted to LW form and displayed with McIDAS keyin
 * 'nvtest.pgm.'
 *
 * The second part is nearly automatic. The user is prompted
 * for a latitude, longitude, and rotation increment (integer
 * degrees.) Every possible axis and pointing vector is then
 * generated, from -90 to +90 lat and -180 to +180 lon, and
 * the pointing vector rotated from -180 to +180, using the
 * increments specified. Cases where the two vector heads
 * are identical are excluded. The reference (rotation of 0)
 * and rotated pointing vectors are both projected into
 * the plane of rotation and the angle between them computed
 * by two methods, M0VCangle() and the cross product M0VCcross(),
 * and statistics kept on the agreement between these estimates
 * and the actual rotation.
 */

int main(int argc, char *argv[])
{
	/* local typedefs, structs, enums	*/

	/* local variables		*/

	M0DT		DTcurrent;	/* current time (to keep
					 * M0PLpt_convert happy*/
	char		szBuf[BUFSIZ];	/* text input buffer	*/
	double		incRot;		/* rotation increment	*/

	double		axis_lat;	/* axis head latitude	*/
	double		axis_lon;	/* axis head longitude	*/
	double		ptg_lat;	/* pointing head lat	*/
	double		ptg_lon;	/* pointing head lon	*/

	M0flag		graphics;	/* graphic output ?	*/
	M0flag		listing;	/* text output ?	*/

	M0ST		STangle;	/* angle stats		*/
	M0ST		STcross;	/* cross product stats	*/

	int		dlat;		/* latitude increment	*/
	int		dlon;		/* longitude increment	*/
	int		drot;		/* rotation increment	*/

	int		alat;		/* axis latitude	*/
	int		alon;		/* axis longitude	*/
	int		plat;		/* pointing lat		*/
	int		plon;		/* pointing lon		*/
	int		lon_diff;	/* alon-plon		*/
	int		handle;		/* nav instance		*/


	printf("\nBeginning vector.c unit test %s\n", argv[0]);

	if(argc < 5) {
		usage(argv[0]);
		return(-1);
	}

	/* Get current time (needed to satisfy coordinate conversion
	 * routines) and set current planet to "Earth" */

	DTcurrent	= M0DTcurrent();
	handle		= 1;
	if ( M0PLinit(handle, "earth") < 0 ) {
		fprintf(stderr,"\nCould not set planet \"earth\"\n");
		return(-2);
	}

	/* fetch command line axis head, pointing vector head,
	 * and angular increment */

	if( sscanf(argv[1], "%lf", &axis_lat) != 1 ) {
		fprintf(stderr,"\nCould not fetch axis_lat");
		return(-3);
	}
	if( sscanf(argv[2], "%lf", &axis_lon) != 1 ) {
		fprintf(stderr,"\nCould not fetch axis_lon");
		return(-3);
	}
	if( sscanf(argv[3], "%lf", &ptg_lat) != 1 ) {
		fprintf(stderr,"\nCould not fetch ptg_lat");
		return(-3);
	}
	if( sscanf(argv[4], "%lf", &ptg_lon) != 1 ) {
		fprintf(stderr,"\nCould not fetch ptg_lon");
		return(-3);
	}
	if( sscanf(argv[5], "%lf", &incRot) != 1 ) {
		fprintf(stderr,"\nCould not fetch incLon");
		return(-3);
	}


	axis_lat *= M0D2R;
	axis_lon *= M0D2R;
	ptg_lat  *= M0D2R;
	ptg_lon  *= M0D2R;
	incRot   *= M0D2R;

	/* prepare angle statistics	*/

	M0STclear(&STangle);
	M0STclear(&STcross);

	/* turn graphics switch on	*/

	graphics = M0TRUE;
	listing  = M0TRUE;

	/* Run single-case test with command line arguments
	 * for axis, pointing vector, and rotation increments */

	rottest(handle, DTcurrent, axis_lat, axis_lon, ptg_lat,
	  ptg_lon, incRot, graphics, listing, &STangle, &STcross);

	/* Display the statistics for the single-case test */

	M0STshow(&STangle, "angle-fabs(rot) <degrees>", stdout);
	M0STshow(&STcross, "cross-fabs(rot) <degrees>", stdout);


	/* Second part of unit test begins here. Prompt user
	 * for latitude, longitude, and rotation increments. */

	M0STclear(&STangle);
	M0STclear(&STcross);

	fprintf(stderr,"\nBeginning exhaustive test");
	fprintf(stderr,"\nEnter lat, lon, rotation increments "
	  "in whole degrees: ");
	fgets(szBuf, BUFSIZ, stdin);

	/* Over-write the trailing newline and then pick
	 * out the increments */

	szBuf [ strlen(szBuf)-1 ] = '\0';
	sscanf( szBuf, "%d %d %d", &dlat, &dlon, &drot);

	graphics = M0FALSE;
	listing  = M0FALSE;

	for( alat= -90; alat<= 90; alat+= dlat) {
	for( alon=-180; alon<=180; alon+= dlon) {

	for( plat= -90; plat<= 90; plat+= dlat) {
	for( plon=-180; plon<=180; plon+= dlon) {

		/* skip identical or complementary
		 * vectors */

		if( (alat==plat) && (alon==plon) ) {
			continue;
		}
		lon_diff = alon-plon;
		lon_diff = lon_diff < 0 ? lon_diff+360 : lon_diff;

		if( (alat==-plat) && (lon_diff==180) ) {
			continue;
		}

		axis_lat = alat*M0D2R;
		axis_lon = alon*M0D2R;
		ptg_lat	 = plat*M0D2R;
		ptg_lon  = plon*M0D2R;

		incRot	 = drot*M0D2R;

		rottest(handle, DTcurrent, axis_lat, axis_lon,
		  ptg_lat, ptg_lon, incRot, graphics, listing,
		  &STangle, &STcross);


	} }	/* end of nested pointing vector loops	*/
	} }	/* end of nested axis lat/lon loops	*/

	M0STshow(&STangle, "M0VCangle() <degrees>", stdout);
	M0STshow(&STcross, "Cross Product <degrees>", stdout);

	printf("vector.c unit test %s done!\n\n", argv[0]);
	return 0;
}


void
usage(char *szExename)
{
	fprintf(stderr,"\n\n");
	fprintf(stderr,"Usage: %s <axis> <ptg_ref> <step>\n\n",
	  szExename);
	fprintf(stderr,"       <axis>    = axis head lat, lon\n");
	fprintf(stderr,"       <ptg_ref> = ptg vector lat, lon\n");
	fprintf(stderr,"       <step>    = angular step to "
	  "rotate vector\n");
	fprintf(stderr,"\n");
	fprintf(stderr,"The unit test rotates the pointing vector "
	  "about the axis by multiples\n");
 	fprintf(stderr, "of the angular step. It then projects "
	  "the original and rotated pointing\n");
	fprintf(stderr, "vectors into the axis normal plane and "
	  "computes the angle between them\n");
	fprintf(stderr, "using both M0VCangle() (dot produce) and "
	  "the cross product and compares\n");
	fprintf(stderr, "these computed angles to the actual rotation "
	  "angle to get statistics\n");
	fprintf(stderr, "on the accuracy of the rotation and angular "
	  "difference measurements.\n");
	fprintf(stderr, "\nAll statistics are in degrees.\n\n");
	fprintf(stderr,"\n\n");
}


void
log_angle(double axis_lat, double axis_lon,
  double ptg_lat, double ptg_lon,
  double rot, double angle, double cross)
{
	printf("axis %5.1f %6.1f ptg %5.1f %6.1f rot=%6.1f "
	  "ang=%8.3f cross=%8.3f\n",
	  axis_lat/M0D2R, axis_lon/M0D2R, ptg_lat/M0D2R, ptg_lon/M0D2R,
	  fabs(rot)/M0D2R, angle/M0D2R, cross/M0D2R);
	return;
}

void
rottest(int handle, M0DT DTcurrent, double axis_lat, double axis_lon,
  double ptg_lat, double ptg_lon, double incRot, 
  M0flag graphics, M0flag listing, M0ST *pSTangle, M0ST *pSTcross)
{
	char		szTitle[BUFSIZ];/* graphics log title	*/
	double		mnRot;		/* rotation start angle	*/
	double		mxRot;		/* rotation end   angle	*/
	M0PT		PTaxis;		/* axis vector head	*/
	M0VC		VCaxis;		/* axis vector		*/

	M0PT		PTorigin;	/* coordinate origin	*/

	M0PT		PTptg_ref;	/* pointing vector head	*/
	M0VC		VCptg;		/* pointing vector 	*/
	M0VC		VCptg_ref;	/* pointing vector
					 * reference location
					 * (zero rotation)	*/
	M0VC		VCptg_ref_proj;	/* pointing vector
					 * reference projected
					 * into rotation plane	*/
	M0VC		VCptg_proj;	/* pointing vector pro-
					 * jected into rotation 
					 * plane		*/	
	M0VC		VCcross;	/* cross product of
					 * pointing and ref
					 * vector projections	*/
	double		rot;		/* rotation angle 	*/
	double		angle;		/* rotation angle
					 * computed from data	*/
	double		cross;		/* rotation angle
					 * computed from cross
					 * product		*/
	double		proj_mag;	/* length of projection
					 * of pointing vector	*/
	double		ref_mag;	/* length of projection
					 * of reference pointing
					 * vector		*/
	int		color;		/* graphic color	*/


	mnRot = -180.*M0D2R;
	mxRot =  180.*M0D2R;

	sprintf(szTitle,"axis %5.1f %6.1f, "
	  "ptg %5.1f %6.1f (lat/lon)",
	  axis_lat/M0D2R, axis_lon/M0D2R,
	  ptg_lat/M0D2R, ptg_lon/M0D2R);

	if(listing) {
		puts(szTitle);
	}

	/* Prepare the rotation axis and pointing vector */

	PTaxis = M0PTmake(axis_lon, axis_lat,
	  0., LLT);
	PTaxis = M0PLpt_convert(handle, PTaxis, DTcurrent, CT);
	VCaxis = M0PTvc(PTaxis);

	PTptg_ref = M0PTmake(ptg_lon, ptg_lat, 0., LLT);
	PTptg_ref = M0PLpt_convert( handle, PTptg_ref, DTcurrent, CT);
	VCptg_ref = M0PTvc(PTptg_ref);

	VCptg_ref_proj = M0VCproject(VCaxis, VCptg_ref);

	PTorigin = M0PTmake(0., 0., 0., CT);

#ifdef M0NVLOG

	/* Open the graphics log and put the rotation
	 * axis, the reference pointing vector, and its
	 * projection into the rotation plane into the
	 * log */

	 if(graphics) {

		M0NLopen(handle, "vectest.log", szTitle);
		color = RED;
		M0NLvc(handle, color, VCaxis, PTorigin);
		color = GREEN;
		M0NLvc(handle, color, VCptg_ref, PTorigin);
		M0NLvc(handle, color, VCptg_ref_proj, PTorigin);
	}
#endif

	/* Now ready to do pointing vector rotation */

	for(rot = mnRot; rot <= mxRot; rot += incRot) {

		/* Compute the rotated pointing vector
		 * and its projection */

		VCptg = M0VCrotate(VCaxis, rot, VCptg_ref);
		VCptg_proj = M0VCproject(VCaxis, VCptg);

		/* Compute the angle between the rotated
		 * and reference vectors projected into
		 * the plane, using both M0VCangle() and
		 * the cross-product methos. Write the
		 * results to stdout, and to the
		 * graphics log if so requested */
		
		if(M0VCzero(VCptg_ref_proj) ||
		  M0VCzero(VCptg_proj) ) {
#ifdef UNDEF
			fprintf(stderr,"zero vector\n");
#endif
			continue;
		}

		angle = M0VCangle(VCptg_ref_proj, VCptg_proj);

		VCcross  = M0VCcross(VCptg_proj, VCptg_ref_proj);
		proj_mag = M0VCmag(VCptg_proj);
		ref_mag  = M0VCmag(VCptg_ref_proj);
		cross	 = M0VCmag(VCcross) / (proj_mag*ref_mag);
		if ( fabs(cross) > 1. ) {
#ifdef UNDEF
			fprintf(stderr,"cross = %15.12f\n", cross);
#endif
			cross = cross > 0. ? 1. : -1.;
		}
		/* dropped directionality on cross product
		 * means that angles are always in first quadrant.
		 * put 'cross' angle estimate in correct quadrant
		 * based on size of 'rot' */

		cross	 = asin(cross);
		cross	 = fabs(rot) > M0PI/2. ? M0PI-cross : cross;

		if(listing) {
			log_angle(axis_lat, axis_lon,
			  ptg_lat, ptg_lon, rot, angle, cross);
		}
#ifdef M0NVLOG
		if(graphics) {
			color = WHITE;
			M0NLvc(handle, color, VCptg, PTorigin);
			color = BLUE;
			M0NLvc(handle, color, VCptg_proj, PTorigin);
		}	
	#endif
		M0STupdate(pSTangle, (angle-fabs(rot))/M0D2R);
		M0STupdate(pSTcross, (cross-fabs(rot))/M0D2R);
	}
	return;
}
#endif
