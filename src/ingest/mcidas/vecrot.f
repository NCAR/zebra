C THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED.

C *** McIDAS Revision History ***
C 1 VECROT.FOR 27-Feb-96,13:17:22,`ROBERTM' initial checkin of DMSP nav
C 2 VECROT.FOR 17-Apr-96,14:54:44,`USER' Released
C *** McIDAS Revision History ***



C Change log:
C 95.10.20 rtm	Add documentation blocks to m0vcpro, m0vcrot, and
C		supporting routines and bring to core standards.
C 95.01.05 rtm  Convert input and output to radians throughout
C 95.01.04 rtm	Port to 'hawkeye' and convert to double precision
C		throughout

*| Name:
*|	m0vcpro - compute projection of vector into plane
*|
*| Interface:
*|	subroutine
*|	m0vcpro(double precision vcnorm(3), double precision vc(3),
*|	  double precision vcpro(3) )
*|
*| Input:
*|	vcnorm	- vector normal to projection plane
*|	vc	- input vector
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	vcpro	- projection of v
*|
*| Return values:
*|	none
*|
*| Remarks:
*|	    The algorithm is as follows; rotate about X axis to
*|	bring 'vcnorm' into the X-Z plane, thence about Y to
*|	make 'vcnorm' coincident with Z. Zero the Z component
*|	of 'vc,' and then reverse the transformations to restore
*|	'vcnorm' to its original orientation.
*|	    The computations are made with 4-d (homogeneous) vectors;
*|	input vectors are 3-d. This method can also support
*|	translation, scaling, and perspective transformations as
*|	well as rotation. See Foley, van Dam, Feiner, and Hughes
*|	"Computer Graphics: Principles and Practice" Ch. 5 for a
*|	description of the homogeneous transform method.
*|
*| Categories: 
*|	vector operations


	subroutine m0vcpro ( vcnorm, vc, vcpro )

	implicit		NONE
 

C	// Symbolic constants and shared data

	double precision	vc(3)		! vector to project
	double precision	vcnorm(3)	! normal to plane
	double precision	vcpro(3)	! projected vector


C	// External functions

	logical			lvceq		! are vectors equal ?
	double precision	vcmag		! compute vector length


C	// Local variables

	double precision	alpha		! rotation about X
	double precision	beta		! rotation about Y
	double precision	cnx		! X direction cosine
	double precision	cny		! Y direction cosine
	double precision	cnz		! Z direction cosine
	double precision	dnorm		! length of 'vcnorm'
	double precision	dnyz		! length of 'vcnorm'
C						! unit vector projected
C						! into Y-Z plane
	double precision	ftran4(4,4)	! forward transform
	double precision	itran4(4,4)	! inverse transform
	double precision	vch(4)		! vector to project
C						! (homogeneous form)
	double precision	vcht(4)		! intermediate (transformed)
C						! homogeneous vector
	double precision	vcproh(4)	! projected vector
C						! (homogeneous form)
	double precision	xr(4,4)		! rotation matrix
C						! about X axis
	double precision	yr(4,4)		! rotation matrix
C						! about Y axis


C	// Initialized variables

	double precision	vcold(3)	! normal to plane for
C						! current transform matrix
	data vcold/0.d0, 0.d0, 0.d0/


C	//////////////////////////////////
C	//				//
C	//    INITIALIZE TRANSFORM	//
C	//				//
C	//////////////////////////////////
 
C	// Re-initialize the transform matrix on first call, or if
C	// the plane normal 'vcnorm' has changed since the last
C	// initialization

	if ( .not.lvceq(vcnorm, vcold) ) then
 
	    call vccopy(vcnorm, vcold)
 
C	    // compute direction cosines of 'vcnorm' and length
C	    // of projection of 'vcnorm' unit vector into Y-Z plane

	    dnorm = vcmag(vcnorm)
	    cnx   = vcnorm(1) / dnorm
	    cny   = vcnorm(2) / dnorm
	    cnz   = vcnorm(3) / dnorm
	    dnyz  = dsqrt(cny*cny+cnz*cnz)

C	    // Compute 'alpha,' the rotation about the X axis that will
C	    // place 'vcnorm' in the X-Z plane.  Compute 'beta' that will
C	    // then rotate it about Y axis to align it with the Z axis.
 
	    if ( dnyz.gt.0.d0 ) then
		alpha = datan2(cny,cnz)
	    else
		alpha = 0.d0
	    end if
            beta = - datan2(cnx,dnyz)
 
C	    // now build up forward and inverse transform matrices
C	    // (these align 'vcnorm' with the Z axis and then restore it
C	    // to its original orientation)./ Rotation matrices xr()
C	    // and yr() to do rotations about X and Y axes are computed
C	    // separately and then multiplied together to make a one-step
C	    // transform matrix.
 
	    call xrot4(alpha, xr)
	    call yrot4(beta,  yr)
	    call m4mul(yr, xr, ftran4)

	    call yrot4( -beta,  yr)
	    call xrot4( -alpha, xr)
	    call m4mul(xr,yr,itran4)
	
	end if 

C	//////////////////////////////////
C	//				//
C	//    APPLY TRANSFORM		//
C	//				//
C	//////////////////////////////////
 

C	// The projection is actually done here:
C	//   1) convert input vector to homogeneous form (4-d)
C	//   2) apply forward transform
C	//   3) throw away Z component
C	//   4) apply inverse transform
C	//   5) restore to physical (3-d) form for output

	call vctovh(vc, vch)
	call vhtran(ftran4, vch, vcht)
	vcht(3) = 0.d0
	call vhtran(itran4, vcht, vcproh)
	call vhtovc(vcproh, vcpro)

	return
	end
 

*| Name:
*|	m0vcrot - rotate a vector about an arbitrary axis
*|
*| Interface:
*|	subroutine
*|	m0vcrot(double precision vcaxis(3), double precision angle,
*|	  double precision vc(3), double precision vcrot(3) )
*|
*| Input:
*|	vcaxis	- vector axis of rotation
*|	angle	- angle of rotation
*|	vc	- input vector
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	vcrot	- vector after rotation
*|
*| Return values:
*|	none
*|
*| Remarks:
*|	    The rotation is right-handed, in radians. The
*|	transformation works by rotating 'axis' and 'vc'
*|	about the X and Y axes until parallel to Z, rotating
*|	'vc' about the Z axis by 'angle,' and then reversing
*|	the rotations about Y and X to return 'axis' to its
*|	original orientation. All of these transformations are
*|	combined together in a single 4x4 matrix.
*|	    The computations are made with 4-d (homogeneous) vectors;
*|	input vectors are 3-d. This method can also support
*|	translation, scaling, and perspective transformations as
*|	well as rotation. See Foley, van Dam, Feiner, and Hughes
*|	"Computer Graphics: Principles and Practice" Ch. 5 for a
*|	description of the homogeneous transform method.
*|
*| Categories: 
*|	vector operations

	subroutine m0vcrot(vcaxis, angle, vc, vcrot)

	implicit	NONE

C	// Symbolic constants and shared data

	double precision	angle		! rotation angle
	double precision	vc(3)		! vector to rotate
	double precision	vcaxis(3)	! rotation axis
	double precision	vcrot(3)	! vector after rotation

C	// External functions

	logical			lvceq		! are vectors equal ?
	double precision	vcmag		! compute length of vector


C	// Local variables

	double precision	vch(4)		! input vector in
C						! homogeneous form
	double precision	vcroth(4)	! rotated vector
C						! in homogeneous form
	double precision	vcaxold(3)	! axis of previous
C						! transform
	double precision	xr(4,4)		! matrix for rotation
C						! about X axis
	double precision	yr(4,4)		! matrix for rotation
C						! about Y axis
	double precision	zr(4,4)		! matrix for rotation
C						! about Z axis
	double precision	work4(4,4)	! temporary homogeneous
C						! vector transform
 	double precision	tran4(4,4)	! one-step transform
C						! matrix
	double precision	alpha		! rotation about X axis
	double precision	beta		! rotation about Y axis
	double precision	cax		! X direction cosine
	double precision	cay		! Y direction cosine
	double precision	caz		! Z direction cosine
	double precision	daxis		! 'axis' magnitude
	double precision	dayz		! length of projection
C						! of 'axis' in Y-Z plane

	




C	// Initialized variables

	double precision	angold		! angle of rotation for
C						! previous transform

	data angold/-99999.d0/
	

C	//////////////////////////////////
C	//				//
C	//    INITIALIZE TRANSFORM	//
C	//				//
C	//////////////////////////////////
 
C	// Initialization: if axis of rotation or rotation angle
C	// have changed (or on first call), recompute the transform
C	// matrix. 

	if(.not.lvceq(vcaxis, vcaxold) .or. angle.ne.angold ) then
	    angold = angle
	    call vccopy(vcaxis, vcaxold)

C	    // compute direction cosines for 'vcaxis' and length of
C	    // its unit vector projection into Y-Z plane

	    daxis = vcmag(vcaxis)
	    cax   = vcaxis(1) / daxis
	    cay   = vcaxis(2) / daxis
	    caz   = vcaxis(3) / daxis
	    dayz  = dsqrt(cay*cay + caz*caz)

C	    // Compute 'alpha,' the rotation about X axis that will
C	    // place 'vcaxis' in the X-Z plane.  Compute 'beta' that
C	    // will then rotate it about Y axis to align it with
C	    // the Z axis
 
	    if( dayz.gt.0.d0 ) then
		alpha = datan2(cay, caz)
	    else
		alpha = 0.d0
            end if
	    beta = -datan2(cax, dayz)

C	    // now build up transform matrix by computing matrices
C	    // for each coordinate axis rotation and multiplying them.
C	    // Rotate about X by 'alpha,' about Y by 'beta,' about Z by
C	    // 'angle,' about Y by '-beta,' and about X by '-alpha.'
 
	    call xrot4( alpha, xr )
	    call yrot4( beta,  yr )
	    call m4mul( yr, xr, work4 )

	    call zrot4( angle, zr )
	    call m4mul( zr, work4, tran4 )

	    call yrot4(-beta,  yr )
	    call m4mul( yr, tran4, work4 )

	    call xrot4(-alpha, xr )
	    call m4mul( xr, work4, tran4 )

	end if


C	//////////////////////////////////
C	//				//
C	//    APPLY TRANSFORM		//
C	//				//
C	//////////////////////////////////
 
C	// apply the transform. It takes only three steps:
C	//   1) convert input vector to homogeneous form
C	//   2) pre-multiply with the 'one-step' transform matrix
C	//   3) convert back to physical form for output
 
	call vctovh(vc, vch)
	call vhtran(tran4, vch, vcroth)
	call vhtovc(vcroth, vcrot)

	return
	end
 
 
*| Name:
*|	lvceq - Compare two 3-d vectors
*|
*| Interface:
*|	logical function
*|	lvceq(double precision vca(3), double precision vcb(3) )
*|
*| Input:
*|	vca	- a 3-d vector
*|	vcb	- another 3-d vector
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	.TRUE. if vectors are identical, otherwise .FALSE.
*|
*| Remarks:
*|	    The vectors must agree exactly, element-by-element,
*|	to test .TRUE.. The test is designed to detect copies,
*|	not 'nearly' equal results of calculations.
*|
*| Categories: 
*|	vector operations (private)
 

	logical function lvceq(vca, vcb)

	implicit		NONE

C	// Symbolic constants and shared data

	double precision	vca(3)	! A vector
	double precision	vcb(3)	! Another vector

C	// Local variables

	integer			i	! Vector element index


C	// Set .FALSE. and return as soon as a mismatch between
C	// vector elements is found; no further comparisons are needed

	do i = 1,3
            if( vca(i).ne.vcb(i) ) then
	       lvceq = .FALSE.
	       return
	    end if
	end do

	lvceq = .TRUE.

	return
	end
 

*| Name:
*|	m4mul - Multiply two 4x4 matrices
*|
*| Interface:
*|	subroutine
*|	m4mul(a, b, c)
*|
*| Input:
*|	a	- a 4x4 matrix
*|	b	- another 4x4 matrix
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	c	- a x b (4x4 matrix product)
*|
*| Return values:
*|	none
*|
*| Remarks:
*|	None.
*|
*| Categories: 
*|	vector operations (private)
 

	subroutine m4mul(a, b, c)

	implicit		NONE

C	// Symbolic constants and shared data

	double precision	a(4,4)
	double precision	b(4,4)
	double precision	c(4,4)

C	// Local variables

	integer			i, j, k	!  matrix element indices


C	// Nothing fancy, just a multiplication of each row of
C	// a() by each column of b() to produce the elements of c().

	do i = 1, 4
	    do j = 1, 4
		c(i,j) = 0.d0
		do k = 1, 4
		    c(i,j) = c(i,j) + a(i,k)*b(k,j)
		end do
	    end do
	end do

	return
	end

 
*| Name:
*|	vhtovc - Convert a homogeneous vector to a physical vector
*|
*| Interface:
*|	subroutine
*|	vhtovc(double precision vh(4), double precision vc(3) )
*|
*| Input:
*|	vh	- homogeneous vector
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	vc	- physical (3-d) vector
*|
*| Return values:
*|	none
*|
*| Remarks:
*|	    The physical representation of homogeneous vector
*|	(x, y, z, h) is (x/h, y/h, z/h). If h is zero, this implies
*|	a bug in the caller. Application of linear transforms to
*|	a properly created (use vctovh() ) homogeneous vector
*|	(x, y, z, 1) should never result in h=0. This condition
*|	is therefore not handled.
*|
*| Categories: 
*|	vector operations (private)

	subroutine vhtovc(vh, vc)
 
	implicit		NONE

C	// Symbolic constants and shared data

	double precision	vh(4)	! input homogeneous vector
	double precision	vc(3)	! output physical vector

	vc(1) = vh(1) / vh(4)
	vc(2) = vh(2) / vh(4)
	vc(3) = vh(3) / vh(4)

	return
	end
 
*| Name:
*|	vctovh - Convert a physical vector to a homogeneous vector
*|
*| Interface:
*|	subroutine
*|	vctovh(double precision vc(3), double precision vh(4) )
*|
*| Input:
*|	vc	- physical (3-d) vector
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	vh	- homogeneous (4-d) vector
*|
*| Return values:
*|	none
*|
*| Remarks:
*|	    The homogeneous representation of physical vector
*|	(x, y, z) is (x, y, z, 1.d0).
*|
*| Categories: 
*|	vector operations (private)

 
	subroutine vctovh(vc, vh)

	implicit		NONE

C	// Symbolic constants and shared data

	double precision	vc(3)	! input physical vector
	double precision	vh(4)	! output homogeneous vector

	vh(1) = vc(1)
	vh(2) = vc(2)
	vh(3) = vc(3)
	vh(4) = 1.d0

	return
	end
 

*| Name:
*|	vccopy - Make a copy of a physical (3-d) vector 
*|
*| Interface:
*|	subroutine
*|	vccopy(double precision vcold(3), double precision vcnew(3) )
*|
*| Input:
*|	vcold	- input physical (3-d) vector
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	vcnew	- output physical (3-d) vector
*|
*| Return values:
*|	none
*|
*| Remarks:
*|	None.
*|
*| Categories: 
*|	vector operations (private)

 
	subroutine vccopy(vcold, vcnew)

	implicit		NONE

C	// Symbolic constants and shared data

	double precision	vcold(3)	! input  vector
	double precision	vcnew(3)	! output vector

C	// Local variables

	integer			i		! vector element

	do i = 1, 3
	    vcnew(i) = vcold(i)
	end do

	return
	end
 
*| Name:
*|	vcmag - Compute the length (magnitude) of a physical vector
*|
*| Interface:
*|	double precision
*|	vcmag(double precision vc(3) )
*|
*| Input:
*|	vc	- input physical (3-d) vector
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	vector length
*|
*| Remarks:
*|	None.
*|
*| Categories: 
*|	vector operations (private)

 
	double precision function vcmag(vc)

	implicit		NONE

C	// Symbolic constants and shared data

	double precision	vc(3)	! input  vector

	vcmag = dsqrt(vc(1)*vc(1) + vc(2)*vc(2) + vc(3)*vc(3) )

	return
	end
 



*| Name:
*|	vhtran - Apply linear transformation to homogeneous vector
*|
*| Interface:
*|	subroutine
*|	vhtran(double precision tm(4,4), double precision vh(4),
*|	  double precision vht(4) )
*|
*| Input:
*|	tm	- linear transformation matrix
*|	vh	- homogeneous vector to transform
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	vht	- transformed homogeneous vector
*|
*| Return values:
*|	none
*|
*| Remarks:
*|	    Linear transform matrices to perform rotations about
*|	coordinate axes can be prepared using xrot4(), yrot4(), and
*|	zrot4(). The homogeneous transform method can also support
*|	translation, scaling, and perspective transformations as
*|	well as rotation. See Foley, van Dam, Feiner, and Hughes
*|	"Computer Graphics: Principles and Practice" Ch. 5 for a
*|	description of this method.
*|
*| Categories: 
*|	vector operations (private)

	subroutine vhtran(tm, vh, vht)

	implicit		NONE

C	// Symbolic constants and shared data

	double precision	tm(4,4)	! transform matrix
	double precision	vh(4)	! input homogeneous vector
	double precision	vht(4)	! transformed homogeneous vector

C	// Local variables

	integer			i, j	! matrix element indices


C	// No tricks; just a multiplication of each row of tm()
C	// with vh() to produce the output elements (4) of vht().

	do i = 1, 4
	    vht(i) = 0.d0
	    do j = 1, 4
		vht(i) = vht(i) + vh(j)*tm(i,j)
	    end do
	end do

	return
	end
 


*| Name:
*|	xrot4 - Compute transform matrix for rotation about X axis
*|
*| Interface:
*|	subroutine
*|	xrot4(double precision alpha, double precision xr(4,4))
*|
*| Input:
*|	alpha	- angle of rotation
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	xr	- homogeneous rotation matrix
*|
*| Return values:
*|	none
*|
*| Remarks:
*|	    The matrix 'xr,' when pre-multiplied to a vector
*|	'vc,' will rotate 'vc' through 'alpha' radians about
*|	the X axis.
*|
*| Categories: 
*|	vector operations (private)
 

	subroutine xrot4(alpha, xr)

	implicit		NONE

C	// Symbolic constants and shared data

	double precision	alpha		! rotation angle
	double precision	xr(4,4)		! transform matrix

C	// Local variables

	double precision	cosa		! cosine(alpha)
	double precision	sina		! sine(alpha)


C	// It's all done with sines, cosines, ones, and zeroes
C	// in the right places

	cosa	= dcos(alpha)
	sina	= dsin(alpha)
  
 
C	// first column
 
	xr(1,1) = 1.d0
	xr(2,1) = 0.d0
	xr(3,1) = 0.d0
	xr(4,1) = 0.d0
 
C	// second column
 
	xr(1,2) = 0.d0
	xr(2,2) = cosa
	xr(3,2) = sina
	xr(4,2) = 0.d0
 
C	// third column
 
	xr(1,3) = 0.d0
	xr(2,3) =-sina
	xr(3,3) = cosa
	xr(4,3) = 0.d0
 
C	// fourth column
 
	xr(1,4) = 0.d0
	xr(2,4) = 0.d0
	xr(3,4) = 0.d0
	xr(4,4) = 1.d0
 
	return
	end
 

*| Name:
*|	yrot4 - Compute transform matrix for rotation about Y axis
*|
*| Interface:
*|	subroutine
*|	yrot4(double precision beta, double precision yr(4,4))
*|
*| Input:
*|	beta	- angle of rotation
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	yr	- homogeneous rotation matrix
*|
*| Return values:
*|	none
*|
*| Remarks:
*|	    The matrix 'yr,' when pre-multiplied with a vector
*|	'vc,' will rotate 'vc' through 'beta' radians about
*|	the Y axis.
*|
*| Categories: 
*|	vector operations (private)
 

	subroutine yrot4(beta, yr)
 
	implicit		NONE

C	// Symbolic constants and shared data

	double precision	beta		! rotation angle
	double precision	yr(4,4)		! transform matrix

C	// Local variables

	double precision	cosb		! cosine(beta)
	double precision	sinb		! sine(beta)


C	// It's all done with sines, cosines, ones, and zeroes
C	// in the right places

	cosb = dcos(beta)
	sinb = dsin(beta)
 
C	// first column
 
	yr(1,1) = cosb
	yr(2,1) = 0.d0
	yr(3,1) =-sinb
	yr(4,1) = 0.d0
 
C	// second column
 
	yr(1,2) = 0.d0
	yr(2,2) = 1.d0
	yr(3,2) = 0.d0
	yr(4,2) = 0.d0
 
C	// third column
 
	yr(1,3) = sinb
	yr(2,3) = 0.d0
	yr(3,3) = cosb
	yr(4,3) = 0.d0
 
C	// fourth column
 
	yr(1,4) = 0.d0
	yr(2,4) = 0.d0
	yr(3,4) = 0.d0
	yr(4,4) = 1.d0
 
	return
	end
 
*| Name:
*|	zrot4 - Compute transform matrix for rotation about Z axis
*|
*| Interface:
*|	subroutine
*|	zrot4(double precision gamma, double precision zr(4,4))
*|
*| Input:
*|	gamma	- angle of rotation
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	zr	- homogeneous rotation matrix
*|
*| Return values:
*|	none
*|
*| Remarks:
*|	    The matrix 'zr,' when pre-multiplied with a vector
*|	'vc,' will rotate 'vc' through 'gamma' radians about
*|	the Z axis.
*|
*| Categories: 
*|	vector operations (private)
 

	subroutine zrot4(gamma, zr)
 
	implicit		NONE

C	// Symbolic constants and shared data

	double precision	gamma		! rotation angle
	double precision	zr(4,4)		! transform matrix

C	// Local variables

	double precision	cosg		! cosine(gamma)
	double precision	sing		! sine(gamma)

 
C	// It's all done with sines, cosines, ones, and zeroes
C	// in the right places

	cosg = dcos (gamma)
	sing = dsin (gamma)
 
C	// first column
 
	zr(1,1) = cosg
	zr(2,1) = sing
	zr(3,1) = 0.d0
	zr(4,1) = 0.d0
 
C	// second column
 
	zr(1,2) =-sing
	zr(2,2) = cosg
	zr(3,2) = 0.d0
	zr(4,2) = 0.d0
 
C	// third column
 
	zr(1,3) = 0.d0
	zr(2,3) = 0.d0
	zr(3,3) = 1.d0
	zr(4,3) = 0.d0
 
C	// fourth column
 
	zr(1,4) = 0.d0
	zr(2,4) = 0.d0
	zr(3,4) = 0.d0
	zr(4,4) = 1.d0
 
	return
	end

