/*
 * User coordinate <--> pixel coordinate conversion macros
 *
 * The coordinates are defined as shown in the diagram below:
 *
 *		0		      Gwidth
 *		|_______________________|
 *	      0-|	       (fx1,fy1)|
 *		|	 __________/	|
 *		|	|	  /|	|
 *		|	| (ux1,uy1)|	|
 *		|	|	   |	|
 *		|	|	   |	|
 *		|	|	   |	|
 *		|	|(ux0,uy0) |	|
 *		|	|/_________|	|
 *		|	/		|
 *		|  (fx0,fy0)		|
 *		|			|
 *		|			|
 *		|			|
 *      Gheight-|_______________________|
 *
 *
 *
 *	The larger box is the X drawable being used and is defined
 *	by its width and height in pixels (pixwidth and pixheight).
 *	In X pixel coordinates, the origin is at the UPPER LEFT
 *	corner.
 *
 *	The smaller box is represented by two sets of coordinates:
 *	
 *	The "f" coordinates are fractional values representing the 
 *	subwindow relative to the entire drawable.  The "f" coordinates 
 *	range from (0.0,0.0) at the LOWER LEFT corner to (1.0,1.0) at 
 *	the upper right corner of the drawable.  Note that the origin 
 *	is different from the origin of the X pixel coordinates.  In 
 *	the diagram above, (fx0,fy0) would be about (0.3,0.4) and 
 *	(fx1,fy1) would be about (0.8,0.9).
 *
 *	The "u" coordinates are the user coordinates to use for
 *	the subwindow.  The user coordinates of the lower left
 *	corner are (ux0,uy0) and the user coordinates of the upper
 *	right corner are (ux1,uy1).
 */


/*
 * Macros to convert user coordinates into pixel locations
 */
# define F_X0 0.05
# define F_X1 0.85
# define F_Y0 0.05
# define F_Y1 0.85

# define XPIX(ux)	(short)(0.5 + (float)(GWWidth (Graphics)) * \
	(((ux) - Xlo) / (Xhi - Xlo) * (F_X1 - F_X0) + F_X0))

# define YPIX(uy)	(short)(0.5 + (float)(GWHeight (Graphics)) * \
	(1.0 - (((uy) - Ylo) / (Yhi - Ylo) * (F_Y1 - F_Y0) + F_Y0)))
