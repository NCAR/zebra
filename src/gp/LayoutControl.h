/*		Copyright (C) 1987,88,89,90,91 by UCAR
 *	University Corporation for Atmospheric Research
 *		   All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced
 * or used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular purpose.
 * UCAR does not indemnify any infringement of copyright, patent, or trademark
 * through use or modification of this software.  UCAR does not provide 
 * maintenance or updates for its software.
 */
/*
 * User coordinate <--> pixel coordinate conversion macros
 *
 * The coordinates are defined as shown in the diagram below:
 *
 *		0		      Gwidth
 *	       _|_______________________|
 *	      0 |	       (fx1,fy1)|
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
 *		|_______________________|
 *		|			| \_ ICONSPACE buffer for icons
 *      Gheight_|_______________________| /
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
 *	subwindow relative to the entire drawable (less the lowest 
 *	ICONSPACE pixels, which are reserved for putting up icons).  
 *	The "f" coordinates range from (0.0,0.0) at the LOWER LEFT 
 *	corner to (1.0,1.0) at the upper right corner of the drawable.  
 *	Note that the origin is different from the origin of the X 
 *	pixel coordinates.  In the diagram above, (fx0,fy0) would be 
 *	about (0.3,0.4) and (fx1,fy1) would be about (0.8,0.9).
 *
 *	The "u" coordinates are the user coordinates to use for
 *	the subwindow.  The user coordinates of the lower left
 *	corner are (ux0,uy0) and the user coordinates of the upper
 *	right corner are (ux1,uy1).
 */


/*
 defined in "PixelCoords.h"
# define ICONSPACE	50
*/
# define AXIS_BOTTOM    0
# define AXIS_LEFT      1
# define AXIS_TOP       2
# define AXIS_RIGHT     3
# define F_PIX_HEIGHT   ((int)((FY1-FY0)*GWHeight(Graphics)))
# define F_PIX_WIDTH    ((int)((FX1-FX0)*GWWidth(Graphics)))

extern float   UX0, UY0, UX1, UY1;
extern float   FX0, FY0, FX1, FY1;
float   AxisX0[4],AxisX1[4],AxisY0[4],AxisY1[4];
float   IconX0,IconX1,IconY0,IconY1;
float   AnnotateX0,AnnotateX1,AnnotateY0,AnnotateY1;
float   LegendX0,LegendX1,LegendY0,LegendY1;
/*
 * User coordinate to pixel coordinate macros
 */
# define LC_FXPIX(ux)      (int)((short)(0.5 + (float)(GWWidth (Graphics)) * \
        (FX1 - (((ux) - UX0) / (UX1 - UX0) * (FX1 - FX0)))))

# define LC_XPIX(ux)       (int)((short)(0.5 + (float)(GWWidth (Graphics)) * \
        (((ux) - UX0) / (UX1 - UX0) * (FX1 - FX0) + FX0)))

# define LC_FYPIX(uy)       (int)((GWHeight(Graphics) - \
			(FY1 * GWHeight(Graphics))) +\
        (int)(( (((uy) - UY0) / (UY1 - UY0)))* F_PIX_HEIGHT))

# define LC_YPIX(uy)       (int)((GWHeight(Graphics) - \
			(FY1 * GWHeight(Graphics))) +\
        (int)((1.0 - (((uy) - UY0) / (UY1 - UY0)))* F_PIX_HEIGHT))

/*
 * Pixel coordinate to user coordinate macros
 */
# define LC_FXUSER(xp)      (UX1 - (((xp)/(float)GWWidth(Graphics)) - FX0) * \
                                (UX1 - UX0)/(FX1 - FX0) )
# define LC_XUSER(xp)      ((((xp)/(float)GWWidth(Graphics)) - FX0) * \
                                (UX1 - UX0)/(FX1 - FX0) + UX0)

# define LC_FYUSER(yp)      UY1 - (UY1-UY0) * (1.0 - \
	((float)(yp - (GWHeight(Graphics) - (FY1*GWHeight(Graphics))))/(float)(F_PIX_HEIGHT))) 
# define LC_YUSER(yp)      UY0 + (UY1-UY0) * (1.0 - \
	((float)(yp - (GWHeight(Graphics) - (FY1*GWHeight(Graphics))))/(float)(F_PIX_HEIGHT))) 

#define INVERT_Y 	(1<<0)
#define INVERT_X 	(1<<1)
#define LOG_Y 		(1<<2)
#define LOG_X 		(1<<3)
typedef enum { DataTrans, DeviceTrans, IconTrans, LegendTrans,
	       ATTrans, ARTrans, ALTrans, ABTrans } TransRegion;

extern void     lc_SetUserCoord();
extern void     lc_SetIconDim();
extern void     lc_SetAnnotateDim();
extern void     lc_SetLegendDim();
extern void     lc_SetAxisDim();
extern void     lc_ClearTrans();
extern float    userY();
extern float    userX();
extern int      devX();
extern int      devY();
