/*		Copyright (C) 1987, 88, 89, 90, 91 by UCAR
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
 *	      0 |	       (fx1, fy1) |
 *		|	 __________/	|
 *		|	|	  /|	|
 *		|	| (ux1, uy1) |	|
 *		|	|	   |	|
 *		|	|	   |	|
 *		|	|	   |	|
 *		|	| (ux0, uy0) |	|
 *		|	|/_________|	|
 *		|	/		|
 *		|  (fx0, fy0) 		|
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
 *	by its width and height in pixels (pixwidth and pixheight) .
 *	In X pixel coordinates, the origin is at the UPPER LEFT
 *	corner.
 *
 *	The smaller box is represented by two sets of coordinates:
 *	
 *	The "f" coordinates are fractional values representing the 
 *	subwindow relative to the entire drawable (less the lowest 
 *	ICONSPACE pixels, which are reserved for putting up icons) .  
 *	^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
 *	This has been changed.  The Icon region is now treated exactly
 *	as the other regions: side, top, and axes.  The origin of the
 *	normalized coordinate space is now the lower left of the whole
 *	window, corresponding to point (0,Gheight) in pixel coordinates.
 *	The location of the Icon region itself is stored in normalized
 *	coordinates just like the other regions.  All transformations
 *	from normalized to pixel now take the forms:
 *
 *		Xp = Xndc * Width(Graphics)
 * 		Yp = (1.0 - Yndc) * Height(Graphics)
 *
 *	The "f" coordinates range from (0.0, 0.0) at the LOWER LEFT 
 *	corner to (1.0, 1.0) at the upper right corner of the drawable.  
 *	Note that the origin is different from the origin of the X 
 *	pixel coordinates.  In the diagram above, (fx0, fy0) would be 
 *	about (0.3, 0.4) and (fx1, fy1) would be about (0.8, 0.9) .
 *
 *	The "u" coordinates are the user coordinates to use for
 *	the subwindow.  The user coordinates of the lower left
 *	corner are (ux0, uy0) and the user coordinates of the upper
 *	right corner are (ux1, uy1) .
 */

# ifndef _layoutcontrol_h_
# define _layoutcontrol_h_

# define F_PIX_HEIGHT   ((int) ((FY1-FY0) * GWHeight (Graphics)))
# define F_PIX_WIDTH    ((int) ((FX1-FX0) * GWWidth (Graphics)))
# define DATA_SERIES    (1 << 1)
# define DATA_SNAPSHOT  (1 << 2)
# define FUDGEAMOUNT	50	/* jc -- amount of extra space on bot */

/*
 * Axis side enum.  NumSides is provided for constructs like:
 *
 *	for (s = 0; s < NumSides; s++)
 *
 * to traverse the sides.
 */
typedef enum 
{
	SideLeft = 0, SideRight, SideTop, SideBottom, NumSides
} AxisSide;

/*
 * Macros to turn an AxisSide enumerator into a letter or a name
 */
# define SIDE_LETTER(s) ((s) == SideLeft ? 'l' : \
			 (s) == SideRight ? 'r' : \
			 (s) == SideTop ? 't' : \
			 (s) == SideBottom ? 'b' : '?')

# define SIDE_NAME(s) ((s) == SideLeft ? "left" : \
		       (s) == SideRight ? "right" : \
		       (s) == SideTop ? "top" : \
		       (s) == SideBottom ? "bottom" : "?")

# define AXIS_SPACE(s) (((s) == SideLeft || (s) == SideRight) ? \
			(AxisX1[(s)] - AxisX0[(s)]) * GWWidth(Graphics) : \
			(AxisY1[(s)] - AxisY0[(s)]) * GWHeight(Graphics))

/*
 * The current dimensions
 */
extern float	FX0, FY0, FX1, FY1;
extern float	AxisX0[NumSides], AxisX1[NumSides];
extern float	AxisY0[NumSides], AxisY1[NumSides];
extern float	IconX0, IconX1, IconY0, IconY1;
extern float	AnnotateX0, AnnotateX1, AnnotateY0, AnnotateY1;
extern float	LegendX0, LegendX1, LegendY0, LegendY1;
extern int	Zlevel;


typedef struct DataVal_
{
    char	type; /* 'i', 'f', 'd', or 't' */
    union
    {
	int	i;
	float	f;
	double	d;
	ZebTime	t;
    } val;
} DataValRec, *DataValPtr;

/*
 * User coordinate to pixel coordinate macros
 */
# define LC_XPIX(ux,ux0,ux1)	((int) (0.5 + GWWidth (Graphics) * \
		(FX0 + ((ux) - (ux0)) / ((ux1) - (ux0)) * (FX1 - FX0))))

# define LC_YPIX(uy,uy0,uy1)	((int) (0.5 + GWHeight (Graphics) * \
		(1.0 - FY0 - ((uy) - (uy0)) / ((uy1) - (uy0)) * (FY1 - FY0))))

/*
 * Pixel coordinate to user coordinate macros
 */
# define LC_XUSER(xp,ux0,ux1)	((ux0) + ((ux1) - (ux0)) / (FX1 - FX0) * \
			 ((xp) / (float) GWWidth (Graphics) - FX0))

# define LC_YUSER(yp,uy0,uy1)	((uy0) + ((uy1) - (uy0)) / (FY1 - FY0) * \
			 (1.0 - (yp) / (float) GWHeight (Graphics) - FY0))

typedef enum {DataTrans, DeviceTrans, IconTrans, LegendTrans,
	       ATTrans, ARTrans, ALTrans, ABTrans} TransRegion;

extern void	lc_Init FP ((void));
extern void	lc_SetAxisSpace FP ((AxisSide, int));
extern void	lc_SetIconSpace FP ((int));
extern void	lc_SetLegendSpace FP ((int));
extern void	lc_SetAnnotateSpace FP ((int));
extern void	lc_SetBaseUserCoord FP ((DataValPtr xleft, DataValPtr xright,
					 DataValPtr ybottom, DataValPtr ytop));
extern void	lc_SetUserCoord FP ((DataValPtr, DataValPtr, DataValPtr,
				     DataValPtr));
extern void	lc_GetUserCoord FP ((DataValPtr, DataValPtr, DataValPtr,
				     DataValPtr));
extern void	lc_GetTime FP ((UItime *, time_t));
extern void	lc_DecrData FP ((DataValPtr, double));
extern void	lc_IncrData FP ((DataValPtr, double));
extern int	lc_CompareData FP ((DataValPtr, DataValPtr));
extern void	lc_Zoom FP ((double, double, double, double));
extern int	lc_UnZoom FP ((int));
extern void	lc_LoadZoom FP ((void));
extern int	devY FP ((DataValPtr));
extern int	devX FP ((DataValPtr));
extern DataValRec	userX FP ((int));
extern DataValRec	userY FP ((int));

#endif /* ! _layoutcontrol_h_ */
