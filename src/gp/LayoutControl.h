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


# define F_PIX_HEIGHT   ((int)((FY1-FY0)*GWHeight(Graphics)))
# define F_PIX_WIDTH    ((int)((FX1-FX0)*GWWidth(Graphics)))
# define DATA_SERIES    (1<<1)
# define DATA_SNAPSHOT  (1<<2)
# define AUTO         (1<<0)
# define MANUAL       (1<<1)
# define INVERT       (1<<2)
# define LOG	      (1<<3)
# define FUDGEBOT     (1 << 4)	/* jc -- allow extra space on bottom */
# define AXIS_BOTTOM	0
# define AXIS_LEFT      1
# define AXIS_TOP       2
# define AXIS_RIGHT     3

# define FUDGEAMOUNT	50	/* jc -- amount of extra space on bot */



extern float   FX0, FY0, FX1, FY1;
extern float   AxisX0[4],AxisX1[4],AxisY0[4],AxisY1[4];
extern float   IconX0,IconX1,IconY0,IconY1;
extern float   AnnotateX0,AnnotateX1,AnnotateY0,AnnotateY1;
extern float   LegendX0,LegendX1,LegendY0,LegendY1;
extern int     Zlevel;
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
/*extern  DataValRec	UX0,UX1,UY0,UY1;*/
/*
 * User coordinate to pixel coordinate macros
 */
# define LC_FXPIX(ux,ux0,ux1)      (int)(0.5 + (float)(GWWidth (Graphics)) * \
        (FX1 - (((ux) - ux0) / (ux1 - ux0) * (FX1 - FX0))))

# define LC_XPIX(ux,ux0,ux1)       (int)(0.5 + (float)(GWWidth (Graphics)) * \
        (((ux) - ux0) / (ux1 - ux0) * (FX1 - FX0) + FX0))

# define LC_FYPIX(uy,uy0,uy1)       (int)((GWHeight(Graphics) - \
			(FY1 * GWHeight(Graphics))) +\
        (int)(( (((uy) - uy0) / (uy1 - uy0)))* F_PIX_HEIGHT))

# define LC_YPIX(uy,uy0,uy1)       (int)((GWHeight(Graphics) - \
			(FY1 * GWHeight(Graphics))) +\
        (int)((1.0 - (((uy) - uy0) / (uy1 - uy0)))* F_PIX_HEIGHT))

/*
 * Pixel coordinate to user coordinate macros
 */
# define LC_FXUSER(xp,ux0,ux1)      (ux1 - (((xp)/(float)GWWidth(Graphics)) - FX0) * \
                                (ux1 - ux0)/(FX1 - FX0) )
# define LC_XUSER(xp,ux0,ux1)      ((((xp)/(float)GWWidth(Graphics)) - FX0) * \
                                (ux1 - ux0)/(FX1 - FX0) + ux0)

# define LC_FYUSER(yp,uy0,uy1)      uy1 - (uy1-uy0) * (1.0 - \
	((float)(yp - (GWHeight(Graphics) - (FY1*GWHeight(Graphics))))/(float)(F_PIX_HEIGHT))) 
# define LC_YUSER(yp,uy0,uy1)      uy0 + (uy1-uy0) * (1.0 - \
	((float)(yp - (GWHeight(Graphics) - (FY1*GWHeight(Graphics))))/(float)(F_PIX_HEIGHT))) 

typedef enum { DataTrans, DeviceTrans, IconTrans, LegendTrans,
	       ATTrans, ARTrans, ALTrans, ABTrans } TransRegion;

extern void lc_SetAxisDim FP((int,int));
extern void lc_SetIconDim FP((int,int));
extern void lc_SetLegendDim FP((int,int));
extern void lc_SetAnnotateDim FP((int,int));
extern void lc_SetUserCoord FP(( DataValPtr, DataValPtr,DataValPtr,DataValPtr));
extern void lc_GetTime FP(( time *, time_t ));
extern void lc_DecrData FP(( DataValPtr,double ));
extern void lc_IncrData FP(( DataValPtr,double ));
extern int lc_CompareData FP(( DataValPtr,DataValPtr ));
extern int devY FP((DataValPtr, unsigned short ));
extern int devX FP((DataValPtr,unsigned short ));
extern DataValRec userX FP((int,unsigned short));
extern DataValRec userY FP((int ,unsigned short ));
extern void lc_Zoom FP((float,float,float,float));
extern void lc_UnZoom FP(());
