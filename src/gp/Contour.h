/*
 * Contour.h -- Common definitions for Contour.c and FillContour.c
 */

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
 * The array we're contouring, its x and y dimensions
 * and a macro to reference it by vertex
 */
static float	*Z;
static int	Nx, Ny;
static float	Badflag;
static int	Use_flag = FALSE;
# define ZVAL(i,j)	Z[(i)*Ny + (j)]

/*
 * Color stuff
 */
static int	Color_center = 1, Ncolor = 1;
static XColor	*Colors, Color_outrange, Color_mono;
static int	Monoflag;

/*
 * Do out-of-range contours?
 */
static int	Do_outrange;

/*
 * Clipping rectangle
 */
static XRectangle	Clip;

/*
 * Graphics context
 */
/* static GC	Gcontext = NULL; */

/*
 * The widget and drawable we're using (the drawable should belong to the
 * widget, i.e., either its window or an associated pixmap)
 */
static Widget		W;
static Drawable		D;

/*
 * Arrays for pixel coordinates in x and y plus
 * x and y spacing.
 */
static float	*Xpos, *Ypos;
static float	Xinc, Yinc;

/*
 * General definitions
 */
# ifndef TRUE
#	define TRUE	1
#	define FALSE	0
# endif
