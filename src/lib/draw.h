/* $Id: draw.h,v 2.1 1995-06-29 22:50:39 granger Exp $ */
/*		Copyright (C) 1987-1995 by UCAR
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
 * Requires defs.h and X11/Intrinsic.h
 */
#ifndef _zeb_draw_h_
#define _zeb_draw_h_

extern void draw_vector FP ((Display *W, Drawable D, GC, int x, int y, 
			     double u, double v, double unit));

extern void draw_barb FP ((Display *W, Drawable D, GC, int x, int y,
			   double angle, double spd, int shaftlen,
			   int doKnots));

#endif /* ! _zeb_draw_h_ */
