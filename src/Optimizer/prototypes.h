/*
 * Prototypes for public functions
 * $Id: prototypes.h,v 1.2 1991-09-17 16:32:03 burghart Exp $
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

# include <X11/Intrinsic.h>
# include <ui.h>
# include "radar.h"

# ifdef __STDC__
	void	bm_BuildBitmaps (Widget);
	Widget	cw_CWidget (Widget);
	void	GenScan (Radar *, double, double, double, int);
	Widget	LeftRightButtons (Widget, void (*)());
	Widget	mw_MainWidget (Widget);
	void	opt_Finish (void);
	Widget	rw_RWidget (Widget);
	void	ScanOptions (void);
	void	so_Display (int);
	Widget	so_SOWidget (Widget);
	Widget	sw_SWidget (Widget);
	void	vol_InitBoundary (void);
# else
	void	bm_BuildBitmaps ();
	Widget	cw_CWidget ();
	void	GenScan ();
	Widget	LeftRightButtons ();
	Widget	mw_MainWidget ();
	void	opt_Finish ();
	Widget	rw_RWidget ();
	void	ScanOptions ();
	void	so_Display ();
	Widget	so_SOWidget ();
	Widget	sw_SWidget ();
	void	vol_InitBoundary ();
# endif
