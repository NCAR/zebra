/*
 * Prototypes for public functions
 * $Id: prototypes.h,v 1.1 1991-06-16 17:12:09 burghart Exp $
 */
# include <X11/Intrinsic.h>
# include <ui.h>
# include "radar.h"

# ifdef __STDC__
	void	bm_BuildBitmaps (Widget);
	Widget	cw_CWidget (Widget);
	void	GenScan (Radar *, double, double, double, int);
	Widget	LeftRightButtons (Widget, void (*)());
	void	mw_DefMainWidget (void);
	void	opt_Finish (void);
	Widget	rw_RWidget (Widget);
	void	ScanOptions (void);
	void	so_Display (struct ui_command *);
	Widget	so_SOWidget (Widget);
	void	vol_InitBoundary (void);
# else
	void	bm_BuildBitmaps ();
	Widget	cw_CWidget ();
	void	GenScan ();
	Widget	LeftRightButtons ();
	void	mw_DefMainWidget ();
	void	opt_Finish ();
	Widget	rw_RWidget ();
	void	ScanOptions ();
	void	so_Display ();
	Widget	so_SOWidget ();
	void	vol_InitBoundary ();
# endif
