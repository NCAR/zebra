/* $Id: GraphProc.h,v 2.32 1993-12-27 17:40:35 corbet Exp $ */
/*
 * Graphics process definitions.
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
 * Some of the prototypes rely on the DataChunk type
 */
#include <DataStore.h>

/*
 * This flag is set when something happens which should abort an ongoing
 * plot operation.
 */
extern bool Abort;
/*
 * Our name.
 */
extern char Ourname[40];

/*
 * Keep the variable table around, since we use it at times.
 */
extern stbl Vtable;

/*
 * Two plot descriptions are maintained in the graphics process -- the
 * "current" plot description, and the defaults table.
 */
extern plot_description Pd, Defaults;

/*
 * The current plot parameters.
 */
extern ZebTime PlotTime;	/* Time currently shown on the screen	*/
extern enum pmode PlotMode;	/* The current plot mode		*/
extern bool MovieMode;		/* Movie mode?				*/
/*
 * Post processing stuff.
 */
extern bool PostProcMode;	/* Post processing mode?		*/
extern ZebTime PostProcTime;	/* Post processing mode history time	*/
/*
 * Needed for opening the FrameFile.
 */
extern char FrameFilePath[40];	/* Path to the FrameFile 		*/
extern int  FrameFileFlag;	/* True when FrameFile should be opened */
extern int  MaxFrames;		/* Maximun number of frames		*/
/*
 * Our plot type and user plot limits
 */
# define Pltype PlotType	/* temp kludge jc */
extern int	PlotType;
extern float	Xlo, Xhi, Ylo, Yhi;
extern float	Alt;		/* CAP plot altitude			*/

/*
 * Search path for icon and map files.
 */
# define PathLen	120
extern char	IconPath[PathLen];	/* The icon path */
extern char	MapPath[PathLen];	/* Path for maps */

/*
 * The "altitude control" component -- that which regulates the altitude
 * at which CAP plots are done.
 */
extern int	AltControlComp;
/*
 *  Maximum number of frames in the frame cache.
 */
# define NCACHE 100

/*
 * How much of the screen is used for the top annotation.
 */
# define TOPANNOTHEIGHT	0.035
/*
 * The state of the window.
 */
enum wstate { UP, DOWN };
extern enum wstate WindowState;

/*
 * A couple of variables for passing event locationns out of UserEvent.  You
 * should only use these if your are sure you're being called as a result
 * of a user event -- preferably a pointer event -- or they may fool you.
 */
extern int	Event_X, Event_Y;
/*
 * Window specific stuff.  We protect it in an ifdef so that not all the
 * code has to include all those files.
 */
# ifdef _XtIntrinsic_h
extern GC Gcontext;			/* A global graphics context	*/
extern Widget Top;			/* The top level widget		*/
extern Widget Graphics, GrShell;	/* The graphics widget		*/
extern Display *Disp;			/* The display we use		*/
extern XtAppContext Actx;		/* The application context	*/
extern Cursor BusyCursor, NormalCursor;	/* Our cursors		*/
# endif
extern int FrameCount;			/* How many frames?		*/
extern int DisplayFrame;		/* Frame to display		*/
extern int DrawFrame;			/* Frame to draw in		*/

/*
 *  Set to 1 for PlotExec to trigger a global update
 */
extern int TriggerGlobal;

/*
 * Definition of help topic names, in a central place for convenience and
 * accuracy.  Note that length is no longer significant; it is corrected
 * elsewhere.
 */
# define GP_HELP_MOVIE		"movies"
# define GP_HELP_OVERLAYS	"overlaytimes"
# define GP_HELP_LIMITS		"limitwidgets"
# define GP_HELP_XYGRAPHS	"xygraph"
# define GP_HELP_XSECTIONS	"Cross-section"
# define GP_HELP_TSERIES	"timeseries"
# define GP_HELP_ICONS		"icons"
# define GP_HELP_GPOSITION	"getposition"
# define GP_HELP_ALTITUDE	"altitude"

/*
 * Routines of interest.
 */
/* Basic graphic utilities */
extern int SetLWidth FP((char *, char *, char *, int));
extern void FixLWidth FP((int));
extern void FixForeground FP((long));
extern void SetClip FP ((int));
extern void ResetGC FP ((void));
extern void SetColor FP ((char *, char *, char *, char *));

/* Plot control modules. */
extern void pc_CancelPlot FP ((void));
extern void pc_PlotHandler FP ((void));
extern void pc_ParamChange FP ((char *));
extern int pc_TimeTrigger FP ((char *));
extern void pc_TriggerGlobal FP ((void));

/* Plot executive modules. */
extern void px_PlotExec FP ((char *));
extern void px_GlobalPlot FP ((ZebTime *));
extern void px_FixPlotTime FP ((ZebTime *));
extern char *px_FldDesc FP ((char *, char *));

/* Grid access */
extern bool ga_GridBBox FP ((ZebTime *, char *, float *, float *, float *,
		float *));
extern void ga_RotateGrid FP ((float *, float *, int, int));
extern bool ga_AvailableAlts FP ((ZebTime *, char *, float *, int *));
extern DataChunk *ga_GetGrid FP ((ZebTime *, char *, char *, char *, int *,
		int *, float *, float *, float *, float *, float *, int *));

/* Frame cache routines */
extern void fc_InitFrameCache FP ((void));
extern void fc_InvalidateCache FP ((void));
extern void fc_UnMarkFrames FP ((void));
extern void fc_CreateFrameFile FP ((void));
extern void fc_SetNumFrames FP ((int));
extern char *fc_GetInfo FP ((int));
extern void fc_AddFrame FP ((ZebTime *, int));
extern int fc_LookupFrame FP ((ZebTime *, char **));
extern int fc_GetFrame FP ((void));
extern void fc_MarkFrames FP ((ZebTime *, int));

/* Movie control */
extern void mc_DefMovieWidget FP ((void));
extern void mc_ParamChange FP ((void));
extern void mc_PDChange FP ((void));
extern void mc_Dial FP ((int));
extern void mc_LoadParams FP ((void));

/* Icons */
extern void I_DoIcons FP ((void));
extern void I_ColorIcons FP ((char *));
extern void I_PositionIcon FP ((char *, char *, ZebTime *, char *, int, 
				int, int));
extern void I_ClearPosIcon FP (());
extern int ov_PositionIcon FP ((char *, int, int, int));
extern void I_ActivateArea FP ((int, int, int, int, char *, char *, char *,
				char *));

/* User events */
extern void Ue_Override FP ((void (*) (), void (*) (), void (*) ()));
extern void Ue_ResetOverride FP ((void));
extern void Ue_UnHighlight FP ((void));

/* Annotation utilities */
extern void An_AddAnnotProc FP ((void (*) (), char *, char *, int, int,
		int, int));
extern void An_DoSideAnnot FP (());
extern void An_ColorBar FP ((char *, char *, int, int, int));
extern void An_ColorNumber FP ((char *, char *, int, int, int));
extern void An_ColorVector FP ((char *, char *, int, int, int));
extern void An_BarbLegend FP ((char *, char *, int, int, int));
extern void An_ColorString FP ((char *, char *, int, int, int));
extern void An_ColorScale FP ((char *, char *, int, int, int));
extern int An_GetLeft FP (());
extern void An_GetSideParams FP ((char *, float *, int *));
extern void An_ResetAnnot FP ((int));
extern void An_SetScale FP ((double));
extern void An_AnnotLimits FP ((int *, int *, int *, int *));
extern void An_SAUsed FP ((int));
extern void An_XYGString FP ((char *, char *, int, int, int));

/* Coord space transformations */
extern void cvt_ToXY FP ((double, double, float *, float *));
extern void cvt_ToLatLon FP ((double, double, float *, float *));
extern void cvt_GetOrigin FP ((float *, float *));
extern bool cvt_Origin FP ((double, double));

/* PLot description monitor protocol */
extern void pdm_Init FP ((void));
extern void pdm_ScheduleUpdate FP ((void));
extern void pdm_Finish FP ((void));

/* Other stuff */
extern int GetLocation FP ((char *, ZebTime *, Location *));
extern int AgeCheck FP ((char *, char *, ZebTime *));
extern long GetSec FP(( UItime ));
extern void sync FP ((void));
extern int  reset_limits FP ((char *, char *, char *));
extern void eq_ResetAbort FP ((void));
extern void eq_ReturnPD FP ((void));
extern void tr_InitAcWidget FP ((void));
extern char *lw_Status FP ((void));
extern void Require FP ((char *));
extern void DoRequires FP ((void));
extern void GetRange FP ((float *, int, double, float *, float *));
extern void CalcCenterStep FP ((double, double, int, float *, float *));
extern void FindCenterStep FP ((DataChunk *, FieldId, int, float *, float *));
extern int ApplySpatialOffset FP ((DataChunk *, char *, ZebTime *));

/* This stuff contains window system oriented stuff, so is only brought
   in if this module is doing X things. */
# ifdef _XtIntrinsic_h
	extern void HelpCallback FP ((Widget w, XtPointer client_data,
				      XtPointer call_data));
	extern bool ct_LoadTable FP ((char *, XColor**, int *));
	extern void ct_FreeColors FP ((void));
	extern void ct_DeleteTable FP ((char *));
	extern int ct_GetColorByName FP ((char *, XColor *));
	extern int ct_GetColorByRGB FP ((XColor *));
	extern void An_TopAnnot FP ((char *, Pixel));
	extern void An_GetTopParams FP ((XColor *, int *));
	extern Widget LeftRightButtons FP ((Widget, void *,XtTranslations));
	extern void draw_vector FP ((Display *, Drawable, GC, int, int,
		double, double, double));
	extern Pixmap I_GetPMap FP ((char *, int *, int *, int *, int *));

	void RasterPlot FP ((Widget w, Drawable d, float *array, 
		     int xdim, int ydim,
		     int xlo, int ylo, int xhi, int yhi));
	void RP_Init FP ((XColor *colors, int count, XColor c_outrange,
		  XRectangle clip, double dmin, double dmax, 
		  Boolean highlight, double hvalue, XColor hcolor,
		  double hrange));
	void RasterImagePlot FP ((Widget w, int frame, unsigned char *grid,
			  int xd, int yd, int xlo, int ylo, int xhi, int yhi,
			  double scale, double bias));
	void RasterXIPlot FP ((Widget w, Drawable d, float *array, 
		       int xdim, int ydim, 
		       int xlo, int ylo, int xhi, int yhi,
		       bool fast));
# ifdef SHM
	void RP_ZapSHMImage FP ((Widget w));
# endif /* SHM */
# endif /* _XtIntrinsic_h */
