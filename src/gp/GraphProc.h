/* $Id: GraphProc.h,v 2.76 2001-11-30 21:29:28 granger Exp $ */
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
#include <defs.h>
#include <DataStore.h>
#include <dm.h>

#define PathLen CFG_FILEPATH_LEN

/*
 * Specify the space required for a list of platforms.  If we figure platform
 * names to need about 8 characters on average, 128*8 gives us 1024 bytes,
 * which should (we've said that before) be enough for ANY platform list.
 * Use this definition for any string which will receive a 'platform'
 * parameter value.
 */
#define MaxPlatforms		(128)	/* station lists with short names */
#define PlatformListLen		(MaxPlatforms * 8)

/*
 * Something similar for field lists, used primarily by XY graphs
 */
#define MaxFields		(16)
#define FieldListLen		(MaxFields * 8)

/*
 * This flag is set when something happens which should abort an ongoing
 * plot operation.
 */
extern zbool Abort;

#ifdef UI_H_SYMBOLS	/* dependent on ui.h */
/*
 * Keep the variable table around, since we use it at times.
 */
extern stbl Vtable;
#endif

/*
 * Two plot descriptions are maintained in the graphics process -- the
 * "current" plot description, and the defaults table.  Only the "current"
 * plot description has a global reference.  The defaults pd is kept by the
 * pd library under the name "defaults".
 */
extern plot_description Pd;

/*
 * The current plot parameters.
 */
extern ZebTime PlotTime;	/* Time currently shown on the screen	*/
extern enum pmode PlotMode;	/* The current plot mode		*/
extern zbool MovieMode;		/* Movie mode?				*/
extern long ForecastOffset;	/* Forecast offset time (for model data)*/
extern zbool ValidationMode;	/* Use validation mode for model data?	*/

/*
 * Post processing stuff.
 */
extern zbool PostProcMode;	/* Post processing mode?		*/
extern ZebTime PostProcTime;	/* Post processing mode history time	*/
/*
 * Needed for opening the FrameFile.
 */
extern char FrameFilePath[PathLen];/* Path to the FrameFile 		*/
extern int  FrameFileFlag;	/* True when FrameFile should be opened */
extern int  MaxFrames;		/* Maximun number of frames		*/
/*
 * Our plot type and user plot limits
 */
extern float	Xlo, Xhi, Ylo, Yhi;

/*
 * Search path for icon and map files.
 */
extern char	IconPath[PathLen];	/* The icon path */
extern char	MapPath[PathLen];	/* Path for maps */

/*
 *  Maximum number of frames in the frame cache, and the default number.
 */
# define NCACHE 		100
# define DEFAULT_MAXFRAMES	20

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
 * A couple of variables for passing event locations out of UserEvent.  You
 * should only use these if your are sure you're being called as a result
 * of a user event -- preferably a pointer event -- or they may fool you.
 */
extern int	Event_X, Event_Y;

/*
 * To avoid conflicts with the exported UI symbol Top, define the gp Top
 * symbol to a unique variable name.
 */
# define Top	_gp_Top_

/*
 * Window specific stuff.  We protect it in an ifdef so that not all the
 * code has to include all those files.
 */
# ifdef _XtIntrinsic_h
extern GC Gcontext;			/* A global graphics context	*/
extern Widget Top;			/* The top level widget		*/
extern Widget Graphics, GrShell;	/* The graphics widget		*/
extern Widget Dock;
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
# define GP_HELP_MODEL		"model-time"
# define GP_HELP_OVERLAYS	"overlaytimes"
# define GP_HELP_LIMITS		"limitwidgets"
# define GP_HELP_XYGRAPHS	"xygraph"
# define GP_HELP_XSECTIONS	"Cross-section"
# define GP_HELP_TSERIES	"timeseries"
# define GP_HELP_ICONS		"icons"
# define GP_HELP_GPOSITION	"getposition"
# define GP_HELP_ALTITUDE	"altitude"


/*
 * An attempt to detect and avoid infinity and NaN values in the
 * data arrays, which really screw up the contouring algorithms.
 * On Sun's && SGI's, we can at least use the finite() function, otherwise
 * we rely on the POSIX __infinity() function.
 */
#if defined(sun) || defined(sgi) || defined (AIXV3) || defined (__osf__)
#define FINITE(x)	(finite((double)(x)))
#else
#define FINITE(x)	(!isinf(x) && !isnan(x))
#endif

/*
 * Try to avoid those annoying domain error messages from atan2 by
 * checking for dual zero arguments.  For some reason using an inline
 * static function breaks things.
 */
#ifdef notdef
static inline double ATAN2(y,x) double y, x;
{ return ((y==0.0 && x==0.0)?(0.0):atan2(y,x)); }
#endif
# define ATAN2(y,x) ((((y)==0.0) && ((x)==0.0)) ? ((double)0.0) : \
		     (atan2((double)(y),(double)(x))))

/*
 * The attribute used to store original lat/lon spacing into data chunks
 * for when it's needed for projections.
 */
# define ATTR_LATLON "original_lat_lon_spacing"

/*
 * Routines of interest.
 */
/* Basic graphic utilities */
extern int GetLocation FP ((char *, ZebTime *, Location *));
extern int FancyGetLocation FP ((const char *c, const char *platform, 
				 ZebTime *when, ZebTime *actual, 
				 Location *loc));
extern int SetLWidth FP((char *, char *, char *, int));
extern void FixLWidth FP((int));
extern void FixForeground FP((long));
extern void SetClip FP ((int));
extern int Intersects FP ((double x0, double y0, double x1, double y1));
extern void ResetGC FP ((void));
extern void SetColor FP ((char *, char *, char *, char *));
extern void ImageDump FP ((int format, char *file));

/* Plot control modules. */
extern void pc_CancelPlot FP ((void));
extern void pc_PlotHandler FP ((void));
extern void pc_ParamChange FP ((char *));
extern int pc_TimeTrigger FP ((char *));
extern void pc_TriggerGlobal FP ((void));
extern void pc_PopCoords FP ((void));
extern void pc_UnZoom FP ((void));
#ifdef UI_H_SYMBOLS	/* dependent on ui.h */
extern void pc_PushCoords FP ((struct ui_command *cmds));
extern void pc_Zoom FP ((struct ui_command *cmds));
#endif

/* Plot executive modules. */
extern void px_PlotExec FP ((char *));
extern void px_GlobalPlot FP ((ZebTime *));
extern void px_FixPlotTime FP ((ZebTime *));
extern char *px_FldDesc FP ((char *));
extern char *px_ModelTimeLabel FP ((void));
extern void px_SetEOPHandler FP ((void (*handler) ()));
extern void px_ClearEOPHandler FP ((void));

/* Grid access */
extern zbool ga_GridBBox FP ((ZebTime *, char *, float *, float *, float *,
			     float *));
extern void ga_RotateGrid FP ((float *, float *, int, int));
/* extern zbool ga_AvailableAlts FP ((ZebTime *, char *, float *, int *)); */
extern DataChunk *ga_GetGrid FP ((ZebTime *, char *, char *, FieldId, int *,
				  int *, float *, float *, float *, float *, 
				  float *, int *));

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
extern void fc_MarkFramesByOffset FP ((int *offsets, int noffsets));

/* Movie control */
extern void mc_DefMovieWidget FP ((void));
extern void mc_ParamChange FP ((char *));
extern void mc_PDChange FP ((void));
extern void mc_Dial FP ((int));
extern void mc_MovieStop FP ((void));

/* Model widget */
extern void mw_DefModelWidget FP ((void));
extern void mw_ParamChange FP ((char *));
extern void mw_Update FP ((void));

/* Colors */
extern void ct_Init FP ((void));

/* Icons */
extern void I_init FP ((void));
extern void I_DoIcons FP ((void));
extern void I_ColorIcons FP ((char *));
extern void I_PositionIcon FP ((const char *, const char *, ZebraTime *, 
				const char *, int, int, int));
extern void I_ClearPosIcons FP ((void));
extern int ov_PositionIcon FP ((char *, int, int, int));
extern void I_ActivateArea FP ((int, int, int, int, const char *, const char *,
				const char *, const char *));

/* Projections. */
extern int	prj_Setup FP ((void));
extern int	prj_Project FP ((double, double, float *, float *));
extern int	prj_Reverse FP ((double, double, float *, float *));
extern void	prj_GetOrigin FP ((float *, float *));
extern int	prj_ProjKey FP ((void));
extern int	prj_FancyProjection FP ((void));
extern char *	prj_GetProjName FP ((void));


/* Altitude control */
extern void	alt_Initialize FP ((void));
extern void	alt_Step FP ((int));

/* User events */
extern void Ue_Override FP ((void (*) (), void (*) (), void (*) ()));
extern void Ue_ResetOverride FP ((void));
extern void Ue_UnHighlight FP ((void));
extern void Ue_ResetHighlight FP ((void));
extern void Ue_Init FP ((void));
extern void Ue_NewBinding FP((struct dm_ebchange *dmsg));

/* Annotation utilities */
extern void An_AddAnnotProc FP ((void (*) (), char *, char *, int, int,
		int, int));
extern int An_SaShow FP((char *comp, char *qual));
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
extern void An_XYZGString FP ((char *, char *, int, int, int));
extern void An_TopAnnot FP ((const char *));

/* PLot description monitor protocol */
extern void pdm_Init FP ((void));
extern void pdm_ScheduleUpdate FP ((void));
extern void pdm_Finish FP ((void));

/* Overlay times widget */
extern void	ot_Init FP ((void));
extern void	ot_SetString FP ((char *));
extern void	ot_Append FP ((char *));
extern void	ot_AddStatusLine FP ((char *, char *, char *, ZebTime *));
extern char	*ot_GetString FP ((void));

/* Limit widgets */
extern void lw_InitWidgets FP ((void));
#ifdef UI_H_SYMBOLS	/* dependent on ui.h */
extern void lw_ActivateWidget FP ((int type, struct ui_command *cmds));
#endif

/* Init functions */
extern void iw_Initialize FP ((void));

/* DataMenu */
extern void InitDataMenu FP ((void));
extern char *SetupDataMenu (char *spec);
extern char *CheckDataMenu (char *spec, char *title);

/* Annotation widget */
extern void aw_InitAnnot FP ((void));
extern void aw_SetLoc FP ((void));

/* Position widget */
extern void pw_InitPos FP ((void));
extern void pw_PosStatus FP ((void));

/* Radar accomodations */
typedef enum { R_ANY = 0, R_SUR, R_RHI, R_PPI, R_VER, R_IDL } R_ScanMode;

char *r_ScanModeAtt (R_ScanMode i);
char *r_ScanModeName (R_ScanMode i);
int r_ImageTime (char *c, PlatformId pid, double angle, ZebTime *when);
int r_HoldAngle (char *comp);
int r_RadarSpace (char *comp);
R_ScanMode r_ScanMode (char *comp);
int r_GetAlts (PlatformId pid, char *comp, int nstep, float *alts);
int r_GetAngle (PlatformId pid, ZebTime *t, float *angle, R_ScanMode *scan);
void r_NewAlt (char *comp, float alt);
void r_AddAnnot (char *comp, char *platform);

/* Rubber bands */
#ifdef UI_H_SYMBOLS	/* dependent on ui.h */
extern void rb_Box FP ((struct ui_command *cmds));
extern void rb_Line FP ((struct ui_command *cmds));
extern void rb_PolyLine FP ((struct ui_command *cmds));
#endif

/* Other stuff */
extern void ChangePD FP ((struct dm_pdchange *dmp));
extern void GPShutDown FP ((void));
#ifdef UI_H_SYMBOLS	/* dependent on ui.h */
extern int dispatcher FP ((int junk, struct ui_command *cmds));
#endif
extern void parameter FP ((char *comp, char *param, char *value));
extern int xtEvent FP ((int fd));
extern int AgeCheck FP ((const char *, const char *, ZebTime *));
extern long GetSec FP(( UItime ));
extern int  reset_limits FP ((char *, char *, char *));
extern void eq_ResetAbort FP ((void));
extern void eq_ReturnPD FP ((void));
extern void eq_sync FP ((void));
extern void tr_InitAcWidget FP ((void));
extern void Require FP ((char *));
extern void DoRequires FP ((void));
extern void GetRange FP ((float *, int, double, float *min, float *max));
extern void GetByteRange FP((unsigned char *, int np, float *min, float *max));
extern void CalcCenterStep FP ((double, double, int, float *, float *));
extern void FindCenterStep FP ((DataChunk *, FieldId, int, float *, float *));
extern int ApplySpatialOffset FP ((DataChunk *, char *, ZebTime *));
extern zbool ImageDataTime FP ((char *c, PlatformId pid, double alt,
			       ZebTime *dtime));
extern zbool ClosestRHI FP ((char *c, PlatformId pid, double azimuth,
			    ZebTime *dtime, float *angdiff));
extern int GetLLSpacings FP ((DataChunk *, float *, float *));
extern void FreeColors FP ((plot_description pd));
extern int ParseFieldList (char *string, char **substrings);
#ifdef UI_H_SYMBOLS	/* dependent on ui.h */
extern void ov_Feature FP ((struct ui_command *cmds));
#endif

# if defined(hpux) || defined(SVR4) || defined (linux)
/* Defined in Utilities.c */
extern int nint FP ((double x));
# endif
void LabelStep FP ((char *lbl, double step, double cval));


typedef struct _WindInfo {
	int wi_polar;		/* 0 == use uwind/vwind; 1 == use wspd/wdir */
	FieldId wi_wspd;	/* field id's of found fields		    */
	FieldId wi_wdir;
	FieldId wi_uwind;
	FieldId wi_vwind;
} WindInfo;

extern char *SimpleFieldName (FieldId fid);
extern void FindWindsFields FP ((char *comp, PlatformId, ZebTime *, 
				 FieldId *, WindInfo *));
extern void GetWindData FP ((WindInfo *, float *, float *, double));

extern DataChunk *GetVorticity ( ZebTime *, char *, char *, FieldId, int *, 
				 int *,  float *, float *, float *, float *, 
				 float *, int * );


/* This stuff contains window system oriented stuff, so is only brought
   in if this module is doing X things. */
# ifdef _XtIntrinsic_h
	extern void HelpCallback FP ((Widget w, XtPointer client_data,
				      XtPointer call_data));
	extern zbool ct_LoadTable FP ((char *, XColor**, int *));
	extern void ct_FreeColors FP ((void));
	extern void ct_DeleteTable FP ((char *));
	extern int ct_GetColorByName FP ((char *, XColor *));
	extern int ct_GetColorByRGB FP ((XColor *));
	extern void An_TopAnnotMatch FP ((const char *, Pixel, const char *,
					  const char *));
	extern void An_DoTopAnnot FP ((const char *, Pixel, const char *, 
				       const char *));
	extern void An_GetTopParams FP ((XColor *, int *));
	extern Widget LeftRightButtons FP ((Widget, void *,XtTranslations));
	extern void WindGrid FP((Widget w, Drawable d, GC Gcontext, 
				 float *u_array, float *v_array, int xdim, 
				 int ydim, int xlo, int ylo, int xhi, int yhi, 
				 double vlen, double bad, XColor color, 
				 int degrade, int vector));
	extern void WindProjGrid FP ((float *, float *, int, int, Location *,
			double, double, double, double, Pixel, int, int));
	extern Pixmap I_GetPMap FP ((const char *, int *, int *, int *, 
				     int *));
	extern void I_RepositionMenu FP ((Widget w));
	extern void I_RedirectButton FP ((Window win, XEvent *ev));

	void RasterPlot FP ((DataChunk *, Location *, float *, int, int));
	void RP_Init FP ((XColor *colors, int count, XColor c_outrange,
		  XRectangle clip, double dmin, double dmax, 
		  int highlight, double hvalue, XColor hcolor,
		  double hrange));
	void RP_Transparent (int setting);
	void RasterImagePlot FP ((Widget w, int frame, unsigned char *grid,
			  int xd, int yd, int xlo, int ylo, int xhi, int yhi,
			  double scale, double bias, Location *, RGrid *));
	void RasterXIPlot FP ((Widget w, Drawable d, float *array, 
		       int xdim, int ydim, 
		       int xlo, int ylo, int xhi, int yhi,
		       int fast));
	void ChangeCursor FP ((Widget w, Cursor cursor));

/* Contouring */
	extern void	CO_Init FP ((XColor *, int, int, XColor *, XRectangle,
				int, double));
	extern void	CO_InitMono FP ((XColor, XRectangle, int, double));
	extern void	Contour FP ((Widget, Drawable, float *, int, int,
				int, int, int, int, double, double, int, int));
	extern void	CO_ProjSetup FP ((Location *, double, double));
	extern void 	FillContour FP ((Widget, Drawable, float *, int, int,
				int, int, int, int, double, double));
	extern void	FC_Init FP ((XColor *, int, int, XColor *, XRectangle,
				int, double));
	extern void	FC_ProjSetup FP ((Location *, double, double));

/* Raster */
	void RP_ZapShmImage FP ((Widget w));

# endif /* _XtIntrinsic_h */
