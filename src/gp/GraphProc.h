/* $Id: GraphProc.h,v 2.1 1991-07-30 16:12:00 kris Exp $ */
/*
 * Graphics process definitions.
 */

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
extern time PlotTime;		/* Time currently shown on the screen	*/
extern enum pmode PlotMode;	/* The current plot mode		*/
extern bool MovieMode;		/* Movie mode?				*/
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
 * Routines of interest.
 */
# ifdef __STDC__
extern void tr_InitAcWidget ();
extern void pc_CancelPlot (void);
extern void pc_PlotHandler (void);
extern void pc_ParamChange (char *);
extern void sync (void);
extern int  reset_limits(char *, char *, char *);
extern void eq_ResetAbort (void);
extern void eq_ReturnPD (void);
extern void px_PlotExec (char *);
extern int pc_TimeTrigger (char *);
extern void px_GlobalPlot (time *);
extern void px_FixPlotTime (time *);
extern char *af_OpenFile (char *);
extern char *af_Setup (char *, time *, int, char **);
extern bool af_NextSample (char *, int *, float *);
extern void af_ReleaseCtx (char *);
extern bool af_FieldOK (char *, char *);
extern float *ga_MudrasGrid (time *, char *, char *, int *, int *, int *, 
	float *, float *, float *, float *);
extern bool ga_GridBBox (time *, char *, float *, float *, float *, float *);
extern void ga_RotateGrid (float *, float *, int, int);
extern bool ga_AvailableAlts (time *, char *, float *, int *);
extern void fc_InvalidateCache (void);
extern void fc_UnMarkFrames (void);
extern void fc_CreateFrameFile (void);
extern void fc_SetNumFrames (int);
extern char *fc_GetInfo (int);
extern void fc_AddFrame (time *, int);
extern int fc_LookupFrame (time *);
extern int fc_GetFrame (void);
extern void fc_MarkFrames (time *, int);
extern void mc_DefMovieWidget (void);
extern void mc_ParamChange (void);
extern void mc_PDChange (void);
extern void mc_Dial (int);
extern char *px_FldDesc (char *, char *);
extern void I_DoIcons (void);
extern int ov_PositionIcon (char *, int, int, int);
extern void Ue_Override (void (*) (), void (*) (), void (*) ());
extern void Ue_ResetOverride (void);
extern void An_ResetAnnot (int);
extern void An_SetScale (double);
extern void An_AnnotLimits (int *, int *, int *, int *);
extern void An_SAUsed (int);
extern float *ga_GetGrid (time *, char *, char *, int *, int *, float *,
		float *, float *, float *, float *);
extern void cvt_ToXY (double, double, float *, float *);
extern void cvt_ToLatLon (double, double, float *, float *);
extern void cvt_GetOrigin (float *, float *);
extern bool cvt_Origin (double, double);
extern int GetLocation (char *, time *, Location *);
extern void SetClip (int);
extern void ResetGC (void);
extern void SetColor (char *, char *, char *, char *);
extern int AgeCheck (char *, time *);
# ifdef _XtIntrinsic_h
	extern bool ct_LoadTable (char *, XColor**, int *);
	extern void ct_FreeColors (void);
	extern void ct_DeleteTable (char *);
	extern int ct_GetColorByName (char *, XColor *);
	extern int ct_GetColorByRGB (XColor *);
	extern void An_TopAnnot (char *, Pixel);
	extern Widget LeftRightButtons (Widget, void *);
# endif
# else
	extern void tr_InitAcWidget ();
	extern void pc_CancelPlot ();
	extern void pc_PlotHandler ();
	extern void pc_ParamChange ();
	extern void sync ();
	extern int  reset_limits();
	extern void eq_ResetAbort ();
	extern void eq_ReturnPD ();
	extern void px_PlotExec ();
	extern void px_GlobalPlot();
	extern void px_FixPlotTime();
	extern int pc_TimeTrigger ();
	extern char *af_OpenFile ();
	extern char *af_Setup ();
	extern bool af_NextSample ();
	extern void af_ReleaseCtx ();
	extern bool af_FieldOK ();
	extern float *ga_MudrasGrid ();
	extern void ga_RotateGrid ();
	extern bool ga_GridBBox ();
	extern bool ga_AvailableAlts ();
	extern void fc_InvalidateCache ();
	extern void fc_UnMarkFrames ();
	extern void fc_CreateFrameFile ();
	extern void fc_SetNumFrames ();
	extern char *fc_GetInfo ();
	extern void fc_AddFrame ();
	extern int fc_LookupFrame ();
	extern int fc_GetFrame ();
	extern void fc_MarkFrames ();
	extern void mc_DefMovieWidget ();
	extern void mc_ParamChange ();
	extern void mc_PDChange ();
	extern void mc_Dial ();
	extern char *px_FldDesc ();
	extern void I_DoIcons ();
	extern int ov_PositionIcon ();
	extern void Ue_Override ();
	extern void Ue_ResetOverride ();
	extern void An_ResetAnnot ();
	extern void An_AnnotLimits ();
	extern void An_SAUsed ();
	extern float *ga_GetGrid ();
	extern void cvt_ToXY ();
	extern void cvt_ToLatLon ();
	extern void cvt_GetOrigin ();
	extern bool cvt_Origin ();
	extern Widget LeftRightButtons ();
	extern int GetLocation ();
	extern void SetClip ();
	extern void ResetGC ();
	extern void SetColor ();
	extern int AgeCheck ();
# ifdef _XtIntrinsic_h
	extern bool ct_LoadTable ();
	extern void ct_FreeColors ();
	extern void ct_DeleteTable ();
	extern int ct_GetColorByName ();
	extern int ct_GetColorByRGB ();
	extern void An_TopAnnot ();
	extern Widget LeftRightButtons ();
# endif
# endif
