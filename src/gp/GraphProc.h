/* $Id: GraphProc.h,v 1.1 1990-05-07 16:08:05 corbet Exp $ */
/*
 * Graphics process definitions.
 */

/*
 * This flag is set when something happens which should abort an ongoing
 * plot operation.
 */
extern bool Abort;

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
extern XFontStruct *Afontstruct;	/* Temporary	*/
extern GC Agc;
extern Widget Top;			/* The top level widget		*/
extern Widget Graphics, GrShell;	/* The graphics widget		*/
extern XtAppContext Actx;		/* The application context	*/
extern XColor	*Colors;		/* color array (temporary)	*/
extern int	Ncolors;		/* number of colors (temporary)	*/
# endif

/*
 * Routines of interest.
 */
# ifdef __STDC__
extern void pc_CancelPlot (void);
extern void pc_SetUp (void);
extern void sync (void);
extern void eq_ResetAbort (void);
extern void px_PlotExec (char *);
# ifdef _XtIntrinsic_h
extern bool ct_LoadTable (char *, XColor**, int *);
extern void ct_FreeColors (void);
extern void ct_DeleteTable (char *);
extern int ct_GetColorByName (char *, XColor *);
extern int ct_GetColorByRGB (XColor *);
# endif
# else
extern void pc_CancelPlot ();
extern void pc_SetUp ();
extern void sync ();
extern void eq_ResetAbort ();
extern void px_PlotExec ();
# ifdef _XtIntrinsic_h
extern bool ct_LoadTable ();
extern void ct_FreeColors ();
extern void ct_DeleteTable ();
extern int ct_GetColorByName ();
extern int ct_GetColorByRGB ();
# endif
# endif
