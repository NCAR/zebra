/*
 * Movie control functions.
 */
static char *rcsid = "$Id: MovieControl.c,v 1.2 1990-06-25 14:54:48 corbet Exp $";

# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/Cardinals.h>
# include <X11/Xaw/AsciiText.h>

# include "../include/defs.h"
# include "../include/message.h"
# include "../include/pd.h"
# include "../include/timer.h"
# include "GraphProc.h"
# include "GraphicsW.h"
# include "EventQueue.h"
# include <ui_date.h>


/*
 * Globals.
 */
# define ATSLEN	80	/* Length for AsciiText strings		*/
static char Minutes[ATSLEN], Endt[ATSLEN], Frate[ATSLEN];
static Widget StatusLabel;

/*
 * The actual movie control parameters.
 */
# define MAXFRAME	40
static time Mtimes[MAXFRAME];
static int CurrentFrame;
static int MovieSlot = -1;
static int Nframes;
static int Rate;

/*
 * Forward definitions.
 */
# ifdef __STDC__
	static Widget mc_MWCreate (int, Widget, XtAppContext);
	static bool mc_SetupParams (void);
	static void mc_SetStatus (char *);
	static bool mc_GetFrameTimes (time *, int);
	static void mc_MovieRun ();
	static void mc_MovieStop ();
	static void mc_MovieRT ();
	static void mc_GenFrames (void);
	static void mc_DoNextFrame (void);
	static void mc_NextFrame (void);
# else
	static bool mc_SetupParams ();
	static void mc_SetStatus ();
	static bool mc_GetFrameTimes ();
	static Widget mc_MWCreate ();
	static void mc_MovieRun ();
	static void mc_MovieStop ();
	static void mc_MovieRT ();
	static void mc_GenFrames ();
	static void mc_NextFrame ();
	static void mc_DoNextFrame ();
# endif





void
mc_DefMovieWidget ()
/*
 * Hook the movie widget into UI.
 */
{
	uw_def_widget ("movie", "Movie control", mc_MWCreate, 0, 0);
}




static Widget
mc_MWCreate (junk, parent, appc)
int junk;
Widget parent;
XtAppContext appc;
/*
 * Create the movie widget.
 */
{
	Widget form, w, above, label;
	Arg args[20];
	int n;
	time t;
/*
 * Load some defaults.
 */
	if (! pda_Search (Pd, "global", "movie-minutes", NULL, Minutes, 
			SYMT_STRING))
		strcpy (Minutes, "30");
	if (! pda_Search (Pd, "global", "movie-end-time", NULL, Endt, 
			SYMT_STRING))
	{
		tl_GetTime (&t);
		ud_format_date (Endt, &t, UDF_FULL);
	}
	if (! pda_Search (Pd, "global", "frame-rate", NULL, Frate,SYMT_STRING))
		strcpy (Frate, "2");

/*
 * Create a form widget to hold everything.
 */
	n = 0;
 	XtSetArg (args[n], XtNdefaultDistance, 5); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	form = XtCreateManagedWidget ("movieform", formWidgetClass, parent,
		args, n);
/*
 * The label which holds our title.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNlabel, "Movie Control:"); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	above = w = XtCreateManagedWidget ("movieLabel", labelWidgetClass,
		form, args, n);
/*
 * The movie control buttons.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNlabel, "Run"); n++;
	w = XtCreateManagedWidget ("movieRun", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, mc_MovieRun, 0);
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNlabel, "Stop"); n++;
	w = XtCreateManagedWidget ("movieStop", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, mc_MovieStop, 0);
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNlabel, "Real Time"); n++;
	w = XtCreateManagedWidget ("movieRT", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, mc_MovieRT, 0);
/*
 * Next line: times.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Movie for"); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	label = w = XtCreateManagedWidget ("MovieFor", labelWidgetClass, form,
		args, n);
/*
 * Movie minutes.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNdisplayPosition, 0); n++;
	XtSetArg (args[n], XtNinsertPosition, 0); n++;
	XtSetArg (args[n], XtNlength, ATSLEN); n++;
	XtSetArg (args[n], XtNresize, XawtextResizeNever); n++;
	XtSetArg (args[n], XtNwidth, 50); n++;
	XtSetArg (args[n], XtNheight, 20); n++;
	XtSetArg (args[n], XtNstring, Minutes); n++;
	XtSetArg (args[n], XtNtype, XawAsciiString); n++;
	XtSetArg (args[n], XtNuseStringInPlace, True); n++;
	XtSetArg (args[n], XtNleftMargin, 5); n++;
	XtSetArg (args[n], XtNeditType, XawtextAppend); n++;
	w = XtCreateManagedWidget ("movieMin", asciiTextWidgetClass, form,
		args, n);
/*
 * More label.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "min. ending at"); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	w = XtCreateManagedWidget ("Movieend", labelWidgetClass, form, args,n);
/*
 * End time.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNdisplayPosition, 0); n++;
	XtSetArg (args[n], XtNinsertPosition, 0); n++;
	XtSetArg (args[n], XtNlength, ATSLEN); n++;
	XtSetArg (args[n], XtNresize, XawtextResizeNever); n++;
	XtSetArg (args[n], XtNwidth, 140); n++;
	XtSetArg (args[n], XtNheight, 20); n++;
	XtSetArg (args[n], XtNstring, Endt); n++;
	XtSetArg (args[n], XtNtype, XawAsciiString); n++;
	XtSetArg (args[n], XtNuseStringInPlace, True); n++;
	XtSetArg (args[n], XtNleftMargin, 5); n++;
	XtSetArg (args[n], XtNeditType, XawtextAppend); n++;
	w = XtCreateManagedWidget ("movieendt", asciiTextWidgetClass, form,
		args, n);
/*
 * Next line: frame rate.
 */
	above = label;
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Frames/second:"); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	label = w = XtCreateManagedWidget ("Moviefr", labelWidgetClass, form,
		args, n);
/*
 * The frame rate text widget.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNdisplayPosition, 0); n++;
	XtSetArg (args[n], XtNinsertPosition, 0); n++;
	XtSetArg (args[n], XtNlength, ATSLEN); n++;
	XtSetArg (args[n], XtNresize, XawtextResizeNever); n++;
	XtSetArg (args[n], XtNwidth, 140); n++;
	XtSetArg (args[n], XtNheight, 20); n++;
	XtSetArg (args[n], XtNstring, Frate); n++;
	XtSetArg (args[n], XtNtype, XawAsciiString); n++;
	XtSetArg (args[n], XtNuseStringInPlace, True); n++;
	XtSetArg (args[n], XtNleftMargin, 5); n++;
	XtSetArg (args[n], XtNeditType, XawtextAppend); n++;
	w = XtCreateManagedWidget ("moviefr", asciiTextWidgetClass, form,
		args, n);
/*
 * The status line:
 */
	above = label;
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Status"); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	label = w = XtCreateManagedWidget ("Moviest", labelWidgetClass, form,
		args, n);
/*
 * The actual status.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "--"); n++;
	XtSetArg (args[n], XtNwidth, 300); n++;
	XtSetArg (args[n], XtNresize, False); n++;
	XtSetArg (args[n], XtNjustify, XtJustifyLeft); n++;
	StatusLabel = XtCreateManagedWidget ("movieStatus", labelWidgetClass,
		form, args,n);

	return (form);
}





static void
mc_MovieRun ()
/*
 * Run the movie.
 */
{
	float sec;
/*
 * Figure out our parameters.
 */
	mc_SetStatus ("Initializing");
	if (! mc_SetupParams ())
	{
		msg_ELog (EF_DEBUG, "SetupParams failure");
		return;
	}
/*
 * If everything worked until now, cancel any current timer events.
 */
	tl_AllCancel ();
/*
 * Generate each movie frame.
 */
	mc_GenFrames ();
/*
 * Display the first frame.
 */
	mc_SetStatus ("Running");
	CurrentFrame = 0;
	mc_NextFrame ();
/*
 * Start the timer to do the rest.
 */
	sec = 1.0/(float) Rate;
	MovieSlot = tl_AddRelativeEvent (mc_NextFrame, 0, (int) (sec*INCFRAC),
		(int) (sec*INCFRAC));
}





static bool
mc_SetupParams ()
/*
 * Set up everything for movie mode.
 */
{
	int minutes;
	union usy_value v;
/*
 * Figure out what is in the control widget now.
 */
	if (! sscanf (Minutes, "%d", &minutes))
	{
		mc_SetStatus ("Unable to understand MINUTES value");
		return (FALSE);
	}
	if (! uit_parse_date (Endt, &v, FALSE))
	{
		mc_SetStatus ("Unable to understand end time");
		return (FALSE);
	}
	if (! sscanf (Frate, "%d", &Rate))
	{
		mc_SetStatus ("Unable to understand frame rate");
		return (FALSE);
	}
/*
 * Store these values in the PD.
 */
	pd_Store (Pd, "global", "movie-minutes", &minutes, SYMT_INT);
	pd_Store (Pd, "global", "movie-end-time", &v.us_v_date, SYMT_DATE);
	pd_Store (Pd, "global", "frame-rate", &Rate, SYMT_INT);
/*
 * Now calculate our frame times.
 */
	if (! mc_GetFrameTimes (&v.us_v_date, minutes))
		return (FALSE);
	return (TRUE);
}




static void
mc_GenFrames ()
/*
 * Generate all our movie frames.
 */
{
	int f;
	Arg args[2];

	mc_SetStatus ("Generating frames...");
/*
 * Make sure there are enough movie frames.  Also invalidate the cache
 * for now.  The latter is really unnecessary, if the cache did LRU
 * replacement.  Soon.
 */
	if (FrameCount < Nframes)
	{
		FrameCount = Nframes;
		XtSetArg (args[0], XtNframeCount, Nframes);
		XtSetValues (Graphics, args, ONE);
	}
	fc_InvalidateCache ();
/*
 * Now plot each frame, thus loading it into the cache.
 */
	for (f = 0; f < Nframes; f++)
	{
		msg_ELog (EF_DEBUG, "Gen frame at %d %d", Mtimes[f].ds_yymmdd,
			Mtimes[f].ds_hhmmss);
		PlotTime = Mtimes[f];
		px_PlotExec ("global");
	}
}




static bool
mc_GetFrameTimes (end, minutes)
time *end;
int minutes;
/*
 * Calculate the frame times for this movie.
 */
{
	char ctrigger[80];
	int trigger, incr, f, seconds;
	time t;
/*
 * Find our trigger condition.  This code for now assumes a time trigger
 * only.  That will eventually have to change.
 */
	if (! pda_Search (Pd, "global", "trigger", 0, ctrigger, SYMT_STRING) ||
		! (trigger = pc_TimeTrigger (ctrigger)))
	{
		mc_SetStatus ("Missing or bad TRIGGER");
		return (FALSE);
	}
/*
 * Now fix up the plot time.
 */
	seconds = (end->ds_hhmmss/10000)*60*60 +
		  ((end->ds_hhmmss/100) % 100)*60 + (end->ds_hhmmss % 100);
	seconds -= seconds % trigger;
	end->ds_hhmmss = (seconds/3600)*10000 + ((seconds/60) % 60)*100 +
				seconds % 60;
/*
 * Figure out how many frames we have.
 */
	Nframes = (minutes*60)/trigger + 1;
	if (Nframes > MAXFRAME)
	{
		mc_SetStatus ("Too many frames");
		return (FALSE);
	}
/*
 * Go through and figure out each frame time.
 */
	incr = (trigger/60)*100 + (trigger % 60);
	t = *end;
	for (f = Nframes - 1; f >= 0; f--)
	{
		Mtimes[f] = t;
		pmu_dsub (&t.ds_yymmdd, &t.ds_hhmmss, incr);
	}
	return (TRUE);
}





static void
mc_MovieStop ()
/*
 * Stop the movie.
 */
{
	mc_SetStatus ("Stopped");
	if (MovieSlot >= 0)
	{
		tl_Cancel (MovieSlot);
		MovieSlot = -1;
	}
}




static void
mc_MovieRT ()
/*
 * Stop the movie and go back to real time mode.
 */
{
	mc_SetStatus ("Inactive (real time)");
}





static void
mc_SetStatus (string)
char *string;
/*
 * Set the status line to this string.
 */
{
	Arg args[5];

	XtSetArg (args[0], XtNlabel, string);
	XtSetValues (StatusLabel, args, ONE);
	eq_sync ();
}





static void
mc_NextFrame ()
/*
 * Arrange to have the next frame displayed.
 */
{
	Eq_AddEvent (PDisplay, mc_DoNextFrame, NULL, 0, Bounce);
}



static void
mc_DoNextFrame ()
/*
 * Actually display the next frame in the movie.
 */
{
	PlotTime = Mtimes[CurrentFrame];
	if (++CurrentFrame >= Nframes)
		CurrentFrame = 0;
	px_PlotExec ("global");
}
