/*
 * Movie control functions.
 */
static char *rcsid = "$Id: MovieControl.c,v 1.3 1990-07-08 12:54:56 corbet Exp $";

# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/Cardinals.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Scrollbar.h>

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
static char Minutes[ATSLEN], Endt[ATSLEN], Frate[ATSLEN], Fskip[ATSLEN];
static Widget StatusLabel;	/* Where the current status goes	*/
static Widget Indicator;

/*
 * The actual movie control parameters.
 */
# define MAXFRAME	40
static time Mtimes[MAXFRAME];
static int CurrentFrame;
static int MovieSlot = -1;
static int Nframes = 0;
static int TimeSkip = 1;	/* Minutes between frames		*/
static int Rate;

/*
 * Forward definitions.
 */
# ifdef __STDC__
	static Widget mc_MWCreate (int, Widget, XtAppContext);
	static bool mc_SetupParams (void);
	static void mc_SetStatus (char *);
	static bool mc_GetFrameTimes (time *, int);
	void mc_MovieRun ();
	void mc_MovieStop ();
	void mc_MovieRT ();
	static void mc_GenFrames (void);
	static void mc_DoNextFrame (int *);
	static void mc_NextFrame (void);
	static void mc_SetIndicator (int);
	static void mc_ScrollCB (Widget, XtPointer, XtPointer);
	static void mc_Jump (Widget, XtPointer, XtPointer);
	static void mc_GenNFrame (int *);
# else
	static bool mc_SetupParams ();
	static void mc_SetStatus ();
	static bool mc_GetFrameTimes ();
	static Widget mc_MWCreate ();
	void mc_MovieRun ();
	void mc_MovieStop ();
	void mc_MovieRT ();
	static void mc_GenFrames ();
	static void mc_NextFrame ();
	static void mc_DoNextFrame ();
	static void mc_SetIndicator ();
	static void mc_ScrollCB ();
	static void mc_Jump ();
	static void mc_GenNFrame ();
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
	if (! pda_Search (Pd, "global", "frame-skip", NULL, Fskip,SYMT_STRING))
		strcpy (Fskip, "3");
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
	XtSetArg (args[n], XtNwidth, 30); n++;
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
	XtSetArg (args[n], XtNwidth, 30); n++;
	XtSetArg (args[n], XtNheight, 20); n++;
	XtSetArg (args[n], XtNstring, Frate); n++;
	XtSetArg (args[n], XtNtype, XawAsciiString); n++;
	XtSetArg (args[n], XtNuseStringInPlace, True); n++;
	XtSetArg (args[n], XtNleftMargin, 5); n++;
	XtSetArg (args[n], XtNeditType, XawtextAppend); n++;
	w = XtCreateManagedWidget ("moviefr", asciiTextWidgetClass, form,
		args, n);
/*
 * More label.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Frame skip: "); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	w = XtCreateManagedWidget ("Movieskpl", labelWidgetClass, form,
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
	XtSetArg (args[n], XtNwidth, 30); n++;
	XtSetArg (args[n], XtNheight, 20); n++;
	XtSetArg (args[n], XtNstring, Fskip); n++;
	XtSetArg (args[n], XtNtype, XawAsciiString); n++;
	XtSetArg (args[n], XtNuseStringInPlace, True); n++;
	XtSetArg (args[n], XtNleftMargin, 5); n++;
	XtSetArg (args[n], XtNeditType, XawtextAppend); n++;
	w = XtCreateManagedWidget ("moviefs", asciiTextWidgetClass, form,
		args, n);
/*
 * More label.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "minutes."); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	w = XtCreateManagedWidget ("Movieskpm", labelWidgetClass, form,
		args, n);
/*
 * The status line:
 */
	above = label;
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Status:"); n++;
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
/*
 * Also a scrollbar to indicate what is happening.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, label); n++;
	XtSetArg (args[n], XtNfromVert, label); n++;
	XtSetArg (args[n], XtNwidth, 300); n++;
	XtSetArg (args[n], XtNsensitive, True); n++;
	XtSetArg (args[n], XtNorientation, XtorientHorizontal); n++;
	Indicator = XtCreateManagedWidget ("movieSB", scrollbarWidgetClass,
		form, args, n);
	XtAddCallback (Indicator, XtNscrollProc, mc_ScrollCB, 0);
	XtAddCallback (Indicator, XtNjumpProc, mc_Jump, 0);
	return (form);
}





void
mc_MovieRun ()
/*
 * Run the movie.
 */
{
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
 * If everything worked until now, cancel any current timer events and drop
 * out of real time mode.
 */
	tl_AllCancel ();
	PlotMode = History;
/*
 * Start the process of generating the movie frames.
 */
	fc_MarkFrames (Mtimes, Nframes);
	mc_GenFrames ();
}





static bool
mc_SetupParams ()
/*
 * Set up everything for movie mode.
 */
{
	int minutes;
	union usy_value v;
	char fskipk[ATSLEN];
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
	if (! sscanf (Fskip, "%d", &TimeSkip))
	{
		mc_SetStatus ("Unable to understand frame skip");
		return (FALSE);
	}
/*
 * Store these values in the PD.
 */
	pd_Store (Pd, "global", "movie-minutes", (char *) &minutes, SYMT_INT);
	pd_Store (Pd, "global", "movie-end-time", (char *) &v.us_v_date,
			SYMT_DATE);
	pd_Store (Pd, "global", "frame-rate", (char *) &Rate, SYMT_INT);
	sprintf (fskipk, "%dm", TimeSkip);
	pd_Store (Pd, "global", "frame-skip", fskipk, SYMT_STRING);
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
		pd_Store (Pd, "global", "time-frames", (char *)&Nframes,
			SYMT_INT);
	}
	/* fc_InvalidateCache (); */
/*
 * Start the process of generating the frames.  This is done through the
 * event queue, allowing for event processing and stopping the whole process
 * if necessary.
 */
	f = 0;
	Eq_AddEvent (PDisplay, mc_GenNFrame, &f, sizeof (int), Override);
}





static void
mc_GenNFrame (which)
int *which;
/*
 * Generate the WHICHth movie frame.
 */
{
	char msg[50];
	int next;
	float sec;
/*
 * Update the message.
 */
	sprintf (msg, "Generating frames: %d/%d", *which, Nframes);
	mc_SetStatus (msg);
/*
 * Do the frame.
 */
	CurrentFrame = *which;
	mc_SetIndicator (*which);
	PlotTime = Mtimes[*which];
	px_PlotExec ("global");
/*
 * If we're not done, arrange to have the next frame done.
 */
	if ((next = *which + 1) < Nframes)
	{
		Eq_AddEvent (PDisplay, mc_GenNFrame, &next, sizeof (int),
			Augment);
		return;
	}
/*
 * Otherwise now it's time to start the movie sequence process.
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
mc_GetFrameTimes (end, minutes)
time *end;
int minutes;
/*
 * Calculate the frame times for this movie.
 */
{
	int incr, f, seconds;
	time t;
/*
 * Now fix up the plot time.
 */
	seconds = (end->ds_hhmmss/10000)*60*60 +
		  ((end->ds_hhmmss/100) % 100)*60 + (end->ds_hhmmss % 100);
	seconds -= seconds % 60;
	end->ds_hhmmss = (seconds/3600)*10000 + ((seconds/60) % 60)*100 +
				seconds % 60;
/*
 * Figure out how many frames we have.
 */
	Nframes = minutes/TimeSkip + 1;
	if (Nframes > MAXFRAME)
	{
		mc_SetStatus ("Too many frames");
		return (FALSE);
	}
/*
 * Go through and figure out each frame time.
 */
	incr = TimeSkip*100;
	t = *end;
	for (f = Nframes - 1; f >= 0; f--)
	{
		Mtimes[f] = t;
		pmu_dsub (&t.ds_yymmdd, &t.ds_hhmmss, incr);
	}
	return (TRUE);
}





void
mc_MovieStop ()
/*
 * Stop the movie.
 */
{
	mc_SetStatus ("Stopped (history mode)");
/*
 * If there are timer events active, get rid of them.
 */
	if (MovieSlot >= 0)
	{
		tl_Cancel (MovieSlot);
		MovieSlot = -1;
	}
/*
 * Get rid of any event queue entries relating to frame generation.
 */
	Eq_ZapProc (PDisplay, mc_GenNFrame);
/*
 * Throw the system into history mode, showing the current frame.
 */
	HistoryMode (&Mtimes[CurrentFrame]);
}




void
mc_MovieRT ()
/*
 * Stop the movie and go back to real time mode.
 */
{
	mc_MovieStop ();
	mc_SetStatus ("Inactive (real time)");
	pd_Store (Pd, "global", "plot-mode", "real-time", SYMT_STRING);
	pc_PlotHandler ();
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
	xtEvent (0);	/* XXX */
}





static void
mc_NextFrame ()
/*
 * Arrange to have the next frame displayed.
 */
{
	Eq_AddEvent (PDisplay, mc_DoNextFrame, &CurrentFrame, sizeof (int), 
		Bounce);
	if (++CurrentFrame >= Nframes)
		CurrentFrame = 0;
}



static void
mc_DoNextFrame (frame)
int *frame;
/*
 * Actually display the next frame in the movie.
 */
{
	PlotTime = Mtimes[*frame];
	mc_SetIndicator (*frame);
	px_PlotExec ("global");
}






static void
mc_SetIndicator (frame)
int frame;
/*
 * Set the indicator to show that we're on this frame.
 */
{
	float width = 1.0/Nframes;

	XawScrollbarSetThumb (Indicator, frame*width, width);
	eq_sync ();
}




void
mc_Dial (motion)
int motion;
/*
 * Deal with dial motion.
 */
{
/*
 * Insist on having some stuff around.
 */
	if (Nframes <= 0)
	{
		msg_ELog (EF_PROBLEM, "No movie frames saved!");
		return;
	}
/*
 * Do the change.
 */
	if ((CurrentFrame += motion) < 0)
		CurrentFrame += Nframes;
	else if (CurrentFrame >= Nframes)
		CurrentFrame -= Nframes;
	Eq_AddEvent (PDisplay, mc_DoNextFrame, &CurrentFrame, sizeof (int), 
		Augment);
}





static void
mc_ScrollCB (sb, junk, position)
Widget sb;
XtPointer junk, position;
/*
 * The scrollbar callback.
 */
{
	int ipos = (int) position;

	mc_Dial (ipos < 0 ? 1 : -1);
}




static void
mc_Jump (sb, junk, pct)
Widget sb;
XtPointer junk, pct;
/*
 * Implement jump scrolling.
 */
{
	int frame = Nframes * (* (float *) pct);
/*
 * Make sure something exists.
 */
	if (Nframes <= 0)
	{
		msg_ELog (EF_PROBLEM, "No movie information saved");
		return;
	}
/*
 * Range checking.
 */
	if (frame < 0)
		frame = 0;
	else if (frame >= Nframes)
		frame = Nframes - 1;
/*
 * Redisplay only if they have moved far enough to get a different frame.
 */
	if (frame != CurrentFrame)
	{
		CurrentFrame = frame;
		Eq_AddEvent (PDisplay, mc_DoNextFrame, &CurrentFrame,
			sizeof (int), Override);
	}
}
