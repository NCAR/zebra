/*
 * Movie control functions.
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
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/Cardinals.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Scrollbar.h>

# include <RdssMenu.h>
# include <X11/Xaw/SmeLine.h>
# include <X11/Xaw/SmeBSB.h>
# include <X11/Xaw/MenuButton.h>

# include <ui.h>
# include <ui_date.h>

# include <defs.h>
# include <message.h>
# include <pd.h>
# include <timer.h>
# include <DataStore.h>
# include <GraphicsW.h>
# include "GraphProc.h"
# include "EventQueue.h"
# include "ActiveArea.h"

RCSID ("$Id: MovieControl.c,v 2.33 2001-11-30 21:29:28 granger Exp $")

# define ATSLEN		80	/* Length for AsciiText strings		*/
# define FLEN 		60	/* Length of a field string		*/
# define DAPERCENT      .6666	/* Percent of Frame Skip that must have */
				/* elapsed before newly available data  */
	 			/* will result in regenerating frames.  */
# define MOVIE_NAME	"movie" /* Name of the movie controller widget  */

/*
 * Time units and table with unit names and their scaling factors
 */
typedef enum 
{
	tu_seconds = 0, tu_minutes, tu_hours, tu_days
} t_units;

struct _tu_table
{
	char	*units;
	int	scale;
} TUTable[] =
{
	{"seconds", 1},
	{"minutes", 60},
	{"hours", 60*60},
	{"days", 60*60*24}
};

/*
 * Globals.
 */
static int	ParamsLoaded = FALSE;
static char 	MovieLen[ATSLEN], Endt[ATSLEN], Frate[ATSLEN], Fskip[ATSLEN];
static t_units	TimeUnits = tu_minutes;
static Widget 	StatusLabel = NULL;	/* Where the current status goes*/
static Widget 	Indicator = NULL;
static Widget 	WEndt, WMovieLen, WUnits1, WUnits2, WFrate, WFskip;
/*
 * The actual movie control parameters.
 */
static ZebTime 	Mtimes[NCACHE];		/* The time of each frame	*/
static int 	MovieSlot = -1;
static int 	Nframes = 0;		/* Number of frames in the movie*/
static int 	TimeSkip = 1;		/* Time between frames		*/
static int 	Rate;			/* Display Rate frames/second	*/
static int 	OldFrameCount = 0;	/* FrameCount before movie  	*/
static zbool 	Now;			/* Should endtime track realtime*/
static zbool 	ReGenFrame = FALSE;
static zbool 	Notification = FALSE;
static ZebTime 	NotTime;
static char	EndTime[ATSLEN];
static int 	CurrentFrame;
static int 	DisplayedFrame;

/*
 * If a movie is running, are we the ones controlling it?
 */
static zbool	MyMovie = FALSE;

/*
 * CurrentFrame is the frame we are supposed to be actually looking at.
 * DisplayedFrame is what we really *are* looking at.  They diverge when
 * multiple update events happen before we can actually update the screen,
 * due to system load.
 */
/*
 *  Frame pregeneration globals.
 */
static zbool Pregenerate = FALSE;	/* Should frames be pregenerated*/	
static char Field1[FLEN], Field2[FLEN]; /* Fields to be pregenerated	*/	
static char PGComp[FLEN];		/* Component which wants pregen.*/


/*
 * Forward definitions.
 */
Widget	mc_MWCreate FP ((int, Widget, XtAppContext));
static zbool	mc_SetupParams FP ((void));
static void	mc_SetStatus FP ((char *));
static zbool	mc_GetFrameTimes FP ((ZebTime *, int));
static void	mc_ReGetFrameTimes FP ((ZebTime *));
void		mc_MovieRun FP (());
void		mc_MovieStop FP (());
void		mc_MovieRT FP (());
void		mc_ResetFrameCount FP ((void));
static void	mc_GenFrames FP ((void));
static void	mc_DoNextFrame FP ((int *));
static void	mc_NextFrame FP ((void));
static void	mc_SetIndicator FP ((int));
static void	mc_ScrollCB FP ((Widget, XtPointer, XtPointer));
static void	mc_Jump FP ((Widget, XtPointer, XtPointer));
static void	mc_GenNFrame FP ((int *));
static void	mc_ReGenFrames FP ((void));
static void	mc_ReGenFramesDS FP ((void));
static void	mc_SetupPreGen FP ((void));
static ZebTime	mc_FixTime FP ((ZebTime));
static void	mc_Notification FP ((PlatformId, int, ZebTime *));
static void	mc_MovieDismiss ();
static void	mc_ChangeTimeUnits FP ((Widget, XtPointer, XtPointer));
static void	mc_LoadParams FP ((void));
static void	mc_UpdateWidgets FP ((void));


void
mc_DefMovieWidget ()
/*
 * Hook the movie widget into UI.
 */
{
	stbl vtbl = usy_g_stbl ("ui$variable_table");

	uw_def_widget (MOVIE_NAME, "Movie Control:", mc_MWCreate, 0, 0);
	uw_NoHeader (MOVIE_NAME);
	usy_c_indirect (vtbl, "movietime", Endt, SYMT_STRING, ATSLEN);
	usy_c_indirect (vtbl, "movielen", MovieLen, SYMT_STRING, ATSLEN);
}


static void
mc_LoadParams ()
/*
 * Load the movie params from the PD.
 */
{
	char units[128];
	ZebTime t;
	int i;

	TimeUnits = tu_minutes;
	if (pda_Search (Pd, "global", "movie-length", NULL, MovieLen, 
			SYMT_STRING) &&
	    pda_Search (Pd, "global", "movie-units", NULL, units,
			SYMT_STRING))
	{
		/* Use the new plot parameters to derive length and units */
		for (i = 0; i < sizeof(TUTable)/sizeof(TUTable[0]); ++i)
		{
			if (units[0] && 
			    !strncmp(units, TUTable[i].units, strlen(units)))
			{
				TimeUnits = (t_units) i;
				break;
			}
		}
		msg_ELog (EF_DEBUG, "found movie-length %s and movie-units %s",
			  MovieLen, TUTable[TimeUnits].units);
	}
	else if (! pda_Search (Pd, "global", "movie-minutes", NULL, MovieLen, 
			       SYMT_STRING))
	{
		strcpy (MovieLen, "30");
	}

	if (! pda_Search (Pd, "global", "movie-end-time", NULL, Endt, 
			SYMT_STRING))
	{
		tl_Time (&t);
		TC_EncodeTime (&t, TC_Full, Endt);
	}
	if (! pda_Search (Pd, "global", "frame-rate", NULL, Frate,SYMT_STRING))
		strcpy (Frate, "2");
	if (! pda_Search (Pd, "global", "frame-skip", NULL, Fskip,SYMT_STRING))
		strcpy (Fskip, "3");
	ParamsLoaded = TRUE;
}



Widget
mc_MWCreate (junk, parent, appc)
int junk;
Widget parent;
XtAppContext appc;
/*
 * Create the movie widget.
 */
{
	Widget form, w, above, label, tumenu, entry;
	Arg args[20];
	char string[20];
	int n, i;
/*
 * Load some defaults.
 */
	mc_LoadParams ();
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
 * Help, Run, Stop, and Real Time buttons
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNlabel, "Help"); n++;
	w = XtCreateManagedWidget ("movieHelp", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, HelpCallback, 
		       (XtPointer)GP_HELP_MOVIE);
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
	XtAddCallback (w, XtNcallback, (XtCallbackProc) mc_MovieStop, 0);
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNlabel, "Real Time"); n++;
	w = XtCreateManagedWidget ("movieRT", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, mc_MovieRT, 0);
/*
 * Time units menu button.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Time Units"); n++;
	tumenu = XtCreatePopupShell ("TUMenu", rdssMenuWidgetClass, Top,
				     args, n);

	XtCreateManagedWidget ("TUMLine", smeLineObjectClass, tumenu, NULL, 0);

	for (i = 0; i < sizeof (TUTable) / sizeof (struct _tu_table); i++)
	{
		n = 0;
		XtSetArg (args[n], XtNlabel, TUTable[i].units); n++;
		entry = XtCreateManagedWidget (TUTable[i].units, 
					       smeBSBObjectClass, tumenu, 
					       args, n);
		XtAddCallback (entry, XtNcallback, 
			       (XtCallbackProc) mc_ChangeTimeUnits, 
			       (XtPointer)(long) i);
	}

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNlabel, "Time Units"); n++;
	XtSetArg (args[n], XtNmenuName, "TUMenu"); n++;
	w = XtCreateManagedWidget ("movieTUnits", menuButtonWidgetClass, form,
		args, n);
	if (! Dock)
	{
	    /*
	     * Dismiss button.
	     */
	    n = 0;
	    XtSetArg (args[n], XtNfromHoriz, w); n++;
	    XtSetArg (args[n], XtNfromVert, NULL); n++;
	    XtSetArg (args[n], XtNlabel, "Dismiss"); n++;
	    w = XtCreateManagedWidget ("movieDismiss", commandWidgetClass, 
				       form, args, n);
	    XtAddCallback (w, XtNcallback, mc_MovieDismiss, 0);
	}
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
 * Movie length.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNdisplayPosition, 0); n++;
	XtSetArg (args[n], XtNinsertPosition, 0); n++;
	XtSetArg (args[n], XtNlength, ATSLEN); n++;
	XtSetArg (args[n], XtNresize, XawtextResizeNever); n++;
	XtSetArg (args[n], XtNwidth, 30); n++;
	XtSetArg (args[n], XtNstring, MovieLen); n++;
	XtSetArg (args[n], XtNtype, XawAsciiString); n++;
	XtSetArg (args[n], XtNuseStringInPlace, True); n++;
	XtSetArg (args[n], XtNleftMargin, 5); n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit); n++;
	WMovieLen = XtCreateManagedWidget ("movieLen", asciiTextWidgetClass,
		form, args, n);
/*
 * More label.
 */
	sprintf (string, "%s, every", TUTable[TimeUnits].units);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, WMovieLen); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, string); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	w = WUnits1 = XtCreateManagedWidget ("Movieend", labelWidgetClass, 
					     form, args, n);
/*
 * The frame skip text widget.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNdisplayPosition, 0); n++;
	XtSetArg (args[n], XtNinsertPosition, 0); n++;
	XtSetArg (args[n], XtNlength, ATSLEN); n++;
	XtSetArg (args[n], XtNresize, XawtextResizeNever); n++;
	XtSetArg (args[n], XtNwidth, 30); n++;
	XtSetArg (args[n], XtNstring, Fskip); n++;
	XtSetArg (args[n], XtNtype, XawAsciiString); n++;
	XtSetArg (args[n], XtNuseStringInPlace, True); n++;
	XtSetArg (args[n], XtNleftMargin, 5); n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit); n++;
	WFskip = XtCreateManagedWidget ("moviefs", asciiTextWidgetClass, form,
		args, n);
/*
 * More label.
 */
	sprintf (string, "%s,", TUTable[TimeUnits].units);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, WFskip); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, string); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	w = WUnits2 = XtCreateManagedWidget ("Movieskpm", labelWidgetClass, 
					     form, args, n);
/*
 * Next line: end time and frame rate.
 */
	above = label;

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "ending at"); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	w = XtCreateManagedWidget ("Movieat", labelWidgetClass, form,
		args, n);
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
	XtSetArg (args[n], XtNstring, Endt); n++;
	XtSetArg (args[n], XtNtype, XawAsciiString); n++;
	XtSetArg (args[n], XtNuseStringInPlace, True); n++;
	XtSetArg (args[n], XtNleftMargin, 5); n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit); n++;
	WEndt = w = XtCreateManagedWidget ("movieendt", asciiTextWidgetClass,
		form, args, n);
/*
 * Frame rate.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, WEndt); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "    Frames/Second:"); n++;
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
	XtSetArg (args[n], XtNstring, Frate); n++;
	XtSetArg (args[n], XtNtype, XawAsciiString); n++;
	XtSetArg (args[n], XtNuseStringInPlace, True); n++;
	XtSetArg (args[n], XtNleftMargin, 5); n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit); n++;
	WFrate = XtCreateManagedWidget ("moviefr", asciiTextWidgetClass, form,
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


static void
mc_DoOneWidget (w, string)
Widget w;
char *string;
/*
 * Update a single text widget.
 */
{
	Arg args[5];
	int n = 0;

	XtSetArg (args[n], XtNstring, string); n++;
	XtSetArg (args[n], XtNtype, XawAsciiString); n++;
	XtSetArg (args[n], XtNuseStringInPlace, True); n++;
	XtSetValues (w, args, n);
}


static void
mc_UpdateWidgets ()
/*
 * Make the widgets reflect reality, if they've been created.
 */
{
	if (StatusLabel)
	{
		mc_DoOneWidget (WEndt, Endt);
		mc_DoOneWidget (WMovieLen, MovieLen);
		mc_DoOneWidget (WFrate, Frate);
		mc_DoOneWidget (WFskip, Fskip);
	}
}



static void
mc_MovieDismiss ()
/*
 * Popdown the movie controller widget through UI
 */
{
/*
 * Go ahead and stop the movie.  There are situations where it might make
 * sense to keep things running, but we risk less user confusion this
 * way.
 */
	mc_MovieStop ();
	mc_SetStatus ("Inactive (real time).");
/*
 *  Reset the FrameCount to its value before the movie started.
 */
	fc_UnMarkFrames();
	Eq_AddEvent (PWhenever, mc_ResetFrameCount, 0, 0, Bounce);
/*
 * Ah yes, also get rid of the widget.
 */
	uw_popdown (MOVIE_NAME);
}



void
mc_MovieRun ()
/*
 * Run the movie.
 */
{
	char trigger[200];
	PlatformId pid;

	if (! ParamsLoaded)
		mc_LoadParams();
/*
 * If something else has a movie going, then complain.
 */
	if (MovieMode && ! MyMovie)
	{
		mc_SetStatus ("Another movie is already going...");
		return;
	}
/*
 * Figure out our parameters.
 */
	Now = FALSE;
	Pregenerate = FALSE;
	mc_SetStatus ("Initializing.");
	if (! mc_SetupParams ())
	{
		msg_ELog (EF_PROBLEM, "SetupParams failure.");
		return;
	}
	mc_SetupPreGen();
/*
 * If everything worked until now, cancel any current timer events and drop
 * out of real time mode.
 */
	Eq_ZapProc (PWhenever, mc_ResetFrameCount);
	tl_AllCancel ();
	ds_CancelNotify ();
	PlotMode = History;
	MovieMode = TRUE;
	MyMovie = TRUE;
	pd_Store (Pd, "global", "plot-mode", "history", SYMT_STRING);
	pd_Store (Pd, "global", "movie-mode", (char *) &MovieMode, SYMT_BOOL);
	Eq_AddEvent (PWhenever, eq_ReturnPD, 0, 0, Bounce);
	mc_UpdateWidgets ();
/*
 *  If the global trigger is a platform, get ready to create new frames
 *  whenever data arrives.  Used in 'now' mode.
 */
	if(Now && ! ReGenFrame)
	{
		pda_Search(Pd, "global", "trigger",0,trigger,SYMT_STRING);
		if((pid = ds_LookupPlatform(trigger)) != BadPlatform)
			ds_RequestNotify(pid, 0, mc_Notification);
	}
/*
 * Start the process of generating the movie frames.
 */
	fc_UnMarkFrames();
	fc_MarkFrames (Mtimes, Nframes);
	mc_GenFrames ();
}


static zbool
mc_SetupParams ()
/*
 * Set up everything for movie mode.
 */
{
	int movielen;
	UItime temptime;
	ZebTime t, t1, zt;
	union usy_value v;
	char trigger[200];
	PlatformId pid;
/*
 * Figure out what is in the control widget now.
 */
	if(strcmp(Endt, "now") == 0)
	{
		if (PostProcMode)
		{
			TC_EncodeTime (&PostProcTime, TC_Full, EndTime);
			TC_ZtToUI (&PostProcTime, &temptime);
			v.us_v_date = temptime;
		}
		else
		{
			Now = TRUE;
			tl_Time (&t);
			pda_Search(Pd, "global", "trigger", 0, trigger, 
				SYMT_STRING);
			if((pid = ds_LookupPlatform(trigger)) != BadPlatform)
			{
				if(! ds_DataTimes(pid, &t, 1, DsBefore, &t1))
				{
				    mc_SetStatus ("Unable to find any data");
				    return(FALSE);
				}
				ReGenFrame = FALSE;
				TC_EncodeTime (&t1, TC_Full, EndTime);
				TC_ZtToUI (&t1, &temptime);
				v.us_v_date = temptime;
			}
			else	 
			{
				ReGenFrame = TRUE;
				TC_EncodeTime (&t, TC_Full, EndTime);
				TC_ZtToUI (&t, &temptime);
				v.us_v_date = temptime;
			}
		}
	}
	else 
	{
		strcpy(EndTime, Endt);
		if (! uit_parse_date (EndTime[0] == ' ' ? EndTime+1 : EndTime,
					&v, FALSE))
		{
			mc_SetStatus ("Unable to understand end time.");
			return (FALSE);
		}
	}
	msg_ELog(EF_DEBUG, "Now: %s.", Now ? "True" : "False");
	msg_ELog(EF_DEBUG, "EndTime: %s.", EndTime);
	if (! sscanf (MovieLen, "%d", &movielen))
	{
		mc_SetStatus ("Unable to understand MovieLen value.");
		return (FALSE);
	}
	msg_ELog (EF_DEBUG, "MovieLen %d %s", movielen, 
		  TUTable[TimeUnits].units);
	if (! sscanf (Frate, "%d", &Rate))
	{
		mc_SetStatus ("Unable to understand frame rate.");
		return (FALSE);
	}
	msg_ELog (EF_DEBUG, "Frame rate %d", Rate);
	if (! sscanf (Fskip, "%d", &TimeSkip))
	{
		mc_SetStatus ("Unable to understand frame skip.");
		return (FALSE);
	}
	msg_ELog (EF_DEBUG, "Frame skip %d", TimeSkip);
/*
 * Store these values in the PD.
 */
#ifdef notdef
	pd_Store (Pd, "global", "movie-minutes", (char *) &movielen, SYMT_INT);
#endif
	pd_Store (Pd, "global", "movie-length", (char *) &movielen, SYMT_INT);
	pd_Store (Pd, "global", "movie-units", TUTable[TimeUnits].units, 
		  SYMT_STRING);
	if (! strcmp (Endt, "now"))
		pd_Store (Pd, "global", "movie-end-time", "now", SYMT_STRING);
	else
		pd_Store (Pd, "global", "movie-end-time",
				(char *) &v.us_v_date, SYMT_DATE);
	pd_Store (Pd, "global", "frame-rate", (char *) &Rate, SYMT_INT);
	pd_Store (Pd, "global", "frame-skip", (char *) &TimeSkip, SYMT_INT);
/*
 * Now calculate our frame times.
 */
	TC_UIToZt (&v.us_v_date, &zt);
	if (! mc_GetFrameTimes (&zt, movielen))
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
 * Make sure there are enough movie frames.  If not, allocate some more, and
 * modify the PD to reflect this.
 */
	if (FrameCount < Nframes)
	{
		OldFrameCount = FrameCount;
		FrameCount = Nframes;
		fc_SetNumFrames(FrameCount);
		msg_ELog (EF_DEBUG, "Asking for %d frames", Nframes);
		XtSetArg (args[0], XtNframeCount, Nframes);
		XtSetValues (Graphics, args, ONE);
		pd_Store (Pd, "global", "time-frames", (char *)&Nframes,
			SYMT_INT);
		Eq_AddEvent (PWhenever, eq_ReturnPD, 0, 0, Bounce);
	}
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
	char msg[50], *info;
	int next;
	float sec;
/*
 * Update the message.
 */
	sprintf (msg, "Generating frame %d of %d.", *which + 1, Nframes);
	mc_SetStatus (msg);
/*
 * Do the frame.
 */
	CurrentFrame = *which;
	mc_SetIndicator (*which);
	PlotTime = Mtimes[*which];
	if(Pregenerate)
	{
		msg_ELog(EF_DEBUG, "Pregenerating %s.", Field2);
		pd_Store(Pd, PGComp, "field", Field2, SYMT_STRING);
		if(fc_LookupFrame(&PlotTime,&info) < 0)
		{
			aa_ResetAreas ();
			px_GlobalPlot(&PlotTime);
			fc_AddFrame (&PlotTime, DisplayFrame);
			An_DoSideAnnot ();
		}
		pd_Store(Pd, PGComp, "field", Field1, SYMT_STRING);
	}
	px_PlotExec ("global");
	fc_MarkFrames(Mtimes, Nframes);
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
	mc_SetStatus ("Running.");
	DisplayedFrame = CurrentFrame = Nframes + 1;
	mc_NextFrame ();
/*
 * Start the timer to do the rest.
 */
	sec = 1.0/(float) Rate;
	MovieSlot = tl_AddRelativeEvent (mc_NextFrame, 0, (int) (sec*INCFRAC),
		(int) (sec*INCFRAC));
	if(MovieSlot < 0)
		msg_ELog(EF_PROBLEM, "Movie won't start.");
}


static zbool
mc_GetFrameTimes (end, movielen)
ZebTime	*end;
int	movielen;
/*
 * Calculate the frame times for this movie.
 */
{
	int incr, f;
	ZebTime t;
	char msg[60];
/*
 * Now fix up the end time.
 */
	*end = mc_FixTime (*end);
/*
 * Figure out how many frames we have.
 */
	Nframes = movielen/TimeSkip + 1;
	if (Nframes > MaxFrames)
	{
		sprintf(msg,"Too many frames (%i), maximum is %i",
			Nframes, MaxFrames);
		mc_SetStatus (msg);
		return (FALSE);
	}
/*
 * Go through and figure out each frame time.
 */
	incr = TimeSkip * TUTable[TimeUnits].scale;
	t = *end;
	for (f = Nframes - 1; f >= 0; f--)
	{
		Mtimes[f] = t;
		t.zt_Sec -= incr;
	}
	return (TRUE);
}


static void
mc_ReGetFrameTimes (end)
ZebTime *end;
/*
 * Calculate the frame times for this movie.
 */
{
	int f;
/*
 * Now fix up the end time.
 */
	*end = mc_FixTime (*end);
/*
 * Go through and figure out each frame time.
 */
	for (f = 0; f <= Nframes - 2 ; f++)
	{
		Mtimes[f] = Mtimes[f+1];
	}
	Mtimes[Nframes - 1] = *end;
}


static ZebTime
mc_FixTime(zt)
ZebTime zt;
{
/*
 * Fix up the time.
 * 10/17/94 jc: I sure wish I knew *why* the time needs to be "fixed up".
 *		Certainly, modulo arithmetic on this sort of quantity seems
 *		a little weird.  I'm gonna zap it and see what breaks.
 */
# ifdef notdef
	int timestep = TUTable[TimeUnits].scale;
	zt.zt_Sec -= zt.zt_Sec % timestep;
	zt.zt_Sec += timestep;
# endif
	zt.zt_MicroSec = 0;
	return (zt);
}


void
mc_MovieStop ()
/*
 * Stop the movie.
 */
{
	if (! ParamsLoaded)
		mc_LoadParams ();
	mc_SetStatus ("Stopped (history mode).");
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
 *  Cancel the data available notification for the movies.
 */
	ds_CancelNotify();
	tl_AllCancel();
/*
 * Throw the system into history mode, showing the current frame, if any.
 */
	MovieMode = FALSE;
	MyMovie = FALSE;
	pd_Store (Pd, "global", "movie-mode", (char *) &MovieMode, SYMT_BOOL);

	if (Nframes > 0)
	{
	    if (CurrentFrame >= Nframes)
		CurrentFrame = Nframes - 1;
	    PlotMode = History;
	    PlotTime = Mtimes[CurrentFrame];
	    pd_Store(Pd, "global", "plot-time", (char *) &PlotTime, SYMT_DATE);
	    pd_Store(Pd, "global", "plot-mode", "history", SYMT_STRING);
	}
	Eq_AddEvent (PWhenever, eq_ReturnPD, 0, 0, Bounce);
}


void
mc_ResetFrameCount()
{
	char *info;
	Arg args[2];
	
	if(OldFrameCount > 0)
	{
	/*
	 * Sure would be nice if we could move the PlotTime frame down
	 * to slot zero so it would stay around.  Someday.
	 */
		FrameCount = OldFrameCount;
		fc_SetNumFrames(FrameCount);
		px_FixPlotTime(&PlotTime);
		DisplayFrame = DrawFrame = fc_LookupFrame (&PlotTime, &info);
		if(DisplayFrame < 0)
			px_PlotExec("global");	
		else
		{
			GWDrawInFrame(Graphics, DrawFrame);
			GWDisplayFrame(Graphics, DisplayFrame);
		/*
		 * Update the overlay times widget
		 */
			ot_SetString (info);
		}
		XtSetArg (args[0], XtNframeCount, FrameCount);
		XtSetValues (Graphics, args, ONE);
		pd_Store (Pd, "global", "time-frames", (char *) &FrameCount, 
			SYMT_INT);
	}
}


void
mc_MovieRT ()
/*
 * Stop the movie and go back to real time mode.
 */
{
	mc_MovieStop ();
	mc_SetStatus ("Inactive (real time).");
/*
 *  Reset the FrameCount to its value before the movie started.
 */
	fc_UnMarkFrames();
	Eq_AddEvent (PWhenever, mc_ResetFrameCount, 0, 0, Bounce);
/*
 *  Set the PlotMode to RealTime and return to regular plotting.
 */
	PlotMode = RealTime;
	pd_Store (Pd, "global", "plot-mode", "real-time", SYMT_STRING);
	Eq_AddEvent (PWhenever, eq_ReturnPD, 0, 0, Bounce);
	pc_PlotHandler ();
}


static void
mc_SetStatus (string)
char *string;
/*
 * Set the status line to this string.
 */
{
	if (StatusLabel)
	{
		Arg args[5];

		XtSetArg (args[0], XtNlabel, string);
		XtSetValues (StatusLabel, args, ONE);
		eq_sync ();
		xtEvent (0);	/* XXX */
	}
}


static void
mc_NextFrame ()
/*
 * Arrange to have the next frame displayed.
 */
{
/*
 * If we are out of sync -- the last update not yet done -- blow this one
 * off completely.
 */
	if (CurrentFrame != DisplayedFrame)
		return;
/*
 * Increment the frame count.  We allow it to go one over the number of frames
 * around, to implement a one-cycle pause at the end of the movie.  If the
 * endtime should track realtime (Now is TRUE) then regenerate the frames.
 */
	if (++CurrentFrame > Nframes)
	{
		CurrentFrame = 0;
		if(Now)
		{
			if(ReGenFrame) mc_ReGenFrames();
			if(Notification)
			{
				mc_ReGenFramesDS();
				Notification = FALSE;
			}
		}
	}
/*
 * Now schedule an update of the next frame.
 */
	Eq_AddEvent (PDisplay, mc_DoNextFrame, &CurrentFrame, sizeof (int), 
		Bounce);
}


static void
mc_DoNextFrame (frame)
int *frame;
/*
 * Actually display the next frame in the movie.
 */
{
	if (*frame >= 0 && *frame < Nframes)
	{
/*
		DoSound ("click"); 
*/
		PlotTime = Mtimes[*frame];
		pd_Store(Pd, "global", "plot-time", (char *) &PlotTime, 
			SYMT_DATE);
		mc_SetIndicator (*frame);
		px_PlotExec ("global");
	}
	DisplayedFrame = *frame;
}


static void
mc_SetIndicator (frame)
int frame;
/*
 * Set the indicator to show that we're on this frame.
 */
{
	if (Indicator && (Nframes > 0))
	{
		float width = 1.0/Nframes;

		XawScrollbarSetThumb (Indicator, frame*width, width);
		eq_sync ();
	}
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
 * If we're really in History mode, do the change.
 */
	if(PlotMode == History)
	{
		if ((CurrentFrame += motion) < 0)
			CurrentFrame += Nframes;
		else if (CurrentFrame >= Nframes)
			CurrentFrame -= Nframes;
		Eq_AddEvent (PDisplay, mc_DoNextFrame, &CurrentFrame, 
			sizeof (int), Augment);
	}
}


static void
mc_ScrollCB (sb, junk, position)
Widget sb;
XtPointer junk, position;
/*
 * The scrollbar callback.
 */
{
	long ipos = (long) position;

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


void
mc_ParamChange (param)
char	*param;
/*
 * Handle a parameter change.  If the movie is running, we update it and
 * keep it running.
 */
{
/*
 * If it's one of our movie parameters, then reload and return
 */
	if (! strcmp (param, "frame-rate") ||
	    ! strcmp (param, "frame-skip") ||
	    ! strcmp (param, "movie-end-time") ||
	    ! strcmp (param, "movie-minutes"))
	{
		mc_LoadParams ();
		return;
	}
/*
 * If we're running a movie, mark good frames (if any) and regenerate
 */
	if (MovieMode && MyMovie)
	{
		fc_UnMarkFrames ();
		fc_MarkFrames (Mtimes, Nframes);
		tl_Cancel(MovieSlot);
		MovieSlot = -1;
		mc_GenFrames ();
	}
}


void 
mc_PDChange()
/*
 *  What to do if a component changes while the movie is running. 
 */
{
	float sec;
	char trigger[200];
	PlatformId pid;

/*
 *  Unmark the old frames so they don't hog the pixmaps.  
 */
	fc_UnMarkFrames();
/*
 *  If Now and our trigger is a platform, then redo the ds_RequestNotify 
 *  because it was cancelled somewhere like the plot handler.
 */
	if (Now && ! ReGenFrame)
		if(pda_Search(Pd, "global", "trigger",0,trigger,SYMT_STRING))
			if((pid = ds_LookupPlatform(trigger)) != BadPlatform)
				ds_RequestNotify(pid, 0, mc_Notification);
/* 
 *  All events have been cancelled (in the plot handler) so start the movie 
 *  up again.  New frames will be generated as it needs them. 
 */ 
	sec = 1.0/(float) Rate;
	MovieSlot = tl_AddRelativeEvent (mc_NextFrame, 0, (int) (sec*INCFRAC),
		(int) (sec*INCFRAC));
	if(MovieSlot < 0)
		msg_ELog(EF_PROBLEM, "Movie won't re-start.");
}



static void
mc_ReGenFrames()
{
/*
 *  When the end time is 'now' used to regenerate frames as the time changes.
 */
	UItime	tmp;
	ZebTime t, endtime;
	int movielen, diff;
	float sec;
/*
 *  See if enough time has elapsed.
 */
	tl_Time (&t);
	t = mc_FixTime (t);
	uit_parse_date (EndTime[0] == ' ' ? EndTime + 1 : EndTime, &tmp, FALSE);
	TC_UIToZt (&tmp, &endtime);
	endtime = mc_FixTime(endtime);
	diff = t.zt_Sec - endtime.zt_Sec;
	if (diff < (TimeSkip * TUTable[TimeUnits].scale))
		return;
/*
 *  Cancel all timer events and event queue entries related to the movies.
 */
	if(MovieSlot >= 0)
	{
		tl_Cancel(MovieSlot);
		MovieSlot = -1;
	}
	Eq_ZapProc(PDisplay, mc_GenNFrame);
	fc_UnMarkFrames();
/*
 *  Convert stuff into the format we want.
 */
	TC_EncodeTime (&t, TC_Full, EndTime);
	if(! sscanf (MovieLen, "%d", &movielen))
	{
		msg_ELog(EF_PROBLEM, "Unable to understand MovieLen value.");
		return;
	}
/*
 *  Get the new frame times based on this new end time and start regenerating
 *  the frames.
 */
	mc_ReGetFrameTimes (&t);
	DisplayedFrame = CurrentFrame = 0;
	sec = 1.0/(float) Rate;
	MovieSlot = tl_AddRelativeEvent(mc_NextFrame, 0, (int) (sec*INCFRAC),
		(int) (sec*INCFRAC));
	if(MovieSlot < 0)
		msg_ELog(EF_PROBLEM, "Movie won't re-start.");
}


static void
mc_SetupPreGen()
{
/*
 *  Decide if pregeneration of frames is to be done, and if so which
 *  component and which fields will be pregenerated.
 */
	int i;
 	char **complist, f[FLEN]; 
	char platform[PlatformListLen];
 	char fields[4*FLEN], *fieldlist[8];
/*
 *  Loop through the component list looking for a movie-pregenerate parameter.
 */ 
	complist = pd_CompList(Pd);
	for(i = 1; complist[i]; i++)
	{	
/*
 *  If we find one get its fields.
 */
		pd_Retrieve(Pd, complist[i], "platform", platform, SYMT_STRING);
		if(pda_Search(Pd, complist[i], "movie-pregenerate", platform,
			fields, SYMT_STRING))
		{
			if (ParseFieldList (fields, fieldlist) != 2)
			{
				msg_ELog(EF_PROBLEM,"Pregenerate fields bad.");
				Pregenerate = FALSE;
				return;
			}	
/* 
 *  If the field of the component matches one of the two fields of the
 *  movie-pregenerate parameter, then set up the globals to do pregeneration.
 */
			pd_Retrieve(Pd, complist[i], "field", f, SYMT_STRING);
			if((strcmp(f, fieldlist[0]) == 0 )||
			   (strcmp(f, fieldlist[1]) == 0))
			{
				Pregenerate = TRUE;
				if(strcmp(fieldlist[0], f) == 0)
				{
					strcpy(Field1, fieldlist[0]);
					strcpy(Field2, fieldlist[1]);
				}		
				else
				{
					strcpy(Field1, fieldlist[1]);
					strcpy(Field2, fieldlist[0]);
				}		
				strcpy(PGComp, complist[i]);
				msg_ELog(EF_DEBUG, "Pregenerate: True");
				msg_ELog(EF_DEBUG, "Component: %s", PGComp);
				msg_ELog(EF_DEBUG, "Field1: %s  Field2: %s", 
					Field1, Field2);
				return;
			}
		}
	}
	Pregenerate = FALSE;
	msg_ELog(EF_DEBUG, "Pregenerate: False");
}


static void
mc_Notification(pid, global, t)
PlatformId pid;
int global;
ZebTime *t;
{
/*
 *  When the end time is 'now' used to regenerate frames as new data 
 *  becomes available.
 */
	Notification = TRUE;
	NotTime = *t;
}


static void
mc_ReGenFramesDS()
{
	UItime tmp;
	ZebTime endtime;
	int movielen, diff;
	float sec;
/*
 *  See if enough time has elapsed.
 */
	NotTime = mc_FixTime (NotTime);
	uit_parse_date (EndTime[0] == ' ' ? EndTime + 1 : EndTime, &tmp, FALSE);
	TC_UIToZt (&tmp, &endtime);
	endtime = mc_FixTime (endtime);
	diff = NotTime.zt_Sec - endtime.zt_Sec;
	if ((diff < TimeSkip * TUTable[TimeUnits].scale * DAPERCENT) || 
	    (diff < 0)) 
		return;

/*
 *  Cancel all timer events and event queue entries related to the movies.
 */
	if(MovieSlot >= 0)
	{
		tl_Cancel(MovieSlot);
		MovieSlot = -1;
	}
	Eq_ZapProc(PDisplay, mc_GenNFrame);
	fc_UnMarkFrames();
/*
 *  Get the current time and convert it into the format we want.
 */
	TC_ZtToUI (&NotTime, &tmp);
	TC_EncodeTime (&NotTime, TC_Full, EndTime);
	if(! sscanf (MovieLen, "%d", &movielen))
	{
		msg_ELog(EF_PROBLEM, "Unable to understand MovieLen value.");
		return;
	}
/*
 *  Get the new frame times based on this new end time and start regenerating
 *  the frames.
 */
	mc_ReGetFrameTimes(&NotTime);
	DisplayedFrame = CurrentFrame = 0;
	sec = 1.0/(float) Rate;
	MovieSlot = tl_AddRelativeEvent(mc_NextFrame, 0, (int) (sec*INCFRAC),
		(int) (sec*INCFRAC));
	if(MovieSlot < 0)
		msg_ELog(EF_PROBLEM, "Movie won't re-start.");
}


static void
mc_ChangeTimeUnits (w, entry, junk)
Widget		w;
XtPointer	entry, junk;
/*
 * Make the movie widget use the newly specified time units, but convert the
 * current values into the new units if possible.
 */
{
	Arg	arg;
	char	string[20];

	TimeUnits = (t_units) entry;
	
	sprintf (string, "%s, every", TUTable[(int)TimeUnits].units);
	XtSetArg (arg, XtNlabel, string);
	XtSetValues (WUnits1, &arg, ONE);

	sprintf (string, "%s,", TUTable[(int)TimeUnits].units);
	XtSetArg (arg, XtNlabel, string);
	XtSetValues (WUnits2, &arg, ONE);
}
