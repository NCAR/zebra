/*
 * Movie control functions.
 */
static char *rcsid = "$Id: ModelWidget.c,v 2.1 1994-03-19 20:48:54 burghart Exp $";
/*		Copyright (C) 1994 by UCAR
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
# include <X11/Xaw/Toggle.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/AsciiText.h>
# include <defs.h>
# include <message.h>
# include <pd.h>
# include <timer.h>
# include <DataStore.h>
# include <GraphicsW.h>
# include "GraphProc.h"
# include "EventQueue.h"

# define MODEL_NAME	"model" /* Name of the movie controller widget  */


/*
 * Globals.
 */
static Widget	WModel = NULL;	/* The outer form for the model widget	*/
static Widget 	StatusLabel;	/* Where the current status goes	*/
static Widget 	WFrate;
static Widget	FrameButton[NCACHE];	/* Button for each frame	*/
static Widget	RadioGroup;		/* Radio group ID for frame btns*/
static Widget	WValidOrIssue;		/* valid/issue radio pair	*/
static Widget	WIssueTime;		/* text widget for issue time	*/
static Widget	WApplyIssueTime;	/* button for applying new i.t.	*/

/*
 * The actual movie control parameters.
 */
static int	Foffsets[NCACHE];	/* Forecast offset for each frame */
static int 	Nframes = 0;		/* Number of frames in the loop	*/
static int 	TimerSlot = -1;
static int 	Rate;			/* Display Rate frames/second	*/
static bool 	Notification = FALSE;
static ZebTime 	NotTime;
static ZebTime	IssueTime;		/* Issue time for model data	*/

/*
 * If a movie is running, is it ours?
 */
static bool	MyMovie = FALSE;

/*
 * CurrentFrame is the frame we are supposed to be actually looking at.
 * DisplayedFrame is what we really *are* looking at.  They diverge when
 * multiple update events happen before we can actually update the screen,
 * due to system load.
 */
static int 	CurrentFrame;
static int 	DisplayedFrame;

/*
 * Forward definitions.
 */
static Widget	mw_MWCreate FP ((int, Widget, XtAppContext));
static void	mw_Update FP ((void));
static void	mw_SetStatus FP ((char *));
static void	mw_GetFrameOffsets FP ((void));
static void	mw_StartLoop FP ((Widget, XtPointer, XtPointer));
static void	mw_StopLoop FP ((Widget, XtPointer, XtPointer));
static void	mw_TextChange FP ((Widget, XtPointer, XtPointer));
static void	mw_DoNextFrame FP ((int *));
static void	mw_NextFrame FP ((void));
static void	mw_IndicateFrame FP ((int));
static void	mw_GenNFrame FP ((int *));
static void	mw_Notification FP ((PlatformId, int, ZebTime *));
static void	mw_Dismiss FP ((Widget, XtPointer, XtPointer));
static void	mw_ShowFrame FP ((Widget, XtPointer, XtPointer));
static void	mw_Cursor FP ((Widget, Cursor, int));
static void	mw_FrameStep FP ((Widget, XEvent *, String *, Cardinal *));
static void	mw_ValidOrIssue FP ((Widget, XtPointer, XtPointer));
static void	mw_NewIssueTime FP ((Widget, XtPointer, XtPointer));




void
mw_DefModelWidget ()
/*
 * Hook the movie widget into UI.
 */
{
	stbl vtbl = usy_g_stbl ("ui$variable_table");

	uw_def_widget (MODEL_NAME, "Model Control", mw_MWCreate, 0, 0);
	uw_NoHeader (MODEL_NAME);
}




static Widget
mw_MWCreate (junk, parent, appc)
int junk;
Widget parent;
XtAppContext appc;
/*
 * Create the model widget.
 */
{
	Widget	form, w, above, src;
	Arg	args[20];
	char	string[40];
	int	n, i;
	char	*oneofmany = "<Btn1Down>,<Btn1Up>: set() notify()";
	char	*updown = "<Btn1Down>: set() \n\
			   <Btn1Up>: FrameStep() unset() \n\
			   <Btn3Down>: set() \n\
			   <Btn3Up>: FrameStep() unset()";
	XtActionsRec	actions[] = {
		{"FrameStep", mw_FrameStep},
	};
/*
 * Create a form widget to hold everything.
 */
	WModel = parent;

	n = 0;
 	XtSetArg (args[n], XtNdefaultDistance, 2); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	form = XtCreateManagedWidget ("modelform", formWidgetClass, parent, 
				      args, n);
/*
 * The label which holds our title.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNwidth, 450); n++;
	sprintf (string, "Model Handling for %s", Ourname);
	XtSetArg (args[n], XtNlabel, string); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	w = XtCreateManagedWidget ("modelLabel", labelWidgetClass, form, 
				   args, n);
/*
 * Model issue time, with a button to apply time update if the text is changed.
 */
	above = w;

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Model issue time:"); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	w = XtCreateManagedWidget ("itimeLabel", labelWidgetClass, form,
				   args, n);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNwidth, 150); n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit); n++;
	w = WIssueTime = XtCreateManagedWidget ("itime", asciiTextWidgetClass, 
						form, args, n);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNsensitive, False); n++;
	XtSetArg (args[n], XtNlabel, "Apply"); n++;
	w = WApplyIssueTime = 
		XtCreateManagedWidget ("itimeApply", commandWidgetClass, form, 
				       args, n);
	XtAddCallback (w, XtNcallback, mw_NewIssueTime, 0);
/*
 * Get the AsciiSrc widget associated with WIssueTime, so we can add a callback
 * for text changes.
 */
	n = 0;
	XtSetArg (args[n], XtNtextSource, &src); n++;
	XtGetValues (WIssueTime, args, n);
	XtAddCallback (src, XtNcallback, mw_TextChange, 0);
/*
 * Create a set of radio buttons for NCACHE frames.  We manage them as we need
 * them.
 */
	above = w;
	w = NULL;
	FrameButton[0] = NULL;
	
	for (i = 0; i < NCACHE; i++)
	{
		n = 0;
		XtSetArg (args[n], XtNfromHoriz, w); n++;
		XtSetArg (args[n], XtNfromVert, above); n++;
		XtSetArg (args[n], XtNwidth, 30); n++;
		XtSetArg (args[n], XtNlabel, "XX"); n++;
		XtSetArg (args[n], XtNradioGroup, FrameButton[0]); n++;
	/*
	 * We use i+1 to identify the button, since we can't use zero for
	 * radioData.
	 */
		XtSetArg (args[n], XtNradioData, i + 1); n++;

		sprintf (string, "FButton%d", i);
		w = FrameButton[i] = XtCreateWidget (string, toggleWidgetClass,
						     form, args, n);
		XtAddCallback (w, XtNcallback, mw_ShowFrame, (XtPointer) i);
	}
/*
 * Use the first button to identify the radio group
 */
	RadioGroup = FrameButton[0];
/*
 * Start loop button
 */
	above = w;

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNwidth, 100); n++;
	XtSetArg (args[n], XtNlabel, "Start Loop"); n++;
	w = XtCreateManagedWidget ("loopStart", commandWidgetClass, form, 
				   args, n);
	XtAddCallback (w, XtNcallback, mw_StartLoop, 0);
/*
 * Frame step button 
 * (left button -> back 1 frame, right button -> forward 1 frame)
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, " < > "); n++;
	w = XtCreateManagedWidget ("loopStep", commandWidgetClass, form, 
				   args, n);

	XtAppAddActions (Actx, actions, XtNumber (actions));
	XtOverrideTranslations (w, XtParseTranslationTable (updown));
/*
 * Stop loop button
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNwidth, 100); n++;
	XtSetArg (args[n], XtNlabel, "Stop Loop"); n++;
	w = XtCreateManagedWidget ("loopStop", commandWidgetClass, form, 
				   args, n);
	XtAddCallback (w, XtNcallback, mw_StopLoop, 0);
/*
 * Frame rate.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "   Frames/Second:"); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	w = XtCreateManagedWidget ("Modelfr", labelWidgetClass, form,
				   args, n);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNwidth, 30); n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit); n++;
	w = WFrate = XtCreateManagedWidget ("modelfr", asciiTextWidgetClass, 
					    form, args, n);
/*
 * Get the AsciiSrc widget associated with WFrate, so we can add a callback
 * for text changes.
 */
	n = 0;
	XtSetArg (args[n], XtNtextSource, &src); n++;
	XtGetValues (WFrate, args, n);
	XtAddCallback (src, XtNcallback, mw_TextChange, 0);
/*
 * Normal/validation mode control with a radio button pair that we force
 * to be "one of many" rather than the default "zero or one of many"
 */
	above = w;
	
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, 
		  "Use observational data matching model"); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	w = XtCreateManagedWidget ("validlabel1", labelWidgetClass, form, 
				   args, n);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNradioData, 'V'); n++;
	w = WValidOrIssue = XtCreateManagedWidget ("valid", toggleWidgetClass, 
						   form, args, n);
	XtAddCallback (w, XtNcallback, mw_ValidOrIssue, (XtPointer) 'V');
	XtOverrideTranslations (w, XtParseTranslationTable (oneofmany));

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNradioData, 'I'); n++;
	XtSetArg (args[n], XtNradioGroup, WValidOrIssue); n++;
	w = XtCreateManagedWidget ("issue", toggleWidgetClass, form, args, n);
	XtAddCallback (w, XtNcallback, mw_ValidOrIssue, (XtPointer) 'I');
	XtOverrideTranslations (w, XtParseTranslationTable (oneofmany));

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "time"); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	w = XtCreateManagedWidget ("validlabel2", labelWidgetClass, form, 
				   args, n);

/*
 * The status line:
 */
	above = w;

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Status:"); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	w = XtCreateManagedWidget ("ModelStLabel", labelWidgetClass, 
					   form, args, n);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNwidth, 300); n++;
	XtSetArg (args[n], XtNresize, False); n++;
	XtSetArg (args[n], XtNjustify, XtJustifyLeft); n++;
	w = StatusLabel = XtCreateManagedWidget ("ModelStatus", 
						 labelWidgetClass, form, 
						 args, n);
/*
 * Dismiss button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Dismiss"); n++;
	w = XtCreateManagedWidget ("modelDismiss", commandWidgetClass, form,
				   args, n);
	XtAddCallback (w, XtNcallback, mw_Dismiss, 0);
/*
 * Apply current PD information
 */
	mw_Update ();

	return (form);
}



static void
mw_Update ()
/*
 * Apply current PD info to update the widget
 */
{	
	int	i;
	char	string[40];
	Arg	arg;
/*
 * Make sure the model widget's been built
 */
	if (! WModel)
		return;
/*
 * Model issue time
 */
	IssueTime = PlotTime;
	if (ValidationMode)
		IssueTime.zt_Sec -= ForecastOffset;

	TC_EncodeTime (&IssueTime, TC_Full, string);
	XtSetArg (arg, XtNstring, string);
	XtSetValues (WIssueTime, &arg, 1);
/*
 * Frame rate
 */
	strcpy (string, "2");
	pda_Search (Pd, "global", "frame-rate", NULL, string, SYMT_STRING);

	if (! sscanf (string, "%d", &Rate))
	{
		msg_ELog (EF_PROBLEM, "Bad frame-rate '%s', using 2", string);
		Rate = 2;
		strcpy (string, "2");
	}

	XtSetArg (arg, XtNstring, string);
	XtSetValues (WFrate, &arg, 1);
/*
 * Label and manage the frame radio buttons we need right now
 */
	mw_GetFrameOffsets ();

	for (i = 0; i < Nframes; i++)
	{
		sprintf (string, "%d hr", Foffsets[i] / 3600);
		XtSetArg (arg, XtNlabel, string);
		XtSetValues (FrameButton[i], &arg, 1);
	}

	XtManageChildren (FrameButton, Nframes);
	XtUnmanageChildren (FrameButton + Nframes, NCACHE - Nframes);
/*
 * Update the status and highlight the appropriate frame button
 */
	sprintf (string, "%d hr forecast", ForecastOffset / 3600);
	mw_SetStatus (string);

	XawToggleUnsetCurrent (RadioGroup);

	for (i = 0; i < Nframes; i++)
	{
		if (ForecastOffset == Foffsets[i])
		{
		/*
		 * We choose the button by its "radioData", which is just
		 * the frame number + 1.
		 */
			XawToggleSetCurrent (RadioGroup, (XtPointer)(i + 1));
			break;
		}
	}
/*
 * Get the valid/issue time buttons right
 */
	XawToggleSetCurrent (WValidOrIssue, 
			     ValidationMode ? (XtPointer)'V' : (XtPointer)'I');
}




static void
mw_Dismiss (w, junk1, junk2)
Widget	w;
XtPointer	junk1, junk2;
/*
 * Popdown the model widget
 */
{
	uw_popdown (MODEL_NAME);
}




static void
mw_StartLoop (w, junk1, junk2)
Widget	w;
XtPointer	junk1, junk2;
/*
 * Start the loop
 */
{
	int	f;
	Arg	arg;
/*
 * Make sure we can do this
 */
	if (MovieMode && !MyMovie)
	{
		mw_SetStatus ("A movie is already running...");
		return;
	}
/*
 * Figure out our parameters.
 */
	mw_SetStatus ("Initializing");
/*
 * If everything worked until now, cancel any current timer events and drop
 * out of real time mode.
 */
	tl_AllCancel ();
	ds_CancelNotify ();
	PlotMode = History;
	MovieMode = TRUE;
	MyMovie = TRUE;
	pd_Store (Pd, "global", "plot-mode", "history", SYMT_STRING);
	pd_Store (Pd, "global", "movie-mode", (char *) &MovieMode, SYMT_BOOL);
	Eq_AddEvent (PWhenever, eq_ReturnPD, 0, 0, Bounce);
/*
 * Start the process of generating the movie frames.
 */
	mw_SetStatus ("Generating frames...");
/*
 * Make sure there are enough movie frames.  If not, allocate some more, and
 * modify the PD to reflect this.
 */
	if (FrameCount < Nframes)
	{
		FrameCount = Nframes;
		fc_SetNumFrames (FrameCount);
		msg_ELog (EF_DEBUG, "mw_StartLoop: Asking for %d frames", 
			  Nframes);
		XtSetArg (arg, XtNframeCount, Nframes);
		XtSetValues (Graphics, &arg, 1);
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
	Eq_AddEvent (PDisplay, mw_GenNFrame, &f, sizeof (int), Override);
}




static void
mw_StopLoop (w, junk1, junk2)
Widget	w;
XtPointer	junk1, junk2;
/*
 * Stop the loop
 */
{
	Arg	arg;
/*
 * If there are timer events active, get rid of them.
 */
	if (TimerSlot >= 0)
	{
		tl_Cancel (TimerSlot);
		TimerSlot = -1;
	}
/*
 * Get rid of any event queue entries relating to frame generation.
 */
	Eq_ZapProc (PDisplay, mw_GenNFrame);
/*
 *  Cancel the data available notification for the movies.
 */
	ds_CancelNotify ();
	tl_AllCancel ();
/*
 * Throw the system into history mode, showing the current frame.
 */
	MovieMode = FALSE;
	MyMovie = FALSE;
	PlotMode = History;
	pd_Store (Pd, "global", "plot-mode", "history", SYMT_STRING);
	pd_Store (Pd, "global", "movie-mode", (char *) &MovieMode, SYMT_BOOL);
	Eq_AddEvent (PWhenever, eq_ReturnPD, 0, 0, Bounce);
}




static void
mw_TextChange (w, junk, junk2)
Widget	w;
XtPointer	junk, junk2;
/*
 * Text in an AsciiText widget was modified, so deal with it
 */
{
	Arg	arg;
	String	string;
	char	frate[8];
	int	newrate;
/*
 * The widget that causes the callback is and AsciiSrc widget, and we
 * want its parent AsciiText widget.
 */
	w = XtParent (w);
/*
 * Frame rate?
 */
	if (w == WFrate)
	{
	/*
	 * Get the frame rate string
	 */
		XtSetArg (arg, XtNstring, &string);
		XtGetValues (WFrate, &arg, 1);
	/*
	 * Save it if it's a valid frame rate, otherwise go back to what
	 * we had before.
	 */
		if (strlen (string) == 0)
			return;

		if (sscanf (string, "%d", &newrate))
		{
			Rate = newrate;
			pd_Store (Pd, "global", "frame-rate", (char *) &Rate, 
				  SYMT_INT);
		}
		else
		{
			sprintf (frate, "%d", Rate);
			XtSetArg (arg, XtNstring, frate);
			XtSetValues (WFrate, &arg, 1);
		}
	}
/*
 * Issue time?
 */
	else if (w == WIssueTime)
	{
	/*
	 * Just activate the associated "Apply" button
	 */
		XtSetArg (arg, XtNsensitive, True);
		XtSetValues (WApplyIssueTime, &arg, 1);
	}
}




static void
mw_GenNFrame (which)
int *which;
/*
 * Generate the which'th movie frame.
 */
{
	char	string[50], *info;
	int	frame = *which, next, interval;
/*
 * Update the message.
 */
	sprintf (string, "Generating %d hr forecast frame", 
		 Foffsets[frame] / 3600);
	mw_SetStatus (string);
/*
 * Do the frame by using the toggle widget callback.  We choose the right
 * button by its "radioData", which is just the frame number + 1.
 */
	XawToggleSetCurrent (RadioGroup, (XtPointer)(frame + 1));
	fc_MarkFramesByOffset (Foffsets, Nframes);
/*
 * The loop might have been stopped during the X action that goes with
 * XawToggleSetCurrent().  If so, bail out before we try to start another 
 * frame.
 */
	if (! MovieMode)
		return;
/*
 * If we're not done, arrange to have the next frame done.
 */
	if ((next = frame + 1) < Nframes)
	{
		Eq_AddEvent (PDisplay, mw_GenNFrame, &next, sizeof (int),
			     Augment);
		return;
	}
/*
 * Otherwise now it's time to start the loop sequence.
 */
	mw_SetStatus ("Running");
	DisplayedFrame = CurrentFrame = Nframes + 1;
	mw_NextFrame ();
/*
 * Start the timer to do the rest.
 */
	interval = INCFRAC / Rate;
	TimerSlot = tl_AddRelativeEvent (mw_NextFrame, 0, interval, interval);

	if (TimerSlot < 0)
	{
		msg_ELog (EF_PROBLEM, "Movie won't start.");
		mw_SetStatus ("Timer problem...movie won't start");
	}
}




static void
mw_GetFrameOffsets ()
/*
 * Get all the possible forecast offsets from all the platforms in the
 * plot description
 */
{
	int	i, f, n, insert_pos, offsets[20], noffsets;
	char	**complist, platform[40];
	bool	disabled;
	PlatformId	pid;
/*
 * Start with one offset of zero seconds
 */
	Foffsets[0] = 0;
	Nframes = 1;	/* number of frames in the loop = number of offsets */
/*
 * Loop through the plot components and add forecast offsets for each 
 * model platform we find
 */
	complist = pd_CompList(Pd);
	for (i = 1; complist[i]; i++)
	{
	/*
	 * Ignore disabled components
	 */
		disabled = FALSE;
		pd_Retrieve (Pd, complist[i], "disable", (char *) &disabled, 
			     SYMT_BOOL);

		if (disabled)
			continue;
	/*
	 * Get the platform and go on if it's not a model platform
	 */
		pd_Retrieve (Pd, complist[i], "platform", platform, 
			     SYMT_STRING);

		if ((pid = ds_LookupPlatform (platform)) == BadPlatform ||
		    ! ds_IsModelPlatform (pid))
			continue;
	/*
	 * Get the offset list for this platform
	 */
		if (! ds_GetForecastTimes (pid, &PlotTime, offsets, 
					     &noffsets))
			continue;
	/*
	 * Merge these offsets into the main list
	 */
		for (n = 0; n < noffsets; n++)
		{
			for (f = 0; f < Nframes; f++)
				if (offsets[n] <= Foffsets[f])
					break;
		/*
		 * Move on if we matched an existing entry
		 */
			if ((f < Nframes) && (offsets[n] == Foffsets[f]))
				continue;
		/*
		 * Otherwise, shift out the remaining offsets and insert
		 * this one.
		 */
			insert_pos = f;
			for (f = Nframes; f > insert_pos; f--)
				Foffsets[f] = Foffsets[f-1];

			Foffsets[insert_pos] = offsets[n];
			Nframes++;
		}
	}

	return;
}




static void
mw_SetStatus (string)
char *string;
/*
 * Set the status line to this string.
 */
{
	Arg args[5];

	XtSetArg (args[0], XtNlabel, string);
	XtSetValues (StatusLabel, args, 1);
	eq_sync ();
	xtEvent (0);	/* XXX */
}




static void
mw_NextFrame ()
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
 * around, to implement a one-cycle pause at the end of the movie.
 */
	if (++CurrentFrame > Nframes)
		CurrentFrame = 0;
/*
 * Now schedule an update of the next frame.
 */
	Eq_AddEvent (PDisplay, mw_DoNextFrame, &CurrentFrame, sizeof (int), 
		     Bounce);
}




static void
mw_DoNextFrame (which)
int *which;
/*
 * Actually display the next frame in the movie.
 */
{
	int	frame = *which;

	char	string[20];
/*
 * Do the frame by using the toggle widget callback.  We choose the right
 * button by its "radioData", which is just the frame number + 1.
 */
	if (frame >= 0 && frame < Nframes)
		XawToggleSetCurrent (RadioGroup, (XtPointer)(frame + 1));

	DisplayedFrame = frame;
}




void
mw_ParamChange (param)
char	*param;
/*
 * A parameter changed.  Deal with it.
 */
{
/*
 * Update the widget
 */
	mw_Update ();
/*
 * If we have a loop going, stop and restart it
 */
	if (MovieMode && MyMovie)
	{
		mw_StopLoop (NULL, NULL, NULL);
		mw_StartLoop (NULL, NULL, NULL);
	}
}




static void
mw_Notification (pid, global, t)
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
	msg_ELog (EF_DEBUG, "mw_Notification: New data for %s", 
		  ds_PlatformName(pid));
}




static void
mw_ShowFrame (w, cdata, junk)
Widget	w;
XtPointer	cdata, junk;
/*
 * Callback for the per-frame buttons
 */
{
	int	frame = (int) cdata;
	char	string[40];
	Boolean	set;
/*
 * If this is a call because the button became unset, we can just return
 */
	XtVaGetValues (FrameButton[frame], XtNstate, &set, NULL);
	if (! set)
		return;
/*
 * Stash the desired forecast offset and plot time in the PD
 */
	ForecastOffset = Foffsets[frame];

	sprintf (string, "%ds", ForecastOffset);
	pd_Store (Pd, "global", "forecast-offset", string, SYMT_STRING);

	PlotTime = IssueTime;
	if (ValidationMode)
		PlotTime.zt_Sec += ForecastOffset;
	TC_EncodeTime (&PlotTime, TC_Full, string);
	pd_Store (Pd, "global", "plot-time", string, SYMT_STRING);

	Eq_AddEvent (PWhenever, eq_ReturnPD, 0, 0, Bounce);
/*
 * Replot
 */
	mw_Cursor (WModel, BusyCursor, TRUE);
	px_PlotExec ("global");
	mw_Cursor (WModel, None, TRUE);
/*
 * Finish up
 */
	DisplayedFrame = frame;

	sprintf (string, "%d hr forecast", Foffsets[DisplayedFrame] / 3600);
	mw_SetStatus (string);
}




static void
mw_Cursor (w, cursor, do_sync)
Widget	w;
Cursor	cursor;
int	do_sync;
/*
 * Change to the selected cursor in the widget and its children.  If do_sync
 * is true, we force a sync after making the change.
 */
{
	int	n, i, numchildren;
	Arg	args[2];
	WidgetList	children;

	if (w && XtWindow (w))
		XDefineCursor (XtDisplay (w), XtWindow (w), cursor);
/*
 * Recurse through children, if any
 */
	numchildren = 0;

	n = 0;
	XtSetArg (args[n], XtNnumChildren, &numchildren); n++;
	XtSetArg (args[n], XtNchildren, &children); n++;
	XtGetValues (w, args, n);

	for (i = 0; i < numchildren; i++)
		mw_Cursor (children[i], cursor, FALSE);
/*
 * Sync if requested
 */
	if (do_sync)
		eq_sync ();
}




static void
mw_FrameStep (w, event, params, num_params)
Widget	w;
XEvent	*event;
String	*params;
Cardinal	*num_params;
/*
 * Handle a button event from the frame step widget
 */
{
	int	step, frame;
/*
 * Don't do anything if a movie is running
 */
	if (MovieMode)
		return;
/*
 * Step back a frame if the left button was pressed, otherwise forward
 */
	step = (event->type == ButtonRelease && 
		event->xbutton.button == Button1) ? -1 : 1;
/*
 * Figure out the new frame and display it
 */
	frame = DisplayedFrame + step;

	if (frame < 0)
		frame = Nframes - 1;

	if (frame > Nframes - 1)
		frame = 0;
/*
 * We select the FrameButton by its "radioData", which is just the frame 
 * number + 1.
 */
	XawToggleSetCurrent (RadioGroup, (XtPointer)(frame + 1));
}

	

static void
mw_ValidOrIssue (w, id_ptr, junk)
Widget	w;
XtPointer	id_ptr, junk;
/*
 * The 'valid' or 'issue' button was pressed
 */
{
	char	id = (char)((int) id_ptr);
	Boolean	set;
/*
 * If this is a call because the button became unset, we can just return
 */
	XtVaGetValues (w, XtNstate, &set, NULL);
	if (! set)
		return;
/*
 * If the new mode matches the current mode, just return
 */
	if ((id == 'V' && ValidationMode) || (id == 'I' && !ValidationMode))
		return;
/*
 * Otherwise, update the PD and let plot control know about it
 */
	ValidationMode = (id == 'V');
	pd_Store (Pd, "global", "validation-mode", (char *)&ValidationMode, 
		  SYMT_BOOL);

	Eq_AddEvent (PWhenever, pc_ParamChange, "validation-mode", 16, 
		     Augment);
}




static void
mw_NewIssueTime (w, junk1, junk2)
Widget	w;
XtPointer	junk1, junk2;
/*
 * Apply the issue time from our WIssueTime text widget
 */
{
	Arg	arg;
	char	*itstring, scratch[40];
/*
 * Make the issue time "Apply" widget insensitive again
 */
	XtSetArg (arg, XtNsensitive, False);
	XtSetValues (WApplyIssueTime, &arg, 1);
/*
 * Grab the string from the WIssueTime widget
 */
	XtSetArg (arg, XtNstring, &itstring);
	XtGetValues (WIssueTime, &arg, 1);
/*
 * If we can't read a good time from the string, just go back to the old
 * issue time.
 */
	if (! TC_DecodeTime (itstring, &IssueTime))
	{
	/*
	 * Failure.  Just keep the old issue time.
	 */
		msg_ELog (EF_INFO, "Bad issue time: %s", itstring);

		TC_EncodeTime (&IssueTime, TC_Full, scratch);
		XtSetArg (arg, XtNstring, scratch);
		XtSetValues (WIssueTime, &arg, 1);

		return;
	}
/*
 * Update the PD and let plot control know about it.
 */
	PlotTime = IssueTime;
	if (ValidationMode)
		PlotTime.zt_Sec += ForecastOffset;

	TC_EncodeTime (&PlotTime, TC_Full, scratch);
	pd_Store (Pd, "global", "plot-time", scratch, SYMT_STRING);

	Eq_AddEvent (PWhenever, pc_ParamChange, "plot-time", 10, Augment);
}
