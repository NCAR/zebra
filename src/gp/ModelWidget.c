/*
 * Movie control functions.
 */
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
# include <timer.h>
# include <pd.h>
# include <dm.h>
# include <DataStore.h>
# include <GraphicsW.h>
# include "GraphProc.h"
# include "EventQueue.h"

RCSID("$Id: ModelWidget.c,v 2.14 2001-11-30 21:29:28 granger Exp $")

# define MODEL_NAME	"model" /* Name of the movie controller widget  */


/*
 * Globals.
 */
static Widget	WModel = NULL;	/* The outer form for the model widget	*/
static Widget 	StatusLabel;	/* Where the current status goes	*/
static Widget 	WFrate;
static Widget	FrameButton[NCACHE];	/* Button for each frame	*/
static Widget	UseButton[NCACHE];	/* Toggle use of frames in loop	*/
static Widget	WAllNone;		/* Toggle use of all/no frames	*/
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
#ifdef notdef
static zbool 	Notification = FALSE;
static ZebTime 	NotTime;
#endif
static ZebTime	IssueTime;		/* Issue time for model data	*/

/*
 * If a movie is running, is it ours?
 */
static zbool	MyMovie = FALSE;

/*
 * The last frame we asked to be displayed (CurrentFrame), and the frame
 * that's actually up right now (DisplayedFrame).
 */
static int 	CurrentFrame, DisplayedFrame;

/*
 * Forward definitions.
 */
Widget	mw_MWCreate FP ((int, Widget, XtAppContext));
static void	mw_SetStatus FP ((char *));
static void	mw_GetFrameOffsets FP ((void));
static void	mw_StartLoop FP ((Widget, XtPointer, XtPointer));
static void	mw_StopLoop FP ((Widget, XtPointer, XtPointer));
static void	mw_TextChange FP ((Widget, XtPointer, XtPointer));
static void	mw_NextFrame FP ((void));
static void	mw_GenNFrame FP ((int *));
static void	mw_Dismiss FP ((Widget, XtPointer, XtPointer));
static void	mw_ShowFrame FP ((int *));
static void	mw_FrameStep FP ((Widget, XEvent *, String *, Cardinal *));
static void	mw_FrameAction FP ((Widget, XEvent *, String *, Cardinal *));
static void	mw_ValidOrIssue FP ((Widget, XtPointer, XtPointer));
static void	mw_NewIssueTime FP ((Widget, XtPointer, XtPointer));
static void	mw_UseAllOrNone FP ((Widget, XtPointer, XtPointer));




void
mw_DefModelWidget ()
/*
 * Hook the movie widget into UI.
 */
{
	uw_def_widget (MODEL_NAME, "Model Control", mw_MWCreate, 0, 0);
	uw_NoHeader (MODEL_NAME);
}




Widget
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
	char	string[120];
	int	n, i;
	Pixmap	pm;
	XtTranslations	trans;
	static char	oneofmany[] = "<Btn1Down>,<Btn1Up>: set() notify()";
	static char	updown[] = "<Btn1Down>: set() \n\
			   <Btn1Up>: FrameStep() unset() \n\
			   <Btn3Down>: set() \n\
			   <Btn3Up>: FrameStep() unset()";
	static char	chooseframe[] = 
		"<Btn1Down>,<Btn1Up>: toggle() notify() ShowFrame()";
	static XtActionsRec	actions[] = {{"FrameStep", mw_FrameStep},
					     {"ShowFrame", mw_FrameAction}};
	static unsigned char	allnone_bits[] = 
	{
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03, 0x00, 0xff, 0x9f, 
		0xfd, 0x7f, 0x11, 0x91, 0xfd, 0x7f, 0x11, 0xd1, 0xfc, 0x7f, 
		0xff, 0xdf, 0xfc, 0x7f, 0x00, 0x60, 0x00, 0x00, 0x00, 0x00, 
		0x00, 0x00
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
 * Add the actions we'll need
 */
	XtAppAddActions (Actx, actions, XtNumber (actions));
/*
 * The label which holds our title.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNwidth, 600); n++;
	sprintf (string, "Model Handling for %s", dm_WindowName());
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
 * The frame buttons below will get a new translation to cause a frame
 * draw when the button is pushed.
 */
	trans = XtParseTranslationTable (chooseframe);
/*
 * Create a set of radio buttons for NCACHE frames.
 */
	above = w;
	w = NULL;
	FrameButton[0] = NULL;

	
	for (i = 0; i < NCACHE; i++)
	{
		n = 0;
		XtSetArg (args[n], XtNfromHoriz, w); n++;
		XtSetArg (args[n], XtNfromVert, above); n++;
		XtSetArg (args[n], XtNleft, XtChainLeft); n++;
		XtSetArg (args[n], XtNright, XtChainLeft); n++;
		XtSetArg (args[n], XtNwidth, 35); n++;
		XtSetArg (args[n], XtNresize, False); n++;
		XtSetArg (args[n], XtNlabel, "XX"); n++;
		XtSetArg (args[n], XtNradioGroup, FrameButton[0]); n++;
	/*
	 * We use i+1 to identify the button, since we can't use zero for
	 * radioData.
	 */
		XtSetArg (args[n], XtNradioData, i + 1); n++;

		sprintf (string, "FButton%d", i);
		w = FrameButton[i] = 
			XtCreateManagedWidget (string, toggleWidgetClass,
					       form, args, n);

		XtOverrideTranslations (w, trans);
	}
/*
 * Use the first button to identify the radio group
 */
	RadioGroup = FrameButton[0];
/*
 * Sister buttons for the frame buttons to toggle which frames will be used
 * in a loop.
 */
	above = w;
	w = NULL;
	
	for (i = 0; i < NCACHE; i++)
	{
		n = 0;
		XtSetArg (args[n], XtNfromHoriz, w); n++;
		XtSetArg (args[n], XtNfromVert, above); n++;
		XtSetArg (args[n], XtNleft, XtChainLeft); n++;
		XtSetArg (args[n], XtNright, XtChainLeft); n++;
		XtSetArg (args[n], XtNvertDistance, 0); n++;
		XtSetArg (args[n], XtNwidth, 35); n++;
		XtSetArg (args[n], XtNheight, 8); n++;
		XtSetArg (args[n], XtNstate, True); n++;
		XtSetArg (args[n], XtNresize, False); n++;
		XtSetArg (args[n], XtNlabel, " "); n++;

		sprintf (string, "UseButton%d", i);
		w = UseButton[i] = 
			XtCreateManagedWidget (string, toggleWidgetClass,
					       form, args, n);
	}
/*
 * Toggle to use all/none.
 */
	pm = XCreateBitmapFromData (XtDisplay (parent), 
				    RootWindowOfScreen (XtScreen (parent)), 
				    (const char *) allnone_bits, 31, 8);

	n = 0;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNvertDistance, 0); n++;
	XtSetArg (args[n], XtNleft, XtChainLeft); n++;
	XtSetArg (args[n], XtNright, XtChainLeft); n++;
	XtSetArg (args[n], XtNwidth, 39); n++;
	XtSetArg (args[n], XtNheight, 8); n++;
	XtSetArg (args[n], XtNbitmap, pm); n++;

	w = WAllNone = XtCreateManagedWidget ("AllNone", commandWidgetClass, 
					      form, args, n);
	XtAddCallback (w, XtNcallback, mw_UseAllOrNone, (XtPointer)(long) i);
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
	XtSetArg (args[n], XtNstate, ValidationMode); n++;
	XtSetArg (args[n], XtNradioData, 'V'); n++;
	w = WValidOrIssue = XtCreateManagedWidget ("valid", toggleWidgetClass, 
						   form, args, n);
	XtAddCallback (w, XtNcallback, mw_ValidOrIssue, (XtPointer) 'V');
	XtOverrideTranslations (w, XtParseTranslationTable (oneofmany));

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNstate, !ValidationMode); n++;
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
	XtSetArg (args[n], XtNwidth, 400); n++;
	XtSetArg (args[n], XtNresize, False); n++;
	XtSetArg (args[n], XtNjustify, XtJustifyLeft); n++;
	w = StatusLabel = XtCreateManagedWidget ("ModelStatus", 
						 labelWidgetClass, form, 
						 args, n);
/*
 * Help button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Help"); n++;
	w = XtCreateManagedWidget ("modelHelp", commandWidgetClass, form,
				   args, n);
	XtAddCallback (w, XtNcallback, HelpCallback, 
		       (XtPointer)GP_HELP_MODEL);

	if (! Dock)
	{
	    /*
	     * Dismiss button.
	     */
	    n = 0;
	    XtSetArg (args[n], XtNfromHoriz, w); n++;
	    XtSetArg (args[n], XtNfromVert, above); n++;
	    XtSetArg (args[n], XtNlabel, "Dismiss"); n++;
	    w = XtCreateManagedWidget ("modelDismiss", commandWidgetClass, 
				       form, args, n);
	    XtAddCallback (w, XtNcallback, mw_Dismiss, 0);
	}
/*
 * Apply current PD information
 */
	mw_Update ();

	return (form);
}



void
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
 * Busy cursor, since this stuff can take a while...
 */
	ChangeCursor (Graphics, BusyCursor);
	ChangeCursor (WModel, BusyCursor);
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
	if (Pd)
		pda_Search (Pd, "global", "frame-rate", NULL, string, 
			    SYMT_STRING);

	if (! sscanf (string, "%d", &Rate))
	{
		msg_ELog (EF_PROBLEM, "Bad frame-rate '%s', using 2", string);
		Rate = 2;
		strcpy (string, "2");
	}

	XtSetArg (arg, XtNstring, string);
	XtSetValues (WFrate, &arg, 1);
/*
 * Label and manage the frame buttons we need right now
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
 * Manage the right number of use-frame toggle buttons and put the all/none
 * button in the right place.
 */
	XtManageChildren (UseButton, Nframes);
	XtUnmanageChildren (UseButton + Nframes, NCACHE - Nframes);

	XtSetArg (arg, XtNfromHoriz, UseButton[Nframes - 1]);
	XtSetValues (WAllNone, &arg, 1);
/*
 * Start with all frames included in the loop
 */
	for (i = 0; i < Nframes; i++)
		XtCallActionProc (UseButton[i], "set", NULL, NULL, 0);
/*
 * Update the status and highlight the appropriate frame button
 */
	sprintf (string, "%li hr forecast", ForecastOffset / 3600);
	mw_SetStatus (string);

	XawToggleUnsetCurrent (RadioGroup);

	for (i = 0; i < Nframes; i++)
	{
		if (ForecastOffset == Foffsets[i])
		{
			DisplayedFrame = i;
		/*
		 * We choose the button by its "radioData", which is just
		 * the frame number + 1.
		 */
			XawToggleSetCurrent (RadioGroup, 
					     (XtPointer)(long)(i + 1));
			break;
		}
	}
/*
 * Get the valid/issue time buttons right
 */
	XawToggleSetCurrent (WValidOrIssue, 
			     ValidationMode ? (XtPointer)'V' : (XtPointer)'I');
/*
 * Return to normal cursors
 */
	ChangeCursor (Graphics, None);
	ChangeCursor (WModel, None);
}




#ifdef notdef
static Widget
UWShell (name)
char *name;
/*
 * Return the top-level shell widget for this UI widget
 */
{
	Widget shell = uw_IWWidget (name);

	return (shell);
}
#endif



static void
mw_Dismiss (w, junk1, junk2)
Widget	w;
XtPointer	junk1, junk2;
/*
 * Popdown the model widget.
 *
 * XXX
 * The first is to make sure UI is up with what's going on.  It won't
 * affect the display since the widget is already mapped else we wouldn't
 * have gotten this callbak.  The second step actually pops it down in case
 * UI is confused and failed the first step.  This is only necessary (we
 * think) when popping up a model widget from a require module and changing
 * configs while loading modules.
 * XXX
 */
{
	Widget shell = w;

	uw_popup (MODEL_NAME);
	uw_popdown (MODEL_NAME);

	while (shell && !XtIsSubclass (shell, wmShellWidgetClass))
		shell = XtParent(shell);
	if (shell)
		XtPopdown (shell);
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
/*
 * Update the PD and make sure it gets back to the display manager and pdmon
 * at some point
 */
	pd_Store (Pd, "global", "plot-mode", "history", SYMT_STRING);
	pd_Store (Pd, "global", "movie-mode", "false", SYMT_STRING);

	Eq_AddEvent (PWhenever, eq_ReturnPD, 0, 0, Bounce);
	pdm_ScheduleUpdate ();
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
	char	string[50];
	Boolean	use_frame;
	int	frame = *which, next, interval;
/*
 * Do the frame generation if this frame is wanted in the loop
 */
	XtVaGetValues (UseButton[frame], XtNstate, &use_frame, NULL);

	if (use_frame)
	{
	/*
	 * Update the message.
	 */
		sprintf (string, "Generating %d hr forecast frame", 
			 Foffsets[frame] / 3600);
		mw_SetStatus (string);
	/*
	 * Show the frame and mark frames in the cache
	 */
		mw_ShowFrame (&frame);
		fc_MarkFramesByOffset (Foffsets, Nframes);
	/*
	 * The loop might have been stopped during the X action that goes with
	 * mw_ShowFrame().  If so, bail out before we try to start
	 * another frame.
	 */
		if (! MovieMode)
			return;
	}
/*
 * If this isn't the last frame, arrange to have the next frame done.
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
	CurrentFrame = DisplayedFrame;
	mw_NextFrame ();
/*
 * Start the timer to do the rest.
 */
	interval = INCFRAC / Rate;
	TimerSlot = tl_AddRelativeEvent (mw_NextFrame, 0, interval, interval);

	if (TimerSlot < 0)
	{
		msg_ELog (EF_PROBLEM, "Loop won't start.");
		mw_SetStatus ("Timer problem...loop won't start");
	}
}




static void
mw_GetFrameOffsets ()
/*
 * Get all the possible forecast offsets from all the platforms in the
 * plot description
 */
{
	int	i, f, n, insert_pos, offsets[NCACHE], noffsets;
	char	**complist;
	char	platform[PlatformListLen];
	zbool	disabled;
	PlatformId	pid;
/*
 * Start with one offset of zero seconds
 */
	Foffsets[0] = 0;
	Nframes = 1;	/* number of frames in the loop = number of offsets */
/*
 * Bail out now if we don't have a plot description yet.
 */
	if (! Pd)
		return;
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
		if (! ds_GetForecastTimes (pid, &IssueTime, offsets, 
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
	static Boolean	pausing = False;
	Boolean	use_frame;
/*
 * If we are out of sync -- the last update not yet done -- blow this one
 * off completely.
 */
	if (CurrentFrame != DisplayedFrame)
		return;
/*
 * Start at the beginning of the loop if we just paused
 */
	if (pausing)
	{
		CurrentFrame = -1;
		pausing = False;
	}
/*
 * Increment until we find an enabled frame.
 */
	use_frame = False;
	while (! use_frame)
	{
		if (++CurrentFrame == Nframes)
		{
		/*
		 * If we've hit the end of the list, just redisplay the last
		 * good frame to cause a one cycle pause at the end of the loop
		 */
			pausing = True;
			CurrentFrame = DisplayedFrame;
			break;
		}

		XtVaGetValues (UseButton[CurrentFrame], XtNstate, &use_frame,
			       NULL);
	}
/*
 * Now schedule an update of the next frame.
 */
	Eq_AddEvent (PDisplay, mw_ShowFrame, &CurrentFrame, sizeof (int), 
		     Augment);
}




void
mw_ParamChange (param)
char	*param;
/*
 * A parameter changed.  Deal with it.
 */
{
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
mw_FrameAction (w, event, params, num_params)
Widget	w;
XEvent	*event;
String	*params;
Cardinal	*num_params;
/*
 * Action procedure called when one of the frame buttons is used.
 */
{
	int	frame;
/*
 * The frame number is just the widget's radio data value - 1.
 */
	XtVaGetValues (w, XtNradioData, &frame, NULL);
	frame--;
	mw_ShowFrame (&frame);
}

	


static void
mw_ShowFrame (frame)
int	*frame;
/*
 * Draw the chosen frame.
 */
{
	char	string[40];
/*
 * We select the FrameButton by its "radioData", which is just the frame 
 * number + 1.
 */
	XawToggleSetCurrent (RadioGroup, (XtPointer)(long)(*frame + 1));
/*
 * Stash the desired forecast offset and plot time in the PD
 */
	ForecastOffset = Foffsets[*frame];

	sprintf (string, "%lis", ForecastOffset);
	pd_Store (Pd, "global", "forecast-offset", string, SYMT_STRING);

	PlotTime = IssueTime;
	if (ValidationMode)
		PlotTime.zt_Sec += ForecastOffset;
/*
 * Make sure DM and pdmon get the changes
 */
	Eq_AddEvent (PWhenever, eq_ReturnPD, 0, 0, Bounce);
	pdm_ScheduleUpdate ();
/*
 * Replot
 */
	ChangeCursor (WModel, BusyCursor);
	px_PlotExec ("global");
	ChangeCursor (WModel, None);
/*
 * Finish up
 */
	DisplayedFrame = *frame;

	sprintf (string, "%d hr forecast", Foffsets[DisplayedFrame] / 3600);
	mw_SetStatus (string);
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
	Boolean	use_frame;
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
 * Find the next loop-enabled frame and display it
 */
	frame = DisplayedFrame;
	use_frame = False;

	while (! use_frame)
	{
		frame = (frame + Nframes + step) % Nframes;
	/*
	 * If we go all the way around and don't find a frame, just keep
	 * the one we have.
	 */
		if (frame == DisplayedFrame)
			break;
	/*
	 * See if this frame is enabled for use in the loop
	 */
		XtVaGetValues (UseButton[frame], XtNstate, &use_frame, NULL);
	}

	mw_ShowFrame (&frame);
}

	

static void
mw_ValidOrIssue (w, id_ptr, junk)
Widget	w;
XtPointer	id_ptr, junk;
/*
 * The 'valid' or 'issue' button was pressed
 */
{
	char	id = (char)((long) id_ptr);
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
 * Otherwise toggle
 */
	ValidationMode = (id == 'V');
	parameter ("global", "validation-mode", 
		   ValidationMode ? "true" : "false");
/*
 * Tweak plot time
 */
	if (ValidationMode)
		PlotTime.zt_Sec += ForecastOffset;
	else
		PlotTime.zt_Sec -= ForecastOffset;
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
	ZebTime	prevtime;
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
	prevtime = IssueTime;

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

	if (TC_Eq (prevtime, IssueTime))
		return;
/*
 * Use parameter() to update the PD and replot
 */
	parameter ("global", "plot-time", itstring);
}




static void
mw_UseAllOrNone (w, junk1, junk2)
Widget	w;
XtPointer	junk1, junk2;
/*
 * If any frames are set to be used in the loop, turn them all off.  Otherwise
 * turn them all on.
 */
{
	int	i;
	Boolean	state, any_on = False;
/*
 * See if any of the frames are currently "enabled"
 */
	for (i = 0; i < Nframes; i++)
	{
		XtVaGetValues (UseButton[i], XtNstate, &state, NULL);
		any_on |= state;
	}
/*
 * If any were on, turn them all off, otherwise turn them all on
 */
	for (i = 0; i < Nframes; i++)
		XtCallActionProc (UseButton[i], any_on ? "unset" : "set",
				  NULL, NULL, 0);
}
