/*
 * The new event logger.
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

# include <stdio.h>
# include <errno.h>
# include <X11/Intrinsic.h>
# include <X11/Xaw/Form.h>
# include <X11/Shell.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/MenuButton.h>
# include <X11/Xaw/SimpleMenu.h>
# include <X11/Xaw/SmeBSB.h>
# include <X11/Xaw/SmeLine.h>
# include <X11/Xaw/Cardinals.h>
# include <X11/Xaw/Dialog.h>
# include <X11/Xaw/Toggle.h>
# include "defs.h"
# include "message.h"
# include "timer.h"
# include "dm.h"
# include "config.h"
# include "copyright.h"
# ifndef lint
MAKE_RCSID ("$Id: EventLogger.c,v 2.18 1993-09-07 18:19:53 granger Exp $")
# endif

# define EL_NAME "EventLogger"

#ifndef streq
#define streq(a,b) (!strcmp((a),(b)))
#endif

/*
 * Event mask info.  Emask is the bitwise or of Display_mask and Log_mask,
 * telling us all of the events we are accepting from clients and logging
 * to one place and/or another.
 */
#define DEFAULT_MASK (EF_EMERGENCY | EF_PROBLEM | EF_INFO)

int Emask = DEFAULT_MASK;		/* All the events we need 	*/
int Display_mask = DEFAULT_MASK;	/* Event mask for the display 	*/

struct EMMap
{
	int	em_flag;
	char	*em_name;
	char	em_code;
} EMap[] =
{
	{ EF_EMERGENCY,		"Emergencies",	'E' 	},
	{ EF_PROBLEM,		"Problems",	'P' 	},
	{ EF_CLIENT,		"Client events", 'C'	},
	{ EF_INFO,		"Informational", 'I'	},
	{ EF_DEBUG,		"Debugging",	'D' 	},
	{ 0, 0, 0 }
};


/*
 * Keep things from getting too big.
 */
# define MAXCHAR	5000	/* This is too big	*/
# define TRIMCHAR	3000	/* Trim back to this	*/

/*
 * Text info.
 */
static int Buflen = 0;
static char *Initmsg = "$Id: EventLogger.c,v 2.18 1993-09-07 18:19:53 granger Exp $\n\
Copyright (C) 1991 UCAR, All rights reserved.\n";

/*
 * Special info to enable debugging logging on selected clients only.
 */
stbl ProcTable;		/* Keep track of all processes */
typedef struct _ProcInfo
{
	char	pi_name[40];	/* Process name		*/
	Widget	pi_menu;	/* The menu entry	*/
	bool	pi_enabled;	/* Is debugging enabled? */
} ProcInfo;

/*
 * Our widgets.
 */
Widget Top, Text, Shell, Form, Wm, ProcMenu;
XtAppContext Appc;
bool Visible = FALSE;
bool Override = TRUE;

/*
 * Currently active entry from timestamp periods menu
 */
#ifdef notdef
#define DEFAULT_PERIOD 	300 	/* 5 minutes */
bool TimestampEnabled = FALSE;	/* false until timer started */
#endif

#define DEFAULT_PERIOD 	0 	/* must start out disabled */
int TSSecs = -1;
Widget TimestampEntry = NULL;
bool TimestampEnabled = TRUE;	/* must assume timer is around already, */
   /* but we won't try anything until the user explicitly requests it	*/

static String Resources[] = 
{
	"	*input:		True",
	"	*Label*font:	-*-helvetica-bold-r-*-*-*-120-*-*-*-*-*-*",
	"	*Toggle*font:	-*-helvetica-medium-r-*-*-*-120-*-*-*-*-*-*",
	"	*Text*font:	-*-times-medium-r-*-*-*-120-*-*-*-*-*-*",
	0
};

/*
 * Bitmap for our check mark in the menus
 */
#define check_width ((unsigned int) 9)
#define check_height ((unsigned int) 8)
const char Check_bits[] = {
	0x00, 0x01, 0x80, 0x01, 0xc0, 0x00, 0x60, 0x00,
	0x31, 0x00, 0x1b, 0x00, 0x0e, 0x00, 0x04, 0x00
};
Pixmap Check;

/*
 * Bitmap for the log file popup
 */
#define scroll_width 32
#define scroll_height 32
static char scroll_bits[] = {
   0x00, 0xe0, 0x01, 0x00, 0x00, 0x10, 0x1f, 0x00, 0x00, 0x08, 0xe0, 0x01,
   0x00, 0x08, 0x00, 0x1e, 0x00, 0x08, 0x00, 0x38, 0x00, 0x10, 0x00, 0x44,
   0x00, 0xe0, 0x00, 0x82, 0x00, 0x40, 0x07, 0x82, 0x00, 0x20, 0x78, 0x82,
   0x00, 0x20, 0x80, 0xc7, 0x00, 0x10, 0x00, 0x78, 0x00, 0x48, 0x00, 0x40,
   0x00, 0x28, 0x00, 0x40, 0x00, 0x14, 0x00, 0x20, 0x00, 0x12, 0x06, 0x20,
   0x00, 0x0a, 0x89, 0x10, 0x00, 0x89, 0x48, 0x13, 0x80, 0x90, 0x28, 0x08,
   0x80, 0xa0, 0x24, 0x08, 0x40, 0x00, 0x13, 0x04, 0x20, 0x00, 0x88, 0x02,
   0x2c, 0x00, 0xb0, 0x02, 0xf2, 0x00, 0x40, 0x01, 0x02, 0x0f, 0x00, 0x01,
   0x01, 0xf0, 0x80, 0x00, 0x01, 0x00, 0x87, 0x00, 0x01, 0x80, 0x48, 0x00,
   0x02, 0x40, 0x50, 0x00, 0x7c, 0x40, 0x30, 0x00, 0x80, 0x47, 0x10, 0x00,
   0x00, 0xf8, 0x08, 0x00, 0x00, 0x80, 0x07, 0x00};
Pixmap Scroll;


FILE *Log_file = (FILE *) 0;	/* File being logged to 		*/
char Log_path[320];		/* File path of log file		*/
bool Log_enabled = FALSE;	/* Whether actively logging to the file	*/
int Log_mask = DEFAULT_MASK;	/* Event mask for the log file		*/

/*
 * Is there somebody out there interested in our problems?  If so, this
 * is their name.
 */
char *Mother = 0;

/*
 * Forwards.
 */
void	SendToMother FP((char code, char *from, char *text));
Widget	MakeDbgButton FP ((Widget));
void	NewProc FP ((char *));
void	DeadProc FP ((char *));
void	ToggleProc FP ((Widget, XtPointer, XtPointer));
int	SendDbgMask FP ((char *, int, SValue *, long));
int	PassDebug FP ((int,  char *));
static void AppendToLog FP ((char *buf));
static Widget MakeTimestampButton FP ((Widget left, int default_period));
static void TimestampSetup FP ((int period));
static void Timestamp FP ((ZebTime *t, void *param));
static void TimestampCallback FP ((Widget w, XtPointer client_data, 
				   XtPointer call_data));
static void PopupLogSettings FP ((Widget w, XtPointer client_data,
				  XtPointer call_data));
static void LogToggle FP ((Widget w, XtPointer client_data, 
			   XtPointer call_data));
static Widget CreateLogSettings FP ((Widget w, Widget top));
static void CreateMaskMenu FP ((Widget button, char *name, int *maskp));
static void ChangeMaskCallback FP ((Widget w, XtPointer, XtPointer));
static void LogMessage FP ((int flag,  char *from,  char *text));
static char *FormatMessage FP ((char code, char *from_in,
				char *msg_in));
static void AppendToDisplay FP ((char *fmtbuf));
static void AppendToLogFile FP ((char *fmtbuf));
static void DoLog FP ((char *from, char *msg));



#define USAGE \
"EventLogger [-h] [-o|-w] [-f logfile] [-m eventmask] [-l filemask] [mom]\n\
   -h\tPrint this usage message\n\
   -o\tOverride redirect---give display manager control\n\
   -w\tNo override redirect---gives window manager control\n\
   -f\tSpecify a file to which log messages will be written\n\
   -m\tSpecify the event mask for the EventLogger display\n\
   -l\tSpecify the event mask for the log file\n\
   mom\tThe name of a process to echo emergencies and problems to\n\
Environment variables are checked if the equivalent option above\n\
is not present:\n\
   ZEB_LOGFILE	The name of the log file.\n\
   LOG_MASK	The event mask for the log file.  If not specified, it\n\
                defaults to the value of EVENT_MASK.\n\
   EVENT_MASK	The event mask for the display\n\
Specifying a log file automatically enables file logging.  File and\n\
display masks default to Emergency, Problem, and Information messages.\n\
If the event masks are not specified as numbers, they will be interpreted\n\
as strings of characters naming the events to include.  For example,\n\
'epid' represents Emergency, Problem, Information, and Debugging messages.\n"



static int
StringToMask (flags)
const char *flags;
/*
 * Convert the characters in the string 'flags' to an event mask
 */
{
	int mask, i;
	const char *c;

	mask = 0;
	for (c = flags; *c; ++c)
		switch (tolower(*c))
		{
		   case 'e':
			mask |= EF_EMERGENCY;
			break;
		   case 'p':
			mask |= EF_PROBLEM;
			break;
		   case 'c':
			mask |= EF_CLIENT;
			break;
		   case 'i':
			mask |= EF_INFO;
			break;
		   case 'd':
			mask |= EF_DEBUG;
			break;
		   default:
			printf ("Illegal character '%c' in mask\n");
			break;
		}
	return (mask);
}



static void
GetOptions (argc, argv)
int argc;
char *argv[];
/*
 * Process command-line options and environment variables to establish
 * our initial settings.  Unfortunately, because the command line is
 * also parsed by XtAppInitialize, we have to make sure these options
 * do not conflict with the standard Xt options.
 */
{
	extern int optind, opterr;
	extern char *optarg;
	bool dmask_set = FALSE, lmask_set = FALSE;
	char c;

	Log_path[0] = '\0';
	while ((c = getopt(argc, argv, "howf:m:l:")) != -1)
	{
		switch (c)
		{
		   case 'h':
			printf ("%s", USAGE);
			exit (0);
		   case 'o':
			Override = TRUE;
			break;
		   case 'w':
			Override = FALSE;
			break;
		   case 'f':
			strcpy (Log_path, optarg);
			break;
		   case 'm':
			Display_mask = atoi(optarg);
			if (!Display_mask)
				Display_mask = StringToMask (optarg);
			dmask_set = TRUE;
			break;
		   case 'l':
			Log_mask = atoi(optarg);
			if (!Log_mask)
				Log_mask = StringToMask (optarg);
			lmask_set = TRUE;
			break;
		   case '?':
			printf ("%s", USAGE);
			exit (1);
		}
	}

	/*
	 * If there is an argument left, it's somebody to send problems to.
	 */
	if (optind < argc)
		Mother = argv[optind];

	/*
	 * Try to initialize missing options from the environment,
	 * otherwise we go to defaults.
	 */
	if (!Log_path[0])
	{
		if (getenv ("ZEB_LOGFILE"))
			strcpy (Log_path, (char *) getenv ("ZEB_LOGFILE"));
	}
	if (!dmask_set)
	{
		if (getenv ("EVENT_MASK"))
		{
			Display_mask = atoi (getenv ("EVENT_MASK"));
			if (!Display_mask)
			   Display_mask = StringToMask (getenv("EVENT_MASK"));
		}
		else
			Display_mask = DEFAULT_MASK;
	}
	if (!lmask_set)
	{
		if (getenv ("LOG_MASK"))
		{
			Log_mask = atoi(getenv ("LOG_MASK"));
			if (!Log_mask)
			   Log_mask = StringToMask (getenv("LOG_MASK"));
		}
		else
			Log_mask = Display_mask;
	}
#ifdef notdef
	if (getenv ("EVENT_TIMESTAMP"))
		TSSecs = atoi (getenv ("EVENT_TIMESTAMP"));
	else
#endif
	TSSecs = DEFAULT_PERIOD;

	Emask = Log_mask | Display_mask;

	/*
	 * Open the log file if we got one.
	 */
	if (Log_path[0])
	{
		Log_file = fopen (Log_path, "w");

		if (! Log_file)
		{
			printf ("Error %d opening log file '%s'\n", errno, 
				Log_path);
			Log_enabled = FALSE;
		}
		else
			Log_enabled = TRUE;
	}
}



main (argc, argv)
int argc;
char **argv;
{
	Arg args[20];
	Widget w, label;
	int xevent (), msg_event (), clearbutton (), wm (), n;
	char *fname;
	char buf[128];
/*
 * Get set up with the toolkit, removing it's options first.
 */
	Top = XtAppInitialize (&Appc, "EventLogger", NULL, 0, &argc, argv,
		Resources, NULL, 0);
	Check = XCreateBitmapFromData (XtDisplay (Top),
		RootWindowOfScreen (XtScreen (Top)), Check_bits,
		check_width, check_height);
	Scroll = XCreateBitmapFromData (XtDisplay (Top),
		RootWindowOfScreen (XtScreen (Top)), scroll_bits,
		scroll_width, scroll_height);
/*
 * Retrieve our command line options and initialize parameters accordingly
 */
	GetOptions (argc, argv);
/*
 * Initialize UI.
 */
	usy_init ();
	ProcTable = usy_c_stbl ("ProcTable");
/*
 * Hook into the message system.
 */
	if (! msg_connect (msg_event, "Event logger"))
	{
		printf ("Unable to connect to message handler\n");
		exit (1);
	}
/*
 * Create our shell.
 */
	n = 0;
	XtSetArg (args[n], XtNinput, True);		n++;
	XtSetArg (args[n], XtNoverrideRedirect, 
		  (Override) ? True : False);		n++;
	XtSetArg (args[n], XtNallowShellResize, True);	n++;
	Shell = XtCreatePopupShell ("Event Logger", topLevelShellWidgetClass,
		Top, args, n);
/*
 * Put a form inside it.
 */
	Form = XtCreateManagedWidget ("form", formWidgetClass, Shell, args, 0);
/*
 * The label.
 */
	XtSetArg (args[0], XtNfromHoriz, NULL);
	XtSetArg (args[1], XtNfromVert, NULL);
	XtSetArg (args[2], XtNlabel, "Event Logger: ");
	XtSetArg (args[3], XtNborderWidth, 0);
	XtSetArg (args[4], XtNtop, XtChainTop);
	XtSetArg (args[5], XtNbottom, XtChainTop);
	XtSetArg (args[6], XtNleft, XtChainLeft);
	XtSetArg (args[7], XtNright, XtChainLeft);
	label = XtCreateManagedWidget ("label", labelWidgetClass, Form,args,8);
/*
 * Add the clear button.
 */
	XtSetArg (args[0], XtNfromHoriz, label);
	XtSetArg (args[1], XtNfromVert, NULL);
	XtSetArg (args[2], XtNtop, XtChainTop);
	XtSetArg (args[3], XtNbottom, XtChainTop);
	XtSetArg (args[4], XtNleft, XtChainLeft);
	XtSetArg (args[5], XtNright, XtChainLeft);
	w = XtCreateManagedWidget ("Clear", commandWidgetClass, Form, args, 6);
	XtAddCallback (w, XtNcallback, (XtCallbackProc) clearbutton, 
		(XtPointer) 0);
/*
 * The window manager button.
 */
	XtSetArg (args[0], XtNfromHoriz, w);
	XtSetArg (args[1], XtNfromVert, NULL);
	XtSetArg (args[2], XtNtop, XtChainTop);
	XtSetArg (args[3], XtNbottom, XtChainTop);
	XtSetArg (args[4], XtNleft, XtChainLeft);
	XtSetArg (args[5], XtNright, XtChainLeft);
	XtSetArg (args[6], XtNlabel, "Ctl: DM");
	w = Wm = XtCreateManagedWidget("wm",commandWidgetClass, Form, args, 7);
	XtAddCallback (Wm, XtNcallback, (XtCallbackProc) wm, (XtPointer) 0);
/*
 * The filter button for the display mask.
 */
	XtSetArg (args[0], XtNfromHoriz, w);
	XtSetArg (args[1], XtNfromVert, NULL);
	XtSetArg (args[2], XtNtop, XtChainTop);
	XtSetArg (args[3], XtNbottom, XtChainTop);
	XtSetArg (args[4], XtNleft, XtChainLeft);
	XtSetArg (args[5], XtNright, XtChainLeft);
	w = XtCreateManagedWidget ("Events ->", menuButtonWidgetClass,
		Form, args, 6);
	CreateMaskMenu (w, "DisplayMask", &Display_mask);
/*
 * The debug button.
 */
	w = MakeDbgButton (w);
/*
 * The timestamp interval button
 */
	w = MakeTimestampButton (w, TSSecs);
/*
 * The log file settings button and popup callback.  The Shell will parent
 * the popup shell, and also let the settings pop down when the Shell does.
 */
	XtSetArg (args[0], XtNfromHoriz, w);
	XtSetArg (args[1], XtNfromVert, NULL);
	XtSetArg (args[2], XtNtop, XtChainTop);
	XtSetArg (args[3], XtNbottom, XtChainTop);
	XtSetArg (args[4], XtNleft, XtChainLeft);
	XtSetArg (args[5], XtNright, XtChainLeft);
	XtSetArg (args[6], XtNmenuName, "EventMasks");
	w = XtCreateManagedWidget ("logfile", commandWidgetClass,
				   Form, args, 7);
	XtAddCallback (w, XtNcallback, PopupLogSettings, Shell);
/*
 * Now the big, hairy, text widget.
 */
	XtSetArg (args[0], XtNresize, XawtextResizeNever);
	XtSetArg (args[1], XtNfromHoriz, NULL);
	XtSetArg (args[2], XtNfromVert, label);
	XtSetArg (args[3], XtNtype, XawAsciiString);
	XtSetArg (args[4], XtNeditType, XawtextRead);
	XtSetArg (args[5], XtNscrollVertical, XawtextScrollAlways);
	XtSetArg (args[6], XtNtop, XtChainTop);
	XtSetArg (args[7], XtNstring, Initmsg);
	XtSetArg (args[8], XtNleft, XtChainLeft);
	XtSetArg (args[9], XtNbottom, XtChainBottom);
	XtSetArg (args[10], XtNright, XtChainRight);
	Text = XtCreateManagedWidget ("text", asciiTextWidgetClass, Form,
		args, 11);
	Buflen = strlen (Initmsg);
/*
 * Join the client event and event logger groups.
 */
	msg_join ("Client events");
	msg_join ("Event logger");
/*
 * Tell msglib about our X connection.
 */
	msg_add_fd (XConnectionNumber (XtDisplay (Shell)), xevent);
	reconfig (625, 600, 500, 150);
/*
 * Log a message about our log file, if there is one
 */
	if (Log_file)
		sprintf (buf, "Logging to file '%s'", Log_path);
	else
		sprintf (buf, "No log file specified.");
	LogMessage (EF_INFO, EL_NAME, buf);
/*
 * Now we wait.
 */
	sync ();
	xevent ();
	msg_await ();
}





Widget
MakeDbgButton (left)
Widget left;
/*
 * Create the menu button to control debugging.
 */
{
	Widget button;
	Arg args[10];
	int n;
/*
 * Create the menu button to start with.
 */
	n = 0;
	XtSetArg (args[0], XtNfromHoriz, left);		n++;
	XtSetArg (args[1], XtNfromVert, NULL);		n++;
	XtSetArg (args[2], XtNtop, XtChainTop);		n++;
	XtSetArg (args[3], XtNbottom, XtChainTop);	n++;
	XtSetArg (args[4], XtNleft, XtChainLeft);	n++;
	XtSetArg (args[5], XtNright, XtChainLeft);	n++;
	XtSetArg (args[6], XtNmenuName, "Processes");	n++;
	button = XtCreateManagedWidget ("Debug ->", menuButtonWidgetClass,
		Form, args, n);
/*
 * The menu that goes with it.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Debugging");	n++;
	ProcMenu = XtCreatePopupShell ("Processes", simpleMenuWidgetClass,
		Top, args, n);
/*
 * The actual entries will be created as the connect notifications arrive.
 */
	return (button);
}



static Widget
MakeTimestampButton (left, deflt)
Widget left;
int deflt;
/*
 * Create the menu button to control the timestamp period.
 */
{
	Widget button, menu, entry;
	Arg args[10];
	int n, i;
#	define NPERIOD_ENTRIES	9
	static int periods[10] = { 0, 1, 5, 10, 30, 60, 300, 600, 1800 };
	static char *pnames[10] = { "None", "1 second", "5 seconds", 
				    "10 seconds", "30 seconds", "1 minute",
				    "5 minutes", "10 minutes", "30 minutes" };
/*
 * Create the menu button to start with.
 */
	n = 0;
	XtSetArg (args[0], XtNfromHoriz, left);		n++;
	XtSetArg (args[1], XtNfromVert, NULL);		n++;
	XtSetArg (args[2], XtNtop, XtChainTop);		n++;
	XtSetArg (args[3], XtNbottom, XtChainTop);	n++;
	XtSetArg (args[4], XtNleft, XtChainLeft);	n++;
	XtSetArg (args[5], XtNright, XtChainLeft);	n++;
	XtSetArg (args[6], XtNmenuName, "Timestamps");	n++;
	button = XtCreateManagedWidget ("Timestamps ->", menuButtonWidgetClass,
					Form, args, n);
/*
 * The menu that goes with it.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Period");	n++;
	menu = XtCreatePopupShell ("Timestamps", simpleMenuWidgetClass,
				   Top, args, n);
	XtCreateManagedWidget ("line", smeLineObjectClass, menu, NULL, 0);
	XtSetArg (args[0], XtNleftMargin, check_width + 7);
	for (i = 0; i < NPERIOD_ENTRIES; ++i)
	{
		n = 1;
		XtSetArg (args[n], XtNlabel, pnames[i]); ++n;
		XtSetArg (args[n], XtNleftBitmap, None); ++n;
		entry = XtCreateManagedWidget ("entry", smeBSBObjectClass,
					       menu, args, n);
		if (periods[i] == deflt)
			TimestampEntry = entry;
		XtAddCallback (entry, XtNcallback, 
			       TimestampCallback, (XtPointer)periods[i]);
	/*
	 * If this is the default, and the default is 0, then it's ok
	 * to call the callback to draw the checkmark
	 */
		if ((periods[i] == 0) && (deflt == 0))
			XtCallCallbacks (TimestampEntry, XtNcallback, 0);
	}
	return (button);
}




/*ARGSUSED*/
static void
PopupLogSettings (w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
/*
 * Create the log settings button and the associated dialog popup
 */
{
	static Widget shell = NULL;

	/*
	 * If we don't yet have a shell, we need to create it first
	 */
	if (! shell)
	{
		Widget top = (Widget) client_data;
		shell = CreateLogSettings (w, top);
	}

	XtPopup (shell, XtGrabNone);
	XtSetSensitive (w, False);
}




static Widget
CreateLogSettings (w, top)
Widget w;	/* the button which will pop us up */
Widget top;	/* our top-level parent 	   */
{
	static XtPopdownIDRec pop_rec;
	Widget shell, dialog, toggle, mbutton;
	Arg args[10];
	Cardinal n;
	Position root_x, root_y;
	Dimension width, height;
	Dimension root_width, root_height;

	/*
	 * We need a popup shell, parented by the 'top' widget
	 * passed in as client data.
	 */
	shell = XtCreatePopupShell ("logpopup", transientShellWidgetClass,
				    top, NULL, 0);
	XtSetMappedWhenManaged (shell, False);

	/*
	 * Our shell's single child, the dialog widget.
	 */
	n = 0;
	XtSetArg (args[n], XtNvalue, Log_path);  n++;
	XtSetArg (args[n], XtNicon, Scroll); n++;
	dialog = XtCreateManagedWidget ("logdialog", dialogWidgetClass,
					shell, args, n);

	/*
	 * Now start adding the buttons we need.  The most basic is the one
	 * for popping down the shell.  We also add a popdown callback to
	 * the top shell so that this shell pops down when it does.
	 */
	pop_rec.shell_widget = shell;
	pop_rec.enable_widget = w;
	XawDialogAddButton (dialog, "dismiss", XtCallbackPopdown,
			    (XtPointer) &pop_rec);
	XtAddCallback (top, XtNpopdownCallback, XtCallbackPopdown,
		       (XtPointer) &pop_rec);

	/*
	 * Toggle button for activating and deactivating file logging
	 */
	n = 0;
	XtSetArg (args[n], XtNlabel, (Log_enabled) ? "Enabled" : "Disabled");
	++n;
	toggle = XtCreateManagedWidget ("toggle", commandWidgetClass,
					dialog, args, n);
	XtAddCallback (toggle, XtNcallback, LogToggle, (XtPointer)dialog);

	/*
	 * Now most importantly, create a pulldown menu for changing the mask
	 */
	mbutton = XtCreateManagedWidget ("mask", menuButtonWidgetClass,
					 dialog, NULL, 0);
	CreateMaskMenu (mbutton, "FileMask", &Log_mask);

	/*
	 * Realize the shell to get it to calculate its size; note that 
	 * mapped_when_managed is false, so its window will not be mapped.
	 */
	XtRealizeWidget (shell);

	/*
	 * Get its size, and use this to position it entirely on the screen.
	 */
	n = 0;
	XtSetArg (args[n], XtNwidth, &width); n++;
	XtSetArg (args[n], XtNheight, &height); n++;
	XtGetValues (shell, args, n);
	root_width = WidthOfScreen(XtScreen(shell));
	root_height = HeightOfScreen(XtScreen(shell));
	XtTranslateCoords (w, 5, 5, &root_x, &root_y);
	if (root_x + width + 5 > root_width)
		root_x = root_width - width - 5;  /* minus any borders */
	if (root_y + height + 5 > root_height)
		root_y = root_height - height - 5;
	n = 0;
	XtSetArg (args[n], XtNx, root_x); n++;
	XtSetArg (args[n], XtNy, root_y); n++;
	XtSetValues (shell, args, n);
	XtSetMappedWhenManaged (shell, True);
	return (shell);
}




/*ARGSUSED*/
static void
LogToggle (w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
/*
 * Change the status of file logging.  If we're currently enabled, just
 * change to disable.  If we have to enable, check the file path in the
 * dialog widget value.  If the path has changed, we need to close the any
 * former log file and try to open a new one.
 */
{
	Widget dialog = (Widget) client_data;
	char *filepath;
	Arg arg;
	char buf[128];

	if (Log_enabled == TRUE)
	{
		Log_enabled = FALSE;
		XtSetArg (arg, XtNlabel, "Disabled");
		XtSetValues (w, &arg, (Cardinal)1);
		LogMessage (EF_DEBUG, EL_NAME, "File logging disabled.");
		return;
	}

	filepath = XawDialogGetValueString (dialog);
	if (Log_path[0] && streq(filepath, Log_path))
	{
		/*
		 * File path is still the same, so just re-enable logging
		 */
		Log_enabled = TRUE;
		XtSetArg (arg, XtNlabel, "Enabled");
		XtSetValues (w, &arg, (Cardinal)1);
		LogMessage (EF_DEBUG, EL_NAME, "File logging enabled.");
		return;
	}

	/*
	 * Oh well, do the tough stuff.  If we have a log file currently,
	 * then close it.  Then try to open the new one.
	 */
	if (Log_file)
	{
		fclose (Log_file);
		sprintf (buf, "Log file %s closed.", Log_path);
		LogMessage (EF_INFO, EL_NAME, buf);
	}
	Log_file = NULL;
	strcpy (Log_path, filepath);

	if (filepath[0])	/* avoid empty strings */
	{
		Log_file = fopen (Log_path, "w");
		if (! Log_file)
		{
			sprintf (buf, "Error %d opening log file '%s'",
				 errno, filepath);
			LogMessage (EF_PROBLEM, EL_NAME, buf);
		}
		else
		{
			sprintf (buf, "Log file %s opened.", filepath);
			LogMessage (EF_INFO, EL_NAME, buf);
		}
	}
	else
	{
		LogMessage (EF_PROBLEM, EL_NAME, 
			    "Could not open log file, no file name specified");
	}

	Log_enabled = (Log_file) ? TRUE : FALSE;
	XtSetArg (arg, XtNlabel, (Log_enabled) ? "Enabled" : "Disabled");
	XtSetValues (w, &arg, (Cardinal)1);
	sprintf (buf, "File logging %s.", 
		 (Log_enabled) ? "enabled" : "disabled");
	LogMessage (EF_DEBUG, EL_NAME, buf);
}




static void
CreateMaskMenu (button, name, maskp)
Widget button;	/* The menu button used to pop up this menu		*/
char *name;	/* The name to give this menu; should be unique 	*/
int *maskp;	/* Pointer to the mask field which will be changed	*/
/*
 * Add the event popup menu to this widget.
 */
{
	Widget pulldown, w;
	int i;
	Arg args[2];
/*
 * Make the pulldown.
 */
	pulldown = XtCreatePopupShell (name, simpleMenuWidgetClass,
				       button, NULL, ZERO);
	XtSetArg (args[0], XtNmenuName, name);
	XtSetValues (button, args, (Cardinal)1);
/*
 * Add the entries.  In Ardent land, we can not set the bitmap in at 
 * creation time, for whatever reason.
 */
	XtSetArg (args[0], XtNleftMargin, check_width + 7);
	for (i = 0; EMap[i].em_flag; i++)
	{
		XtSetArg (args[1], XtNleftBitmap,
			EMap[i].em_flag & (*maskp) ? Check : None);
		w = XtCreateManagedWidget (EMap[i].em_name, smeBSBObjectClass,
					   pulldown, args, 2);
		XtAddCallback (w, XtNcallback, 
			       (XtCallbackProc) ChangeMaskCallback,
			       (XtPointer) maskp);
	}
}





/*ARGSUSED*/
static void
ChangeMaskCallback (w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
/*
 * Deal with the event popup.  The client_data is a pointer to the global
 * mask variable which we are changing.  The menu entry object making the
 * callback, w, must also have its left bitmap changed.  We get the flag
 * being changed from the name of the widget.
 */
{
	int *mask = (int *)client_data;
	int i;
	Arg args[2];

	for (i = 0; EMap[i].em_flag; ++i)
	{
		if (streq(EMap[i].em_name, XtName(w)))
			break;
	}
	if (!EMap[i].em_flag)	/* flag not found, for whatever reason */
		return;
/*
 * Tweak the event mask bit.
 */
	(*mask) ^= EMap[i].em_flag;
/*
 * Modify the menu accordingly.
 */
	XtSetArg (args[0], XtNleftBitmap,
		  EMap[i].em_flag & (*mask) ? Check : None);
	XtSetValues (w, args, (Cardinal)1);
/*
 * Update the global mask and broadcast it.
 */
	Emask |= (*mask);
	SendEMask ();
}





xevent ()
/*
 * Deal with an Xt event.
 */
{
	XEvent event;
/*
 * Deal with events as long as they keep coming.
 */
	sync ();
 	while (XtAppPending (Appc))
	{
		XtAppNextEvent (Appc, &event);
		XtDispatchEvent (&event);
		sync ();
	}
	return (0);
}






msg_event (msg)
struct message *msg;
/*
 * Log a client event.
 */
{
	struct mh_client *client = (struct mh_client *) msg->m_data;
	char mb[200];
/*
 * Check for log messages.
 */
	if (msg->m_proto == MT_LOG)
	{
		DoLog (msg->m_from, msg->m_data);
		return (0);
	}
/*
 * Maybe it's a display manager message.
 */
	else if (msg->m_proto == MT_DISPLAYMGR)
	{
		dm_msg (msg->m_data);
		return (0);
	}
/*
 * If it's an extended message, do something with it.
 */
	else if (msg->m_proto == MT_ELOG)
	{
		struct msg_elog *el = (struct msg_elog *) msg->m_data;

		LogMessage (el->el_flag, msg->m_from, el->el_text);
		return (0);
	}
/*
 * Everything else is assumed to be a message handler event.
 */	
	switch (client->mh_type)
	{
	   case MH_CLIENT:
	   	switch (client->mh_evtype)
		{
		   case MH_CE_CONNECT:
			NewProc (client->mh_client);
		   	sprintf (mb,"Connect on %d", msg->m_seq);
			break;
		   case MH_CE_DISCONNECT:
			DeadProc (client->mh_client);
		   	sprintf (mb,"Disconnect on %d", msg->m_seq);
			break;
		   case MH_CE_JOIN:
		   	sprintf (mb,"Group %s joined on %d",
				client->mh_group, msg->m_seq);
			break;
		   case MH_CE_QUIT:
		   	sprintf (mb,"Group %s quit on %d",
				client->mh_group, msg->m_seq);
			break;
		}
		if (Emask & EF_CLIENT || Emask == 0)
			LogMessage (EF_CLIENT, client->mh_client, mb);
		break;
	   case MH_SHUTDOWN:
		exit (0);
		break;
	   default:
	   	sprintf (mb, "Strange message type: %d %d", msg->m_proto,
			client->mh_type);
	   	LogMessage (EF_PROBLEM, "EventLogger", mb);
		break;
	}
	return (0);
}



static void
LogMessage (flag, from, text)
int flag;
char *from;
char *text;
/*
 * Check our flag and masks and write the log message to whomever wants it
 */
{
	int i;
	char code;
	char *msg;

	/*
	 * If somebody's tweaking the event mask, we'll ignore it.
	 */
	 	if (flag & EF_SETMASK)
			return;
	/*
	 * If this is in our mask, we log it.  Assume flags are in order of
	 * importance, and take the first code character present in the
	 * mask and exit the loop, rather than overwrite it with the
	 * presence of less important flags.
	 */
	code = '?';
	for (i = 0; EMap[i].em_flag; i++)
		if (EMap[i].em_flag & flag)
		{
			code = EMap[i].em_code;
			break;
		}
	/*
	 * Things in the PROBLEM and EMERGENCY classes get sent back to 
	 * mother.
	 */
	if (Mother && (flag & (EF_PROBLEM | EF_EMERGENCY)))
		SendToMother (code, from, text);
	/*
	 * If we need to log this message, format it
	 */
	if ((Emask & flag) || Emask == 0 ||
	    PassDebug (flag, from))
	{
		msg = FormatMessage (code, from, text);
		if (!msg)
			return;
	}
	else
	{
		/*
		 * Otherwise assume that somebody is out of sync with the
		 * event mask, and we send it out to the world.
		 */
		static int nsend = 0;
		if ((nsend++ % 15) == 0)
			SendEMask ();
			return;
	}
	/*
	 * Now find out what gets this message and send it along
	 */
	if ((flag & Log_mask) || PassDebug (flag, from))
		AppendToLogFile (msg);
	if ((flag & Display_mask) || PassDebug (flag, from))
		AppendToDisplay (msg);
}




static void
DoLog (from, msg)
char *from;
char *msg;
/*
 * Log a message that has no flags.  Assign a code of '-' and
 * send it everywhere.
 */
{
	char *fmt;

	fmt = FormatMessage ('-', from, msg);
	AppendToLogFile (fmt);
	AppendToDisplay (fmt);
}



static char *
FormatMessage (code, from_in, msg_in)
char code;
char *from_in;
char *msg_in;
/*
 * Format the code, from, and msg strings into the log message.  If the
 * message should not be logged for repeat reasons, NULL is returned.
 */
{
#	define FMTLEN 256
	static char fmtbuf[FMTLEN];
	static char last_msg[FMTLEN];
	static int repeat_count = 0;
	char msg[FMTLEN];
	char from[16];
/*
 * Format the message to be logged.  For robustness, make sure the msg is
 * not too long for our buffer.
 */
	strncpy (from, from_in, 15);
	from[15] = '\0';
	strncpy (msg, msg_in, FMTLEN - 30);
	msg[FMTLEN - 30] = '\0';
	sprintf (fmtbuf, "%c %-16s%s\n", code, from, msg);
/*
 * See if we've seen this message before: if so, print the repeat count
 * at regular intervals; otherwise reset repeat count and proceed as usual.
 * Note that repeat_count will always be at least one except for the first
 * time this function is entered.
 */
	if ((repeat_count > 0) && !strcmp (fmtbuf, last_msg))
	{
		++repeat_count;
		if ((repeat_count == 5) ||
		    ((repeat_count < 50) && !(repeat_count % 10)) ||
		    (!(repeat_count % 50)))
		{
			sprintf (fmtbuf,"%c %-16s%s, %d repeats\n",
				 'R', from, msg, repeat_count);
		}
		else if (repeat_count > 5)
		{
			return NULL;
		}
		/*
		 * Else leave fmtbuf unchanged
		 */
	}
	else
	{
		strcpy (last_msg, fmtbuf);
	/*
	 * If messages were previously being skipped, make a note about the
	 * final number of repeats.
	 */
		if (repeat_count > 5)
			sprintf (fmtbuf, 
				 "R %-14sLast message repeated %d times\n%s",
				 EL_NAME, repeat_count, last_msg);
		repeat_count = 1;
	}

	return (fmtbuf);
}



static void
AppendToLogFile (fmtbuf)
char *fmtbuf;
/*
 * Actually append the text to the log file
 */
{
	if (Log_file && Log_enabled)
	{
		fprintf (Log_file, fmtbuf);
		fflush (Log_file);
	}
}



static void
AppendToDisplay (fmtbuf)
char *fmtbuf;
/*
 * Actually append the text to the log file and to the text widget buffer
 */
{
	Arg args[10];
	XawTextBlock tb;

	tb.firstPos = 0;
	tb.length = strlen (fmtbuf);
	tb.ptr = fmtbuf;
	tb.format = FMT8BIT;
/*
 * Add it to the buffer.  Turn on editing only for long enough to do this
 * operation.
 */
	XtSetArg (args[0], XtNeditType, XawtextAppend);
	XtSetValues (Text, args, 1);
	XawTextReplace (Text, Buflen, Buflen, &tb);
/*
 * If this is getting too big, trim it back.
 */
	if ((Buflen += strlen (fmtbuf)) > MAXCHAR)
	{
		tb.firstPos = tb.length = 0;
		XawTextReplace (Text, 0, Buflen - TRIMCHAR, &tb);
		Buflen = TRIMCHAR;
	}
/*
 * Update.
 */
	XtSetArg (args[0], XtNeditType, XawtextRead);
	XtSetValues (Text, args, 1);
	XawTextDisplay (Text);
	sync ();
	XawTextSetInsertionPoint (Text, Buflen);
}




clearbutton ()
/*
 * Clear log message buffer, then clear text window.
 */	
{
	Arg args[2];

	AppendToLogFile ("\n");
	XtSetArg (args[0], XtNstring, "");
	XtSetValues (Text, args, 1);
	XawTextDisplay (Text);
	Buflen = 0;
}




sync ()
/*
 * Synchronize with the window system.
 */
{
	XSync (XtDisplay (Top), False);
}






dm_msg (dmsg)
struct dm_msg *dmsg;
/*
 * Deal with a DM message.
 */
{
	struct dm_hello reply;
/*
 * See what we got.
 */
	switch (dmsg->dmm_type)
	{
	/*
	 * Maybe it's a DM scoping us out.
	 */
	   case DM_HELLO:
	   	reply.dmm_type = DM_HELLO;
	   	reply.dmm_win = 0;
		msg_send ("Displaymgr", MT_DISPLAYMGR, FALSE, &reply, 
			sizeof (reply));
		break;
	/*
	 * Maybe it's a reconfig.
	 */
	   case DM_RECONFIG:
		if (Override)
		   	reconfig (dmsg->dmm_x, dmsg->dmm_y, dmsg->dmm_dx,
				dmsg->dmm_dy);
		break;
	/*
	 * They might want us to go away entirely.
	 */
	   case DM_SUSPEND:
	   	if (Visible && Override)
		{
			Visible = FALSE;
			XtPopdown (Shell);
		}
		break;
 	/*
	 * DM currently sends a defaults table, which does us very 
	 * little good.
	 */
	   case DM_DEFAULTS:
		break;

	   default:
	   	LogMessage (EF_PROBLEM, "DM", "Funky DM message");
	}
}







reconfig (x, y, w, h)
int x, y, w, h;
/*
 * Reconfigure the window.
 */
{
	Arg args[5];
	int n;
/*
 * For positioning, move the shell.
 */
	n = 0;
	XtSetArg (args[n], XtNx, x);	n++;
	XtSetArg (args[n], XtNy, y);	n++;
	XtSetValues (Shell, args, n);
/*
 * For sizing, change the form inside the shell.
 */
	n = 0;
	XtSetArg (args[n], XtNwidth, w);	n++;
	XtSetArg (args[n], XtNheight, h);	n++;
	XtSetValues (Form, args, n);
/* 
 * If they can't see us yet, make it so now.
 */
	if (! Visible)
	{
		XtPopup (Shell, XtGrabNone);
		Visible = TRUE;
	}
	sync ();
}






wm ()
/*
 * Try to change override redirect.
 */
{
	Arg args[2];
/*
 * If the window is up, take it down.
 */
	if (Visible)
		XtPopdown (Shell);
/*
 * Set the parameter.
 */
	Override = ! Override;
	XtSetArg (args[0], XtNoverrideRedirect, Override);
	XtSetValues (Shell, args, 1);
/*
 * Set the label on the command widget too.
 */
	if (Override)
		XtSetArg (args[0], XtNlabel, "Ctl: DM");
	else
		XtSetArg (args[0], XtNlabel, "Ctl: WM");
	XtSetValues (Wm, args, 1);
/*
 * Put the window back if it was before.
 */
	if (Visible)
		XtPopup (Shell, XtGrabNone);
}





SendEMask ()
/*
 * Broadcast the event mask to the world.
 */
{
	int flag;
/*
 * Broadcast out the default mask to everybody.
 */
	flag = Emask | EF_SETMASK;
	msg_send ("Everybody", MT_ELOG, TRUE, &flag, sizeof (flag));
/*
 * Now go through and fix up any processes for which debugging has been
 * requested.
 */
	usy_search (ProcTable, SendDbgMask, 0, FALSE, 0);
}





/*ARGSUSED*/
int 
SendDbgMask (proc, type, v, junk)
char *proc;
int type;
SValue *v;
long junk;
/*
 * Maybe send out a debug-enabled event mask to this process.
 */
{
	int flag = Emask | EF_SETMASK | EF_DEBUG;
	ProcInfo *pinfo = (ProcInfo *) v->us_v_ptr;

	if (pinfo->pi_enabled)
		msg_send (proc, MT_ELOG, FALSE, &flag, sizeof (flag));
	return (TRUE);
}




void
SendToMother (code, from, text)
char code;
char *from;
char *text;
/*
 * Problem messages go back to the data store.
 */
{
	char line[1000], mom[80];
/*
 * Put together a message.
 */
	sprintf (line, " %c %s %s", code, from, text);
	sprintf (mom, "Event Logger@%s", Mother);
	msg_send (mom, MT_LOG, FALSE, line, strlen (line) + 1);
}





void
NewProc (name)
char *name;
/*
 * Deal with a new process.
 */
{
	ProcInfo *pinfo = ALLOC (ProcInfo);
	Arg args[4];
	int n;
	SValue v;
/*
 * Create the widget for this process.
 */
	n = 0;
	XtSetArg (args[n], XtNleftBitmap, None);		n++;
	XtSetArg (args[n], XtNleftMargin, check_width + 7);	n++;
	pinfo->pi_menu = XtCreateManagedWidget (name, smeBSBObjectClass,
		ProcMenu, args, n);
	pinfo->pi_enabled = FALSE;
	strcpy (pinfo->pi_name, name);
	XtAddCallback (pinfo->pi_menu, XtNcallback, ToggleProc,
		(XtPointer) pinfo->pi_name);
/*
 * Add the symbol table entry, and we're done.
 */
	v.us_v_ptr = (char *) pinfo;
	usy_s_symbol (ProcTable, name, SYMT_POINTER, &v);
/*
 * Check for the timer process, so that we know we can start counting
 * our timestamp period.
 */
	if (!strcmp (name, TIMER_PROC_NAME))
	{
		TimestampEnabled = TRUE;
#ifdef notdef
		if (! TimestampEntry)
			TimestampSetup (TSSecs);
		else
			XtCallCallbacks (TimestampEntry, XtNcallback, 0);
#endif
	}		
}




void
DeadProc (name)
char *name;
/*
 * This process is gone.
 */
{
	int type;
	SValue v;
	ProcInfo *pinfo;
/*
 * See if it was the timer that died, if so disable future period changes.
 * Do it before the symbol-table lookup, as its highly likely that we
 * never received the client connect message for the timer.
 */
	if (! strcmp (name, TIMER_PROC_NAME))
	{
		TimestampEnabled = FALSE;
		TimestampSetup(0);		/* disable and reset slot */
	}
/*
 * Look up this process.
 */
	if (! usy_g_symbol (ProcTable, name, &type, &v))
		return;
/*
 * Free up everything.
 */
	pinfo = (ProcInfo *) v.us_v_ptr;
	XtDestroyWidget (pinfo->pi_menu);
	free (v.us_v_ptr);
	usy_z_symbol (ProcTable, name);
}





/*ARGSUSED*/
void
ToggleProc (w, proc, xpinfo)
Widget w;
XtPointer proc, xpinfo;
/*
 * Toggle logging for this process.
 */
{
	Arg args[2];
	ProcInfo *pinfo;
	SValue v;
	int type;
/*
 * Look up this process.
 */
	if (! usy_g_symbol (ProcTable, (char *) proc, &type, &v))
	{
		printf ("Proc '%s' not in ProcInfo!\n", proc);
		return;
	}
/*
 * Toggle the debug flag and tweak the widget.
 */
	pinfo = (ProcInfo *) v.us_v_ptr;
	pinfo->pi_enabled = ! pinfo->pi_enabled;
	XtSetArg (args[0], XtNleftBitmap, pinfo->pi_enabled ? Check : None);
	XtSetValues (w, args, 1);
/*
 * Tweak this process's event mask.
 */
	SendEMask ();
}





int
PassDebug (flag, name)
int flag;
char *name;
/*
 * Return TRUE if this is a debug message that should be logged even though
 * they are disabled in general.
 */
{
	SValue v;
	int type;
	ProcInfo *pinfo;
/*
 * If this is not a debug message, or we have not heard of this process,
 * then we say no.
 */
	if ((flag & EF_DEBUG) == 0 || ! usy_g_symbol (ProcTable,name,&type,&v))
		return (FALSE);
/*
 * Otherwise we need to see what the data structure says.
 */
	pinfo = (ProcInfo *) v.us_v_ptr;
	return (pinfo->pi_enabled);
}



static void
TimestampSetup(period)
int period;		/* timestamp interval, in seconds, 0 to disable */
{
	static slot = -1;
	ZebTime t;

	if (! TimestampEnabled)
	{
		TSSecs = period;
		slot = -1;
		return;
	}
	if (slot >= 0)
	{
		tl_Cancel (slot);
	}
	slot = -1;
/*
 * Set up our timestamp, start timestamp on multiple of the period
 */
	if (period > 0)
	{
		tl_Time (&t);
		t.zt_MicroSec = 0;
		t.zt_Sec = ((t.zt_Sec / period) + 1) * period;
		slot = tl_AbsoluteReq (Timestamp, 0, &t, period*INCFRAC);
	}
}



/*ARGSUSED*/
static void
Timestamp (t, param)
ZebTime *t;
void *param;
/*
 * Append a timestamp message to the log
 */
{
	char buf[100];
	char *msg;

	sprintf (buf, "Timestamp: ");
#ifdef notdef
	TC_EncodeTime (t, (t->zt_Sec % 3600 == 0) ? TC_Full : TC_TimeOnly,
		       buf + strlen(buf));
#endif
	TC_EncodeTime (t, TC_Full, buf + strlen(buf));
	msg = FormatMessage ('T', EL_NAME, buf);
	AppendToLogFile (msg);
	AppendToDisplay (msg);
}



/*ARGSUSED*/
static void
TimestampCallback (entry, client_data, call_data)
Widget entry;
XtPointer client_data;
XtPointer call_data;
{
	int period = (int)client_data;
	Arg args[2];

	TimestampSetup (period);
	XtSetArg (args[0], XtNleftBitmap, None);
	if (TimestampEntry)
		XtSetValues (TimestampEntry, args, 1);
	XtSetArg (args[0], XtNleftBitmap, Check);
	XtSetValues (entry, args, 1);
	TimestampEntry = entry;
}
