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

# include <stdlib.h>
# include <stdio.h>
# include <unistd.h>
# include <errno.h>
# include <ctype.h>
# include <sys/types.h>
# include <time.h>

# include <X11/Intrinsic.h>
# include <X11/Xaw/Form.h>
# include <X11/Shell.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/MenuButton.h>
# include <X11/Xaw/Cardinals.h>
# include <X11/Xaw/Dialog.h>
# include <X11/Xaw/Toggle.h>

# include <RdssMenu.h>
# include <SmeMenu.h>
# include <X11/Xaw/SmeBSB.h>
# include <X11/Xaw/SmeLine.h>

# include <defs.h>
# include <zl_symbol.h>
# include <message.h>
# include <timer.h>
# include <dm.h>
# include <config.h>
# include <copyright.h>

RCSID ("$Id: EventLogger.c,v 2.44 2001-11-30 00:45:07 granger Exp $")

# define LOGNAME "EventLogger"

#ifndef streq
#define streq(a,b) (!strcmp((a),(b)))
#endif

/*
 * Event mask info.  Emask is the bitwise or of Display_mask and Log_mask,
 * telling us all of the events we are accepting from clients and logging
 * to one place and/or another.
 */
#define DEFAULT_MASK (EF_EMERGENCY | EF_PROBLEM | EF_INFO)

int Emask = 0;				/* All the events we need 	*/
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
# define MAXCHAR	10000	/* This is too big	*/
# define TRIMCHAR	8000	/* Trim back to this	*/

# define FORTUNE_CMD 	"fortune -s"
# define FORTUNE_LEN 	512
# define FORTUNE_WAIT	300	/* default seconds to be idle bet fortunes */
static zbool TellFortune = FALSE;
static int FortuneWait = FORTUNE_WAIT;	/* secs idle time between fortunes */

/*
 * Text info.
 */
static int Buflen = 0;
static char Initmsg[256] = "";

/*
 * Special info to enable debugging logging on selected clients only.
 */
stbl ProcTable;		/* Keep track of all processes */
typedef struct _ProcInfo
{
	char	pi_name[40];	/* Process name		*/
	Widget	pi_menu;	/* The menu entry	*/
	zbool	pi_enabled;	/* Is debugging enabled? */
} ProcInfo;

/*
 * Our widgets.
 */
Widget Top, Text, Shell, Form, Wm, ProcMenu;
XtAppContext Appc;
zbool Visible = False;
zbool Override = True;

/*
 * Currently active entry from timestamp periods menu
 */
#ifdef notdef
#define DEFAULT_PERIOD 	300 	/* 5 minutes */
zbool TimestampEnabled = FALSE;	/* false until timer started */
#endif

#define DEFAULT_PERIOD 	0 	/* must start out disabled */
int TSSecs = -1;
Widget TimestampEntry = NULL;
zbool TimestampEnabled = TRUE;	/* must assume timer is around already, */
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
static const unsigned char Check_bits[] = {
	0x00, 0x01, 0x80, 0x01, 0xc0, 0x00, 0x60, 0x00,
	0x31, 0x00, 0x1b, 0x00, 0x0e, 0x00, 0x04, 0x00
};
Pixmap Check;

/*
 * Bitmap for the log file popup
 */
#define scroll_width 32
#define scroll_height 32
static const unsigned char scroll_bits[] = {
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
zbool Log_enabled = FALSE;	/* Whether actively logging to the file	*/
int Log_mask = DEFAULT_MASK;	/* Event mask for the log file		*/
zbool Echo = FALSE;		/* Echo log messages to stdout		*/
zbool Windows = TRUE;		/* Create X Window display		*/
#define MAX_SESSIONS 5		/* Names of sessions to join		*/
char *Session[MAX_SESSIONS+1] = { NULL };

/*
 * Is there somebody out there interested in our problems?  If so, this
 * is their name.
 */
char *Mother = 0;

/*
 * Forwards.
 */
void	SendToMother FP((int code, char *from, char *text));
Widget	MakeDbgButton FP ((Widget));
static void GroupList FP ((struct mh_members *mh));
static void AddProc FP ((char *));
static void NewProc FP ((char *));
static void DeadProc FP ((char *));
static void ToggleProc FP ((Widget, XtPointer, XtPointer));
static void ChangeMask FP ((int op));
static void SendEverybody FP ((int flag));
static void BroadcastEMask FP ((int op));
static void SendEMask FP ((char *who, int op));
int	SendDbgMask FP ((char *, int, SValue *, long));
int	PassDebug FP ((int,  char *));
static void dm_msg FP ((struct dm_msg *dmsg));
static void quitbutton FP ((void));
static void clearbutton FP ((void));
static void reconfig FP ((int x, int y, int w, int h));
static void wm FP ((void));
static void Sync ();
static void CreateEventLogger FP ((void));
static void SendGeometry FP((struct dm_msg *dmm));
static Widget MakeTimestampButton FP ((Widget left, int default_period));
static void TimestampSetup FP ((int period));
static void Timestamp FP ((ZebTime *t, void *param));
static void TimestampCallback FP ((Widget w, XtPointer client_data, 
				   XtPointer call_data));
static void PopupLogSettings FP ((Widget w, XtPointer client_data,
				  XtPointer call_data));
static void LogErase FP ((Widget w, XtPointer client_data, 
			  XtPointer call_data));
static void LogDisable FP ((Widget toggle));
static void LogToggle FP ((Widget w, XtPointer client_data, 
			   XtPointer call_data));
static Widget CreateLogSettings FP ((Widget w, Widget top));
static void CreateMaskMenu FP ((Widget button, char *name, int *maskp));
static void ChangeMaskCallback FP ((Widget w, XtPointer, XtPointer));
static void LogMessage FP ((int flag,  char *from,  char *text));
static char *FormatMessage (char code, char *from_in, char *msg_in);
static void AppendToDisplay FP ((char *fmtbuf));
static void AppendToLogFile FP ((char *fmtbuf));
static void DoLog FP ((char *from, char *msg));
static void LogFortune FP((void));
static void Wait FP ((void));
static int xevent FP((void));
static int msg_event FP((struct message *msg));


#define USAGE \
"EventLogger [-h]\n\
EventLogger [options] [-j secs] [-f logfile] [-m eventmask] [-l filemask]\
 [mom]\n\
   -h\t\tPrint this usage message\n\
   -j\t\tLog a fortune once in a while (0 defaults to 5 minutes)\n\
     \t\tThe fortune command must be in your path.\n\
   -n\t\tNon-windowed, non-interactive EventLogger\n\
   -e\t\tEcho log file messages to stdout\n\
   -o\t\tOverride redirect---give display manager control\n\
   -w\t\tNo override redirect---gives window manager control\n\
   -f\t\tSpecify a file to which log messages will be written\n\
   -m\t\tSpecify the event mask for the EventLogger display\n\
   -l\t\tSpecify the event mask for the log file\n\
   -s name\tJoin this session and report its events (experimental)\n\
   mom\t\tThe name of a process to echo emergencies and problems to\n\
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
	int mask;
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
			printf ("Illegal character '%c' in mask\n", *c);
			break;
		}
	return (mask);
}



static void
GetOptions (rargc, argv)
int *rargc;
char *argv[];
/*
 * Process command-line options and environment variables to establish
 * our initial settings.  Unfortunately, because the command line is
 * also parsed by XtAppInitialize, we have to make sure these options
 * do not conflict with the standard Xt options.
 */
{
	zbool dmask_set = FALSE, lmask_set = FALSE;
	char *optarg;
	int argc = *rargc;
	int i, j;
	int opt;
	char c;
	int s = 0;	/* session count */

	Log_path[0] = '\0';
	i = 1;
	while (i < argc)
	{
		opt = 0;
		optarg = NULL;
		if (argv[i][0] == '-')
			c = argv[i][1];
		else
		{
			++i;
			continue;
		}
		if (strchr ("jfmls", c))
		{
			opt = 1;
			if (i+1 < argc)
				optarg = argv[i+1];
			if (!optarg)
			{
				printf ("option '%c' requires argument\n", c);
				printf ("%s", USAGE);
				exit (1);
			}
		}
		switch (c)
		{
		   case 'h':
			printf ("%s", USAGE);
			exit (0);
		   case 'j':
			TellFortune = TRUE;
			FortuneWait = atoi(optarg);
			if (FortuneWait <= 0)
				FortuneWait = FORTUNE_WAIT;
			break;
		   case 'n':
			Windows = FALSE;
			break;
		   case 'o':
			Override = True;
			break;
		   case 'e':
			Echo = True;
			break;
		   case 'w':
			Override = False;
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
		   case 's':
			if (s < MAX_SESSIONS)
			{
				Session[s++] = optarg;
				Session[s] = NULL;
			}
			else
			{
				printf ("%s: limit is %d\n",
					"Too many sessions", MAX_SESSIONS);
			}
		}
		/*
		 * Now finally remove the option
		 */
		for (j = i; j < argc; ++j)
			argv[j] = argv[j + 1 + opt];
		argc -= 1 + opt;
	}

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
	*rargc = argc;
}



static void
CreateBitmaps()
{
	Check = XCreateBitmapFromData (XtDisplay (Top),
		RootWindowOfScreen (XtScreen (Top)), 
	        (const char *) Check_bits, check_width, check_height);
	Scroll = XCreateBitmapFromData (XtDisplay (Top),
		RootWindowOfScreen (XtScreen (Top)), 
		(const char *) scroll_bits, scroll_width, scroll_height);
}



static void
JoinGroups ()
/*
 * Join the client events and event logger groups.
 */
{
	int s = 0;
	char buf[128];

	msg_join (MSG_CLIENT_EVENTS);
	while (Session[s])
	{
		sprintf (buf, "%s@%s", EVENT_LOGGER_GROUP, Session[s]);
		msg_join (buf);
#ifdef notdef
		/* 
		 * Message manager automatically relays client events to
		 * internet sessions, so joining the local group is enough.
		 */
		sprintf (buf, "%s@%s", MSG_CLIENT_EVENTS, Session[s]);
		msg_join (buf);
#endif
		s++;
	}
	if (! Session[0])
	{
		msg_join (EVENT_LOGGER_GROUP);
	}
}



int
main (argc, argv)
int argc;
char **argv;
{
	char buf[512];
/*
 * Retrieve our command line options and initialize parameters accordingly
 */
	GetOptions (&argc, argv);
/*
 * Get set up with the toolkit and parse its options
 */
	if (Windows)
	{
		Top = XtAppInitialize (&Appc, "EventLogger", NULL, 0, 
				       &argc, argv, Resources, NULL, 0);
		CreateBitmaps();
	}
	else
		Display_mask = 0;
/*
 * If there's one argument left, it's somebody to send problems to.
 */
	if (argc == 2)
		Mother = argv[1];
	else if (argc > 2)
	{
		printf ("%s: Too many arguments\n", argv[0]);
		printf ("%s", USAGE);
		exit (1);
	}
/*
 * Initialize UI.
 */
	usy_init ();
	ProcTable = usy_c_stbl ("ProcTable");
/*
 * Hook into the message system. Disable the default ELOG proto handler.
 */
	sprintf (buf, "%s-%i", EVENT_LOGGER_NAME, (int) getpid());
	if (! msg_connect (msg_event, buf))
	{
		printf ("%s: unable to connect to message handler\n", argv[0]);
		exit (1);
	}
	msg_AddProtoHandler (MT_ELOG, NULL);
	JoinGroups ();
	ChangeMask (EF_ORMASK);
/*
 * Create the EventLogger toplevel shell and widgets
 */
	sprintf (Initmsg, "%s%s\n", Z_version(), Z_cppsymbols());
	if (Windows)
		CreateEventLogger();
	AppendToLogFile (Initmsg);
	sprintf (buf, "Message protocol version: %s", MSG_PROTO_VERSION);
	LogMessage (EF_INFO, LOGNAME, buf);
/*
 * Tell msglib about our X connection.
 */
	if (Windows)
	{
		msg_add_fd (XConnectionNumber (XtDisplay (Shell)), xevent);
		/* reconfig (625, 600, 500, 150); */
		XtPopup (Shell, XtGrabNone);
		Visible = TRUE;
	}
/*
 * Log a message about our log file, if there is one
 */
	if (Log_file)
		sprintf (buf, "Logging to file '%s'", Log_path);
	else
		sprintf (buf, "No log file specified.");
	LogMessage (EF_INFO, LOGNAME, buf);
/*
 * Request a list of clients, then wait and process messages as we get them.
 */
	msg_ListGroup (NULL);
	AddProc (MSG_MGR_NAME);
	Wait();
	return (0);	/* to keep compilers happy */
}



static void
Wait()
/*
 * If we'll be writing fortunes while not busy, use msg_poll,
 * otherwise we can just hang around in msg_await.
 */
{
	int count;
	int code;

	if (Windows)
	{
		Sync ();
		xevent ();
	}
	if (!TellFortune || !Windows)
		msg_await ();
	else
	{
		/*
		 * The count prevents fortunes from overrunning an EventLogger
		 * which sits idle for a long time.
		 */
		count = 0;
		for (;;)
		{
			code = msg_poll(FortuneWait*(count+1));
			if (code == MSG_TIMEOUT)
			{
				LogFortune();
				++count;
			}
			else if (code == 0)
				count = 0;
			else
				break;
		}
	}
	printf ("EventLogger: Message error or disconnect, aborting.\n");
	exit(1);
}



static void
CreateEventLogger()
{
	Arg args[20];
	Cardinal n;
	Widget w, label;
/*
 * Create our shell.
 */
	n = 0;
	XtSetArg (args[n], XtNinput, True);		n++;
	XtSetArg (args[n], XtNoverrideRedirect, Override);	n++;
	XtSetArg (args[n], XtNallowShellResize, True);	n++;
	XtSetArg (args[n], XtNtitle, "Event Logger");	n++;
	XtSetArg (args[n], XtNwinGravity, StaticGravity); n++;
	Shell = XtCreatePopupShell ("eventlogger", topLevelShellWidgetClass,
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
	label = XtCreateManagedWidget ("label", labelWidgetClass,Form,args,8);
/*
 * Add the quit button.
 */
	XtSetArg (args[0], XtNfromHoriz, label);
	XtSetArg (args[1], XtNfromVert, NULL);
	XtSetArg (args[2], XtNtop, XtChainTop);
	XtSetArg (args[3], XtNbottom, XtChainTop);
	XtSetArg (args[4], XtNleft, XtChainLeft);
	XtSetArg (args[5], XtNright, XtChainLeft);
	w = XtCreateManagedWidget ("Quit", commandWidgetClass, Form, args, 6);
	XtAddCallback (w, XtNcallback, (XtCallbackProc) quitbutton, 
		(XtPointer) 0);
/*
 * Add the clear button.
 */
	XtSetArg (args[0], XtNfromHoriz, w);
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
	XtSetArg (args[6], XtNlabel, (Override) ? "Ctl: DM" : "Ctl: WM");
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
	XtSetArg (args[0], XtNstring, Initmsg);
	XtSetArg (args[1], XtNfromHoriz, NULL);
	XtSetArg (args[2], XtNfromVert, label);
	XtSetArg (args[3], XtNtype, XawAsciiString);
	XtSetArg (args[4], XtNeditType, XawtextRead);
	XtSetArg (args[5], XtNscrollVertical, XawtextScrollAlways);
	Text = XtCreateManagedWidget ("text", asciiTextWidgetClass, Form,
		args, 6);
	XawTextDisplayCaret (Text, False);
	Buflen = strlen (Initmsg);
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
	ProcMenu = XtCreatePopupShell ("Processes", rdssMenuWidgetClass,
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
	static long periods[10] = { 0, 1, 5, 10, 30, 60, 300, 600, 1800 };
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
	menu = XtCreatePopupShell ("Timestamps", rdssMenuWidgetClass,
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
	/*
	 * In case the above did nothing, try to raise the window as well
	 */
	XRaiseWindow (XtDisplay(shell), XtWindow(shell));
	/* XtSetSensitive (w, False); */
}




static Widget
CreateLogSettings (w, top)
Widget w;	/* the button which will pop us up */
Widget top;	/* our top-level parent 	   */
{
	static XtPopdownIDRec pop_rec;
	Widget shell, dialog, toggle, erase, mbutton;
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
	 * Toggle button for activating and deactivating file logging.
	 * Create the button with the longer label, then set it to what
	 * it should actually be after it's been realized.
	 */
	n = 0; XtSetArg (args[n], XtNlabel, "Disabled"); ++n;
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
	 * Erasure button for pruning back file
	 */
	erase = XtCreateManagedWidget ("erase", commandWidgetClass,
					dialog, NULL, 0);
	XtAddCallback (erase, XtNcallback, LogErase, (XtPointer)toggle );

	/*
	 * Realize the shell to get it to calculate its size; note that 
	 * mapped_when_managed is false, so its window will not be mapped.
	 */
	XtRealizeWidget (shell);
	XtSetArg (args[0], XtNlabel, (Log_enabled) ? "Enabled" : "Disabled");
	XtSetValues (toggle, args, (Cardinal)1 );

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
	if (root_x + width + (unsigned)5 > root_width)
		root_x = root_width - width - 5;  /* minus any borders */
	if (root_y + height + (unsigned)5 > root_height)
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
LogErase (w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
/*
 * If we have a log file active, truncate it to nothingness
 */
{
	Widget toggle = (Widget) client_data;
	char buf[80];

	if (Log_file)
	{
		Log_file = freopen (Log_path, "w", Log_file);
		if (Log_file)
		{
			sprintf (buf, "Log file '%s' now empty",
				 Log_path);
			LogMessage (EF_INFO, LOGNAME, buf);
		}
		else
		{
			sprintf (buf, 
				 "Error %d trying to reopen log file '%s'",
				 errno, Log_path);
			LogMessage (EF_PROBLEM, LOGNAME, buf);
			LogDisable (toggle);
		}
	}
	else
		LogMessage (EF_DEBUG, LOGNAME, "No log file to truncate");
}




static void
LogDisable (toggle)
Widget toggle;		/* The widget which displays our state */
{
	Arg arg;

	Log_enabled = FALSE;
	XtSetArg (arg, XtNlabel, "Disabled");
	XtSetValues (toggle, &arg, (Cardinal)1);
	LogMessage (EF_DEBUG, LOGNAME, "File logging disabled.");
	ChangeMask (EF_SETMASK);
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
		LogDisable (w);
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
		LogMessage (EF_DEBUG, LOGNAME, "File logging enabled.");
		ChangeMask (EF_SETMASK);
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
		LogMessage (EF_INFO, LOGNAME, buf);
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
			LogMessage (EF_PROBLEM, LOGNAME, buf);
		}
		else
		{
			sprintf (buf, "Log file %s opened.", filepath);
			LogMessage (EF_INFO, LOGNAME, buf);
		}
	}
	else
	{
		LogMessage (EF_PROBLEM, LOGNAME, 
			    "Could not open log file, no file name specified");
	}

	Log_enabled = (Log_file) ? TRUE : FALSE;
	XtSetArg (arg, XtNlabel, (Log_enabled) ? "Enabled" : "Disabled");
	XtSetValues (w, &arg, (Cardinal)1);
	sprintf (buf, "File logging %s.", 
		 (Log_enabled) ? "enabled" : "disabled");
	LogMessage (EF_DEBUG, LOGNAME, buf);
	ChangeMask (EF_SETMASK);
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
	pulldown = XtCreatePopupShell (name, rdssMenuWidgetClass,
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
 * Update the global mask and broadcast the new setting.
 */
	ChangeMask (EF_SETMASK);
}




static int
xevent ()
/*
 * Deal with an Xt event.
 */
{
	XEvent event;
/*
 * Deal with events as long as they keep coming.
 */
	Sync ();
 	while (XtAppPending (Appc))
	{
		XtAppNextEvent (Appc, &event);
		XtDispatchEvent (&event);
		Sync ();
	}
	return (0);
}





static int
msg_event (msg)
struct message *msg;
/*
 * Log a client event.
 */
{
	struct mh_clientevent *client = (struct mh_clientevent *) msg->m_data;
	struct mh_members *mg;
	char mb[256];
/*
 * Check for log messages.
 */
	if (msg->m_proto == MT_LOG)
	{
		DoLog (msg->m_from, msg->m_data);
	}
/*
 * Maybe it's a display manager message.
 */
	else if (msg->m_proto == MT_DISPLAYMGR)
	{
		if (Windows)
			dm_msg ((struct dm_msg *) msg->m_data);
	}
/*
 * If it's an extended log message, do something with it.  We only accept
 * control messages broadcast to our group, or regular log messages.  I.e.,
 * we ignore control messages sent to everybody because we expect it to
 * be followed with a message explicitly to any event loggers.
 */
	else if (msg->m_proto == MT_ELOG)
	{
		struct msg_elog *el = (struct msg_elog *) msg->m_data;

		if (! (el->el_flag & (EF_SETMASK | EF_ORMASK)) ||
		    ((msg->m_flags & MF_BROADCAST) && 
		     !strcmp(msg->m_to, EVENT_LOGGER_GROUP)))
		{
			LogMessage (el->el_flag, msg->m_from, el->el_text);
		}
	}
/*
 * Everything else is assumed to be a message handler event.
 */	
	else if (msg->m_proto != MT_MESSAGE)
	{
		sprintf (mb, "Unexpected protocol %d from %s", 
			 msg->m_proto, msg->m_from);
		LogMessage (EF_PROBLEM, "EventLogger", mb);
	}
	else switch (client->mh_type)
	{
	   case MH_CLIENT:
	   	switch (client->mh_evtype)
		{
		   case MH_CE_CONNECT:
			NewProc (client->mh_client);
		   	sprintf (mb, "Connect on %d", msg->m_seq);
			break;
		   case MH_CE_DISCONNECT:
			DeadProc (client->mh_client);
		   	sprintf (mb, "Disconnect on %d", msg->m_seq);
			break;
		   case MH_CE_JOIN:
		   	sprintf (mb, "Group %s joined on %d",
				client->mh_group, msg->m_seq);
			break;
		   case MH_CE_QUIT:
		   	sprintf (mb, "Group %s quit on %d",
				client->mh_group, msg->m_seq);
			break;
		}
		LogMessage (EF_CLIENT, client->mh_client, mb);
		break;
	   case MH_GROUP:
		mg = (struct mh_members *) msg->m_data;
		GroupList (mg);
		sprintf (mb, "Received list of %d members in %s",
			 mg->mh_nclient, mg->mh_group);
		LogMessage (EF_CLIENT, "EventLogger", mb);
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
	/* 
	 * Check for any pending X events, which we'll want to process right
	 * away.  The theory is that X events will not be very frequent,
	 * relative to Zeb messages, so this function will normally not do
	 * anything.  But if there is X interface stuff to be done, it
	 * needs to be done ASAP, for the user's sake.
	 */
	/* if (Windows) xevent (); */
	return (0);
}



static void
GroupList (mh)
struct mh_members *mh;
/*
 * Add each of the clients in the group to our process table.
 */
{
	int i;
	
	for (i = 0; i < mh->mh_nclient; ++i)
	{
		AddProc (mh->mh_client[i]);
	}
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
 * If someone's setting the mask and it does not include our own mask, we
 * need to broadcast our bits.
 */
	if (flag & EF_SETMASK)
	{
		if ((flag | Emask) != flag)
			BroadcastEMask (EF_ORMASK);
		else if ((flag | EF_DEBUG) != flag)
			usy_search (ProcTable, SendDbgMask, 0, FALSE, 0);
		return;
	}
/*
 * If somebody's adding their bits to event masks, we'll ignore it.
 */
	if (flag & EF_ORMASK)
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
 * If we need to log this message, format it first then do so
 */
	if ((Emask & flag) || PassDebug (flag, from))
	{
		msg = FormatMessage (code, from, text);
		if (!msg)
			return;
	/*
	 * Now find out what gets this message and send it along
	 */
		if ((flag & Log_mask) || PassDebug (flag, from))
			AppendToLogFile (msg);
		if (Windows && ((flag & Display_mask) || 
				PassDebug (flag, from)))
			AppendToDisplay (msg);
	}
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
	if (! fmt)
		return;
	AppendToLogFile (fmt);
	if (Windows)
		AppendToDisplay (fmt);
}


#define FMTLEN 1024
#define FROMLEN 14

static char *
AppendMessage (char *fmtbuf, char *text)
{
	if (text)
	{
		char *cp = fmtbuf + strlen(fmtbuf);
		char *end = fmtbuf + FMTLEN - 1;
		if (cp < end)
			strncpy (cp, text, end - cp);
		*end = '\0';
	}
	return fmtbuf;
}


/*
 * Given the needed fields for a log message: type, from, and message,
 * assemble them into a standard string and append to the message buffer.
 * Include a time stamp, but no trailing newline.
 */
static char *
NewMessage (char *fmtbuf, char code, char *from_in, char *msg)
{
	char tmp[128];
	char from[FROMLEN];
	ZebTime now;

	TC_SysToZt (time(0), &now);
	sprintf (tmp, "%c %20s ", code, TC_AscTime (&now, TC_Full));
	AppendMessage (fmtbuf, tmp);
	strncpy (from, from_in, FROMLEN-1);
	from[FROMLEN-1] = '\0';
	sprintf (tmp, "%-*s ", FROMLEN-1, from);
	AppendMessage (fmtbuf, tmp);
	AppendMessage (fmtbuf, msg);
	return (fmtbuf);
}



static char *
FormatMessage (char code, char *from_in, char *msg_in)
/*
 * See if we've seen this message before: if so, print the repeat count
 * at regular intervals; otherwise reset repeat count and proceed as usual.
 * Note that repeat_count will always be at least one except for the first
 * time this function is entered.  Return NULL if this message should
 * be skipped, else return format buffer with the new log messages.
 */
{
	static char fmtbuf[FMTLEN];
	static char last_msg[FMTLEN];
	static char last_from[MSG_MAXNAMELEN];
	static int repeat_count = 0;

	char repeat_msg[128];
	char tmp[128];

	if (! msg_in)
	    msg_in = "";

	/* 
	 * Only considered a repeat if from the same client.  Messages
	 * longer then FMTLEN will never appear to repeat because they
	 * will be truncated when preserved in the last_msg buffer.
	 */
	fmtbuf[0] = '\0';
	repeat_msg[0] = '\0';
	if ((repeat_count > 0) && 
	    streq (msg_in, last_msg) && streq (from_in, last_from))
	{
		++repeat_count;
		/*
		 * This ignores the first 5 repeats and logs them as usual.
		 * Beyond 5 we report repeats at intervals of 10 until 50.
		 */
		if (repeat_count <= 5)
		{
			/* fall through */
		}
		else if (((repeat_count < 50) && !(repeat_count % 10)) ||
		    (!(repeat_count % 50)))
		{
			sprintf (repeat_msg, ", %d repeats", repeat_count);
		}
		else
		{
			return NULL;
		}
	}
	else
	{
	/*
	 * If messages were previously being skipped, make a note about the
	 * final number of repeats.
	 */
		if (repeat_count > 5)
		{
			NewMessage (fmtbuf, 'R', last_from, last_msg);
			sprintf (tmp, ", repeated %d times\n", repeat_count);
			AppendMessage (fmtbuf, tmp);
		}
		strncpy (last_msg, msg_in, sizeof(last_msg));
		last_msg[sizeof(last_msg)-1] = '\0';
		strncpy (last_from, from_in, sizeof(last_from));
		last_from[sizeof(last_from)-1] = '\0';
		repeat_count = 1;
	}
	/*
	 * Now we can assemble the message.
	 */
	NewMessage (fmtbuf, code, from_in, msg_in);
	if (repeat_msg[0])
		AppendMessage (fmtbuf, repeat_msg);
	AppendMessage (fmtbuf, "\n");
	/* Make sure we always end with a newline */
	strcpy (fmtbuf + FMTLEN - 2, "\n");
	return fmtbuf;
}



#ifdef notdef
static char *
FormatMessage (code, from_in, msg_in)
char code;
char *from_in;
char *msg_in;
/*
 * Format the code, from, and msg strings into the log message.  If the
 * message should not be logged for repeat reasons, NULL is returned.
 * If the msg is NULL, clear the repeat counter and return the "repeated"
 * message, else NULL.
 */
{
#	define FMTLEN 256
#	define FROMLEN 12
	static char fmtbuf[FMTLEN];
	static char last_msg[FMTLEN];
	static int repeat_count = 0;
	char msg[FMTLEN];
	char from[FROMLEN];
/*
 * Format the message to be logged.  For robustness, make sure the msg is
 * not too long for our buffer.
 */
	if (msg_in)
	{
		strncpy (from, from_in, FROMLEN-1);
		from[FROMLEN-1] = '\0';
		strncpy (msg, msg_in, FMTLEN - 30);
		msg[FMTLEN - 30] = '\0';
		sprintf (fmtbuf, "%c %-*s %s\n", code, FROMLEN-1, from, msg);
	}
	else
	{
		fmtbuf[0] = '\0';
	}
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
			sprintf (fmtbuf,"%c %-*s %s, %d repeats\n",
				 'R', FROMLEN-1, from, msg, repeat_count);
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
				 "R %-*s Last message repeated %d times\n%s",
				 FROMLEN-1, LOGNAME, repeat_count, last_msg);
		repeat_count = 1;
	}

	return (fmtbuf);
}
#endif



static void
AppendToLogFile (fmtbuf)
char *fmtbuf;
/*
 * Actually append the text to the log file
 */
{
	if (Echo)
	{
		fprintf (stdout, "%s", fmtbuf);
		fflush (stdout);
	}
	if (Log_file && Log_enabled)
	{
		fprintf (Log_file, "%s", fmtbuf);
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
	XawTextPosition cut;

	tb.firstPos = 0;
	tb.length = strlen (fmtbuf);
	tb.ptr = fmtbuf;
	tb.format = FMT8BIT;
/*
 * Add it to the buffer.  Turn on editing only for long enough to do this
 * operation.  Disable re-display to keep the text from bouncing around.
 */
	XawTextDisableRedisplay (Text);
	XtSetArg (args[0], XtNeditType, XawtextAppend);
	XtSetValues (Text, args, 1);
	XawTextReplace (Text, Buflen, Buflen, &tb);
/*
 * If this is getting too big, trim it back.  Try to trim it at a line break.
 */
	if ((Buflen += strlen (fmtbuf)) > (unsigned)MAXCHAR)
	{
		XawTextSetInsertionPoint (Text, Buflen - TRIMCHAR);
		tb.firstPos = 0;
		tb.length = 1;
		tb.ptr = "\n";
		cut = XawTextSearch (Text, XawsdRight, &tb);
		if (cut == XawTextSearchError)
			cut = Buflen - TRIMCHAR;

		tb.length = 0;
		XawTextReplace (Text, 0, cut + 1, &tb);
		Buflen -= (cut + 1);
	}
/*
 * Update.
 */
	XtSetArg (args[0], XtNeditType, XawtextRead);
	XtSetValues (Text, args, 1);
	/* XawTextDisplay (Text); */
	XawTextSetInsertionPoint (Text, Buflen);
	XawTextEnableRedisplay (Text);
	xevent ();
}




static void
quitbutton ()
/*
 * Quit the event logger.  Broadcast a SETMASK of zero in case we're the
 * last of the loggers.
 */
{
	int flag = EF_SETMASK;
	Emask = 0;
	SendEverybody (flag);
	msg_disconnect ();
	exit (0);
}




static void
clearbutton ()
/*
 * Clear log message buffer, then clear text window.
 */	
{
	Arg args[2];
	char *log;

	/* Clear the text buffer */
	XtSetArg (args[0], XtNstring, "");
	XtSetValues (Text, args, 1);
	XawTextDisplay (Text);
	Buflen = 0;
	/* Clear repeat message, and possibly log the repeat count */
	log = FormatMessage (0, LOGNAME, NULL);
	if (log && Windows)
		AppendToDisplay (log);
	if (log)
		AppendToLogFile (log);
}



static void
Sync ()
/*
 * Synchronize with the window system.
 */
{
	XSync (XtDisplay (Top), False);
}





static void
dm_msg (dmsg)
struct dm_msg *dmsg;
/*
 * Deal with a DM message.
 */
{
/*
 * See what we got.
 */
	switch (dmsg->dmm_type)
	{
	/*
	 * Maybe it's a DM scoping us out.
	 */
	   case DM_HELLO:
		dm_Greet ();
		break;
	/*
	 * Maybe it's a reconfig.
	 */
	   case DM_RECONFIG:
		if (Override)
		{
			dm_Reconfig ((struct dm_reconfig *)dmsg);
		   	reconfig (dmsg->dmm_x, dmsg->dmm_y, dmsg->dmm_dx,
				dmsg->dmm_dy);
		}
		break;
	/*
	 * Geometry request.
	 */
	   case DM_GEOMETRY:
		SendGeometry (dmsg);
		break;
	/*
	 * They might want us to go away entirely.
	 */
	   case DM_SUSPEND:
	   	if (Visible && Override)
		{
			Visible = False;
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





static void
SendGeometry (dmm)
struct dm_msg *dmm;
{
	struct dm_msg reply;
	Dimension width, height;
	Position x, y;
	Arg args[5];
	int n;

	reply.dmm_type = DM_R_GEOMETRY;

	n = 0;
	XtSetArg (args[n], XtNx, (XtArgVal)&x);	n++;
	XtSetArg (args[n], XtNy, (XtArgVal)&y);	n++;
	XtSetArg (args[n], XtNwidth, (XtArgVal)&width);	n++;
	XtSetArg (args[n], XtNheight, (XtArgVal)&height); n++;
	XtGetValues (Shell, args, n);
	reply.dmm_x = x;
	reply.dmm_y = y;
	reply.dmm_dx = width;
	reply.dmm_dy = height;

	msg_send(DISPLAY_MANAGER,MT_DISPLAYMGR,FALSE,&reply,sizeof(reply));
}





static void
reconfig (x, y, w, h)
int x, y, w, h;
/*
 * Reconfigure the window.
 */
{
	Arg args[5];
	int n;
/* 
 * If they can't see us, make it so now.
 */
	if (! Visible)
	{
		XtPopup (Shell, XtGrabNone);
		Visible = TRUE;
	}
/*
 * For sizing, change the form inside the shell.
 */
	n = 0;
	XtSetArg (args[n], XtNwidth, w);	n++;
	XtSetArg (args[n], XtNheight, h);	n++;
	XtSetValues (Form, args, n);
/*
 * For positioning, move the shell.
 */
	n = 0;
	XtSetArg (args[n], XtNx, x);	n++;
	XtSetArg (args[n], XtNy, y);	n++;
	XtSetValues (Shell, args, n);
	Sync ();
}




static void
wm ()
/*
 * Try to change override redirect.
 */
{
	Arg	args[2];
	int	n;
/*
 * If the window is up, take it down.
 */
	if (Visible)
		XtPopdown (Shell);
/*
 * Set the parameter.
 */
	Override = ! Override;
	n = 0;
	XtSetArg (args[n], XtNoverrideRedirect, Override); n++;
	XtSetValues (Shell, args, n);
/*
 * Set the label on the command widget too.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, (Override) ? "Ctl: DM" : "Ctl: WM"); n++;
	XtSetValues (Wm, args, n);
/*
 * Put the window back if it was up before.  We wait for a ReparentNotify
 * event from the X server before we ask to pop up again.
 */
	if (Visible)
	{
	/*
	 * Await the ReparentNotify from the server if we're popping back up
	 * in OverrideRedirect mode.  Otherwise, timing of events may cause
	 * us to pop up and then down never to be seen again...
	 */
		if (Override)
		{
			XEvent	event;

			do
			{
				XtAppNextEvent (Appc, &event);
				XtDispatchEvent (&event);
			}
			while (event.type != ReparentNotify);
		}
	/*
	 * Now we can safely pop up again...
	 */
		XtPopup (Shell, XtGrabNone);
	}
}



static void
ChangeMask (op)
int op;
/*
 * Update the global emask according to current display and file masks.
 */
{
	int mask = (Windows ? Display_mask : 0) | 
		((Log_enabled || Echo) ? Log_mask : 0);
	if (mask != Emask)
	{
		Emask = mask;
		BroadcastEMask (op);
	}
}



static void
SendEverybody (flag)
int flag;
/*
 * Broadcast out the default mask to everybody.  When setting the mask,
 * follow it with a specific message to event loggers, since some may be
 * across the net and will not receive everybody broadcasts, but they may
 * still need to update the new mask setting with OR masks.
 */
{
	if (! Session[0])
	{
		msg_send (MSG_EVERYBODY, MT_ELOG, TRUE, &flag, sizeof (flag));
		if (flag & EF_SETMASK)
		{
			msg_send (EVENT_LOGGER_GROUP, MT_ELOG, TRUE, &flag, 
				  sizeof (flag));
		}
	}
	else
	{
		int s = 0;
		char buf[128];
		while (Session[s])
		{
			sprintf (buf, "%s@%s", MSG_EVERYBODY, Session[s]);
			msg_send (buf, MT_ELOG, TRUE, &flag, sizeof (flag));
			if (flag & EF_SETMASK)
			{
				sprintf (buf, "%s@%s", EVENT_LOGGER_GROUP,
					 Session[s]);
				msg_send (buf, MT_ELOG, TRUE, &flag, 
					  sizeof (flag));
			}
			s++;
		}
	}
}



static void
BroadcastEMask (op)
int op;	/* either EF_SETMASK or EF_ORMASK */
/*
 * Broadcast the event mask to the world.
 */
{
	int flag;

	flag = Emask | op;
	SendEverybody (flag);
/*
 * Now go through and fix up any processes for which debugging has been
 * requested.
 */
	if ((Emask & EF_DEBUG) == 0)
		usy_search (ProcTable, SendDbgMask, 0, FALSE, 0);
}



static void
SendEMask (who, op)
char *who;
int op;
/*
 * Update the event mask of a particular process.
 */
{
	int flag;
	int type;
	SValue v;
	ProcInfo *pinfo;

	flag = Emask | op;
	if (usy_g_symbol (ProcTable, who, &type, &v))
	{
		pinfo = (ProcInfo *) v.us_v_ptr;
		if (pinfo->pi_enabled)
			flag |= EF_DEBUG;
	}
	msg_send (who, MT_ELOG, FALSE, &flag, sizeof (flag));
}





/*ARGSUSED*/
int 
SendDbgMask (proc, type, v, junk)
char *proc;
int type;
SValue *v;
long junk;
/*
 * Maybe send out a debug-enabled event mask to this process.  Since we're
 * just adding bits to existing masks, use EF_ORMASK.
 */
{
	int flag = EF_ORMASK | EF_DEBUG;
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




static void 
NewProc (name)
char *name;
/*
 * Add this process, and send it the mask of messages we want from it.
 */
{
	AddProc (name);
	SendEMask (name, EF_ORMASK);
}



static void
AddProc (name)
char *name;
/*
 * Deal with a new process.
 */
{
	ProcInfo *pinfo = NULL;
	char buf[256];
	Arg args[4];
	int n;
	int type;
	SValue v;
/*
 * Make sure we don't already know about this process, and make sure this
 * process is not us.
 */
	if ((strcmp (name, msg_myname()) == 0) || 
	    usy_g_symbol (ProcTable, name, &type, &v))
		return;
	pinfo = ALLOC (ProcInfo);
	pinfo->pi_enabled = FALSE;
	strcpy (pinfo->pi_name, name);
	sprintf (buf, "Adding process %s", name);
	LogMessage (EF_DEBUG, "EventLogger", buf);
/*
 * Create the widget for this process.
 */
	if (Windows)
	{
		n = 0;
		XtSetArg (args[n], XtNleftBitmap, None);		n++;
		XtSetArg (args[n], XtNleftMargin, check_width + 7);	n++;
		pinfo->pi_menu = XtCreateManagedWidget (name, 
				smeBSBObjectClass, ProcMenu, args, n);
		XtAddCallback (pinfo->pi_menu, XtNcallback, ToggleProc,
			       (XtPointer) pinfo->pi_name);
	}
	else
		pinfo->pi_menu = FALSE;
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




static void
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
	if (Windows)
		XtDestroyWidget (pinfo->pi_menu);
	free (v.us_v_ptr);
	usy_z_symbol (ProcTable, name);
}





/*ARGSUSED*/
static void
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
		printf ("Proc '%s' not in ProcInfo!\n", (char *) proc);
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
 * If we're setting the debug bit, just send it directly as an OR mask.
 */
	if (pinfo->pi_enabled)
	{
		int flag = EF_ORMASK | EF_DEBUG;
		msg_send (proc, MT_ELOG, FALSE, &flag, sizeof (flag));
	}
/*
 * Otherwise, we're resetting the debug bit.  Send a SETMASK to the process
 * and to the event logger group.  The other event loggers will respond by
 * sending out OR's for their masks and debug bits.  We could just broadcast
 * the SETMASK, but this method will save us sending messages to the 
 * processes whose masks we aren't changing.  Note we don't call ChangeMask
 * because a per-process debug flag does not affect our global mask.  We also
 * needn't do anything if our global mask still includes debugging.
 */
	else if ((Emask & EF_DEBUG) == 0)
	{
		int flag = Emask | EF_SETMASK;
		msg_send (proc, MT_ELOG, FALSE, &flag, sizeof (flag));
		msg_send (EVENT_LOGGER_GROUP, MT_ELOG, TRUE, &flag, 
			  sizeof (flag));
	}
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
	static int slot = -1;
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
	msg = FormatMessage ('T', LOGNAME, buf);
	if (! msg)
		return;
	AppendToLogFile (msg);
	if (Windows)
		AppendToDisplay (msg);
}



/*ARGSUSED*/
static void
TimestampCallback (entry, client_data, call_data)
Widget entry;
XtPointer client_data;
XtPointer call_data;
{
	long period = (long)client_data;
	Arg args[2];

	TimestampSetup (period);
	XtSetArg (args[0], XtNleftBitmap, None);
	if (TimestampEntry)
		XtSetValues (TimestampEntry, args, 1);
	XtSetArg (args[0], XtNleftBitmap, Check);
	XtSetValues (entry, args, 1);
	TimestampEntry = entry;
}



static void
LogFortune ()
/*
 * Just for the heck of it, write a short fortune message to the display log
 */
{
	FILE *pipe;
	char command[80];
	char fortune[FORTUNE_LEN];
	int len;

	sprintf (command, "%s", FORTUNE_CMD);
	pipe = (FILE *) popen (command, "r");
	if (!pipe)
	{
		sprintf (fortune, "%s: pipe failed, err %d", command, errno);
		LogMessage (EF_DEBUG, LOGNAME, fortune);
		return;
	}
	strcpy (fortune, "J ");
	len = strlen(fortune);
	while (fgets (fortune+len, FORTUNE_LEN-len-3, pipe))
	{
		strcat (fortune, "  ");
		len = strlen(fortune);
	}
	pclose (pipe);
	if (len < 5)
		return;
	fortune[len-2] = '\0';		/* remove last two spaces */
	AppendToDisplay (fortune);
}
