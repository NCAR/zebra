/*
 * Seriously quick and immensely dirty data store archiver.
 *
 * Basically: we wake up every so often, look at all the files in the system,
 * and write them out to tape in tar format.
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


# include <copyright.h>
# include <unistd.h>
# include <string.h>
# include <stdio.h>
# include <fcntl.h>
# include <errno.h>
# include <sys/types.h>
# include <sys/time.h>
# include <sys/ioctl.h>
# ifdef AIXV3
# include "zl_mtio.h"
# else
# include <sys/mtio.h>
# endif
# include <sys/wait.h>

#if defined(SVR4) || defined(__osf__)
# include <sys/statvfs.h>
#else
# ifndef AIXV3
# include <sys/vfs.h>
# else
# include <sys/statfs.h>
# endif
#endif

# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Command.h>

# include <defs.h>
# include <zl_symbol.h>
# include <message.h>
# include <timer.h>
# include <config.h>
# include <DataStore.h>

# include "Database.h"

RCSID ("$Id: Archiver.cc,v 1.39 1997-10-03 23:49:18 ishikawa Exp $")

/*
 * Issues:
 *
 *	Have tar write to tape, or do it ourselves?
 *	Have tar generate tar fmt, or do it ourselves?
 *	How to keep track of what has been written.
 *	Release drive on demand.
 *	Split into physical files.
 *
 * The scheme:
 
 	cd to the data root directory
	Figure out files dumped some other time
	open tape drive (position?)
	do forever:
		sleep for a while (time or explicit wakeup)
		get a list of files to dump
		popen (humungo tar command)
		while (read on pipe)
			write to device
		mark files dumped
 */

/* The revised strategy to accomodate user-specified delays and writes:

	A global Suspended flag can be set to True by button callbacks
	while (read on pipe)
		write to device
		process x events
		if (suspended flag was set)
			Poll X and msg_fd as long as suspended flag 
			   remains set
	continue reading pipe

	the suspended flag will be reset by either an X event (user presses
		a resume event) or a timer message forcing a resume

	the user can explicitly request a write (other than a finish)
	a request to write during a write (such as the normal timer event
	   during a user-requested write) will be pending and started
	   after the completion of the current write
	finishes can be queued as well and are executed like any other
	   write, except that an explicit finish flag forces a finish
	   once the write completes (rather than continuing normally)

	death messages from the message handler must be flagged
	   and acted on after any current write completes
*/

/*
  Additions 5/12/97 in CASES:

  Enter the dumped files in a database and check for undumped or changed
  files by looking them up in the database.  Allows files to be inserted
  prior to the most recent file of a platform and still be dumped.
 */

/*------------------------------------------------------------------------*/

# define DEF_DUMPINTERVAL 120	/* how often, in minutes, to dump	*/
# define DEF_DEVICEFILE "/dev/nrst8"
# define DEF_MOUNTNAME "eod0"
# define DEF_OUTPUTDIR "/eod0"
# ifdef __STDC__
# define DEF_TAPELIMIT ((unsigned long) 3500000000ul)
# define DEF_MINDISK ((unsigned long) 10000ul)
# else
# define DEF_TAPELIMIT ((unsigned long) 3500000000)
# define DEF_MINDISK ((unsigned long) 10000)
# endif /* __STDC__ */

# define AR_TAPE 1
# define AR_EOD 2

# define MAX_WAITBUTTONS 5

# define DUMPED_FILES "DumpedFiles"

/*------------------------------------------------------------------------
 * GLOBAL VARIABLES and FLAGS
 */

#ifdef notdef
/*
 * This table contains an entry for each platform that we dump, indicating
 * the latest data which has been written.
 */
stbl	DumpedTable;
char listfile[200];	
#endif

/*
 * The indices of the tarred files, kept until after the tar successfully
 * completes, to be added to the database.
 */
int	*DumpedFiles = NULL;
int	NFiles = 0;		/* Number files in DumpedFiles */

/*
 * Global timer event slots
 */
int 	ResumeEvent = -1;	/* If in suspended status, this is the
				 * timer event which will resume the tar.
				 * -1 indicates we're not suspended */
int	RemainingEvent = -1;	/* The event which slowly counts down the
				 * the time remaining in a suspension */

/*
 * Global flags indicating the current status of Archiver
 */
int	Suspended = FALSE;	/* True when a write has been stopped */
int	WriteInProgress = FALSE;/* A tar has been started */
int	Dying = FALSE;		/* Set by Handler() on a MH_SHUTDOWN msg
				 * during a write */

int	DeviceFD = -1;		/* output device file descriptor:
				 * TAR output is piped to here
				 * - for tape mode, it's the tape drive device
				 * - for eod mode, it's a file on the
				 *   mounted output directory
				 * -1 <==> no drive	*/

unsigned long	BytesWritten = 0;	/* Statistics stuff */
int		FilesWritten = 0;

static char Tarbuf[65536]; 	/* Where the tar command is built.  */
unsigned long Tarlen = 0;       /* == strlen(Tarbuf) at all times */

/*
 * Widget info.
 */
Widget Top, Form, WStatus, Bytes;
Widget FinishButton, Action, WriteButton;
Widget WaitButtons[MAX_WAITBUTTONS+1];
XtAppContext Appc;

Pixel RedPix, WhitePix; 	/* Colors for the status widget. */

/*------------------------------------------------------------------------*/


/*---------------------------------------------------------------------
 * Global options which can be set by the user from the command line or
 * the resources database
 */

	unsigned long TapeLimit;/* Tape size (bytes), less a safety margin */
				/* Default is high density Exabyte */
	String	DriveName;	/* TAPE mode: tape drive device */
	String	OutputDir;	/* EOD mode: output directory */
	String	MountName;	/* EOD mode: opt disk mount point */
	int	DumpInterval;	/* Minutes between dumps */
	int	StartMinute;	/* Hour offset at which dumps start */
	int	MinDisk;	/* EOD mode: minimum disk size */
	String  ModeString;	/* cmd-line option "eod" or "tape" */
	int	ArchiveMode;	/* AR_TAPE or AR_EOD mode */
	Boolean	ZeroZFree;	/* Free tape at 0z */
	String	WaitTimes;	/* Delay button times, "n1,n2,n3,..." */
	String  Database;	/* Name of the database file */
	int	BFactor;	/* Blocking factor to calculate blk size */

#define DEF_BFACTOR 120


/*
 * The offset of each of these is zero since we want to store the
 * values directly into the global variable rather than into a single
 * structure
 * NOTE: sizeof(int) == sizeof(long)
 */
static XtResource AppResources[] = {
   { "tapeLimit", "TapeLimit", XtRInt, sizeof(int),
      0, XtRImmediate, (XtPointer)DEF_TAPELIMIT },
   { "driveName", "DriveName", XtRString, sizeof(String),
      0, XtRString, DEF_DEVICEFILE },
   { "outputDir", "OutputDir", XtRString, sizeof(String),
      0, XtRString, DEF_OUTPUTDIR },
   { "mountName", "MountName", XtRString, sizeof(String),
      0, XtRString, DEF_MOUNTNAME },
   { "dumpInterval", "DumpInterval", XtRInt, sizeof(int),
      0, XtRImmediate, (XtPointer)DEF_DUMPINTERVAL },
   { "startMinute", "StartMinute", XtRInt, sizeof(int),
      0, XtRImmediate, (XtPointer)0 },
   { "minDisk", "MinDisk", XtRInt, sizeof(int),
      0, XtRImmediate, (XtPointer)DEF_MINDISK },
   { "mode", "Mode", XtRString, sizeof(String),
      0, XtRImmediate, "tape" },
   { "zeroZFree", "ZeroZFree", XtRBoolean, sizeof(Boolean),
      0, XtRImmediate, (XtPointer)True },
   { "waitTimes", "WaitTimes", XtRString, sizeof(String),
      0, XtRString, "1,2,5" },
   { "database", "Database", XtRString, sizeof(String),
      0, XtRString, DUMPED_FILES },
   { "blockFactor", "BlockFactor", XtRInt, sizeof(int),
      0, XtRImmediate, (XtPointer)DEF_BFACTOR }
};

static XtPointer OptionBase[] = {
   (XtPointer) &TapeLimit, (XtPointer) &DriveName, (XtPointer) &OutputDir, 
   (XtPointer) &MountName, (XtPointer) &DumpInterval, (XtPointer) &StartMinute,
   (XtPointer) &MinDisk, (XtPointer) &ModeString, (XtPointer) &ZeroZFree, 
   (XtPointer) &WaitTimes, (XtPointer) &Database, (XtPointer) &BFactor };

/*
 * For loading these resource from the command line:
 */
static XrmOptionDescRec Options[] = {
   {"-device",	".driveName",	XrmoptionSepArg, 	NULL},
   {"-v",	".driveName",	XrmoptionSepArg,	NULL},
   {"-f",	".driveName",	XrmoptionSepArg,	NULL},
   {"-output",	".outputDir",	XrmoptionSepArg,	NULL},
   {"-mode",	".mode",	XrmoptionSepArg,	NULL},
   {"-z",	".zeroZFree",	XrmoptionSepArg,	NULL},
   {"-start",	".startMinute", XrmoptionSepArg,	NULL},
   {"-time",	".dumpInterval",XrmoptionSepArg,	NULL},
   {"-interval",".dumpInterval",XrmoptionSepArg,	NULL},
   {"-k",	".minDisk",	XrmoptionSepArg,	NULL},
   {"-n",	".mountName",	XrmoptionSepArg,	NULL},
   {"-tapelimit",".tapeLimit",	XrmoptionSepArg,	NULL},
   {"-wait",	".waitTimes",	XrmoptionSepArg,	NULL},
   {"-database",".database",	XrmoptionSepArg,	NULL},
   {"-b",	".blockFactor",	XrmoptionSepArg,	NULL}
};

/*---------------------------------------------------------------------*/

/*-- initialization --*/
static void	InitArchiver FP ((int *, char**));
static void	MakeWidget FP ((int *, char **));
static Widget	CreateWaitButtons FP ((String times, Widget parent,
			Widget left, Widget above));
static void	Usage FP((char *prog, int argc, char *argv[]));
static void	Die FP ((void));

/*-- socket i/o handlers --*/
static int	Handler FP ((Message *));
static int	xevent FP ((int));
static void	Sync FP ((void));

/*-- Xt callbacks --*/
static void	Finish FP ((void));
static void	ActionButton FP ((void));
static void	WriteNow FP((void));
static void 	SuspendWrite FP((Widget w, XtPointer call_data));

/*-- Xt convenience fns --*/
static void	SetStatus FP ((int, char *));
static void	SetWaitSensitivity FP((int sensitive));

/*-- low-level device i/o --*/
static int	OpenTapeDevice FP ((void));
static void	WriteEOF FP ((void));
static void	SpinOff FP ((void));
static int	EjectEOD FP ((void));
static void	MountEOD FP ((void));
static int	netread FP ((int fd, char *dest, int len));

/*-- Timer event handlers --*/
static void	TimerSaveFiles FP ((ZebTime *zt));
static void	TimerResumeWrite FP((ZebTime *zt));
static void	TimerRemaining FP((ZebTime *zt, void *cdata));

/*-- high-level write and datastore interaction */
static void	RequestWrite FP((int finish));
static void	DoTheWriteThing FP((int finish));
static void	SaveFiles FP ((int));
/* static void	LoadFileList FP ((void)); */
static int *	GetFileChain FP ((PlatformId pid, const ZebTime *since, 
				  int *count));
static int	DumpPlatform FP ((PlatformId, PlatformInfo *, int));
static int	RunTar FP ((char *));
static void	UpdateList FP ((void));
static int	WriteFileDate FP ((char *, int, SValue *, FILE *));
static int	TellDaemon FP ((char *, int, SValue *, int));
/* static void	UpdateMem FP ((void)); */
static void	FinishFinishing FP((int error));

/*---------------------------------------------------------------------*/


int
main (argc, argv)
int argc;
char **argv;
{
	int i;
/*
 * Before we do anything, see if the user just wants some
 * help by checking for the -h option and forgetting any others
 */
	for (i = 1; i < argc; ++i)
		if (strncmp(argv[i],"-h",2) == 0)
		{
			Usage(argv[0],1,argv);
			exit(0);
		};
/*
 * Initialize.
 */
	usy_init ();
	if (!msg_connect (Handler, "Archiver"))
	{
		fprintf(stderr,"Archiver: could not connect to message\n");
		Die();
	}
	if (!ds_Initialize ())
	{
		fprintf(stderr,"Archiver: DataStore initialization failed\n");
		Die();
	}
	InitArchiver(&argc,argv);
/*
 * Window sys initialization.
 */
	MakeWidget (&argc, argv);

	chdir ( GetProjDir() );
	/* LoadFileList (); */
	if (db_Open (Database) != 0)
	{
		msg_ELog (EF_PROBLEM, "Cannot open database %s, exiting",
			  Database);
		exit (-9);
	}
	db_Close ();		/* Leave it closed except while needed */
	/* UpdateMem (); */
	switch (ArchiveMode)
	{
	    case AR_TAPE:
		SetStatus (TRUE, "Awaiting tape");
	    break;
	    case AR_EOD:
		SetStatus (TRUE, "Awaiting optical disk");
	    break;
	}
/*
 * Go into our dump loop.
 */
	msg_add_fd (XConnectionNumber (XtDisplay (Top)), xevent);
	msg_await ();
	db_Close ();
	return (-9);
}


static void
InitArchiver (argc, argv)
int *argc;
char **argv;
{
	int i;

	/*
	 * Initialize X and the Toolkit, get our toplevel shell and AppContext,
	 * and get our application options from the resource database
	 */
	Top = XtAppInitialize (&Appc, "Archiver", 
		Options, XtNumber(Options),
		argc, argv,
		NULL, NULL, 0);

	/*
	 * If any args are left, there was an error on the command line
	 */
	if (*argc > 1)
	{
		msg_ELog(EF_PROBLEM,"Illegal command-line syntax");
		Usage(argv[0], *argc, argv);
		Die();
	}

	/*
	 * Now retrieve each of our global options from the rm database
	 */
	for (i = 0; i < XtNumber(AppResources); ++i)
	{
	   XtGetApplicationResources(Top, OptionBase[i], 
			&AppResources[i], 1, NULL, 0);
	}

	/*
	 * Now do some error checking and feedback to the user
	 */
	if (StartMinute > 60)
	{
		msg_ELog (EF_PROBLEM, "Bad start minute %d, using 0", 
			StartMinute);
		StartMinute = 0;
	}
	if ( strcmp ( ModeString, "tape") == 0 )
		ArchiveMode = AR_TAPE;
	else if ( strcmp ( ModeString, "eod") == 0 )
		ArchiveMode = AR_EOD;
	else
	{
		msg_ELog(EF_PROBLEM,"Unknown archive mode %s",ModeString);
		Usage(argv[0], *argc, argv);
		Die();
	}
	msg_ELog (EF_DEBUG, "Free tape on 0Z: %s.", ZeroZFree?
		"true" : "false");

	msg_ELog (EF_INFO, "dump interval: %d, start: %d",
		DumpInterval, StartMinute);
	if ( ArchiveMode == AR_EOD )
	{
		msg_ELog (EF_INFO, "Optical disk mode");
		msg_ELog (EF_INFO, 
			"mount name: %s, dump files to directory: %s",
			MountName, OutputDir);
		msg_ELog (EF_INFO, 
			"minimum optical disk to dump: %d kb",MinDisk);
	}
	else
	{
		msg_ELog (EF_INFO, "Tape drive mode");
		msg_ELog (EF_INFO, "device name: %s, bf:%d, blocksize:%d, "
			  "tapelimit: %lu", DriveName, BFactor,
			  (BFactor*512), TapeLimit);
	}
}


/*
 * Explain all of the command line options of Archiver
 */
static void
Usage(prog, argc, argv)
	char *prog;
	int argc;
	char **argv;
{
   short i;

   /*
    * Mention all of the illegal options
    */
   for (i = 1; i < argc; ++i)
   {
   	fprintf(stderr,"Unknown or ambiguous option: %s\n",argv[i]);
   }

   fprintf(stderr,"Usage: %s [options], where options are the following:\n",
			prog);
   fprintf(stderr,"   %-20s Archive mode: optical disk or tape (tape)\n",
			"-mode eod|tape");
   fprintf(stderr,"   %-20s Device name (%s)\n",
			"-device,-v,-f <dev>",DEF_DEVICEFILE);
   fprintf(stderr,"   %-20s Starting hour offset, 0-60 minutes. (0)\n",
			"-start <min>");
   fprintf(stderr,"   %-20s Dump interval in minutes (%d)\n",
			"-t,-interval <min>",DEF_DUMPINTERVAL);
   fprintf(stderr,"   %-20s Free device at 0z (yes)\n","-z yes|no");
   fprintf(stderr,"   %-20s Delay button times (1,2,5)\n","-wait n1,n2,...");
   fprintf(stderr,"   %-20s Specify database file, default '%s'\n",
	   "-database", DUMPED_FILES);
   fprintf(stderr,"   %-20s Show this information\n","-h,-help");

   fprintf(stderr,"\nTape mode:\n");
   fprintf(stderr,"   %-20s Tape storage limit, minus safety margin (%lu)\n",
			"-tapelimit <bytes>",DEF_TAPELIMIT);

   fprintf(stderr,"\nOptical disk mode:\n");
   fprintf(stderr,"   %-20s Optical disk output directory (%s)\n",
			"-output <dir>",DEF_OUTPUTDIR);
   fprintf(stderr,"   %-20s Optical disk mount name (%s)\n",
			"-n <name>",DEF_MOUNTNAME);
   fprintf(stderr,"   %-20s Minimum required disk size in kilobytes (%li)\n",
			"-k <kb>", DEF_MINDISK);

   fprintf(stderr,"\nDefault values are shown in parentheses.\n");
   fprintf(stderr,"Standard X Toolkit options are understood %s",
   		  "and all options can be abbreviated.\n");
   fprintf(stderr,"The following resources are also available:\n");
   fprintf(stderr,"   %-20s %-20s %-20s\n","NAME","CLASS","TYPE");
   for (i = 0; i < XtNumber(AppResources); ++i)
   {
      fprintf(stderr,"   %-20s %-20s %-20s\n",
			AppResources[i].resource_name,
			AppResources[i].resource_class,
			AppResources[i].resource_type);
   }
}




static void
MakeWidget (argc, argv)
int *argc;
char **argv;
/*
 * Put together the control widget.  The Top widget shell should already
 * have been set in InitArchiver()
 */
{
	Arg args[10];
	int n;
	Widget w, above, button;
	XColor screen, exact;

/*
 * The inevitable form.
 */
	Form = XtCreateManagedWidget ("form", formWidgetClass, Top, NULL, 0);
	above = NULL;
/*
 * Give our status.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Archiver status:");	n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	w = XtCreateManagedWidget ("stitle", labelWidgetClass, Form, args, n);
	n = 0;
	XtSetArg (args[n], XtNlabel, "Unknown");	n++;
	XtSetArg (args[n], XtNfromHoriz, w);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	XtSetArg (args[n], XtNresizable, True);		n++;
	WStatus = XtCreateManagedWidget ("status", labelWidgetClass, Form,
			args, n);
/*
 * The line of control buttons.  All but the "take/release button" start
 * out insensitive, until we have a device to write to.
 * The sensitivity will be updated in the ActionButton() callback,
 * depending on whether the device is being opened or closed.
 */
	n = 0;
	above = w;
	XtSetArg (args[n], XtNlabel, "Finish");		n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	button = XtCreateManagedWidget ("finish", commandWidgetClass, Form,
			args, n);
	XtAddCallback (button, XtNcallback, (XtCallbackProc) Finish, 0);
	FinishButton = button;
	XtSetSensitive(FinishButton, False);
/*
 * A "take/release" button.
 */
	n = 0;
	switch ( ArchiveMode )
	{
	    case AR_TAPE:
		XtSetArg (args[n], XtNlabel, "Take tape");	n++;
	    break;
	    case AR_EOD:
		XtSetArg (args[n], XtNlabel, "Mount optical disk");	n++;
	    break;
	}
	XtSetArg (args[n], XtNfromHoriz, button);	n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	Action = XtCreateManagedWidget ("action", commandWidgetClass, Form,
			args, n);
	XtAddCallback (Action, XtNcallback, (XtCallbackProc) ActionButton, 0);
/*
 * The explicit request for "write now".  While suspended, this button
 * will read "Resume" rather than "Write Now"
 */
	n = 0;
	XtSetArg(args[n], XtNlabel, "Write Now");	n++;
	XtSetArg(args[n], XtNfromHoriz, Action);	n++;
	XtSetArg(args[n], XtNfromVert, above);		n++;
	button = XtCreateManagedWidget ("write", commandWidgetClass,
					Form, args, n);
	XtAddCallback(button, XtNcallback, (XtCallbackProc) WriteNow, 0);
	WriteButton = button;
	XtSetSensitive(WriteButton, False);

	above = CreateWaitButtons(WaitTimes, Form, button, above);
	SetWaitSensitivity(False);

/*
 * Status info.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "0 Bytes in 0 files             "); n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	XtSetArg (args[n], XtNresize, True);            n++;
	Bytes = XtCreateManagedWidget ("bytes", labelWidgetClass, Form,args,n);

	XtRealizeWidget (Top);
	Sync ();

/*
 * Also look up a couple of pixel colors.
 */
	XAllocNamedColor (XtDisplay (Top), DefaultColormap (XtDisplay (Top),0),
		"red", &exact, &screen);
	RedPix = screen.pixel;
	XAllocNamedColor (XtDisplay (Top), DefaultColormap (XtDisplay (Top),0),
		"white", &exact, &screen);
	WhitePix = screen.pixel;
}


/*
 * Create the wait buttons as specified in the string 'times'
 * The global WaitButtons array will be NULL terminated when done
 * Returns the widget which further widgets would come below,
 * i.e. either the first WaitButton, or 'above' if there is none
 */
static Widget
CreateWaitButtons(times, parent, left, above)
	String times;
	Widget parent;
	Widget left, above;
{
	Arg args[5];
	int n,i;
	int ntimes;
	char *scan = times;
	int delays[MAX_WAITBUTTONS];
	char buf[30];

	ntimes = 0;
	while (*scan && (ntimes < MAX_WAITBUTTONS) &&
			(sscanf(scan,"%i",&delays[ntimes]) == 1))
	{
		++ntimes;
		if ((scan = strchr(scan,',')) == NULL)
			break;
		++scan;		/* skip the comma */
	}

	for (i = 0; i < ntimes; ++i)
	{
		n = 0;
		sprintf(buf, "Wait %d min", delays[i]);
		XtSetArg(args[n], XtNlabel, buf);		n++;
		XtSetArg(args[n], XtNfromHoriz, left);		n++;
		XtSetArg(args[n], XtNfromVert, above);		n++;
		WaitButtons[i] = XtCreateManagedWidget ("suspend", 
					commandWidgetClass,
					parent, args, n);
		XtAddCallback(WaitButtons[i], XtNcallback, 
		      (XtCallbackProc) SuspendWrite, 
		      (XtPointer)(delays[i]*60));
		left = WaitButtons[i];
	}
	WaitButtons[ntimes] = NULL;
	if (ntimes == 0)
		return(above);
	else
		return(WaitButtons[0]);
}



static void
Finish ()
/*
 * Finish things out.
 */
{
	if (DeviceFD < 0)
	{
	    switch ( ArchiveMode )
	    {
	      case AR_TAPE:
		SetStatus (TRUE, "Can't finish -- no tape");
	      break;
	      case AR_EOD:
		SetStatus (TRUE, "Can't finish -- no optical disk");
	      break;
	    }
	}
	else
	{
	   msg_ELog(EF_DEBUG, "requesting a write and a finish");
	   RequestWrite(TRUE);	/* Request a write with an explicit finish */
	}
}


/*
 * Things to do when finishing off, such as releasing devices
 * and dying
 */
static void
FinishFinishing(error)
int error;  /* non-zero if we're finishing due to an error */
{
	msg_ELog(EF_DEBUG, "finishing a finish");
	switch ( ArchiveMode )
	{
	    case AR_TAPE:
		XtSetSensitive(FinishButton, False);
		XtSetSensitive(WriteButton, False);
		SpinOff();
	    break;
	    case AR_EOD:
		if (DeviceFD >= 0)
			close(DeviceFD);
		EjectEOD ();
	    break;
	}
	if (error)
		SetStatus (TRUE, "CROAK");
	else
		SetStatus (FALSE, "Finished");
	Die ();
}




static void
ActionButton ()
/*
 * Take or release the tape.
 */
{
	static int TimerEvent;		/* The regular write event */
					/* This function both starts and
					 * cancels it */
	Arg args[2];
	ZebTime current, zt;
	int year, month, day;
	int status;

/*
 * If in the middle of a write, we can't do anything now since we don't
 * want to release the tape while writing.
 */
	if (WriteInProgress)
		return;
/*
 * If we don't have a device, we try to get one.
 */
	if (DeviceFD < 0)
	{
		switch ( ArchiveMode )
		{
			case AR_TAPE:
		        /*
		         * Get the tape.
		         */
			    if (! OpenTapeDevice ())
			    {
				SetStatus (TRUE, "Unable to open tape");
				return;
			    }
	
			    SetStatus (FALSE, "Sleeping");
			    XtSetArg (args[0], XtNlabel, "Free tape");
			break;
			case AR_EOD:
			    MountEOD();
			    XtSetArg (args[0], XtNlabel, "Free optical disk.");
			break;
		}
		XtSetSensitive(FinishButton, True);
		XtSetSensitive(WriteButton, True);
	
		/*
		 * Start saving stuff.  Find the next time by searching 
		 * from the beginning of the day until we get a time 
		 * greater than the current time.
		 */
		tl_Time (&current);
		TC_ZtSplit (&current, &year, &month, &day, 0, 0, 0, 0);
		TC_ZtAssemble (&zt, year, month, day, 0, StartMinute, 0, 0);
		while (zt.zt_Sec < current.zt_Sec)
			zt.zt_Sec += DumpInterval * 60;
						
		TimerEvent = tl_AbsoluteReq (TimerSaveFiles, 0, &zt,
			DumpInterval*60*INCFRAC);
	}
/*
 * Otherwise we give it away.
 */
	else
	{
	    tl_Cancel (TimerEvent);
	    switch ( ArchiveMode )
	    {
		case AR_TAPE:
		    XtSetArg (args[0], XtNlabel, "Take tape");
		    XtSetSensitive(FinishButton, False);
		    XtSetSensitive(WriteButton, False);
		    SpinOff ();
		    close (DeviceFD);
		    DeviceFD = -1;
		    SetStatus (TRUE, "Awaiting tape");
		break;
		case AR_EOD:
		    status = EjectEOD();
		    if ( !status )
		    {
		        XtSetArg (args[0], XtNlabel, "Mount optical disk");
		        DeviceFD = -1;
		        SetStatus (TRUE, "Awaiting optical disk");
		        XtSetSensitive(FinishButton, False);
		        XtSetSensitive(WriteButton, False);
		    }
		    else
		    {
		        XtSetArg (args[0], XtNlabel, "Retry eject");
		        SetStatus (TRUE, "Optical Disk Eject Error!");
		    }
		break;
	    }
	}
	XtSetValues (Action, args, 1);
}



#ifdef notdef
static void
LoadFileList ()
/*
 * Pull in the list of files which have already been dumped to disk.
 */
{
	FILE *fp;
	char pname[200], *colon;
	SValue v;
	date d;
/*
 * Open up the file.
 */
	sprintf (listfile, "%s/%s", GetProjDir(), DUMPED_FILES );
	DumpedTable = usy_c_stbl ("DumpedTable");
	if ((fp = fopen (listfile, "r")) == NULL)
	{
		msg_ELog (EF_INFO, "No dumped file list, '%s'", listfile);
		return;
	}
/*
 * Go through and read each platform.
 */
	msg_ELog (EF_DEBUG, "Reading dumped file list from %s", listfile);
	while (fgets (pname, 200, fp))
	{
	/*
	 * Split apart the entry.
	 */
		if ((colon = strchr (pname, ':')) == 0 || 
		  sscanf(colon+1, "%li %li", &d.ds_yymmdd, &d.ds_hhmmss) != 2)
		{
			msg_ELog (EF_PROBLEM, "Bad %s line: %s",
				  DUMPED_FILES, pname);
			continue;
		}
		*colon = '\0';
	/*
	 * Store it.
	 */
		v.us_v_date = d;
		usy_s_symbol (DumpedTable, pname, SYMT_DATE, &v);
	}
/*
 * All done.
 */
	fclose (fp);
}
#endif




static int
OpenTapeDevice ()
/*
 * Get the device opened.
 */
{
	if ((DeviceFD = open (DriveName, O_RDWR)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d opening %s", errno, DriveName);
		return (0);
	}
	return (1);
}



/*
 * Something or someone wants to do a write!  Every request MUST
 * come through this function.  If a write is already in progress,
 * then a static pending flag is set.  When the current write finishes,
 * the loop will be re-entered because the pending flag is set.
 * 'all' is true for an explicit request to finish.  An explicit finish
 * automatically implies saving all files.  Situations where all files
 * are saved, other than when finishing, are determined in DoTheWriteThing()
 * If a request to finish is made, the follow-through actions are
 * executed from DoTheWriteThing().
 */
static void
RequestWrite(finish)
	int finish;
{
	static int pending = FALSE;  /* A write request is pending */
	static int pending_finish;   /* True if a pending request has made
				      * an explicit request for all files */
	static int working = FALSE;

	/*
	 * If we have no tape, we do nothing.
	 */
	if (DeviceFD < 0)
		return;

	/*
	 * If a SaveFiles is already in progress, just set the pending flag
	 * and return.  If any request wants a finish, then the
	 * next request will do a finish
	 */
	if (working)
	{
		pending = TRUE;
		pending_finish = pending_finish | finish;
		msg_ELog(EF_DEBUG,
			"Write in progress, request to %s is now pending",
			(finish)?"finish":"write");
		return;
	}
	else
	{
		working = TRUE;
		pending_finish = finish;
		pending = TRUE;
	}

	/*
	 * As long as we have pending requests, do the write thing
	 */
	while (pending)
	{
		pending = FALSE;

		msg_ELog(EF_DEBUG, 
			"Beginning a %s request",
			(pending_finish)?"finish":"write");
		/*
		 * The time that this write request actually gets
		 * started will be calculated in this function
		 */
		DoTheWriteThing(pending_finish);
	}

	/*
	 * All writing is finished, so note this in all the necessary
	 * settings
	 */
	working = FALSE;
	SetStatus (FALSE, "Sleeping");
	WriteInProgress = FALSE;
	SetWaitSensitivity(False);
	XtSetSensitive(FinishButton, True);
	XtSetSensitive(Action, True);
	Sync();
}


/*
 * The timer calls this function at regular intervals (DumpInterval)
 * to instigate a write.  This function just does a RequestWrite(),
 * and that takes care of it.  This request could end up pending
 * because of a current write in progress, so the current time is
 * ignored and calculated at the time of the actual write
 */
static void
TimerSaveFiles(zt)
	ZebTime *zt;
{
	char stime[50];

	TC_EncodeTime(zt, TC_Full, stime);
	msg_ELog(EF_DEBUG,"%s, TimerSaveFiles() requesting a write",stime);
	RequestWrite(FALSE);
}


/*
 * Peform checks before and after a SaveFiles() call, such as
 * finishing a day's tape or a full tape.  The current time
 * is retrieved from the timer and used for all of the checks.
 * All writes must go through this function, so FreshTape can keep
 * track of whether the current tape has been written.
 * The 'explicit_finish' parameter is an explicit request to write all files.
 * If false, all files may still be written if deemed necessary here.
 */
static void
DoTheWriteThing(explicit_finish)
	int explicit_finish;
{
	static int FreshTape = TRUE;	/* True if the current tape has
					 * never been written to */
	int status;
	int year, month, day, hour, minute, second;
	int all;		/* Whether we want to write all files */
	int delta;
	ZebTime daystart;
	ZebTime zt;	/* The current time, the time this write begins */
	char datafile[120];
#if defined(SVR4) || defined (__osf__)
	struct statvfs buf;
#else
	struct statfs buf;
#endif
/*
 * Special check -- if this is the first dump of a new day, and not
 *		    already a fresh tape, we clean up everything
 */
	tl_Time(&zt);
	TC_ZtSplit (&zt, &year, &month, &day, &hour, &minute, &second, 0);
	TC_ZtAssemble (&daystart, year, month, day, 0, 0, 0, 0);
	delta = zt.zt_Sec - daystart.zt_Sec;

	all = FALSE;		/* Default, verified below */

	switch ( ArchiveMode )
	{
	   case AR_TAPE:
		/*
		 * If we are starting new tapes at 0z (ZeroZFree), and if this
		 * is the first write of the day, and not a fresh tape,
		 * then we need to finish on this write
		 */
		if (ZeroZFree)
			all = ((delta < 60 * DumpInterval) && (! FreshTape));
		if (all)
			msg_ELog(EF_INFO,"finishing yesterday's tape");
		break;

	   case AR_EOD:
		/*
		 * For optical disks, the ZeroZFree flag is ignored (?) 
		 * Just write until the disk is full
		 */
		sprintf( datafile, "%s/%02d%02d%02d.%02d%02d.tar",
		    OutputDir,year,month,day,hour,minute );
		if ((DeviceFD = open (datafile, O_RDWR|O_CREAT,(int)0664)) < 0)
		{
		    SetStatus ( TRUE, "Bad file open on EOD" );
		    msg_ELog (EF_INFO, "Error %d opening %s", errno, datafile);
		    /*
		     * If we're supposed to finish, then we do so despite the
		     * error
		     */
		    if (explicit_finish)
			FinishFinishing(TRUE);
		}
		break;
	}

	/*
	 * Save ALL files only if we want to finish off this tape, either
	 * because we want to start a new day or because it has been
	 * explicitly requested (most likely by the user)
	 */
	all = all | explicit_finish;
	SaveFiles(all); 	

	/*
	 * If we are finishing, then complete the necessary actions.
	 * This won't return.
	 */
	if (explicit_finish)
	   	FinishFinishing(FALSE);

	switch ( ArchiveMode )
	{
	   case AR_TAPE:
		if (all)	/* else we need a new day's tape */
		{
		    ActionButton ();
		    SetStatus (TRUE, "Need new day's tape");
		    FreshTape = TRUE;
		}
		else if (BytesWritten > TapeLimit)
		{
		    /*
		     * This tape is full; get a new one 
		     */
		    msg_ELog(EF_DEBUG,"tape is full, %li Mb, limit %li",
				BytesWritten/1000000,
				TapeLimit/1000000);
		    ActionButton ();
		    SetStatus (TRUE, "Tape is full, need a new one");
		    FreshTape = TRUE;
		}
		else
		    FreshTape = FALSE;
		if (FreshTape)
		{
			msg_ELog (EF_INFO, "LOAD A NEW TAPE");
		}
	    break;

	   case AR_EOD:
		/*
		 * Finished writing another file onto optical disk
		 * Make sure there is room for more, else request
		 * a new disk
		 */
		close ( DeviceFD );
		status = statfs(DriveName,&buf);
		if ( !status && buf.f_bavail < MinDisk )
		{
			msg_ELog(EF_DEBUG,
			   "Disk full. %li kb left less than %li",
			   buf.f_bavail, MinDisk);
			ActionButton ();
			SetStatus (TRUE, "Need new optical disk.");
		}
	    	break;
	}

	/*
	 * If we're supposed to die when finished with this write action,
	 * then do so.  Note that any PENDING writes will be lost.  No
	 * further writes are possible since Dying implies we have
	 * lost our DataStore connection.
	 */
	if (Dying)
	{
		msg_ELog(EF_DEBUG,"Acting on impending death");
		Die();
	}
}


/*
 * The only function which should call this is DoTheWriteThing()
 */
static void
SaveFiles (all)
int all;
/*
 * Pass through the list of stuff and save files to the tape.
 */
{
	int plat, nplat = ds_GetNPlat ();
	PlatformInfo pi;
	int cmdlen;

	/*
	 * The tar command, less the file names
	 */
	sprintf (Tarbuf, "exec tar cfb - %d ", BFactor);
	cmdlen = strlen (Tarbuf);
	Tarlen = cmdlen;

	/*
	 * Our cache of dumped file indices to which DumpPlatform
	 * appends.
	 */
	if (db_Open (Database) != 0)
	{
		char buf[100];
		sprintf (buf, "Cannot open database '%s'", Database);
		msg_ELog (EF_PROBLEM, "%s", buf);
		SetStatus (TRUE, buf);
		return;
	}
	DumpedFiles = (int *) malloc (2048 * sizeof (int));
	NFiles = 0;
	

	/*
	 * Pass through the platform table and dump things.  Keep adding
	 * files until DumpPlatform indicates it's time to stop.
	 */
	SetStatus (FALSE, "Scanning platforms");
	for (plat = 0; plat < nplat; plat++)
	{
		ds_GetPlatInfo (plat, &pi);
		if (! pi.pl_SubPlatform)
		{
			if (! DumpPlatform (plat, &pi, all))
				break;
		}
	}
	db_Close ();	/* Leave closed during the long delay in writing */

	/*
	 * Run the tar command to put this all together, but only if
	 * we actually got any files to write
	 */
	if ( Tarlen > cmdlen )
	{
		/* 
		 * Do all the settings to signal a write in progress 
		 */
		SetStatus (FALSE, "Writing");
		WriteInProgress = TRUE;
		SetWaitSensitivity(True);
		XtSetSensitive(FinishButton, False);
		XtSetSensitive(Action, False);
		Sync();

		/*
		 * Now try running the tar command
		 */
		if (RunTar (Tarbuf))
		{
			/* The tar succeeded, so put an EOF marker
			 * on the tape */
			switch ( ArchiveMode )
			{
			   case AR_TAPE:
				WriteEOF ();
				break;
			   case AR_EOD:
				break;
			}
			UpdateList ();
		}
	}	

	if (DumpedFiles)
		free (DumpedFiles);
	DumpedFiles = NULL;
	NFiles = 0;
	/*
	 * Another write may be pending so we'll just return.
	 * If all writing is finished, it is up to the calling function to
	 * set all the necessary flags
	 */
}



static int *
GetFileChain (PlatformId pid, const ZebTime *since, int *count)
/*
 * Return an array of data file indices from latest to oldest.
 * The array must be freed by the caller.
 */
{
	DataFileInfo dfi;
	DataSrcInfo dsi;
	int *chain;
	int nchain;
	int findex;
	int n;

	n = 0;
	chain = NULL;
/*
 * Fill the array
 */
	ds_LockPlatform (pid);
	ds_GetDataSource (pid, 0, &dsi);
	findex = dsi.dsrc_FFile;
	if (findex != 0)
		ds_GetFileInfo (findex, &dfi);
	while (findex && TC_Less (*since, dfi.dfi_End))
	{
		if (! chain)
		{
			nchain = 32;
			chain = (int *) malloc (nchain * sizeof(int));
		}
		else if (n == nchain)
		{
			nchain *= 2;
			chain = (int *) realloc (chain, nchain * sizeof(int));

		}
		chain[n++] = findex;
		if ((findex = dfi.dfi_Next) > 0)
			ds_GetFileInfo (findex, &dfi);
	}

	ds_UnlockPlatform (pid);
	*count = n;
	return (chain);
}




#ifdef notdef 
static int
DumpPlatform (pid, pi, all)
PlatformId pid;
PlatformInfo *pi;
int all;
/*
 * Dump out any files from this platform by appending each file name onto
 * the Tarbuf command buffer
 */
{
	ZebTime dumptime;
	SValue v;
	int *chain;
	int type;
	DataSrcInfo dsi;
	DataFileInfo dfi;
	int findex;
	int nfiles, i;
	char buf[512];
	int ret;

/*
 * Find the last time this thing was dumped.
 */
	if (usy_g_symbol (DumpedTable, pi->pl_Name, &type, &v))
		TC_UIToZt (&(v.us_v_date), &dumptime);
	else
		dumptime.zt_Sec = dumptime.zt_MicroSec = 0;
	ds_LockPlatform (pid);
	ds_GetDataSource (pid, 0, &dsi);
	chain = GetFileChain (pid, &dumptime, &nfiles);

	TC_EncodeTime (&dumptime, TC_Full, buf);
	msg_ELog (EF_DEBUG, "platform '%s' has %d files since %s",
		  ds_PlatformName (pid), nfiles, buf);
/*
 * Now step through the file chain as far as we can.  We'll skip the loop
 * entirely if we have no files, or if there is only one and we are not
 * supposed to do all of them.  We have to traverse the file chain in time
 * order in case we have to abort before putting all of the file names on
 * the command line.  That way we can advance the dump time forward with
 * each file as its put on the tar command line.
 */
	ret = 1;
	for (i = nfiles - 1; (nfiles > 0) && (i >= (all ? 0 : 1)); --i)
	{
		char *fname;

		findex = chain[i];
		ds_GetFileInfo (findex, &dfi);
	/*
	 * Fix up the file name
	 */
		fname = dfi.dfi_Name;
		sprintf (buf, "%s/%s ", dsi.dsrc_Where, fname);
	/*
	 * If this file name will not fit in the tar command line, skip it
	 * and the rest of the files in this platform.
	 */
		if (Tarlen + strlen(buf) + 1 >= sizeof(Tarbuf))
		{
			msg_ELog (EF_DEBUG, "tar command line full");
			ret = 0;
		        break;
		}
		msg_ELog (EF_DEBUG, "Dumping file '%s' (index %d)", 
			  fname, findex);
		strcat (Tarbuf, buf);
		Tarlen += strlen(buf);
	/*
	 * Send the MarkArchived request now, even though we do not know
	 * that the tar will succeed.  This is to help insure that nothing
	 * is written to the file while archiving it.  If the archive fails,
	 * we'll try again later, since we go by our own dates, and not the
	 * archived flag, when picking files to write.
	 */
		ds_MarkArchived (findex);
	/*
	 * Advance the dumped time to include this file
	 */
		dumptime = dfi.dfi_End;
	}
	if (chain)
		free (chain);
/*
 * Record the new time.
 */
	TC_EncodeTime (&dumptime, TC_Full, buf);
	msg_ELog (EF_INFO, "platform '%s' being dumped up to %s",
		  ds_PlatformName (pid), buf);
	TC_ZtToUI (&dumptime, &(v.us_v_date));
	usy_s_symbol (DumpedTable, pi->pl_Name, SYMT_DATE, &v);
	ds_UnlockPlatform (pid);
	return (ret);
}
#endif /* notdef */



static int
DumpPlatform (pid, pi, all)
PlatformId pid;
PlatformInfo *pi;
int all;
/*
 * Dump out any files from this platform by appending each file name onto
 * the Tarbuf command buffer
 */
{
	ZebTime dumptime;
	SValue v;
	int *chain;
	int type;
	DataSrcInfo dsi;
	DataFileInfo dfi;
	int findex;
	int ndumped;
	int nfiles, i;
	char buf[512];
	int ret;
/*
 * Get a chain of all the files in this platform.
 */
	ds_LockPlatform (pid);
	ds_GetDataSource (pid, 0, &dsi);
	chain = GetFileChain (pid, &ZT_ALPHA, &nfiles);
/*
 * Make sure there's room in our file list.
 */
	if (nfiles)
		DumpedFiles = (int *) realloc (DumpedFiles, 
					       (NFiles + nfiles)*sizeof(int));

	msg_ELog (EF_DEBUG, "platform '%s' has %d files on disk",
		  ds_PlatformName (pid), nfiles);
/*
 * Now step through the file chain as far as we can.  We'll skip the loop
 * entirely if we have no files, or if there is only one and we are not
 * supposed to do all of them.
 */
	ret = 1;
	ndumped = 0;
	for (i = nfiles - 1; (nfiles > 0) && (i >= (all ? 0 : 1)); --i)
	{
		char *fname;

		findex = chain[i];
		ds_GetFileInfo (findex, &dfi);
	/*
	 * Skip this file if already in the database.  Someday this can
	 * check for number of samples or revision or such.
	 */
		if (! db_Fetch (ds_PlatformName(dfi.dfi_Plat),&dfi,NULL,NULL))
			continue;
	/*
	 * Fix up the file name
	 */
		fname = dfi.dfi_Name;
		sprintf (buf, "%s/%s ", dsi.dsrc_Where, fname);
	/*
	 * If this file name will not fit in the tar command line, skip it
	 * and the rest of the files in this platform.
	 */
		if (Tarlen + strlen(buf) + 1 >= sizeof(Tarbuf))
		{
			msg_ELog (EF_DEBUG, "tar command line full");
			ret = 0;
		        break;
		}
		msg_ELog (EF_DEBUG, "Dumping file '%s' (index %d)", 
			  fname, findex);
		strcat (Tarbuf, buf);
		Tarlen += strlen(buf);
	/*
	 * Send the MarkArchived request now, even though we do not know
	 * that the tar will succeed.  This is to help insure that nothing
	 * is written to the file while archiving it.  If the archive fails,
	 * we'll try again later, since we go by our own database, and not the
	 * archived flag, when picking files to write.  So the datafile
	 * index is kept in an array of files to be added to the database
	 * later.
	 */
		ds_MarkArchived (findex);
		DumpedFiles[NFiles++] = findex;
	/*
	 * Advance the dumped time for this platform
	 */
		++ndumped;
		dumptime = dfi.dfi_End;
	}
	if (chain)
		free (chain);
/*
 * Record the new time.
 */
	if (ndumped > 0)
		msg_ELog (EF_INFO, "%s: dumping %d files up to %s",
			  ds_PlatformName (pid), ndumped, 
			  TC_AscTime (&dumptime, TC_Full));
	else
		msg_ELog (EF_INFO, "%s: no new files to be dumped",
			  ds_PlatformName (pid));
	ds_UnlockPlatform (pid);
	return (ret);
}





static int
RunTar (cmd)
char *cmd;
/*
 * Run this tar command, and deal with writing its output to tape.
 * To help reduce system load, at the cost of some response time,
 * the X and msg fd's are only polled for every other block read
 */
{
	FILE *pfp = (FILE *) popen (cmd, "r");
	static char *fbuf = NULL;
	int rstatus;
	unsigned long nb, tnb = 0;
	int step = 4;
	unsigned long nblocks = 0;
	unsigned long blocksize = BFactor * 512;
# ifdef hpux
	int fd = pfp->__fileL;	/* HP weirdness -- untested */
# else
#  ifdef linux
	int fd = pfp->_fileno;	/* LINUX */
#  else
	int fd = pfp->_file;
#  endif
# endif
/*
 * Be sure we can run tar.
 */
	if (pfp == NULL)
	{
		msg_ELog (EF_PROBLEM, "Tar command execute failed");
		SetStatus (TRUE, "Can't run tar");
		return (FALSE);
	}
/*
 * Set up our block buffer if not done yet.
 */
	if (!fbuf)
		fbuf = (char *) malloc (blocksize);
/*
 * Now read out chunks of stuff.
 */
	while ((nb = netread (fd, fbuf, blocksize)) > 0)
	{
		if (write (DeviceFD, fbuf, nb) < nb) /* oh shit! */
		{
			msg_ELog (EF_EMERGENCY,"Archive device write error %d",
				  errno);
			if ( ArchiveMode == AR_EOD ) close(DeviceFD);
			ActionButton (); /* Free drive */
			SetStatus (TRUE, "Device write error!");
			pclose (pfp);
			/* LoadFileList (); */
			return (FALSE);
		}
		tnb += nb;
		if (tnb / blocksize > nblocks+step)
		{
			char stat[64];

			nblocks += step;
			if (nblocks > (1 << 11))
			{
				sprintf (stat,
					 "Writing... %.1fk blocks, %.1f MB", 
					 (double)nblocks / (1 << 10),
					 (double)tnb / (1 << 20));
				step = 128;
			}
			else
				sprintf (stat,
					 "Writing... %lu blocks, %lu KB", 
					 nblocks, tnb >> 10);
			SetStatus (FALSE, stat);
			Sync ();
		}
				 
		/*
		 * Check for messages and X events between writes
		 */
		while (msg_poll (0) != MSG_TIMEOUT)
			/* keep responding */;
		/*
		 * If suspended, handle messages and X events until
		 * one of them changes our suspended state.  The timeout
		 * can be large since each event handled will cause
		 * msg_poll() to return and the Suspended condition to
		 * be tested. 
		 */
		if (Suspended)
			msg_ELog (EF_DEBUG, "polling for end of suspension");
		while (Suspended)
			msg_poll (300);
	}
/*
 * If tar returned OK, so do we.
 */
	msg_ELog (EF_DEBUG, "Transferred %d bytes, last %d", tnb, nb);
	FilesWritten++;
	BytesWritten += tnb;
	if ((rstatus = pclose (pfp)) == 0)
		return (TRUE);
	msg_ELog (EF_PROBLEM, "Tar returned status %d", rstatus);
	SetStatus (TRUE, "Tar returned failure");
	return (FALSE);
}



static void
MountEOD()
{
    char cmd[80];
    int	status;
    sprintf ( cmd, "eodmount %s /%s", DriveName, MountName );
    status = system(cmd);
    DeviceFD = 0;
}


static int
EjectEOD()
{
    char cmd[80];
    int	 status;
    if ( DeviceFD >= 0 )
    {
	sprintf ( cmd, "eodmount -u %s", DriveName );
	status = system(cmd);
    }
    sprintf ( cmd, "eodutil %s eject", MountName);
    status = system(cmd);
    return ( WEXITSTATUS(status));
}


static int
xevent (fd)
int fd;
/*
 * Deal with an Xt event.
 */
{
	XEvent event;
/*
 * Deal with events as long as they keep coming.
 */
 	while (XtAppPending (Appc))
	{
		XtAppNextEvent (Appc, &event);
		XtDispatchEvent (&event);
	}
	return (0);
}


static int
Handler (msg)
Message *msg;
/*
 * Deal with an incoming message.
 */
{
	struct mh_template *tmpl;

	switch (msg->m_proto)
	{
	   case MT_MESSAGE:
	   	tmpl = (struct mh_template *) msg->m_data;
		if (tmpl->mh_type == MH_DIE)
		{
			/*
			 * We can't die if we're in the middle of a 
			 * write.  If we are, set a flag that will
			 * be tested once the writing is finished.
			 */
			if (WriteInProgress)
			{
				Dying = TRUE;
				msg_ELog(EF_DEBUG,
	   "MH_DIE message: death will ensue after current write completes");
			}
			else
			{
				Die ();
			}
		}
		else
			msg_ELog (EF_PROBLEM, "Weird MH msg %d",tmpl->mh_type);
		break;

	   default:
	   	msg_ELog (EF_PROBLEM, "Unknown msg proto %d", msg->m_proto);
		break;
	}
	return (0);
}



static void
Die ()
/*
 * Ribbit.
 */
{
	db_Close ();
	exit (0);
}



static void
UpdateList ()
/*
 * Update the list that says when we've dumped things.
 */
{
	ZebTime now;
	char string[80];
	Arg args[2];
	int i;
#ifdef notdef
	FILE *fp;
/*
 * Open up the file.
 */
	sprintf (listfile, "%s/DumpedFiles", GetProjDir() );
	if ((fp = fopen (listfile, "w")) == NULL)
	{
		msg_ELog (EF_PROBLEM, "Unable to open %s", listfile);
		return;
	}
/*
 * Scan through our table and write everything.
 */
	msg_ELog (EF_DEBUG, "Writing dumped file dates to '%s'", listfile);
	usy_traverse (DumpedTable, WriteFileDate, (long) fp, FALSE);
	fclose (fp);
#endif
	if (db_Open (Database) != 0)
	{
		char buf[100];
		sprintf (buf, "Failed to update database '%s'", Database);
		msg_ELog (EF_PROBLEM, "%s", buf);
		SetStatus (TRUE, buf);
		Sync ();
		return;
	}
/*
 * Insert the dumped files into the database.
 */
	tl_Time (&now);
	for (i = 0; i < NFiles; ++i)
	{
		DataFileInfo dfi;

		ds_GetFileInfo (DumpedFiles[i], &dfi);
		db_Insert (ds_PlatformName (dfi.dfi_Plat), &dfi, &now);
	}
	db_Close ();
/*
 * Update the widget too.
 */
	sprintf (string, "%.2f MBytes in %d files.",
		(float)BytesWritten/1000000.0, FilesWritten);
	XtSetArg (args[0], XtNlabel, string);
	XtSetValues (Bytes, args, 1);
	Sync ();
}



#ifdef notdef
static int
WriteFileDate (sym, type, v, fp)
char *sym;
int type;
SValue *v;
FILE *fp;
/*
 * Write out a single file date.
 */
{
	fprintf (fp, "%s: %li %li\n", sym, v->us_v_date.ds_yymmdd,
			v->us_v_date.ds_hhmmss);
	return (TRUE);
}
#endif



static int
netread (fd, dest, len)
int fd;
char *dest;
int len;
/*
 * Read a full len from the link.
 */
{
	int nread = 0, nb;

	while (nread < len)
	{
		if ((nb = read (fd, dest + nread, len - nread)) < 0)
		{
			perror ("net read");
			return (nread);
		}
		else if (nb == 0)
			return (nread);
		nread += nb;
	}
	return (nread);
}





static void
WriteEOF ()
/*
 * Write an EOF to the tape.
 */
{
	struct mtop op;

	op.mt_op = MTWEOF;
	op.mt_count = 1;
	if (ioctl (DeviceFD, MTIOCTOP, &op) < 0)
		perror ("Tape WEOF");
}




static void
SpinOff ()
/*
 * Spin off the tape.
 */
{
	struct mtop op;

	if (DeviceFD < 0)
		return;
	op.mt_op = MTOFFL;
	op.mt_count = 1;
	SetStatus (FALSE, "Spinning off tape");
	if (ioctl (DeviceFD, MTIOCTOP, &op) < 0)
		SetStatus (TRUE, "Unable to spin off tape");

	FilesWritten = 0;
	BytesWritten = 0;
}



static void
SetStatus (problem, s)
int problem;
char *s;
/*
 * Set our status window.
 */
{
	Arg args[4];
	int n = 0;

	XtSetArg (args[n], XtNlabel, s);	n++;
	XtSetArg (args[n], XtNbackground, problem ? RedPix : WhitePix); n++;
	XtSetValues (WStatus, args, n);
	Sync ();
	/*
	 * Note the status in the log as well
	 */
	msg_ELog(problem ? EF_PROBLEM : EF_DEBUG, s);
}


/*
 * Set the sensitivity of the wait buttons, since
 * they only make sense while a write is occurring
 */
static void
SetWaitSensitivity(sensitive)
	Boolean sensitive;
{
	Widget *buttons = WaitButtons;

	while (*buttons)
		XtSetSensitive(*buttons++, sensitive);
}



static void
Sync ()
/*
 * Synchronize the display.
 */
{
	XSync (XtDisplay (Top), False);
	xevent (0);
}


#ifdef notdef
static void
UpdateMem ()
/*
 * Update the "archived" flags on the daemon side.
 */
{
/*
 * Scan through our table.
 */
	usy_traverse (DumpedTable, TellDaemon, 0, FALSE);
}



/* ARGSUSED */
static int
TellDaemon (sym, type, v, junk)
char *sym;
int type;
SValue *v;
int junk;
/*
 * Update this platform.
 */
{
	int index;
	ZebTime ftime;
	PlatformId pid;
	DataSrcInfo dsi;
	DataFileInfo dfi;
/*
 * Plow through the file entries, marking everything that we have written 
 * out.
 */
	if ((pid = ds_LookupPlatform (sym)) != BadPlatform)
	{
		TC_UIToZt (&(v->us_v_date), &ftime);

		ds_GetDataSource (pid, 0, &dsi);
		for (index = dsi.dsrc_FFile; index; index = dfi.dfi_Next)
		{
			/*
			 * If this file is already marked or hasn't been done, 
			 * move on.  Otherwise send the notification.
			 */
			ds_GetFileInfo (index, &dfi);
			if (TC_LessEq(dfi.dfi_End, ftime) && !dfi.dfi_Archived)
				ds_MarkArchived (index);
		}
	}
	return (TRUE);
}
#endif /* notdef */




/*
 * The WriteNow callback.  The user-called equivalent to a timer event.
 * If a write is currently suspended, this just resumes the current
 * write instead.
 */
static void
WriteNow()
{
	if (Suspended)	/* Resume from a suspended state */
	{
		/*
		 * Remove any pending timer events
		 */
		if (ResumeEvent != -1)
		{
			tl_Cancel(ResumeEvent);
			ResumeEvent = -1;
		}
		if (RemainingEvent != -1)
		{
			tl_Cancel(RemainingEvent);
			RemainingEvent = -1;
		}

		/* Reset the suspended flag */
		Suspended = FALSE;

		/* Restore our button's label */
		XtVaSetValues(WriteButton, XtNlabel, "Write Now", NULL);

		SetStatus (FALSE, "Writing");
	}
	else
	{
		/* 
		 * We're in an X callback, so we must exit X's event
		 * processing and also start writing.  So register
		 * a timer event 0 seconds from now which will cause
		 * TimerSaveFiles to be entered upon return from
		 * this callback, when Message messages are handled
		 */
		tl_RelativeReq(TimerSaveFiles, 0, 0, 0);
	}
}


/*
 * SuspendWrite() -- If a write is in progress, suspend it and 
 * register a resume timer event the given number of seconds from now.
 * If a resume event already existed, it is cancelled first.
 */
static void
SuspendWrite(w, call_data)
	Widget w;
	XtPointer call_data;
{
	int waitsecs = (int) call_data;
	ZebTime now;
	static ZebTime ends;		/* Time this suspension ends */
					/* Must be static since the
					 * TimerRemaining function accesses
					 * it through the tl_RelativeReq
					 * data varibale */
	char buf[100];			/* status scratch area */

	/*
	 * Make sure there is a write in progress to suspend
	 */
	if (!WriteInProgress)
		return;

	/*
	 * Cancel any existing timer resume events and start another
	 */
	if (ResumeEvent != -1)
	{
		tl_Cancel(ResumeEvent);
	}
	ResumeEvent = tl_RelativeReq(TimerResumeWrite, 0,
				     waitsecs * INCFRAC, 0);

	/*
	 * Figure out the time this suspension will time out so
	 * that we can count down to it.  This value will remain the
	 * same (so that TimerRemaining can access it) until 
	 * another suspension delay is triggered
	 */
	tl_Time(&now);
	ends.zt_MicroSec = 0;
	ends.zt_Sec = now.zt_Sec + waitsecs;

	/*
	 * Now register the relative timer event which will count
	 * down to the end time every 15 seconds and show the value
	 * in the status label. The first call will be ASAP
	 */
	if (RemainingEvent != -1)
	{
		tl_Cancel(RemainingEvent);
	}
	RemainingEvent = tl_RelativeReq(TimerRemaining, &ends, 0, 
				15 * INCFRAC);

	/*
	 * Now set the Suspended flag.  This will be caught by the
	 * RunTar() loop and reading of the pipe will stop as long
	 * as Suspended remains true.  This in turn blocks the
	 * tar process and suspension of the tar is achieved.
	 */
	Suspended = TRUE;
	sprintf(buf,"Write suspended for %i seconds",waitsecs);
	SetStatus(TRUE, buf);
	XtVaSetValues(WriteButton, XtNlabel, "Resume", NULL);
}


/*
 * Given the ending time in *cdata, calculate the time remaining from
 * now and show this time in the status label
 */
static void
TimerRemaining(zt, cdata)
	ZebTime *zt;
	void *cdata;
{
	ZebTime *ends = (ZebTime *)cdata;   /* Time we're counting till */
	long seconds;			    /* Calculated seconds remaining */
	char buf[50];

	/*
	 * Make sure the suspension hasn't ended, if so
	 * we should cancel this timer and do nothing
	 */
	if (!Suspended)
	{
		tl_Cancel(RemainingEvent);
		RemainingEvent = -1;
		return;
	}

	/*
	 * Otherwise calculate (approximate) seconds left
	 */
	seconds = ends->zt_Sec - zt->zt_Sec;

	/*
	 * Display this value in the status label
	 */
	sprintf(buf,"Suspended.  Time remaining: %li:%02li", 
		seconds/60, seconds % 60);
	SetStatus(TRUE,buf);
}


/*
 * Resume a write from a timer event. If a suspended write still
 * exists, call WriteNow() to resume.  Otherwise just ignore the
 * event; it could be a case of this msg being in the queue before
 * a manual WriteNow() could cancel it
 */
/* ARGSUSED */
static void
TimerResumeWrite(zt)
	ZebTime *zt;
{
	char stime[50];

	TC_EncodeTime(zt, TC_Full, stime);
	if (Suspended)
	{
		msg_ELog(EF_DEBUG,
		   "%s, resume event calling WriteNow()", stime);
		WriteNow();
	}
	else
	{
		msg_ELog(EF_DEBUG,
		   "%s, resume event while not suspended", stime);
	}
}


