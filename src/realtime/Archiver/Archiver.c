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
# include <stdio.h>
# include <fcntl.h>
# include <errno.h>
# include <sys/types.h>
# include <sys/time.h>
# include <sys/ioctl.h>
# include <sys/mtio.h>
# include <sys/wait.h>
# include <sys/vfs.h>

# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Command.h>

# include <defs.h>
# include <message.h>
# include <timer.h>
# include <config.h>
# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"

MAKE_RCSID ("$Id: Archiver.c,v 1.15 1992-07-17 23:00:37 granger Exp $")

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
/*------------------------------------------------------------------------*/

# define BFACTOR	64	/* Blocking factor for the tar command */
# define BLOCKSIZE	(BFACTOR*512)

# define DEF_DUMPINTERVAL 120	/* how often, in minutes, to dump	*/
# define DEF_DEVICEFILE "/dev/nrst8"
# define DEF_MOUNTNAME "eod0"
# define DEF_OUTPUTDIR "/eod0"
# define DEF_TAPELIMIT 3500000000
# define DEF_MINDISK 10000

# define AR_TAPE 1
# define AR_EOD 2

# define MAX_WAITBUTTONS 5

/*------------------------------------------------------------------------
 * GLOBAL VARIABLES and FLAGS
 */

/*
 * This table contains an entry for each platform that we dump, indicating
 * the latest data which has been written.
 */
stbl	DumpedTable;
char listfile[200];	

int	TimerEvent;		/* The absolute, regular write event */
int 	ResumeEvent = -1;	/* If in suspended status, this is the
				 * timer event which will resume the tar.
				 * -1 indicates we're not suspended */
int	RemainingEvent = -1;	/* The event which slowly counts down the
				 * the time remaining in a suspension */
int	Suspended = FALSE;	/* True when a write has been stopped */
int	WriteInProgress = FALSE;/* A tar has been started */

int	Dying = FALSE;		/* Set by Handler() on a MH_SHUTDOWN msg
				 * during a write */

int	DeviceFD = -1;		/* -1 = no drive	*/

unsigned long	BytesWritten = 0;

int	FilesWritten = 0;
int	FreshTape = TRUE;

static char Tarbuf[65536]; 	/* Where the tar command is built.  */

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

/*
 * The offset of each of these is zero since we want to store the
 * values directly into the global variable rather than into a single
 * structure
 */
static XtResource AppResources[] = {
   { "tapeLimit", "TapeLimit", XtRInt, sizeof(int) /* == sizeof(long) */,
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
      0, XtRString, "1,2,5" }
};

static XtPointer OptionBase[] = {
   &TapeLimit, &DriveName, &OutputDir, &MountName,
   &DumpInterval, &StartMinute, &MinDisk, &ModeString,
   &ZeroZFree, &WaitTimes };

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
   {"-wait",	".waitTimes",	XrmoptionSepArg,	NULL}
};

/*---------------------------------------------------------------------*/

/*-- initialization --*/
static void	InitArchiver FP ((int *, char**));
static void	MakeWidget FP ((int *, char **));
static void	CreateWaitButtons FP ((String times, Widget parent,
			Widget left, Widget above));
static void	Usage FP((char *prog, int argc, char *argv[]));
static void	Die FP ((void));

/*-- socket i/o handlers --*/
static int	Handler FP ((Message *));
static int	xevent FP ((int));
static void	Sync FP ((void));
static void	PollWhileSuspended FP((void));

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

/*-- Timer event handlers --*/
static void	TimerSaveFiles FP ((ZebTime *));
static void	TimerResumeWrite FP((ZebTime *zt));
static void	TimerRemaining FP((ZebTime *zt, void *cdata));

/*-- high-level write and datastore interaction */
static void	LoadFileList FP ((void));
static void	SaveFiles FP ((int));
static void	DumpPlatform FP ((Platform *, int));
static int	RunTar FP ((char *));
static void	UpdateList FP ((void));
static int	WriteFileDate FP ((char *, int, SValue *, FILE *));
static void	SendMA FP ((int));
static int	TellDaemon FP ((char *, int, SValue *, int));
static void	UpdateMem FP ((void));

/*---------------------------------------------------------------------*/


main (argc, argv)
int argc;
char **argv;
{
/*
 * Initialize.
 */
	usy_init ();
	msg_connect (Handler, "Archiver");
#ifdef notdef
	ds_Initialize ();
#endif
	InitArchiver(&argc,argv);
/*
 * Window sys initialization.
 */
	MakeWidget (&argc, argv);

#ifdef notdef
	chdir (DATADIR);
	LoadFileList ();
	UpdateMem ();
#endif
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
/*
 * msg_await() may return if a handler returns non-zero.  This 
 * occurrence only means something in the msg_await loop in RunTar,
 * so we just loop forever here and ignore return values
 */
	while (1)
		msg_await ();
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
	 * Now do some error checking and feedback for the user
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
			"device: %s, mount name: %s, dump files to directory: %s",
			DriveName, MountName, OutputDir);
		msg_ELog (EF_INFO, 
			"minimum optical disk to dump: %d kb",MinDisk);
	}
	else
	{
		msg_ELog (EF_INFO, "Tape drive mode");
		msg_ELog (EF_INFO, "device name: %s, tapelimit: %lu",
			DriveName, TapeLimit);
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
   fprintf(stderr,"   %-20s Show this information\n","-h,-help");

   fprintf(stderr,"\nTape mode:\n");
   fprintf(stderr,"   %-20s Tape storage limit, minus safety margin (%lu)\n",
			"-tapelimit <bytes>",DEF_TAPELIMIT);

   fprintf(stderr,"\nOptical disk mode:\n");
   fprintf(stderr,"   %-20s Optical disk output directory (%s)\n",
			"-output <dir>",DEF_OUTPUTDIR);
   fprintf(stderr,"   %-20s Optical disk mount name (%s)\n",
			"-n <name>",DEF_MOUNTNAME);
   fprintf(stderr,"   %-20s Minimum required disk size in kilobytes (%d)\n",
			"-k <kb>", DEF_MINDISK);

   fprintf(stderr,"\nDefault values are shown in parentheses.\n");
   fprintf(stderr,"Standard X Toolkit options are understood,\n");
   fprintf(stderr,"and all options can be abbreviated.\n");
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



static void
MakeWidget (argc, argv)
int *argc;
char **argv;
/*
 * Put together the control widget.  The Top widget shell should already
 * have been set in InitArchiver()
 */
{
	Arg args[5];
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
	WStatus = XtCreateManagedWidget ("status", labelWidgetClass, Form,
			args, n);
/*
 * The line of control buttons.
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

	CreateWaitButtons(WaitTimes, Form, button, above);
	SetWaitSensitivity(False);

/*
 * Status info.
 */
	n = 0;
	above = Action;
	XtSetArg (args[n], XtNlabel, "0 Bytes in 0 files");	n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
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
 */
static void
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
	while (*scan && (sscanf(scan,"%i",&delays[ntimes]) == 1))
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
		      (XtCallbackProc) SuspendWrite, (XtPointer)delays[i]);
		left = WaitButtons[i];
	}
	WaitButtons[ntimes] = NULL;
}



static void
Finish ()
/*
 * Finish things out.
 */
{
	char datafile[120];
	ZebTime	zt;
	int	year,month,day,hour,minute;
	int	status = 0;

	/*
	 * If a write is in progress, we can't finish yet because
	 * we can't take the tape offline yet.
	 */
	if (WriteInProgress)
		return;

	if (DeviceFD < 0)
	{
	    switch ( ArchiveMode )
	    {
	      case AR_TAPE:
		SetStatus (TRUE, "Can't finish -- no tape");
		return;
	      break;
	      case AR_EOD:
		SetStatus (TRUE, "Can't finish -- no optical disk");
		return;
	      break;
	    }
	}
	switch ( ArchiveMode )
	{
	    case AR_TAPE:
		SaveFiles (TRUE);
		SpinOff ();
	    break;
	    case AR_EOD:
		tl_Time (&zt);
		TC_ZtSplit (&zt, &year, &month, &day, &hour, &minute, 0, 0);
		sprintf( datafile, "%s/%02d%02d%02d.%02d%02d.tar",
		    OutputDir,year,month,day,hour,minute );
	        if ((DeviceFD = open (datafile, O_RDWR|O_CREAT,(int)0664)) < 0)
		{
		    SetStatus ( TRUE, "Bad file open on EOD" );
		    msg_ELog (EF_INFO, "Error %d opening %s", errno, datafile);
		}
		else
		{
		    SaveFiles (TRUE);
		    close(DeviceFD);
		}
		status = EjectEOD ();
	    break;
	}
	SetStatus (TRUE, "CROAK");
	Die ();
}





static void
ActionButton ()
/*
 * Take or release the tape.
 */
{
	Arg args[2];
	ZebTime current, zt;
	int year, month, day, hour, min, sec;
	int status;

/*
 * If in the middle of a write, ignore this button
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
	/*
	 * Start saving stuff.  Find the next time by searching from the
	 * beginning of the day until we get a time greater than the current
	 * time.
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



static void
LoadFileList ()
/*
 * Pull in the list of files which have already been dumped to disk.
 */
{
	FILE *fp;
	char pname[200], *strchr (), *colon;
	SValue v;
	date d;
/*
 * Open up the file.
 */
	sprintf (listfile, "%s/DumpedFiles", DATADIR);
	DumpedTable = usy_c_stbl ("DumpedTable");
	if ((fp = fopen (listfile, "r")) == NULL)
	{
		msg_ELog (EF_INFO, "No dumped file list");
		return;
	}
/*
 * Go through and read each platform.
 */
	while (fgets (pname, 200, fp))
	{
	/*
	 * Split apart the entry.
	 */
		if ((colon = strchr (pname, ':')) == 0 || 
		  sscanf (colon + 1, "%d %d", &d.ds_yymmdd, &d.ds_hhmmss) != 2)
		{
			msg_ELog (EF_PROBLEM, "Bad DumpedTable line: %s",
					pname);
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





static int
OpenTapeDevice ()
/*
 * Get the device opened.
 */
{
	if ((DeviceFD = open (DriveName, O_RDWR)) < 0)
	{
		msg_ELog (EF_INFO, "Error %d opening %s", errno, DriveName);
		return (0);
	}
	return (1);
}



static void
TimerSaveFiles (zt)
ZebTime *zt;
/*
 * File saving invoked from a timer event.
 */
{
	int status;
	int year, month, day, hour, minute, second;
	int test, delta;
	ZebTime daystart;
	char datafile[120];
	struct statfs buf;
/*
 * Special check -- if this is the first dump of a new day, we clean up 
 * everything
 */
	TC_ZtSplit (zt, &year, &month, &day, &hour, &minute, &second, 0);
	TC_ZtAssemble (&daystart, year, month, day, 0, 0, 0, 0);
	delta = zt->zt_Sec - daystart.zt_Sec;

	switch ( ArchiveMode )
	{
	    case AR_TAPE:
		if (ZeroZFree)
			test = ((delta < 60 * DumpInterval) && (! FreshTape));
		else
			test = FALSE;

		SaveFiles (test);
		if (test)
		{
		    ActionButton ();
		    SetStatus (TRUE, "Need new day's tape");
		    FreshTape = TRUE;
		}
		else if (BytesWritten > TapeLimit)
		{
		    ActionButton ();
		    SetStatus (TRUE, "Need new tape");
		    FreshTape = TRUE;
		}
		else
		    FreshTape = FALSE;
	    break;
	    case AR_EOD:
		sprintf( datafile, "%s/%02d%02d%02d.%02d%02d.tar",
		    OutputDir,year,month,day,hour,minute );
		if ((DeviceFD = open (datafile, O_RDWR|O_CREAT,(int)0664)) < 0)
		{
		    SetStatus ( TRUE, "Bad file open on EOD" );
		    msg_ELog (EF_INFO, "Error %d opening %s", errno, datafile);
		}
	 	else
		{
		    SaveFiles(FALSE);
		    close ( DeviceFD );
		    status = statfs(DriveName,&buf);
		    if ( !status && buf.f_bavail < MinDisk )
		    {
			ActionButton ();
			SetStatus (TRUE, "Need new optical disk.");
		    }
		}
	    break;
	}
}



static void
SaveFiles (all)
int all;
/*
 * Pass through the list of stuff and save files to the tape.
 */
{
	static int pending = FALSE, pending_all;
	static int working = FALSE;
	int plat;
/*
 * If we have no tape, we do nothing.
 */
	if (DeviceFD < 0)
		return;
/*
 * If a SaveFiles is already in progress, just set the pending flag
 * and return
 */
	if (working)
	{
		pending = TRUE;
		pending_all = pending_all | all;
		return;
	}
	else
	{
		working = TRUE;
		pending = TRUE;
		pending_all = all;
	}

/*
 * As long as we have pending requests, save files
 */
	while (pending)
	{
		pending = FALSE;
	/*
	 * The tar command, less the file names
	 */
		sprintf (Tarbuf, "exec tar cfb - %d ", BFACTOR);
	/*
	 * Pass through the platform table and dump things.
	 */
		SetStatus (FALSE, "Scanning platforms");
		for (plat = 0; plat < SHeader->sm_nPlatform; plat++)
			if (! (PTable[plat].dp_flags & DPF_SUBPLATFORM))
				DumpPlatform (PTable + plat, pending_all);
	/*
	 * Run the tar command to put this all together.
	 */
		if ( strlen(Tarbuf) > 20 )
		{
			SetStatus (FALSE, "Writing");
			WriteInProgress = TRUE;
			SetWaitSensitivity(True);
			XtSetSensitive(FinishButton, False);
			XtSetSensitive(Action, False);
		}

		if (strlen (Tarbuf) > 20 && RunTar (Tarbuf))
		{
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
		/*
		 * If we received a death message while writing,
		 * act on it now
		 */
		if (Dying)
			Die();
	}

	SetStatus (FALSE, "Sleeping");
	working = FALSE;
	WriteInProgress = FALSE;
	SetWaitSensitivity(False);
	XtSetSensitive(FinishButton, True);
	XtSetSensitive(Action, True);
}





static void
DumpPlatform (p, all)
Platform *p;
int all;
/*
 * Dump out any files from this platform.
 */
{
	ZebTime last, dumptime;
	SValue v;
	int type, findex;
/*
 * Find the last time this thing was dumped.
 */
	if (usy_g_symbol (DumpedTable, p->dp_name, &type, &v))
		TC_UIToZt (&(v.us_v_date), &last);
	else
		last.zt_Sec = last.zt_MicroSec = 0;
/*
 * Go through the file chain.  We never dump the most recent file, on the 
 * assumption that it is still being written to (unless we've been told
 * to do them all.
 */
	if ((findex = p->dp_LocalData) == 0 || 
			(! all && (findex = DFTable[findex].df_FLink) == 0) ||
			TC_LessEq (DFTable[findex].df_end, last))
		return;		/* Nothing to dump */
	dumptime = DFTable[findex].df_end;
/*
 * Now go through and do it.
 */
	while (findex && TC_Less (last, DFTable[findex].df_end))
	{
		char *fname = DFTable[findex].df_name;
	/*
	 * Fix up the file name and add it to our big tar command.
	 */
		if (! strncmp (fname, DATADIR, strlen (DATADIR)))
			fname += (strlen (DATADIR) + 1);
		msg_ELog (EF_DEBUG, "Dumping file '%s'", fname);
		strcat (Tarbuf, fname);
		strcat (Tarbuf, " ");
	/*
	 * Send the MarkArchived request now, even though we do not know
	 * that the tar will succeed.  This is to help insure that nothing
	 * is written to the file after archive it.  If the archive fails,
	 * we'll try again later, since we go by our own dates, and not the
	 * archived flag, when picking files to write.
	 */
	 	SendMA (findex);
		findex = DFTable[findex].df_FLink;
	}
/*
 * Record the new time.
 */
	TC_ZtToUI (&dumptime, &(v.us_v_date));
	usy_s_symbol (DumpedTable, p->dp_name, SYMT_DATE, &v);
}







static int
RunTar (cmd)
char *cmd;
/*
 * Run this tar command, and deal with writing its output to tape.
 */
{
	FILE *pfp = popen (cmd, "r");
	static char fbuf[BLOCKSIZE];
	int rstatus, nb, tnb = 0;
	int msg_fd;

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
 * Now read out chunks of stuff.
 */
	while ((nb = netread (pfp->_file, fbuf, BLOCKSIZE)) > 0)
	{
		if (write (DeviceFD, fbuf, nb) < nb) /* oh shit! */
		{
			msg_ELog (EF_EMERGENCY, "Archive device write error %d",
				errno);
			if ( ArchiveMode == AR_EOD ) close(DeviceFD);
			ActionButton (); /* Free drive */
			SetStatus (TRUE, "Device write error!");
			pclose (pfp);
			LoadFileList ();
			return (FALSE);
		}
		tnb += nb;
		/*
		 * Get any X events.  If a suspension is requested, we'll
		 * catch it here in the Suspended flag
		 */
		xevent (0);
		if (Suspended)		 /* we must poll msg and X until
					  * the Suspended flag is clear */
		{
			PollWhileSuspended();
		}
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



/*
 * Poll message fd and X fd and handle any messages as long
 * as Suspended remains true
 */
static void
PollWhileSuspended()
{
	static struct timeval timeout = { 1, 0 };
	fd_set fdset;
	int fd = msg_get_fd();

	while (Suspended)
	{
		/*
		 * Poll message 
		 */
		FD_ZERO(&fdset);
		FD_SET(fd, &fdset);
		if (select(fd+1, &fdset, 0, 0, &timeout) > 0)
		{
			msg_incoming(fd);
		}
		/*
		 * Process any xevents
		 */
		xevent(0);
	}
}



static void
Die ()
/*
 * Ribbit.
 */
{
	exit (0);
}





static void
UpdateList ()
/*
 * Update the list that says when we've dumped things.
 */
{
	FILE *fp;
	Arg args[2];
	char string[80];
/*
 * Open up the file.
 */
	sprintf (listfile, "%s/DumpedFiles", DATADIR);
	if ((fp = fopen (listfile, "w")) == NULL)
	{
		msg_ELog (EF_PROBLEM, "Unable to open %s", listfile);
		return;
	}
/*
 * Scan through our table and write everything.
 */
	usy_traverse (DumpedTable, WriteFileDate, (long) fp, FALSE);
	fclose (fp);
/*
 * Update the widget too.
 */
	sprintf (string, "%.2f MBytes in %d files.",
		(float)BytesWritten/1000000.0, FilesWritten);
	XtSetArg (args[0], XtNlabel, string);
	XtSetValues (Bytes, args, 1);
	Sync ();
}




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
	fprintf (fp, "%s: %d %d\n", sym, v->us_v_date.ds_yymmdd,
			v->us_v_date.ds_hhmmss);
	return (TRUE);
}




int
netread (fd, dest, len)
int fd, len;
char *dest;
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






static void
UpdateMem ()
/*
 * Update the "archived" flags in the shm segment.
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
	int dfi;
	ZebTime ftime;
	PlatformId pid = ds_LookupPlatform (sym);
/*
 * Plow through the file entries, marking everything that we have written 
 * out.
 */
	TC_UIToZt (&(v->us_v_date), &ftime);

	for (dfi = LOCALDATA (PTable[pid]); dfi; dfi = DFTable[dfi].df_FLink)
	{
	/*
	 * If this file is already marked, or hasn't been done, move on.  
	 * Otherwise send the notification.
	 */
	 	if (TC_LessEq (DFTable[dfi].df_end, ftime) &&
				(DFTable[dfi].df_flags & DFF_Archived) == 0)
			SendMA (dfi);
	}
	return (TRUE);
}





static void
SendMA (index)
int index;
/*
 * Send the archive mark.
 */
{
	struct dsp_MarkArchived ma;

	ma.dsp_type = dpt_MarkArchived;
	ma.dsp_FileIndex = index;
	msg_send ("DS_Daemon", MT_DATASTORE, FALSE, &ma, sizeof (ma));
}





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
	 * down to the end time every 10 seconds and show the value
	 * in the status label. The first call will be ASAP
	 */
	if (RemainingEvent != -1)
	{
		tl_Cancel(RemainingEvent);
	}
	RemainingEvent = tl_RelativeReq(TimerRemaining, &ends, 0, 10);

	/*
	 * Now set the Suspended flag.  This will be caught by the
	 * RunTar() loop and reading of the pipe will stop as long
	 * as Suspended remains true.  This in turn blocks the
	 * tar process and suspension of the tar is achieved.
	 */
	Suspended = TRUE;
	sprintf(buf,"Write suspended for %i seconds",waitsecs);
	SetStatus(FALSE, buf);
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
	sprintf(buf,"Write suspended.  Time remaining: %li:%02li", 
		seconds/60, seconds % 60);
	SetStatus(FALSE,buf);
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
	if (Suspended)
		WriteNow();
}


