/*
 * Basically: we wake up every so often, look at all the files in the system,
 * and write them out to tape in tar format.
 */
/*		Copyright (C) 1987-99 by UCAR
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
# include <iostream>
# include <fcntl.h>
# include <errno.h>
# include <sys/types.h>
# include <sys/stat.h>
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
# define STATVFS statvfs
#else
# ifndef AIXV3
#  include <sys/vfs.h>
#  define STATVFS statfs
# else
#  include <sys/statfs.h>
#  define STATVFS statvfs
# endif
#endif

# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>

# include <defs.h>
# include <message.h>
extern "C" {
# include <timer.h>
}
# include <Platforms.h>
# include <DataStore.h>

# include "Database.h"
# include "Archiver.h"

RCSID ("$Id: Archiver.cc,v 1.50 2001-02-01 02:03:57 granger Exp $")

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

/*
 * 1/6/99: Hack it all to hell all over again.  Separate Archiver
 * into model and view interfaces, and move the X11 GUI view implementation
 * into a separate file.  Allow the Archiver to be started in
 * automatic mode and without an X display, so that it can archive in
 * the background.
 */

/*------------------------------------------------------------------------*/

# define DEF_DUMPINTERVAL 120	/* how often, in minutes, to dump	*/
# define DEF_DEVICEFILE "/dev/nrst8"
# define DEF_MOUNTNAME "eod0"
# define DEF_OUTPUTDIR "/eod0"
# define DEF_EXCLUDE ""
# ifdef __STDC__
# define DEF_TAPELIMIT ((unsigned long) 3500000ul)
# define DEF_MINDISK ((unsigned long) 10000ul)
# else
# define DEF_TAPELIMIT ((unsigned long) 3500000)
# define DEF_MINDISK ((unsigned long) 10000)
# endif /* __STDC__ */

enum AR_DeviceModes { AR_TAPE = 1, AR_EOD = 2, AR_JAZ = 4 };
# define AR_DISK (AR_EOD|AR_JAZ)

# define ARCHIVER_GROUP "Archiver"

/*
 * Allow various parts to be disabled to facilitate testing.
 */
//# define NO_MOUNT
//# define NO_RUNTAR

/*------------------------------------------------------------------------
 * GLOBAL VARIABLES and FLAGS
 */

/*
 * Platform exclusion stuff.
 */
# define MAX_EXCLUDES 32
PlatformId ExcludePlats[MAX_EXCLUDES];
int 	N_Excl = 0;

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

static char Tarbuf[65536]; 	/* Where the tar command is built.	*/
unsigned long Tarlen = 0;       /* == strlen(Tarbuf) at all times	*/
unsigned long Tarcmdlen = 0;	/* == length of the command portion of	*/
				/*    Tarbuf (i.e., no filenames)	*/


ArchiverModel *model = 0;	/* Keeper of our state */

/*------------------------------------------------------------------------*/

/* Archiver message protocol for starting external views */

# define MT_ARCHIVER 70

struct msg_archiver
{
	int ma_command;
	char ma_data[64];
};


enum msg_archiver_commands
{
	MA_NONE = 0,		// no command
	MA_OPENDISPLAY,		// open a view on display string in data
	MA_OPENDEFAULT,		// ignore display in data and open default
};


/*---------------------------------------------------------------------
 * Global options which can be set by the user from the command line or
 * the resources database
 */

	unsigned long TapeLimit;/* Tape size (kbytes), less a safety margin */
				/* Default is high density Exabyte */
	String	DriveName;	/* TAPE mode: tape drive device */
	String	OutputDir;	/* EOD mode: output directory */
	String	MountName;	/* EOD mode: opt disk mount point */
	int	DumpInterval;	/* Minutes between dumps */
	int	StartMinute;	/* Hour offset at which dumps start */
	unsigned int MinDisk;	/* EOD mode: minimum disk size */
	String  ModeString;	/* cmd-line option "eod" or "tape" */
	int	ArchiveMode;	/* AR_TAPE or AR_EOD mode */
	Boolean	ZeroZFree;	/* Free tape at 0z */
	String	WaitTimes;	/* Delay button times, "n1,n2,n3,..." */
	String  Database;	/* Name of the database file */
	int	BFactor;	/* Blocking factor to calculate blk size */
	String  ExclPlatNames;	/* Excluded platforms	*/
	Boolean	AutomaticOpen;	/* Take device right away. */
	Boolean Hide;		/* No GUI */
	String	Reveal;		/* Tell an existing Archiver to reveal itself*/


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
      0, XtRString, (XtPointer)DEF_DEVICEFILE },
   { "outputDir", "OutputDir", XtRString, sizeof(String),
      0, XtRString, (XtPointer)DEF_OUTPUTDIR },
   { "mountName", "MountName", XtRString, sizeof(String),
      0, XtRString, (XtPointer)DEF_MOUNTNAME },
   { "dumpInterval", "DumpInterval", XtRInt, sizeof(int),
      0, XtRImmediate, (XtPointer)DEF_DUMPINTERVAL },
   { "startMinute", "StartMinute", XtRInt, sizeof(int),
      0, XtRImmediate, (XtPointer)0 },
   { "minDisk", "MinDisk", XtRInt, sizeof(int),
      0, XtRImmediate, (XtPointer)DEF_MINDISK },
   { "mode", "Mode", XtRString, sizeof(String),
      0, XtRImmediate, (XtPointer)"tape" },
   { "zeroZFree", "ZeroZFree", XtRBoolean, sizeof(Boolean),
      0, XtRImmediate, (XtPointer)True },
   { "waitTimes", "WaitTimes", XtRString, sizeof(String),
      0, XtRString, (XtPointer)"1,2,5" },
   { "database", "Database", XtRString, sizeof(String),
      0, XtRString, (XtPointer)DUMPED_FILES },
   { "blockFactor", "BlockFactor", XtRInt, sizeof(int),
      0, XtRImmediate, (XtPointer)DEF_BFACTOR },
   { "exclude", "Exclude", XtRString, sizeof (String),
      0, XtRString, (XtPointer)DEF_EXCLUDE },
   { "automatic", "Automatic", XtRBoolean, sizeof(Boolean),
      0, XtRImmediate, (XtPointer)False },
   { "hide", "Hide", XtRBoolean, sizeof(Boolean),
      0, XtRImmediate, (XtPointer)False },
   { "reveal", "Reveal", XtRString, sizeof(String),
      0, XtRString, (XtPointer)"" }
};

static XtPointer OptionBase[] = {
   (XtPointer) &TapeLimit, (XtPointer) &DriveName, (XtPointer) &OutputDir, 
   (XtPointer) &MountName, (XtPointer) &DumpInterval, (XtPointer) &StartMinute,
   (XtPointer) &MinDisk, (XtPointer) &ModeString, (XtPointer) &ZeroZFree, 
   (XtPointer) &WaitTimes, (XtPointer) &Database, (XtPointer) &BFactor,
   (XtPointer) &ExclPlatNames, (XtPointer) &AutomaticOpen,
   (XtPointer) &Hide, (XtPointer) &Reveal,
   
};

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
   {"-b",	".blockFactor",	XrmoptionSepArg,	NULL},
   {"-exclude",	".exclude",	XrmoptionSepArg,	NULL},
   {"-automatic",".automatic",	XrmoptionNoArg,		(caddr_t) "yes" },
   {"-hide",	".hide",	XrmoptionNoArg,		(caddr_t) "yes" },
   {"-reveal",	".reveal",	XrmoptionSepArg,	NULL}
};

/*---------------------------------------------------------------------*/

/*-- initialization --*/
static void	InitArchiver (int *, char**);
static void	Usage (char *prog, int argc, char *argv[]);
static void	Die  (void);

/*-- socket i/o handlers --*/
static int	Handler  (Message *);

/*-- View routines --*/
static void	SetStatus  (int, char *);
static void	OpenView (char *display);
static void	SendReveal (char *display);

/*-- low-level device i/o --*/
static int	OpenTapeDevice  (void);
static void	WriteEOF  (void);
static void	SpinOff  (void);
static int	EjectEOD  (void);
static int	MountEOD  (void);
static int	EjectJaz  (void);
static int	MountJaz  (void);
static int	netread  (int fd, char *dest, int len);

/*-- Timer event handlers --*/
extern "C" void	TimerSaveFiles  (ZebTime *zt);
extern "C" void	TimerResumeWrite (ZebTime *zt);
extern "C" void TimerRemaining (ZebTime *zt, void *cdata);

/*-- high-level write and datastore interaction */
static void	RequestWrite (int finish);
static void	DoTheWriteThing (int finish);
static void	SaveFiles  (int);
static DataFile* GetFileList (int srcid, const Platform *p, 
			      const ZebTime *since, int *count);
static int	DumpPlatform (const Platform *p, int all);
static int	RunTar  (char *);
static void	TarFailed (void);
static void	FinishFinishing (int error);
static void	SetupExcludes ();
static int	Excluded (PlatformId pid);

/*---------------------------------------------------------------------*/


int
main (int argc, char **argv)
{
	int i;
/*
 * Before we do anything, see if the user just wants some
 * help by checking for the -h option and forgetting any others
 */
	for (i = 1; i < argc; ++i)
	{
		int len = strlen(argv[i]);
		if (len > 1 && strncmp(argv[i],"-help",len) == 0)
		{
			Usage(argv[0],1,argv);
			exit(0);
		}
	}
/*
 * Initialize.  Our model object will be destroyed when we exit
 */
	ArchiverModel our_model(Options, XtNumber(Options), argc, argv);
	model = &our_model;
	char name[64];
	sprintf (name, "Archiver-%li", (long int)getpid());
	if (!msg_connect (Handler, name))
	{
		fprintf(stderr,"Archiver: could not connect to message\n");
		Die();
	}

	if (!ds_Initialize ())
	{
		fprintf(stderr,"Archiver: DataStore initialization failed\n");
		Die();
	}

	InitArchiver (&argc,argv);
	// Join group after init in case we just send a reveal and exit
	msg_join (ARCHIVER_GROUP);
	switch ( ArchiveMode )
	{
	    case AR_TAPE:
		model->setActionStatus ("Take tape");
		break;
	    case AR_EOD:
		model->setActionStatus ("Mount optical disk");
		break;
	    case AR_JAZ:
		model->setActionStatus ("Mount Jaz disk");
		break;
	}
	model->enableAction ();
	model->disableWrite ();
	model->disableFinish ();
	model->disableWait ();
	model->setWriteStatus ("Write Now");
	model->setBytes ("0 Bytes in 0 files             ");
	SetStatus (FALSE, "Opening database");

	chdir ( GetProjDir() );
	if (db_Open (Database) != 0)
	{
		msg_ELog (EF_PROBLEM, "Cannot open database %s, exiting",
			  Database);
		exit (-9);
	}
	db_Close ();		/* Leave it closed except while needed */
/*
 * If not hidden, open our view.
 */
	if (! Hide)
	{
		OpenView (0);
	}
/*
 * Try to open archive device immediately if requested.
 */
	if (AutomaticOpen)
	{
		msg_ELog (EF_INFO,
			  "Attempting to automatically open archive device");
		model->setStatus (FALSE, "Attempting automatic open");
		ActionButton ();
	}
	else
	{
		switch (ArchiveMode)
		{
		case AR_TAPE:
			SetStatus (TRUE, "Awaiting tape");
			break;
		case AR_EOD:
			SetStatus (TRUE, "Awaiting optical disk");
			break;
		case AR_JAZ:
			SetStatus (TRUE, "Awaiting Jaz disk");
			break;
		}
	}
/*
 * Go into our dump loop, disabling a view if it throws any exceptions.
 */
	while (1)
	{
		try {
			msg_await ();
			break;
		}
		catch (ArchiverView::ErrorException &e)
		{
			msg_ELog (EF_DEBUG, "caught view error");
			model->error (e);
		/*
		 * If we aren't background-enabled, then a
		 * window closure forces a full exit.
		 */
			if (! Hide)
			    break;
		}
	}
	db_Close ();
	return (-9);
}



static void
InitArchiver (int *argc, char **argv)
{
	unsigned int i;

	/*
	 * Just initialize X as much as we can short of connecting
	 * to a display, since we don't know yet if we have one.
	 */
	XtToolkitInitialize();
	XrmInitialize();
	XrmDatabase cldb = 0;
	XrmParseCommand (&cldb, Options, XtNumber(Options), "Archiver",
			 argc, argv);
#ifdef notdef
	if (! cldb)
		msg_ELog (EF_PROBLEM, "null database after XrmParseCommand");
	XrmPutFileDatabase (cldb, "options.rdb");
	/*
	 * Initialize X and the Toolkit, get our toplevel shell and AppContext,
	 * and get our application options from the resource database
	 */
	Top = XtAppInitialize (&Appc, "Archiver", Options, XtNumber(Options),
			       argc, argv, NULL, NULL, 0);

	/*
	 * If any args are left, there was an error on the command line
	 */
	if (*argc > 1)
	{
		msg_ELog(EF_PROBLEM,"Illegal command-line syntax");
		Usage(argv[0], *argc, argv);
		Die();
	}
#endif
	/* We can no longer detect bad command lines because any unknown
	 * options may be meant for the toolkit when we open a view, and
	 * I cannot think of any easy way to detect otherwise...
	 */

	/*
	 * Now retrieve each of our global options from the rm database
	 */
	XtResource *res = AppResources;
	XrmValue value;
	XrmQuark qRBoolean = XrmStringToQuark (XtRBoolean);
	XrmQuark qRInt = XrmStringToQuark (XtRInt);
	XrmQuark qRString = XrmStringToQuark (XtRString);
	XrmQuark qRImmediate = XrmStringToQuark (XtRImmediate);
	for (i = 0; i < XtNumber(AppResources); ++i)
	{
		String resource_type;
		char fullname[256];
		char fullclass[256];
		sprintf (fullname, "%s.%s", "Archiver", res->resource_name);
		sprintf (fullclass, "%s.%s", "Archiver", res->resource_class);
		Boolean found = XrmGetResource (cldb, fullname, fullclass,
						&resource_type, &value);
		XrmQuark qrval;
		if (!found)
		{
			/* Use the default */
			value.addr = (char *)res->default_addr;
			value.size = res->resource_size;
			qrval = XrmStringToQuark (res->default_type);
		}
		else
		{
			qrval = XrmStringToQuark (resource_type);
		}
		XrmQuark qrdest = XrmStringToQuark (res->resource_type);
		char *dest = (char *)OptionBase[i] + res->resource_offset;
		if (qrval == qRImmediate)
			memcpy (dest, &(value.addr), sizeof(value.addr));
		else if (qrval != qRString)
			fprintf (stderr, "Unexpected rep for source: %s",
				 XrmQuarkToString(qrval));
		else if (qrdest == qRString)
			*(String *)dest = (String)value.addr;
		else if (qrdest == qRInt)
			*(int *)dest = atoi((String)value.addr);
		else if (qrdest == qRBoolean)
		{
			char *v = (String)value.addr;
			Bool result;
			if (! strcmp (v, "false"))
				result = False;
			else if (! strcmp (v, "true"))
				result = True;
			else if (! strcmp (v, "no"))
				result = False;
			else if (! strcmp (v, "yes"))
				result = True;
			else if (! strcmp (v, "FALSE"))
				result = False;
			else if (! strcmp (v, "TRUE"))
				result = True;
			else
				result = (atoi(v) != 0);
			*(Boolean *)dest = result;
		}
		else
			msg_ELog (EF_PROBLEM, "Unexpected rep for dest: %s",
				  XrmQuarkToString (qrdest));
		++res;
#ifdef notdef
	   XtGetApplicationResources(Top, OptionBase[i], 
			&AppResources[i], 1, NULL, 0);
#endif
	}
	/*
	 * Now do some error checking and feedback to the user
	 */
	if (Reveal[0] != '\0')
	{
		msg_ELog (EF_INFO, 
			  "reveal requested: all other options ignored");
		SendReveal (Reveal);
		msg_disconnect ();
		Die ();
	}
	if (StartMinute > 60)
	{
		msg_ELog (EF_PROBLEM, "Bad start minute %d, using 0", 
			  StartMinute);
		StartMinute = 0;
	}
	if ( strcmp ( ModeString, "tape") == 0 )
	{
		msg_ELog (EF_INFO, "Tape drive mode");
		ArchiveMode = AR_TAPE;
	}
	else if ( strcmp ( ModeString, "eod") == 0 )
	{
		msg_ELog (EF_INFO, "Optical disk mode");
		ArchiveMode = AR_EOD;
	}
	else if ( strcmp ( ModeString, "jaz") == 0 )
	{
		msg_ELog (EF_INFO, "Jaz disk mode");
		ArchiveMode = AR_JAZ;
	}
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
	if ( ArchiveMode & AR_DISK )
	{
		msg_ELog (EF_INFO, 
			"%s: %s, mount name: %s, dump files to directory: %s",
			"device", DriveName, MountName, OutputDir);
		msg_ELog (EF_INFO, 
			"minimum disk to dump: %d kb", MinDisk);
	}
	else
	{
		msg_ELog (EF_INFO, "tape device: %s, bf:%d, blocksize:%d, "
			  "tapelimit: %lu", DriveName, BFactor,
			  (BFactor*512), TapeLimit);
	}
	if (ExclPlatNames[0])
	{
		msg_ELog (EF_INFO, "Excluding %s", ExclPlatNames);
		SetupExcludes ();
	}
	if (Hide)
	{
		msg_ELog (EF_INFO,
			  "Hiding in background without window interface");
	}
	if (Hide && !AutomaticOpen)
	{
		msg_ELog (EF_PROBLEM, "Warning: archiver hidden but %s",
			  "automatic open not enabled");
	}
}



static void
OpenView (char *display)
{
	const char *dstring = display ? display : "<default>";
	msg_ELog (EF_INFO, "Opening X11 view on display: %s", dstring);
	ArchiverView *view = CreateXView (model, display);
	if (! view)
	{
		msg_ELog (EF_PROBLEM, "X11 window interface on %s failed",
			  dstring);
	}
	else
	{
		model->addView (*view);
	}
}



static void
SendReveal (char *display)
/*
 * Send an open display message to the Archiver group, so that any active
 * archiver will try to reveal itself.
 */
{
	struct msg_archiver ma;

	msg_ELog (EF_INFO, "sending reveal message to Archiver group "
		  "for display %s", display ? display : "<default>");
	if (display)
	{
		ma.ma_command = MA_OPENDISPLAY;
		strcpy (ma.ma_data, display);
	}
	else
	{
		ma.ma_command = MA_OPENDEFAULT;
	}
	msg_send (ARCHIVER_GROUP, MT_ARCHIVER, TRUE, &ma, sizeof(ma));
}



static void
SetupExcludes ()
/*
 * Deal with excluded platforms.
 */
{
	char *ourexcl = strdup (ExclPlatNames), *pnames[MAX_EXCLUDES];
	int npname = CommaParse (ourexcl, pnames), i;

	for (i = 0; i < npname; i++)
	{
		PlatformId pid = ds_LookupPlatform (pnames[i]);
		if (pid == BadPlatform)
		{
			msg_ELog (EF_PROBLEM, "Bad exclude platform: %s",
					pnames[i]);
			continue;
		}
		ExcludePlats[N_Excl++] = pid;
	}
	free (ourexcl);
}




static int
Excluded (PlatformId pid)
/*
 * Return TRUE iff this platform should not be archived.
 */
{
	int i;
	for (i = 0; i < N_Excl; i++)
		if (pid == ExcludePlats[i])
			return (TRUE);
	return (FALSE);
}





/*
 * Explain all of the command line options of Archiver
 */
static void
Usage(char *prog, int argc, char **argv)
{
   unsigned short i;

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
			"-mode eod|tape|jaz");
   fprintf(stderr,"   %-20s Device name (%s)\n",
			"-device,-v,-f <dev>",DEF_DEVICEFILE);
   fprintf(stderr,"   %-20s Starting hour offset, 0-60 minutes. (0)\n",
			"-start <min>");
   fprintf(stderr,"   %-20s Dump interval in minutes (%d)\n",
			"-time,-interval <min>",DEF_DUMPINTERVAL);
   fprintf(stderr,"   %-20s Free device at 0z (yes)\n","-z yes|no");
   fprintf(stderr,"   %-20s Delay button times (1,2,5)\n","-wait n1,n2,...");
   fprintf(stderr,"   %-20s Open archive device automatically on startup %s\n",
	   "-automatic", "(off)");
   fprintf(stderr,"   %-20s Run hidden without a window interface (off)\n",
	   "-hide");
   fprintf(stderr,"   %-20s %s (off)\n",
	   "-reveal <display>", 
	   "Tell other Archivers to open views on <display>");
   fprintf(stderr,"   %-20s Specify database file, default '%s'\n",
	   "-database", DUMPED_FILES);
   fprintf(stderr,"   %-20s Show this information\n","-h,-help");

   fprintf(stderr,"\nTape mode:\n");
   fprintf(stderr,"   %-20s Tape storage limit, minus safety margin (%lu)\n",
			"-tapelimit <kbytes>",DEF_TAPELIMIT);

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
	      case AR_JAZ:
		SetStatus (TRUE, "Can't finish -- no Jaz disk");
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
 * Things to do when finishing off, such as releasing devices and dying.
 * error is non-zero if we're finishing due to an error.
 */
static void
FinishFinishing (int error)
{
	msg_ELog(EF_DEBUG, "finishing a finish");
	model->disableFinish ();
	model->disableWrite ();
	switch ( ArchiveMode )
	{
	    case AR_TAPE:
		SpinOff();
		break;
	    case AR_EOD:
		if (DeviceFD >= 0)
			close(DeviceFD);
		EjectEOD ();
		break;
	    case AR_JAZ:
		if (DeviceFD >= 0)
			close(DeviceFD);
		EjectJaz ();
		break;
	}
	if (error)
		SetStatus (TRUE, "CROAK");
	else
		SetStatus (FALSE, "Finished");
	Die ();
}


static int TimerEvent;		/* The regular write event */


static void
SetupTimer ()
{
    ZebTime current, zt;
    int year, month, day;

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
						
    TimerEvent = tl_AbsoluteReq ((void (*)(...))TimerSaveFiles,
				 0, &zt, DumpInterval*60*INCFRAC);
}


static void
CancelTimer ()
{
    tl_Cancel (TimerEvent);
}

    


void
ActionButton ()
/*
 * Take or release the tape.
 */
{
	int status;
	char *action;
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
		SetStatus (FALSE, "Opening archive device...");
		switch ( ArchiveMode )
		{
			case AR_TAPE:
		        /*
		         * Get the tape.
		         */
			    if (! OpenTapeDevice ())
			    {
				SetStatus (TRUE, "Unable to open tape!");
				return;
			    }
			    action = "Free tape";
			    break;
			case AR_EOD:
			    if (MountEOD() != 0)
			    {
				    SetStatus (TRUE, "Mount failed!");
				    return;
			    }
			    DeviceFD = 0;
			    action = "Free optical disk";
			    break;
			case AR_JAZ:
			    if (MountJaz() != 0)
			    {
				    SetStatus (TRUE, "Mount failed!");
				    return;
			    }
			    DeviceFD = 0;
			    action = "Free Jaz disk";
			    break;
		}
		SetStatus (FALSE, "Sleeping");
		model->enableFinish ();
		model->enableWrite ();
	
		SetupTimer ();
	}
/*
 * Otherwise we give it away.
 */
	else
	{
	    CancelTimer ();
	    switch ( ArchiveMode )
	    {
		case AR_TAPE:
		    action = "Take tape";
		    model->disableFinish ();
		    model->disableWrite ();
		    SpinOff ();
		    close (DeviceFD);
		    DeviceFD = -1;
		    SetStatus (TRUE, "Awaiting tape");
		    break;
		case AR_EOD:
		    status = EjectEOD();
		    if ( !status )
		    {
			action = "Mount optical disk";
		        DeviceFD = -1;
		        SetStatus (TRUE, "Awaiting optical disk");
			model->disableFinish ();
			model->disableWrite ();
		    }
		    else
		    {
		        action = "Retry eject";
		        SetStatus (TRUE, "Optical disk eject error!");
		    }
		    break;
		case AR_JAZ:
		    status = EjectJaz();
		    if ( !status )
		    {
			action = "Mount Jaz disk";
		        DeviceFD = -1;
		        SetStatus (TRUE, "Awaiting Jaz disk");
			model->disableFinish ();
			model->disableWrite ();
		    }
		    else
		    {
		        action = "Retry eject";
		        SetStatus (TRUE, "Jaz disk eject error!");
		    }
		    break;
	    }
	}
	model->setActionStatus (action);
}



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
RequestWrite (int finish)
{
	static int pending = FALSE;  /* A write request is pending */
	static int pending_finish;   /* True if a pending request has made
				      * an explicit request for all files */
	static int working = FALSE;

	/*
	 * If we have no tape, we do nothing.
	 */
	if (DeviceFD < 0)
	{
		SetStatus (TRUE, "Write request failed: no archive device");
		return;
	}

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
	if (DeviceFD >= 0)
		SetStatus (FALSE, "Sleeping");  /* Otherwise leave msg */
	WriteInProgress = FALSE;
	model->disableWait ();
	model->enableFinish ();
	model->enableAction ();
}


// Open the DeviceFd to a disk file in OutputDir, for disk archiving
// methods.
//
static int
OpenDiskFile ()
{
    ZebTime zt;
    int year, month, day, hour, minute;
    char datafile[256];

    tl_Time(&zt);
    TC_ZtSplit (&zt, &year, &month, &day, &hour, &minute, 0, 0);
    sprintf( datafile, "%s/%d%02d%02d.%02d%02d.tar",
	     OutputDir,year,month,day,hour,minute );
    /* Ignore any errors, we just want to make sure it exists. */
    mkdir (OutputDir, 0775);

    if ((DeviceFD = open (datafile, O_RDWR|O_CREAT,(int)0664)) < 0)
    {
	SetStatus ( TRUE, "Bad file open on disk" );
	msg_ELog (EF_INFO, "Error %d opening %s", errno, datafile);
    }
    return DeviceFD;
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
DoTheWriteThing (int explicit_finish)
{
	static int FreshTape = TRUE;	/* True if the current tape has
					 * never been written to */
	int status;
	int year, month, day, hour, minute, second;
	int all;		/* Whether we want to write all files */
	int delta;
	ZebTime daystart;
	ZebTime zt;	/* The current time, the time this write begins */
	struct STATVFS buf;
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
	   case AR_JAZ:
		/*
		 * For optical disks, the ZeroZFree flag is ignored (?) 
		 * Just write until the disk is full
		 */
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
				BytesWritten/1000,
				TapeLimit/1000);
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
	   case AR_JAZ:
		/*
		 * Finished writing another file onto optical disk
		 * Make sure there is room for more, else request
		 * a new disk
		 */
		status = STATVFS(DriveName,&buf);
		if ( !status && buf.f_bavail < MinDisk )
		{
			msg_ELog(EF_DEBUG,
			   "Disk full. %li kb left less than %li",
			   buf.f_bavail, MinDisk);
			ActionButton ();
			SetStatus (TRUE, "Need new disk.");
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
SaveFiles (int all)
/*
 * Pass through the list of stuff and save files to the tape.
 */
{
	int pid, nplat = ds_GetNPlat ();
	const Platform *p;

	/*
	 * The tar command, less the file names
	 */
	sprintf (Tarbuf, "exec tar cfb - %d ", BFactor);
	Tarcmdlen = strlen (Tarbuf);
	Tarlen = Tarcmdlen;

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

	/*
	 * Pass through the platform table and dump things.  Keep adding
	 * files until DumpPlatform indicates it's time to stop.
	 *
	 * There is a big of bogosity here in that this loop expects that
	 * PlatformId's are sequential integers starting at zero.  Of
	 * course, PlatformId's are currently sequential integers starting
	 * at zero, but that could change.
	 */
	SetStatus (FALSE, "Scanning platforms");
	for (pid = 0; pid < nplat; pid++)
	{
	    if (Excluded (pid))
		continue;

	    p = dt_FindPlatform (pid);
	    if (! pi_Subplatform (p) && ! DumpPlatform (p, all))
		break;
	}
	db_Close ();	/* Leave closed during the long delay in writing */

	/*
	 * Run the tar command to put this all together, but only if
	 * we actually got any files to write
	 */
	if ( Tarlen > Tarcmdlen )
	{
		/* 
		 * Do all the settings to signal a write in progress 
		 */
		SetStatus (FALSE, "Writing");
		WriteInProgress = TRUE;
		model->enableWait ();
		model->disableFinish ();
		model->disableAction ();

		/*
		 * Now try running the tar command
		 */
# ifndef NO_RUNTAR
		if (RunTar (Tarbuf))
#endif
		{
		    char string[80];
		/*
		 * The tar succeeded, so put an EOF marker
		 * on the tape
		 */
		    switch ( ArchiveMode )
		    {
		    case AR_TAPE:
			WriteEOF ();
			break;
		    case AR_EOD:
			break;
		    case AR_JAZ:
			break;
		    }
		/*
		 * Update the widget too.
		 */
		    sprintf (string, "%.3f GBytes in %d files.",
			     (float)BytesWritten / (1024 * 1024), 
			     FilesWritten);
		    model->setBytes (string);
		}
		else
		    TarFailed ();

		WriteInProgress = FALSE;
	}	

	/*
	 * Another write may be pending so we'll just return.
	 * If all writing is finished, it is up to the calling function to
	 * set all the necessary flags
	 */
}


  
static DataFile*
GetFileList (int srcid, const Platform *p, const ZebTime *since, int *count)
/*
 * Return an array of data files in time-sequential order, starting after the
 * given time.
 *
 * The array must be freed by the caller.
 */
{
    int nfiles = 0, listsize = 0;
    DataFile *dflist = 0;
    const DataFile *df;
/*
 * Fill the array
 */
    for (df = ds_FindDFAfter (srcid, pi_Id (p), since); df; 
	 df = ds_NextFile (df))
    {
	if (nfiles == listsize)
	{
	    listsize += 100;
	    dflist = (DataFile*) realloc (dflist, 
					  listsize * sizeof (DataFile));
	}

	dflist[nfiles++] = *df;
    }

    *count = nfiles;
    return (dflist);
}
  
  
  
  
static int
DumpPlatform (const Platform *p, int all)
/*
 * Dump out any files from this platform by appending each file name onto
 * the Tarbuf command buffer
 */
{
    ZebTime dumptime;
    ZebraTime now;
    DataFile *dflist;
    DataFileCore dfc;
    SourceInfo dsi;
    int ndumped;
    int nfiles, s;
    int ok;
 
    tl_Time (&now);
/*
 * Archive all of the writable sources.  The (somewhat arbitrary) assumption
 * here is that read-only sources are probably archived by someone else.
 */
    for (s = 0; ds_GetSourceInfo (s, &dsi); s++)
    {
	int i;
	
	if (dsi.src_ReadOnly)
	    continue;
    /*
     * Get a list of all the files for this source/platform combo
     */
	dflist = GetFileList (s, p, &ZT_ALPHA, &nfiles);

	msg_ELog (EF_DEBUG, "src %s, platform %s: %d files on disk",
		  dsi.src_Name, pi_Name (p), nfiles);
    /*
     * Now step through the file list, skipping the last file in the list 
     * (i.e., the latest) if we're not doing all of the files.
     */
	ok = 1;
	ndumped = 0;
  
	for (i = 0; i < (all ? nfiles : nfiles - 1); i++)
	{
	    const DataFile *df = dflist + i;
	    const char *fname = df->df_fullname;
	    char buf[1 + sizeof (df->df_fullname)];
  	/*
	 * Skip this file if already in the database and the rev number
	 * is the same.
  	 */
	    if (db_Fetch (fname, &dfc) && dfc.dfc_rev == df->df_core.dfc_rev)
		continue;
  	/*
  	 * If this file name will not fit in the tar command line, skip it
  	 * and the rest of the files in this platform.
  	 */
	    sprintf (buf, " %s", fname);
  
	    if (Tarlen + strlen(buf) >= sizeof(Tarbuf))
	    {
		msg_ELog (EF_DEBUG, "tar command line full");
		ok = 0;
		break;
	    }
  
	    msg_ELog (EF_DEBUG, "Dumping file '%s'", fname);
	    strcat (Tarbuf, buf);
	    Tarlen += strlen(buf);
  	/*
	 * Add this file to the database of dumped files.  We may remove
	 * it later if the tar fails.
  	 */
	    db_Insert (fname, &df->df_core);
# ifdef notdef /* no more MarkArchived */
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
# endif
	/*
	 * Advance the dumped time for this platform
	 */
	    ++ndumped;
	    dumptime = df->df_core.dfc_end;
	}

	if (dflist)
	    free (dflist);
    /*
     * Record the new time.
     */
	if (ndumped > 0)
	    msg_ELog (EF_INFO, "src %s, plat %s: dumping %d files up to %s", 
		      dsi.src_Name, pi_Name (p), ndumped, 
		      TC_AscTime (&dumptime, TC_Full));
	else
	    msg_ELog (EF_INFO, "src %s, plat %s: no new files to be dumped", 
		      dsi.src_Name, pi_Name (p));

	if (! ok)
	    break;
    }

    return (ok);
}





static int
RunTar (char *cmd)
/*
 * Run this tar command, and deal with writing its output to tape.
 * To help reduce system load, at the cost of some response time,
 * the X and msg fd's are only polled for every other block read
 */
{
	FILE *pfp = (FILE *) popen (cmd, "r");
	static char *fbuf = NULL;
	int rstatus;
	long nb;
	unsigned long tnb = 0;
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
 * Open a disk file if necessary.
 */
	if (ArchiveMode & AR_DISK)
	{
	    if (OpenDiskFile () < 0)
	    {
		pclose (pfp);
		return FALSE;
	    }
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
		/*
		 * This assumes the write failed if it doesn't write all the
		 * bytes at once.  It hasn't bitten us yet, but note that 
		 * errno will be zero unless write actually returns < 0.
		 */
	    	if (write (DeviceFD, fbuf, nb) < nb)
		{
			msg_ELog (EF_EMERGENCY,"Archive device write error %d",
				  errno);
			if ( ArchiveMode & AR_DISK ) close(DeviceFD);
			ActionButton (); /* Free drive */
			SetStatus (TRUE, "Device write error!");
			pclose (pfp);
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
 * Close the disk file when finished.
 */
	if (ArchiveMode & AR_DISK)
	{
	    close ( DeviceFD );
	}
/*
 * If tar returned OK, so do we.
 */
	msg_ELog (EF_DEBUG, "Transferred %d bytes, last %d", tnb, nb);
	FilesWritten++;
	BytesWritten += tnb/1000;
	msg_ELog (EF_INFO, "Wrote %d, limit %d", BytesWritten, TapeLimit);
	if ((rstatus = pclose (pfp)) == 0)
		return (TRUE);
	msg_ELog (EF_PROBLEM, "Tar returned status %d", rstatus);
	SetStatus (TRUE, "Tar returned failure");
	return (FALSE);
}



static int
MountEOD()
{
    char cmd[80];
    int	status;
    sprintf ( cmd, "eodmount %s /%s", DriveName, MountName );
    status = system(cmd);
    status = WEXITSTATUS(status);
    if (status)
	    msg_ELog (EF_PROBLEM, "%s: failed with %d", cmd, status);
    return status;
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
	status = WEXITSTATUS(status);
	if (status)
		msg_ELog (EF_PROBLEM, "%s: failed with %d", cmd, status);
    }
    sprintf ( cmd, "eodutil %s eject", MountName);
    status = system(cmd);
    status = WEXITSTATUS(status);
    if (status)
	    msg_ELog (EF_PROBLEM, "%s: failed with %d", cmd, status);
    return status;
}



static int
MountJaz()
{
    int	status = 0;
#ifndef NO_MOUNT
    char cmd[256];
    sprintf ( cmd, "/iss/bin/jaz umount" );
    msg_ELog (EF_INFO, cmd);
    status = system(cmd);
    sprintf ( cmd, "/iss/bin/jaz mount" );
    msg_ELog (EF_INFO, cmd);
    status = system(cmd);
    status = WEXITSTATUS(status);
    if ( status )
	    msg_ELog (EF_PROBLEM, "%s: failed with %d", cmd, status);
    //
    // If we can still access our archive directory, then we assume the
    // disk is already mounted and chalk up our inability to mount it to
    // busy-ness.  This also flags right away what would be a future 
    // failure: the inability to create the output directory.
    //
    status = mkdir (OutputDir, 0775);
    if (status < 0 && errno != EEXIST)
    {
	msg_ELog (EF_PROBLEM, "failed to create outputdir (errno %d): %s",
		  errno, OutputDir);
    }
    else
    {
	msg_ELog (EF_INFO, "output dir exists, proceeding normally (%s)",
		  OutputDir);
	status = 0;
    }
#endif
    return status;
}



static int
EjectJaz()
{
    int	 status = 0;
#ifndef NO_MOUNT
    char cmd[256];
    if ( DeviceFD >= 0 )
    {
	sprintf ( cmd, "/iss/bin/jaz umount" );
	msg_ELog (EF_INFO, cmd);
	status = system(cmd);
	status = WEXITSTATUS(status);
	if (status)
		msg_ELog (EF_PROBLEM, "%s: failed with %d", cmd, status);
    }
    sprintf ( cmd, "/iss/bin/jaz eject" );
    msg_ELog (EF_INFO, cmd);
    status = system(cmd);
    status = WEXITSTATUS(status);
    if (status)
	    msg_ELog (EF_PROBLEM, "%s: failed with %d", cmd, status);
#endif
    return status;
}



static void
ArchiverMessage (struct msg_archiver *ma)
{
	switch (ma->ma_command)
	{
	case MA_NONE:
		msg_ELog (EF_DEBUG, "null command received");
		break;
	case MA_OPENDISPLAY:
		OpenView (ma->ma_data);
		break;
	default:
		msg_ELog (EF_PROBLEM, "Unknown archiver command %d",
			  ma->ma_command);
		break;
	}
}



static int
Handler (Message *msg)
/*
 * Deal with an incoming message.
 */
{
	struct mh_template *tmpl;
	struct msg_archiver *ma = (struct msg_archiver *)msg->m_data;

	switch (msg->m_proto)
	{
	   case MT_ARCHIVER:
		ArchiverMessage (ma);
		break;

	   case MT_MESSAGE:
	   	tmpl = (struct mh_template *) msg->m_data;
		if (tmpl->mh_type == MH_DIE || tmpl->mh_type == MH_SHUTDOWN)
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
	model->quit ();
	exit (0);
}



static void
TarFailed ()
/*
 * Since our tar command failed, remove the files on the tar command line
 * from the database.
 */
{
    char fname[CFG_FILEPATH_LEN];
    const char *pos;

    if (db_Open (Database) != 0)
    {
	char buf[256];
	sprintf (buf, "TarFailed: Cannot open database '%s'", Database);
	msg_ELog (EF_PROBLEM, "%s", buf);
	SetStatus (TRUE, buf);
	return;
    }
/*
 * Remove the files in the failed tar command from the database.
 */
    for (pos = Tarbuf + Tarcmdlen + 1; pos < Tarbuf + Tarlen; 
	 pos += strlen (fname) + 1)
    {
    /*
     * Read the next file name from the tar command line
     */
	sscanf (pos, "%s", fname);
    /*
     * Remove this file from the database
     */
	db_Remove (fname);
    }

    db_Close ();
}
  
  


static int
netread (int fd, char *dest, int len)
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
SetStatus (int problem, char *s)
/*
 * Set our status window.
 */
{
	/*
	 * Note the status in the log as well
	 */
	msg_ELog(problem ? EF_PROBLEM : EF_DEBUG, s);
	model->setStatus (problem, s);
}



/*
 * The WriteNow callback.  The user-called equivalent to a timer event.
 * If a write is currently suspended, this just resumes the current
 * write instead.
 */
void
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
		model->setWriteStatus ("Write Now");
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
		tl_RelativeReq ((void (*)(...))TimerSaveFiles, 0, 0, 0);
	}
}


/*
 * SuspendWrite() -- If a write is in progress, suspend it and 
 * register a resume timer event the given number of seconds from now.
 * If a resume event already existed, it is cancelled first.
 */
#ifdef notdef
void
SuspendWrite(int waitsecs)
#endif
void
SuspendWrite (Widget w, XtPointer call_data)
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
	ResumeEvent = tl_RelativeReq((void (*)(...))TimerResumeWrite,
				     0, waitsecs * INCFRAC, 0);

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
	RemainingEvent = tl_RelativeReq((void (*)(...))TimerRemaining, 
					&ends, 0, 15 * INCFRAC);

	/*
	 * Now set the Suspended flag.  This will be caught by the
	 * RunTar() loop and reading of the pipe will stop as long
	 * as Suspended remains true.  This in turn blocks the
	 * tar process and suspension of the tar is achieved.
	 */
	Suspended = TRUE;
	sprintf(buf,"Write suspended for %i seconds",waitsecs);
	SetStatus(TRUE, buf);

	model->setWriteStatus ("Resume");
}


/*
 * Given the ending time in *cdata, calculate the time remaining from
 * now and show this time in the status label
 */
void
TimerRemaining (ZebTime *zt, void *cdata)
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
void
TimerResumeWrite (ZebTime *zt)
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


/*
 * The timer calls this function at regular intervals (DumpInterval)
 * to instigate a write.  This function just does a RequestWrite(),
 * and that takes care of it.  This request could end up pending
 * because of a current write in progress, so the current time is
 * ignored and calculated at the time of the actual write
 */
void
TimerSaveFiles (ZebTime *zt)
{
	char stime[50];

	TC_EncodeTime(zt, TC_Full, stime);
	msg_ELog(EF_DEBUG,"%s, TimerSaveFiles() requesting a write",stime);
	RequestWrite(FALSE);
}


