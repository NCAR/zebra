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

MAKE_RCSID ("$Id: Archiver.cc,v 1.11 1992-06-26 22:11:30 kris Exp $")

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

/*
 * This table contains an entry for each platform that we dump, indicating
 * the latest data which has been written.
 */
stbl	DumpedTable;
char listfile[200];	

/*
 * Tape drive information.
 */
int	TimerEvent;
int	DeviceFD = -1;		/* -1 = no drive	*/
unsigned long	BytesWritten = 0;
int	FilesWritten = 0;
int	FreshTape = TRUE;
int	ZeroZFree = TRUE;
unsigned long	TapeLimit = 3500000000;	/* in bytes.			   */	
				/* Tape size, less a safety margin */
				/* Default is high density Exabyte */

# define BFACTOR	64	/* Blocking factor for the tar command */
# define BLOCKSIZE	(BFACTOR*512)

# define DUMPTIME	2	/* how often, in hours, to dump	*/
# define AR_TAPE 1
# define AR_EOD 2

char	*DriveName = NULL;
char	*OutputDir = NULL;
char	*MountName = NULL;
int	DumpTime = DUMPTIME;
int	MinDisk = 10000;
int	ArchiveMode = 0;


/*
 * Where the tar command is built.
 */
static char Tarbuf[65536];

/*
 * Widget info.
 */
Widget Top, Form, WStatus, Bytes, Action;
XtAppContext Appc;

/*
 * Colors for the status widget.
 */
Pixel RedPix, WhitePix;


static int	Handler FP ((Message *));
static void	LoadFileList FP ((void));
static void	SaveFiles FP ((int));
static void	TimerSaveFiles FP ((ZebTime *));
static void	DumpPlatform FP ((Platform *, int));
static int	RunTar FP ((char *));
static int	OpenTapeDevice FP ((void));
static void	Die FP ((void));
static void	UpdateList FP ((void));
static int	WriteFileDate FP ((char *, int, SValue *, FILE *));
static void	WriteEOF FP ((void));
static void	Finish FP ((void));
static void	ActionButton FP ((void));
static void	SpinOff FP ((void));
static void	SetStatus FP ((int, char *));
static int	xevent FP ((int));
static void	MakeWidget FP ((int *, char **));
static void	Sync FP ((void));
static void	SendMA FP ((int));
static int	TellDaemon FP ((char *, int, SValue *, int));
static void	UpdateMem FP ((void));
static void	InitArchiver FP ((int, char**));
static int	EjectEOD FP ((void));
static void	MountEOD FP ((void));



main (argc, argv)
int argc;
char **argv;
{
/*
 * Initialize.
 */
	usy_init ();
	msg_connect (Handler, "Archiver");
	ds_Initialize ();
	InitArchiver(argc,argv);
/*
 * Window sys initialization.
 */
	MakeWidget (&argc, argv);

	chdir (DATADIR);
	LoadFileList ();
	UpdateMem ();
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
}

void
InitArchiver (argc, argv)
int argc;
char **argv;
{
    static char defaultDeviceFile[] = "/dev/nrst8";
    static char defaultMountName[] = "eod0";
    static char defaultOutputDir[] = "/eod0";
    MountName = defaultMountName;
    OutputDir = defaultOutputDir;
    DriveName = defaultDeviceFile;
    ArchiveMode = AR_TAPE;
    while ((--argc) > 0 )
    {
	if ( (*++argv)[0] == '-' )
	{
	    switch ( (*argv)[1] )
	    {
		case 'v': /* device name */
		    DriveName = (*++argv);
		    argc--;
		break;
		case 'o': /* output directory */
		    OutputDir = (*++argv);
		    argc--;
		break;
		case 'n': /* optical disk mount name */
		    MountName = (*++argv);
		    argc--;
		break;
		case 'k': /* min required disk size in k-bytes */
		    sscanf ( (*++argv), "%d", &MinDisk );
		    argc--;
		break;
		case 't': /* t(imer) for archive interval */
		    sscanf ( (*++argv), "%d", &DumpTime );
		    argc--;
		break;
		case 'm': /* m(ode) */
		    ++argv;
		    if ( strcmp ( (*argv), "tape") == 0 )
			ArchiveMode = AR_TAPE;
		    else if ( strcmp ( (*argv), "eod") == 0 )
			ArchiveMode = AR_EOD;
		    argc--;
		break;
		case 'z':	/* Free tape at 0Z? */
			++argv;
			if (strcmp (*argv, "no") == 0)
				ZeroZFree = FALSE;
			else
				ZeroZFree = TRUE;
			--argc;
			msg_ELog (EF_DEBUG, "Free tape on 0Z: %s.", ZeroZFree?
				"true" : "false");
		break;
	    }
	}
    }
    msg_ELog (EF_INFO, "Archiver: device: %s dump interval: %d",
	DriveName,DumpTime);
    if ( ArchiveMode == AR_EOD )
    {
    msg_ELog (EF_INFO, "Archiver: dump files to directory: %s",OutputDir);
    msg_ELog (EF_INFO, "Archiver: minimum optical disk to dump: %d kb",MinDisk);
    }

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
 * Put together the control widget.
 */
{
	Arg args[5];
	int n;
	Widget w, above, button;
	XColor screen, exact;
/*
 * Hook into the window system.
 */
	Top = XtAppInitialize (&Appc, "Archiver", NULL, 0, argc, argv,
		NULL, NULL, 0);
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
 * Status info.
 */
	n = 0;
/*	above = Tape; */
	XtSetArg (args[n], XtNlabel, "0 Bytes in 0 files");	n++;
	XtSetArg (args[n], XtNfromHoriz, Action);		n++;
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
	ZebTime zt;
	int year, month, day, hour;
	int status;
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
		    XtSetArg (args[0], XtNlabel, "Free tape");
		break;
		case AR_EOD:
		    MountEOD();
		    XtSetArg (args[0], XtNlabel, "Free optical disk.");
		break;
	    }
	/*
	 * Start saving stuff.  Make the archive time line up nicely
	 * on the hour boundary.
	 */
		tl_Time (&zt);
		TC_ZtSplit (&zt, &year, &month, &day, &hour, 0, 0, 0);
		hour -= hour % DumpTime;
		TC_ZtAssemble (&zt, year, month, day, hour, 0, 0, 0);
		TimerEvent = tl_AbsoluteReq (TimerSaveFiles, 0, &zt,
				DumpTime*60*60*INCFRAC);
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
	int test;
	char datafile[120];
	struct statfs buf;
/*
 * Special check -- if the hour is zero, we clean up everything.
 */
	TC_ZtSplit (zt, &year, &month, &day, &hour, &minute, &second, 0);
	switch ( ArchiveMode )
	{
	    case AR_TAPE:
		if (ZeroZFree)
			test = ((hour == 0) && (! FreshTape));
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
	int plat;
/*
 * If we have no tape, we do nothing.
 */
	if (DeviceFD < 0)
		return;
	sprintf (Tarbuf, "exec tar cfb - %d ", BFACTOR);
/*
 * Pass through the platform table and dump things.
 */
	SetStatus (FALSE, "Scanning platforms");
	for (plat = 0; plat < SHeader->sm_nPlatform; plat++)
		if (! (PTable[plat].dp_flags & DPF_SUBPLATFORM))
			DumpPlatform (PTable + plat, all);
/*
 * Run the tar command to put this all together.
 */
	if ( strlen(Tarbuf) > 20 )
		SetStatus (FALSE, "Writing");

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
	SetStatus (FALSE, "Sleeping");
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
		xevent (0);
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
			Die ();
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
				! DFTable[dfi].df_archived)
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
