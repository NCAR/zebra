/*
 * Seriously quick and immensely dirty data store archiver.
 *
 * Basically: we wake up every so often, look at all the files in the system,
 * and write them out to tape in tar format.
 */
# include <stdio.h>
# include <fcntl.h>
# include <errno.h>
# include <sys/types.h>
# include <sys/ioctl.h>
# include <sys/mtio.h>

# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Command.h>

# include "../include/defs.h"
# include "../include/message.h"
# include "../include/timer.h"
# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"

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
 
 	cd to /fcc/data
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
# define LISTFILE	"/fcc/data/DumpedFiles"

/*
 * Tape drive information.
 */
int	TimerEvent;
int	TapeFD = -1;		/* -1 = no drive	*/
int	BytesWritten = 0, FilesWritten = 0;
# define DriveName	"/dev/nrst0"
# define BLOCKSIZE	(16*512)

# define DUMPTIME	120	/* how often, in minutes, to dump	*/


/*
 * Where the tar command is built.
 */
static char Tarbuf[65536];

/*
 * Widget info.
 */
Widget Top, Form, WStatus, Bytes, Tape;
XtAppContext Appc;



# ifdef __STDC__
	static int	Handler (Message *);
	static void	LoadFileList (void);
	static void	SaveFiles (void);
	static void	DumpPlatform (Platform *);
	static int	RunTar (char *);
	static int	OpenTapeDevice (void);
	static void	Die (void);
	static void	UpdateList (void);
	static int	WriteFileDate (char *, int, SValue *, FILE *);
	static void	WriteEOF (void);
	static void	Finish (void);
	static void	TapeButton (void);
	static void	SpinOff (void);
	static void	SetStatus (char *);
	static int	xevent (int);
	static void	MakeWidget (int *, char **);
	static void	Sync (void);
# else
	static int	Handler ();
	static void	LoadFileList ();
	static void	SaveFiles ();
	static void	DumpPlatform ();
	static int	RunTar ();
	static int	OpenTapeDevice ();
	static void	Die ();
	static void	UpdateList ();
	static int	WriteFileDate ();
	static void	WriteEOF ();
# endif


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
/*
 * Window sys initialization.
 */
	MakeWidget (&argc, argv);

	chdir ("/fcc/data");
	LoadFileList ();
	SetStatus ("Awaiting tape");
# ifdef notdef
# endif
/*
 * Go into our dump loop.
 */
	msg_add_fd (XConnectionNumber (XtDisplay (Top)), xevent);
	msg_await ();
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
/*
 * Hook into the window system.
 */
	Top = XtAppInitialize (&Appc, "Archiver", NULL, 0, argc, argv,
		NULL, NULL, 0);
/*
 * The inevitable form.
 */
	Form = XtCreateManagedWidget ("form", formWidgetClass, Top, NULL, 0);
# ifdef notdef
/*
 * FIrst row.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Zeb tape archiver");	n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);			n++;
	XtSetArg (args[n], XtNfromVert, NULL);			n++;
	above = XtCreateManagedWidget ("title", labelWidgetClass, Form,args,n);
# endif
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
	XtAddCallback (button, XtNcallback, Finish, 0);
/*
 * A "take/release" button.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Take tape");	n++;
	XtSetArg (args[n], XtNfromHoriz, button);	n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	Tape = XtCreateManagedWidget ("tape", commandWidgetClass, Form,
			args, n);
	XtAddCallback (Tape, XtNcallback, TapeButton, 0);
/*
 * Status info.
 */
	n = 0;
/*	above = Tape; */
	XtSetArg (args[n], XtNlabel, "0 Bytes in 0 files");	n++;
	XtSetArg (args[n], XtNfromHoriz, Tape);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	Bytes = XtCreateManagedWidget ("bytes", labelWidgetClass, Form,args,n);

	XtRealizeWidget (Top);
	Sync ();
}




static void
Finish ()
/*
 * Finish things out.
 */
{
	if (TapeFD < 0)
	{
		SetStatus ("Can't finish -- no tape");
		return;
	}
	SaveFiles ();
	SpinOff ();
	SetStatus ("CROAK");
	Die ();
}





static void
TapeButton ()
/*
 * Take or release the tape.
 */
{
	Arg args[2];
/*
 * If we don't have a tape, we try to get one.
 */
	if (TapeFD < 0)
	{
	/*
	 * Get the tape.
	 */
		if (! OpenTapeDevice ())
		{
			SetStatus ("Unable to open tape");
			return;
		}
	/*
	 * Start saving stuff.
	 */
		XtSetArg (args[0], XtNlabel, "Free tape");
		TimerEvent = tl_AddRelativeEvent (SaveFiles, 0,
			INCFRAC, DUMPTIME*60*INCFRAC);
	}
/*
 * Otherwise we give it away.
 */
	else
	{
		XtSetArg (args[0], XtNlabel, "Take tape");
		tl_Cancel (TimerEvent);
		SpinOff ();
		SetStatus ("Awaiting tape");
	}
	XtSetValues (Tape, args, 1);
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
	DumpedTable = usy_c_stbl ("DumpedTable");
	if ((fp = fopen (LISTFILE, "r")) == NULL)
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
	if ((TapeFD = open (DriveName, O_RDWR)) < 0)
	{
		msg_ELog (EF_INFO, "Error %d opening %s", errno, DriveName);
		return (0);
	}
	return (1);
}




static void
SaveFiles ()
/*
 * Pass through the list of stuff and save files to the tape.
 */
{
	int plat;
/*
 * If we have no tape, we do nothing.
 */
	if (TapeFD < 0)
		return;
	strcpy (Tarbuf, "exec tar cfb - 16 ");
/*
 * Pass through the platform table and dump things.
 */
	SetStatus ("Scanning platforms");
	for (plat = 0; plat < SHeader->sm_nPlatform; plat++)
		if (! (PTable[plat].dp_flags & DPF_SUBPLATFORM))
			DumpPlatform (PTable + plat);
/*
 * Run the tar command to put this all together.
 */
	SetStatus ("Writing");
	if (strlen (Tarbuf) > 20 && RunTar (Tarbuf))
	{
		WriteEOF ();
		UpdateList ();
	}
	SetStatus ("Sleeping");
}




static void
DumpPlatform (p)
Platform *p;
/*
 * Dump out any files from this platform.
 */
{
	time last, dumptime;
	SValue v;
	int type, findex;
/*
 * Find the last time this thing was dumped.
 */
	if (usy_g_symbol (DumpedTable, p->dp_name, &type, &v))
		last = v .us_v_date;
	else
		last.ds_yymmdd = last.ds_hhmmss = 0;
/*
 * Go through the file chain.  We never dump the most recent file, on the 
 * assumption that it is still being written to.
 */
	if ((findex = p->dp_LocalData) == 0 || 
			(findex = DFTable[findex].df_FLink) == 0 ||
			DLE (DFTable[findex].df_end, last))
		return;		/* Nothing to dump */
	dumptime = DFTable[findex].df_end;
/*
 * Now go through and do it.
 */
	while (findex && DLT (last, DFTable[findex].df_end))
	{
		char *fname = DFTable[findex].df_name;
		if (! strncmp (fname, "/fcc/data/", 10))
			fname += 10;
		printf ("Dumping file '%s'\n", fname);
		strcat (Tarbuf, fname);
		strcat (Tarbuf, " ");
		findex = DFTable[findex].df_FLink;
	}
/*
 * Record the new time.
 */
	v.us_v_date = dumptime;
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
		return (FALSE);
	}
/*
 * Now read out chunks of stuff.
 */
	while ((nb = netread (pfp->_file, fbuf, BLOCKSIZE)) > 0)
	{
		write (TapeFD, fbuf, nb);
		tnb += nb;
	}
/*
 * If tar returned OK, so do we.
 */
	printf ("Transferred %d bytes, last %d\n", tnb, nb);
	FilesWritten++;
	BytesWritten += tnb;
	if ((rstatus = pclose (pfp)) == 0)
		return (TRUE);
	msg_ELog (EF_PROBLEM, "Tar returned status %d", rstatus);
	return (FALSE);
}





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
	if ((fp = fopen (LISTFILE, "w")) == NULL)
	{
		msg_ELog (EF_PROBLEM, "Unable to open %s", LISTFILE);
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
	sprintf (string, "%d MBytes in %d files.", BytesWritten/1000000,
		FilesWritten);
	XtSetArg (args[0], XtNlabel, string);
	XtSetValues (Bytes, args, 1);
	Sync ();
}




int
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
	if (ioctl (TapeFD, MTIOCTOP, &op) < 0)
		perror ("Tape WEOF");
}




static void
SpinOff ()
/*
 * Spin off the tape.
 */
{
	struct mtop op;

	if (TapeFD < 0)
		return;
	op.mt_op = MTOFFL;
	op.mt_count = 1;
	SetStatus ("Spinning off tape");
	if (ioctl (TapeFD, MTIOCTOP, &op) < 0)
		SetStatus ("Unable to spin off tape");
}




static void
SetStatus (s)
char *s;
/*
 * Set our status window.
 */
{
	Arg args[2];

	XtSetArg (args[0], XtNlabel, s);
	XtSetValues (WStatus, args, 1);
	Sync ();
}




static void
Sync ()
{
	XSync (XtDisplay (Top), False);
	xevent (0);
}
