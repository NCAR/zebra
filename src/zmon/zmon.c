/*
 * zeb process monitor.
 */

# include <stdio.h>
# include <string.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Label.h>
# include <defs.h>
# include <message.h>
# include <timer.h>

MAKE_RCSID ("$Id: zmon.c,v 1.3 1995-06-29 22:38:43 granger Exp $")

typedef enum { S_OK, S_DEAD } State;

/*
 * Individual processes to watch.
 */
typedef struct _WatchProc
{
	char	wp_Name[40];		/* The process to watch	*/
	int	wp_PingRec;		/* We got back the ping answer */
	Widget	wp_Widget;		/* The display widget	*/
	struct _WatchProc *wp_Next;	/* Next in chain	*/
	State	wp_State;		/* Current display state	*/
} WatchProc, *ProcList;

/*
 * The list of machines that we watch.
 */
typedef struct _WatchMachine
{
	char	wm_Name[40];		/* Name of the machine	*/
	ProcList wm_Procs;		/* The list of processes to watch */
	int	wm_PingRec;		/* We got back the ping answer */
	struct _WatchMachine *wm_Next;	/* Next in the list	*/
	Widget	wm_Widget;		/* The display widget	*/
	State	wm_State;		/* Current display state	*/
} WatchMachine;

WatchMachine *WatchList = 0;


/*
 * Widget info.
 */
Widget Top, Form;
XtAppContext Appc;

static char *Host;
/*
 * Colors for the status widget.
 */
Pixel Red, Black;


/*
 * Forwards.
 */
static int 	MsgHandler FP ((Message *));
static void	LoadConfig FP ((char *));
static void	ConfigLine FP ((char *));
static void	BadLine FP ((char *));
static void	MsgInput FP ((XtPointer, int *, XtInputId *));
static void	GetColors FP ((void));
static void	CreateWidget FP ((void));
static void	DoPing FP ((void));
static void	Scan FP ((void));
static void	SendPing FP ((char *, char *));
static void	SetState FP ((State, Widget));
static void	Mark FP ((char *, char *));


main (argc, argv)
int argc;
char **argv;
{
/*
 * Get hooked into the message system.
 */
	if (! msg_connect (MsgHandler, "ZMon"))
	{
		printf ("Unable to connect to message handler\n");
		exit (1);
	}
/*
 * Get hooked into the window system.
 */
	Host = getenv ("HOST");
        Top = XtAppInitialize (&Appc, "ZMon", NULL, 0, &argc, argv,
                NULL, NULL, 0);
/*
 * Load up our platform config.
 */
	LoadConfig (argc > 1 ? argv[1] : "/zeb/lib/ZMon.config");
/*
 * Make our widgets.
 */
        GetColors ();
        CreateWidget ();
/*
 * Wait for something to happen.
 */
	tl_AddRelativeEvent (Scan, 0, 10*INCFRAC, 20*INCFRAC);
	DoPing ();
        XtRealizeWidget (Top);
        XtAppAddInput (Appc, msg_get_fd (), (XtPointer)XtInputReadMask, 
		       MsgInput, 0);
        XtAppMainLoop (Appc);
	return (0);
}



static void
MsgInput (junk, fd, morejunk)
XtPointer junk;
int *fd;
XtInputId *morejunk;
/*
 * Input is happening.
 */
{
        static int nexit = 0;

        if (msg_incoming (*fd))
        {
                msg_ELog (EF_INFO, "Exiting.");
                if (++nexit > 50)
                        exit (1);
        }
}


static void
LoadConfig (file)
char *file;
/*
 * Figure out what processes to watch.
 */
{
	FILE *cfile;
	char line[200];
/*
 * Open up the config file.
 */
        cfile = fopen (file, "r");
        if (cfile == NULL)
        {
                msg_ELog (EF_PROBLEM, "No config file");
                exit (1);
        }
/*
 * Read in lines.
 */
	while (fgets (line, 200, cfile) != NULL)
		ConfigLine (line);
	fclose (cfile);
}



static void
BadLine (line)
char *line;
{
	msg_ELog (EF_EMERGENCY, "Bad config line: '%s'", line);
	exit (1);
}



static void
ConfigLine (line)
char *line;
/*
 * Deal with one configuration line.
 */
{
	char *blank;
	WatchMachine *mach, *mp;
	ProcList proc;
	char oline[200];
/*
 * Split out the machine name.
 */
	strcpy (oline, line);
	if (! (blank = strchr (line, ' ')))
		BadLine (line);
	mach = ALLOC (WatchMachine);
	*blank = '\0';
	strcpy (mach->wm_Name, line);
	mach->wm_Procs = 0;
	mach->wm_State = S_OK;
/*
 * Add the machine to the list.
 */
	if (! WatchList)
		WatchList = mach;
	else
	{
		for (mp = WatchList; mp->wm_Next; mp = mp->wm_Next)
			;
		mp->wm_Next = mach;
	}
	mach->wm_Next = NULL;
/*
 * Pull out any processes.
 */
	line = blank + 1;
	while ((blank = strchr (line, ' ')) || (blank = strchr (line, '\n')))
	{
		int last = (*blank == '\n');
		ProcList prp;
		char *hblank;
	/*
	 * Pull out the info.
	 */
		*blank = '\0';
		proc = ALLOC (WatchProc);
		strcpy (proc->wp_Name, line);
		proc->wp_State = S_OK;
		if (hblank = strchr (proc->wp_Name, '~'))
			*hblank = ' ';
	/*
	 * Add it to the list.
	 */
		if (! mach->wm_Procs)
			mach->wm_Procs = proc;
		else
		{
			for (prp = mach->wm_Procs; prp->wp_Next;
						prp = prp->wp_Next)
				;
			prp->wp_Next = proc;
		}
		proc->wp_Next = NULL;
	/*
	 * IF this is the last one, bail.
	 */
		if (last)
			break;
		line = blank + 1;
	}
}



static void
GetColors ()
/*
 * Find our colors.
 */
{
        XColor xc, exact;
        Display *disp = XtDisplay (Top);

        XAllocNamedColor (disp, DefaultColormap (disp, 0), "black",&xc,&exact);
        Black = xc.pixel;
        XAllocNamedColor (disp, DefaultColormap (disp, 0), "red", &xc, &exact);
        Red = xc.pixel;
}


static void
CreateWidget ()
/*
 * Create the widget.
 */
{
	Arg args[10];
	int n;
	Widget above = NULL, left = NULL, na = NULL;
	WatchMachine *mach;
	ProcList proc;
/*
 * The form which holds it all.
 */
	n = 0;
	/* XtSetArg (args[n], XtNdefaultDistance, 2);	n++; */
	Form = XtCreateManagedWidget ("form", formWidgetClass, Top, args, n);
/*
 * The setup for all the labels we do.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNforeground, Black);	n++;
	XtSetArg (args[n], XtNborderWidth, 0);		n++;
/*
 * Now we go through the machines and plunk in each one.
 */
	for (mach = WatchList; mach; mach = mach->wm_Next)
	{
		int nproc = 0;
		char label[20];
	/*
	 * Make the label for the machine itself.
	 */
		XtSetArg (args[0], XtNfromHoriz, NULL);
		XtSetArg (args[1], XtNfromVert, above);
		sprintf (label, "%-12s", mach->wm_Name);
		XtSetArg (args[n], XtNlabel, label);
		na = left = mach->wm_Widget = XtCreateManagedWidget ("machine",
			labelWidgetClass, Form, args, n + 1);
	/*
	 * Go through and do the processes.
	 */
		for (proc = mach->wm_Procs; proc; proc = proc->wp_Next)
		{
		/*
		 * Split things onto a new line if need be.
		 */
			if (++nproc > 5)
			{
				above = na;
				nproc = 0;
				strcpy (label, "            ");
				XtSetArg (args[0], XtNfromHoriz, NULL);
				XtSetArg (args[1], XtNfromVert, above);
				XtSetArg (args[n], XtNlabel, label);
				na = left = XtCreateManagedWidget ("fill",
					labelWidgetClass, Form, args, n + 1);
			}
		/*
		 * Add this widget.
		 */
			XtSetArg (args[0], XtNfromHoriz, left);
			left = proc->wp_Widget =
				XtCreateManagedWidget (proc->wp_Name,
					labelWidgetClass, Form, args, n);
		}
		above = na;
	}
}





static void
DoPing ()
/*
 * Ping everybody.
 */
{
	WatchMachine *wm = WatchList;
	ProcList proc;

	for (; wm; wm = wm->wm_Next)
	{
	/*
	 * Ping the host itself.
	 */
		if (strcmp (wm->wm_Name, "local"))
			SendPing (wm->wm_Name, MSG_MGR_NAME);
		wm->wm_PingRec = 0;
	/*
	 * Now the processes.
	 */
		for (proc = wm->wm_Procs; proc; proc = proc->wp_Next)
		{
			SendPing (wm->wm_Name, proc->wp_Name);
			proc->wp_PingRec = 0;
		}
	}
}



static void
SendPing (host, proc)
char *host, *proc;
/*
 * Ping this bloke.
 */
{
	char recip[60];
	int proto = MT_PING;

	if (! strcmp (host, "local"))
		strcpy (recip, proc);
	else
		sprintf (recip, "%s@%s", proc, host);
	if (strcmp (proc, MSG_MGR_NAME))
		proto = MT_CPING;
	msg_send (recip, proto, FALSE, recip, 0);
}



static int
MsgHandler (msg)
Message *msg;
/*
 * Deal with incoming stuff.
 */
{
	char *at;
/*
 * Make sure this is what we expect.
 */
	if (msg->m_proto != MT_PING)
	{
		msg_ELog (EF_PROBLEM, "Strange proto %d", msg->m_proto);
		return (0);
	}
/*
 * Split out the response and mark it.
 */
	if (! (at = strchr (msg->m_from, '@')))
		Mark ("local", msg->m_from);
	else
	{
		*at = '\0';
		Mark (at + 1, msg->m_from);
	}
	return (0);
}





static void
Mark (host, name)
char *host, *name;
/*
 * Mark a received ping.
 */
{
	WatchMachine *mach;
	ProcList proc;
/*
 * Find the machine of interest.
 */
	for (mach = WatchList; mach; mach = mach->wm_Next)
		if (! strcmp (mach->wm_Name, host))
			break;
	if (! mach)
	{
		msg_ELog (EF_PROBLEM, "Strange...ping from %s@%s", name, host);
		return;
	}
/*
 * If this is the response from the host itself, mark it and quit.
 */
	if (! strcmp (name, MSG_MGR_NAME))
	{
		mach->wm_PingRec = True;
		return;
	}
/*
 * Otherwise we gotta find the process.
 */
	for (proc = mach->wm_Procs; proc; proc = proc->wp_Next)
		if (! strcmp (proc->wp_Name, name))
			break;
	if (! proc)
		msg_ELog (EF_PROBLEM, "Strange...ping from %s@%s", name, host);
	else
		proc->wp_PingRec = True;
}



static void
Scan ()
/*
 * Scan the world for deadbeats.
 */
{
	WatchMachine *mach;
	ProcList proc;
	State newstate;
/*
 * Scan the machines.
 */
	for (mach = WatchList; mach; mach = mach->wm_Next)
	{
	/*
	 * Check the machine.
	 */
		if (strcmp (mach->wm_Name, "local"))
		{
			newstate = mach->wm_PingRec ? S_OK : S_DEAD;
			if (newstate != mach->wm_State)
			{
				mach->wm_State = newstate;
				SetState (newstate, mach->wm_Widget);
			}
		}
	/*
	 * Now the processes.
	 */
		for (proc = mach->wm_Procs; proc; proc = proc->wp_Next)
		{
			newstate = proc->wp_PingRec ? S_OK : S_DEAD;
			if (newstate != proc->wp_State)
			{
				proc->wp_State = newstate;
				SetState (newstate, proc->wp_Widget);
			}
		}
	}
/*
 * Fire off a new set of pings.
 */
	DoPing ();
}




static void
SetState (state, w)
State state;
Widget w;
/*
 * Tweak the visual state of this widget.
 */
{
	Arg args[2];

	XtSetArg (args[0], XtNforeground, (state == S_OK) ? Black : Red);
	XtSetValues (w, args, 1);
}
