/*
 * Aircraft ingest from FAA black box status widget and transponder code
 * changer.
 */

# include <X11/X.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/AsciiText.h>
# include <string.h>
# include <signal.h>

# include "ac_ingest.h"

# define ATSLEN		80	/* Length for AsciiText strings.	*/

static char	MyAircraft[MAXOURS][STRLEN];
static int	NumAc = 0;
static char	New_Trans[ATSLEN];
static bool	DoFeet = FALSE;

static bool	SWMade = FALSE;
static Widget	AltButton, StatusLabel, TransLabel, TransText;
static char	Platform[STRLEN];
static Ac_Data	Aircraft;
static time	AcTime;
static char	TitleStr[200], StatusStr[MAXOURS][200], AcIngestName[STRLEN];
static Widget	Top;
XtAppContext	Actx;

XtAppContext	Appc;

# ifdef __STDC__
	static Widget MakeStatusWidget (int, Widget, XtAppContext);
	static void StatusWindow (Widget);
	static void Add_Trans (void);
	static void Change_Trans (void);
	static void Del_Trans (void);
	static int Die (void);
	static int Dispatcher (int, struct ui_command *);
	static void Notification (PlatformId, int, time *);
	static void SetStatus(int);
	static void SetLabel(Widget, char *);
	static void SetupIndirect ();
	static void Go ();
	static int DoXevent ();
	static void ChangeAlt ();
# else
	static Widget MakeStatusWidget ();
	static void StatusWindow ();
	static void Add_Trans ();
	static void Change_Trans ();
	static void Del_Trans ();
	static int Die ();
	static int Dispatcher();
	static void Notification ();
	static void SetStatus();
	static void SetLabel();
	static void SetupIndirect ();
	static void Go ();
	static int DoXevent ();
	static void ChangeAlt ();
# endif


main (argc, argv)
int	argc;
char	**argv;
{
	SValue	v;

	msg_connect (Die, "Ac_Status");

	if (argc > 1)
	{
		ui_init ("/fcc/lib/ac_status.lf", FALSE, TRUE);
		v.us_v_ptr = argv[1];
		usy_s_symbol (usy_g_stbl ("ui$variable_table"), "commandfile",
			SYMT_STRING, &v);
	}
	else ui_init ("/fcc/lib/ac_status.lf", TRUE, FALSE);
	ui_setup ("acstatus", &argc, argv, NULL);

	ds_Initialize ();	
	SetupIndirect ();

	signal (SIGINT, Die);
	signal (SIGTERM, Die);

	ui_get_command ("initial", "status>", Dispatcher, 0);
	Die ();
}


static void
SetupIndirect ()
/*
 * Create all the indirect variables which are used to control things.
 */
{
	stbl vtable = usy_g_stbl ("ui$variable_table");

	usy_c_indirect (vtable, "ac_ingest_name", &AcIngestName, SYMT_STRING,
		STRLEN);
}


static void
Notification (pid, global, t)
PlatformId	pid;
int		global;
time		*t;
/*
 * When data becomes avaiable update the status window.
 */
{
	DataObject	*dobj;
	char		*fields[1];
	int		i, itsat = -1, numfields;

	msg_ELog (EF_DEBUG, "***************************************");
	strcpy (Platform, ds_PlatformName (pid));
	msg_ELog (EF_DEBUG, "Data available on %s at %d %d.", Platform,
		t->ds_yymmdd, t->ds_hhmmss);
/*
 * Make sure its a platform we're still interested in.
 */
	for (i = 0; i < NumAc; i++)
		if (strcmp (Platform, MyAircraft[i]) == 0)
		{
			itsat = i;
			break;
		}
/*
 * If it is then set the status label.
 */
	if ((itsat >= 0) && (itsat < NumAc))
	{
		AcTime = *t;
		fields[0] = "trans";
		numfields = 1;
		if ((dobj = ds_GetData (pid, fields, numfields, t, t, 
			OrgScalar, 0.0, BADVAL)) == 0)
		{
			msg_ELog (EF_PROBLEM, "Get failed for %s.", Platform);
			return;
		}
		if (DoFeet)
			Aircraft.altitude = dobj->do_aloc->l_alt * 1000.0 / 
				M_PER_FT;
		else Aircraft.altitude = dobj->do_aloc->l_alt;
		Aircraft.latitude = dobj->do_aloc->l_lat;
		Aircraft.longitude = dobj->do_aloc->l_lon;
		Aircraft.transponder = (int) dobj->do_data[0][0];
		SetStatus(itsat);
	}
}


static int
Dispatcher (junk, cmds)
int	junk;
struct ui_command	*cmds;
/*
 * The command dispatcher.
 */
{
	switch (UKEY (*cmds))
	{
		case AIC_GO:
			Go ();
			break;
		default:
			msg_ELog (EF_PROBLEM, "Unknown kw %d", UKEY (*cmds));
			break;
	}
}


static void
Go ()
/*
 * Popup the widget.  Begin dealing with X events and/or data notifications.
 */
{
	Display	*Disp;
	int	fd;

	uw_def_widget ("acstatus", "acstatus", MakeStatusWidget, 0, 0);
	uw_ForceWindowMode ("acstatus", &Top, &Actx);

	Disp = XtDisplay (Top);
	fd = XConnectionNumber (Disp);
	msg_add_fd (fd, DoXevent);

	msg_await ();
}


static int
Die ()
/*
 * Finish gracefully.
 */
{
	msg_ELog (EF_DEBUG, "Dying...");
	ui_finish ();
	exit (0);
}


static Widget
MakeStatusWidget (junk, parent, appc)
int 		junk;
Widget 		parent;
XtAppContext 	appc;
/*
 * Create the status widget.
 */
{
	Widget	form;
	Arg	args[2];
	int	n;

	SWMade = TRUE;
/* 
 * At the top is a form widget to hold the pieces together.
 */
	n = 0;
	XtSetArg (args[n], XtNdefaultDistance, 5);	n++;
	XtSetArg (args[n], XtNborderWidth, 0);		n++;
	form = XtCreateManagedWidget ("statusform", formWidgetClass, parent, 
		args, n);
/*
 * Add the aircraft ingest status window.
 */
	StatusWindow (form);
	return (form);
}




static void
StatusWindow (parent)
Widget	parent;
{
	Widget	w, above;
	Arg	args[15];
	int	n;
/*
 * The status title.
 */
	sprintf (TitleStr, "%-11s%-6s%-12s%-12s%-12s%-5s\n", "Platform",
		"Trans", "Altitude", "Latitude", "Longitude", "Time"); 
	n = 0;
	XtSetArg (args[n], XtNlabel, TitleStr);		n++;
	XtSetArg (args[n], XtNjustify, XtJustifyLeft);	n++;
	XtSetArg (args[n], XtNwidth, 410);		n++;
	XtSetArg (args[n], XtNresize, True);		n++;
	above = XtCreateManagedWidget ("AcStatusL", labelWidgetClass, 
		parent, args, n);
/*
 * The status window.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	XtSetArg (args[n], XtNlabel, StatusStr);	n++;
	XtSetArg (args[n], XtNjustify, XtJustifyLeft);	n++;
	XtSetArg (args[n], XtNwidth, 410);		n++;
	XtSetArg (args[n], XtNheight, 80);		n++;
	XtSetArg (args[n], XtNresize, True);		n++;
	above = StatusLabel = XtCreateManagedWidget ("AcStatusT", 
		labelWidgetClass, parent, args, n);
/*
 * Feet vs. Km buttom.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, DoFeet ? "Ft" : "Km");	n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);			n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	above = AltButton = XtCreateManagedWidget ("FtKm", commandWidgetClass, 
		parent, args, n);
	XtAddCallback (above, XtNcallback, ChangeAlt, 0);	
/*
/*
 * The instructions line.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	XtSetArg (args[n], XtNwidth, 410);		n++;
	XtSetArg (args[n], XtNjustify, XtJustifyLeft);	n++;
	XtSetArg (args[n], XtNlabel, "Enter:  platform,transponder");	n++;
	above = TransLabel = XtCreateManagedWidget ("AcTransL", 
		labelWidgetClass, parent, args, n);
/*
 * The transponder text widget.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	XtSetArg (args[n], XtNdisplayPosition, 0);	n++;
	XtSetArg (args[n], XtNinsertPosition, 0);	n++;
	XtSetArg (args[n], XtNlength, ATSLEN);		n++;
	XtSetArg (args[n], XtNresize, XawtextResizeNever);	n++;
	XtSetArg (args[n], XtNwidth, 410);		n++;
	XtSetArg (args[n], XtNheight, 20);		n++;
	XtSetArg (args[n], XtNstring, New_Trans);	n++;
	XtSetArg (args[n], XtNtype, XawAsciiString);	n++;
	XtSetArg (args[n], XtNuseStringInPlace, True);	n++;
	XtSetArg (args[n], XtNleftMargin, 5);		n++;
	XtSetArg (args[n], XtNeditType, XawtextAppend);	n++;
	above = TransText = XtCreateManagedWidget ("AcTransT", 
		asciiTextWidgetClass, parent, args, n);
/*
 * Transponder control buttons.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	XtSetArg (args[n], XtNlabel, "Add");		n++;
	w = XtCreateManagedWidget ("TransAdd", commandWidgetClass, 
		parent, args, n);
	XtAddCallback (w, XtNcallback, Add_Trans, 0);
	
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	XtSetArg (args[n], XtNlabel, "Change");		n++;
	w = XtCreateManagedWidget ("TransChange", commandWidgetClass, 
		parent, args, n);
	XtAddCallback (w, XtNcallback, Change_Trans, 0);
	
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	XtSetArg (args[n], XtNlabel, "Delete");		n++;
	w = XtCreateManagedWidget ("TransDel", commandWidgetClass, 
		parent, args, n);
	XtAddCallback (w, XtNcallback, Del_Trans, 0);
}


static void
ChangeAlt ()
/*
 * Change altitude status display between ft and km.
 */
{
	Arg	args[2];

	DoFeet = ! DoFeet;
	XtSetArg (args[0], XtNlabel, DoFeet ? "Ft" : "Km");
	XtSetValues (AltButton, args, 1);
}


static void
Add_Trans()
/*
 * Add a new platform and its transponder code the the ones we are
 * interested in.
 */
{
	PlatformId	pid;
	char		*pname[5], temptrans[ATSLEN], sendstr[STRLEN];
	int		i, pnum;
/*
 * Check if the entry is ok.
 */
	strcpy (temptrans, New_Trans);
	pnum = CommaParse (temptrans, pname);
	if (pnum <= 1)
	{
		msg_ELog (EF_PROBLEM, "Bad syntax %s", New_Trans);
		return;
	}	
	if ((pid = ds_LookupPlatform (pname[0])) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad platform %s.", pname[0]);
		return;
	}
/*
 * Add it to our list.
 */
	for (i = 0; i < NumAc; i++)
		if (strcmp (MyAircraft[i], pname[0]) == 0)
		{
			msg_ELog (EF_PROBLEM, "Can't add platform %s.",
				pname[0]);
			SetLabel (TransLabel, "Can't add:  Platform already added.");
			return;
		}			
	strcpy (MyAircraft[NumAc], pname[0]);
	NumAc++;
/*
 * Tell ac_ingest and the data store about the addition.
 */
	ds_RequestNotify (pid, 0, Notification);
	sprintf (sendstr, "a %s,%s", pname[0], pname[1]);
	msg_send (AcIngestName, MT_ACINGEST, FALSE, sendstr, 
		sizeof (sendstr));
	SetLabel (TransLabel, "Added.");
}


static void
Change_Trans ()
/*
 * Change the transponder code of one of the platforms we are interested in.
 */
{
	PlatformId	pid;
	char		*pname[5], temptrans[ATSLEN];
	char		sendstr[STRLEN];
	int		i, pnum;
/*
 * Check if the entry is ok.
 */
	strcpy (temptrans, New_Trans);
	pnum = CommaParse (temptrans, pname);
	if (pnum <= 1)
	{
		msg_ELog (EF_PROBLEM, "Bad syntax %s", New_Trans);
		return;
	}	
	if ((pid = ds_LookupPlatform (pname[0])) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad platform %s.", pname[0]);
		return;
	}
/*
 * Check to make sure we can change it.
 */
	for (i = 0; i < NumAc; i++)
		if (strcmp (MyAircraft[i], pname[0]) == 0)
			break;
	if (i >= NumAc)
	{
		msg_ELog (EF_PROBLEM, "Can't find platform %s to change.",
			pname[0]);
		SetLabel (TransLabel, "Can't change:  Platform not added.");
		return;
	}
/*
 * Tell ac_ingest about the change.
 */
	sprintf (sendstr, "c %s,%s", pname[0], pname[1]);
	msg_send (AcIngestName, MT_ACINGEST, FALSE, sendstr, sizeof (sendstr));
	SetLabel (TransLabel, "Changed.");
}


static void
Del_Trans ()
/*
 * Delete one of the platforms we are interested in.
 */
{
	PlatformId	pid;
	char		*pname[5], temptrans[ATSLEN];
	char		sendstr[STRLEN];
	int		i, itsat = -1, pnum;

/*
 * Check if the entry is ok.
 */
	strcpy (temptrans, New_Trans);
	pnum = CommaParse (temptrans, pname);
	if (pnum <= 1)
	{
		msg_ELog (EF_PROBLEM, "Bad syntax %s", New_Trans);
		return;
	}	
	if ((pid = ds_LookupPlatform (pname[0])) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad platform %s.", pname[0]);
		return;
	}
/*
 * Delete it from our list.
 */
	for (i = 0; i < NumAc; i++)
		if (strcmp (MyAircraft[i], pname[0]) == 0)
		{
			itsat = i;
			break;
		}
	if (itsat < 0)
	{
		msg_ELog (EF_PROBLEM, "Can't find platform %s to delete.",
			pname[0]);
		SetLabel (TransLabel, "Can't delete:  Platform not added.");
		return;
	}
	for (i = itsat; i < NumAc - 1; i++)
		strcpy (MyAircraft[i], MyAircraft[i+1]);
	NumAc--;
/*
 * Tell ac_ingest about the deletion.
 */
	sprintf (sendstr, "d %s,%s", pname[0], pname[1]);
	msg_send (AcIngestName, MT_ACINGEST, FALSE, sendstr, 
		sizeof (sendstr));
	SetLabel (TransLabel, "Deleted.");
}


static void
SetLabel (w, label)
Widget	w;
char	*label;
/*
 * Set this label.
 */
{
	Arg	args[2];

	XtSetArg (args[0], XtNlabel, label);
	XtSetValues (w, args, 1);
}



void
SetStatus (itsat)
int	itsat;
/*
 * Set the status widget.
 */
{
	char	string[200], sendstr[MAXOURS * 200];
	int	i;

	if (! SWMade)
		return;

	sprintf (string, "%-11s%-6o%-12.2f%-12.2f%-12.2f%2d:%02d\n", 
		Platform, Aircraft.transponder, Aircraft.altitude, 
		Aircraft.latitude, Aircraft.longitude, 
		AcTime.ds_hhmmss/10000, (AcTime.ds_hhmmss/100) % 100);
	strncpy (StatusStr[itsat], string, strlen (string));
	sendstr[0] = '\0';
	for (i = 0; i < NumAc; i++)
		strcat (sendstr, StatusStr[i]);
	SetLabel (StatusLabel, sendstr);
	DoXevent ();
}


static int
DoXevent ()
/*
 * Deal with X events as long as they keep coming.
 */
{
	XEvent	event;
	
	while (XtAppPending (Actx))
	{
		XtAppNextEvent (Actx, &event);
		XtDispatchEvent (&event);
	}
	return (0);
}
