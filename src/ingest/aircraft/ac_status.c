/*
 * Aircraft ingest from FAA black box status widget and transponder code
 * changer.
 */
/*		Copyright (C) 1987-92 by UCAR
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

static char *rcsid = "$Id: ac_status.c,v 1.8 1992-03-31 22:10:09 burghart Exp $";

# include <copyright.h>
# include <X11/X.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Toggle.h>
# include <string.h>
# include <signal.h>

# include <config.h>
# include "ac_ingest.h"

# define ATSLEN	80	/* Length for AsciiText strings.	*/
# define MAXAC	20	/* How many aircraft can we handle	*/

static char	New_Trans[ATSLEN];
static bool	DoFeet = TRUE;

static bool	SWMade = FALSE;
static Widget	PosButton, AltButton, StatusLabel, TransLabel, TransText;
static Widget	ClrButton;
static char	Platform[STRLEN];
static Ac_Data	Aircraft;
static time	Delta, AcTime, SaveTime[MAXOURS];
static char	TitleStr[200], StatusStr[MAXOURS][200], AcIngestName[STRLEN];
static char	AcPlatforms[200];
static char	LabelString[MAXOURS * 200];
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
	static void SetStatus(int, int);
	static void SetLabel(Widget, char *);
	static void SetupIndirect ();
	static void SetupNotify ();
	static void Go ();
	static int DoXevent ();
	static void ChangeAlt ();
	static void ClearStatus ();
	int MsgDispatcher (struct message *);
	static void DeltaUpdate (time *, int);
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
	static void SetupNotify ();
	static void Go ();
	static int DoXevent ();
	static void ChangeAlt ();
	static void ClearStatus ();
	int MsgDispatcher ();
	static void DeltaUpdate ();
# endif


main (argc, argv)
int	argc;
char	**argv;
{
	SValue	v;
	int	i;
	char	loadfile[100];

	msg_connect (MsgDispatcher, "Ac_Status");

	msg_ELog (EF_DEBUG, "Ac_Status begins...");

	fixdir ("ACSLOADFILE", LIBDIR, "ac_status.lf", loadfile);

	if (argc > 1)
	{
		ui_init (loadfile, FALSE, TRUE);
		v.us_v_ptr = argv[1];
		usy_s_symbol (usy_g_stbl ("ui$variable_table"), "commandfile",
			SYMT_STRING, &v);
	}
	else
		ui_init (loadfile, TRUE, FALSE);

	ui_setup ("acstatus", &argc, argv, NULL);

	ds_Initialize ();	
	SetupIndirect ();
	
	for (i = 0; i < MAXOURS; i++)
		SaveTime[i].ds_yymmdd = SaveTime[i].ds_hhmmss = 0;

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

	usy_c_indirect (vtable, "ac_platforms", &AcPlatforms, SYMT_STRING, 200);
	usy_c_indirect (vtable, "ac_ingest_name", &AcIngestName, SYMT_STRING,
		STRLEN);
}


static void
SetupNotify ()
/*
 * Arrange for data notifications for all aircraft platforms and for
 * an update alarm every 5 seconds
 */
{
	PlatformId	pid;
	char		platforms[200], *pnames[MAXAC];
	int		i, nplat;

	strcpy (platforms, AcPlatforms);
	msg_ELog (EF_DEBUG, "Aircraft platforms %s", platforms);
	nplat = CommaParse (platforms, pnames);
	msg_ELog (EF_DEBUG, "nplat = %d", nplat);
	for (i = 0; i < nplat; i++)
	{
		if ((pid = ds_LookupPlatform (pnames[i])) == BadPlatform)
		{
			msg_ELog (EF_DEBUG, "Bad platform '%s'.", pnames[i]);
			continue;
		}
		msg_ELog (EF_DEBUG, "Requesting notification for %s.",
			pnames[i]);
		ds_RequestNotify (pid, 0, Notification);
	}

	tl_AddRelativeEvent (DeltaUpdate, (void *) 0, 100, 100);
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
	char		*fields[1], platforms[200], *pnames[MAXAC];
	int		i, itsat = -1, numfields, nplat;
	time		curtime;

	strcpy (Platform, ds_PlatformName (pid));
	msg_ELog (EF_DEBUG, "Data available on %s at %d %d.", Platform,
		t->ds_yymmdd, t->ds_hhmmss);
/*
 * Make sure it's a platform we're still interested in.
 */
	strcpy (platforms, AcPlatforms);
	nplat = CommaParse (platforms, pnames);
	for (i = 0; i < nplat; i++)
		if (strcmp (Platform, pnames[i]) == 0)
		{
			itsat = i;
			break;
		}
	msg_ELog (EF_DEBUG, "itsat = %d", itsat);
/*
 * If it is then set the status label and save the time for future use.
 */
	if ((itsat >= 0) && (itsat < nplat))
	{
		AcTime = *t;
		tl_GetTime (&curtime);
		ud_sub_date (&curtime, &AcTime, &Delta);
		SaveTime[itsat] = AcTime;
		fields[0] = "trans";
		numfields = 1;
		if ((dobj = ds_GetData (pid, fields, numfields, t, t, 
			OrgScalar, 0.0, BADVAL)) == 0)
		{
			msg_ELog (EF_PROBLEM, "Get failed for %s.", Platform);
			return;
		}
		if (DoFeet)
			Aircraft.altitude = dobj->do_aloc->l_alt / M_PER_FT;
		else 
			Aircraft.altitude = dobj->do_aloc->l_alt;
		Aircraft.latitude = dobj->do_aloc->l_lat;
		Aircraft.longitude = dobj->do_aloc->l_lon;
		Aircraft.transponder = (int) dobj->do_data[0][0];
		ds_FreeDataObject (dobj);
		SetStatus(itsat, nplat);
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
	Display	*disp;
	int	fd;

	uw_def_widget ("acstatus", "acstatus", MakeStatusWidget, 0, 0);
	uw_ForceWindowMode ("acstatus", &Top, &Actx);

	disp = XtDisplay (Top);
	fd = XConnectionNumber (disp);

	SetupNotify ();

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
 * The Quit button.
 */
        n = 0;
        XtSetArg (args[n], XtNlabel, "Quit");           n++;
        XtSetArg (args[n], XtNfromHoriz, NULL);         n++;
        XtSetArg (args[n], XtNfromVert, NULL);          n++;
        above = XtCreateManagedWidget ("Quit", commandWidgetClass,
                parent, args, n);
        XtAddCallback (above, XtNcallback, (XtCallbackProc) Die, 0);
/*
 * The status title.
 */
	sprintf (TitleStr, "%-9s%-8s%-12s%-8s%-8s%-10s%-10s\n", "Platform",
		"Trans", "Altitude", "Lat", "Lon", "Time", "Delta");

	n = 0;
        XtSetArg (args[n], XtNfromHoriz, NULL);         n++;
        XtSetArg (args[n], XtNfromVert, above);         n++;
	XtSetArg (args[n], XtNlabel, TitleStr);		n++;
	XtSetArg (args[n], XtNjustify, XtJustifyLeft);	n++;
	XtSetArg (args[n], XtNwidth, 470);		n++;
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
	XtSetArg (args[n], XtNwidth, 480);		n++;
	XtSetArg (args[n], XtNheight, 80);		n++;
	XtSetArg (args[n], XtNresize, True);		n++;
	above = StatusLabel = XtCreateManagedWidget ("AcStatusT", 
		labelWidgetClass, parent, args, n);
/*
 * Feet vs. M buttom.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, DoFeet ? "Ft" : "M");	n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);			n++;
	XtSetArg (args[n], XtNfromVert, above);			n++;
	AltButton = XtCreateManagedWidget ("FtM", commandWidgetClass, 
		parent, args, n);
	XtAddCallback (AltButton, XtNcallback, ChangeAlt, 0);	
/*
 * Clear button.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Clear");		n++;
	XtSetArg (args[n], XtNfromHoriz, AltButton);	n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	above = ClrButton = XtCreateManagedWidget ("clear", commandWidgetClass, 
		parent, args, n);
	XtAddCallback (ClrButton, XtNcallback, ClearStatus, 0);	
/*
 * The instructions line.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	XtSetArg (args[n], XtNwidth, 480);		n++;
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
	XtSetArg (args[n], XtNwidth, 480);		n++;
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
	XtSetArg (args[0], XtNlabel, DoFeet ? "Ft" : "M");
	XtSetValues (AltButton, args, 1);
}


static void
ClearStatus ()
/*
 * Clear the status display.
 */
{
	int i;

	for (i = 0; i < MAXOURS; i++)
		StatusStr[i][0] = '\0';
	SetLabel (StatusLabel, "");
}


static void
Add_Trans()
/*
 * Add a new platform and its transponder code the the ones we are
 * interested in.
 */
{
	PlatformId	pid;
	char		*pname[MAXAC], temptrans[ATSLEN], sendstr[STRLEN];
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
 * Tell ac_ingest about the addition.
 */
	sprintf (sendstr, "a %s,%s", pname[0], pname[1]);
	msg_send (AcIngestName, MT_ACINGEST, FALSE, sendstr, 
		sizeof (sendstr));
}


static void
Change_Trans ()
/*
 * Change the transponder code of one of the platforms we are interested in.
 */
{
	PlatformId	pid;
	char		*pname[MAXAC], temptrans[ATSLEN];
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
 * Tell ac_ingest about the change.
 */
	sprintf (sendstr, "c %s,%s", pname[0], pname[1]);
	msg_send (AcIngestName, MT_ACINGEST, FALSE, sendstr, sizeof (sendstr));
}


static void
Del_Trans ()
/*
 * Delete one of the platforms we are interested in.
 */
{
	PlatformId	pid;
	char		*pname[MAXAC], temptrans[ATSLEN];
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
 * Tell ac_ingest about the deletion.
 */
	sprintf (sendstr, "d %s,%s", pname[0], pname[1]);
	msg_send (AcIngestName, MT_ACINGEST, FALSE, sendstr, 
		sizeof (sendstr));
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
SetStatus (itsat, nplat)
int	itsat;
/*
 * Set the status widget.
 */
{
	char	string[200];
	int	i;

	if (! SWMade)
		return;

	sprintf(string,"%-9s%6o%10.0f%8.2f%8.2f%4d:%02d:%02d%4d:%02d:%02d\n",
		Platform, Aircraft.transponder, Aircraft.altitude, 
		Aircraft.latitude, Aircraft.longitude, 
		AcTime.ds_hhmmss/10000, (AcTime.ds_hhmmss/100) % 100,
		AcTime.ds_hhmmss % 100, Delta.ds_hhmmss/10000,
		(Delta.ds_hhmmss/100) % 100, Delta.ds_hhmmss % 100);
	strncpy (StatusStr[itsat], string, strlen (string));
	LabelString[0] = '\0';
	for (i = 0; i < nplat; i++)
		strcat (LabelString, StatusStr[i]);
	SetLabel (StatusLabel, LabelString);
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


int
MsgDispatcher (msg)
struct message	*msg;
/*
 * Deal with a message.
 */
{
	struct mh_template *tmpl = (struct mh_template *) msg->m_data;

	switch (msg->m_proto)
	{
		case MT_MESSAGE:
			if (tmpl->mh_type == MH_DIE)
				Die ();
			break;
		case MT_ACINGEST:
			switch (msg->m_data[0])
			{
				case 'a':
					SetLabel (TransLabel, "Added.");
					break;
				case 'c':
					SetLabel (TransLabel, "Changed.");
					break;
				case 'd':
					SetLabel (TransLabel,"Deleted.");
					break;
				case 'n':
					SetLabel (TransLabel,
						"Can't add: Already added.");
					break;
				case 'm':
					SetLabel (TransLabel,
						"Can't change: Not added.");
					break;
				case 'o':
					SetLabel (TransLabel,
						"Can't delete: Not added.");
					break;
				default:
					msg_ELog (EF_PROBLEM, 
					"Unknown transponder command.");
					break;
			}
			break;
		default:
			msg_ELog (EF_DEBUG, "What's this %d?", msg->m_proto);
			break;
	}
	return (0);
}




void
DeltaUpdate (t, junk)
time	*t;
int	junk;
/*
 * Update the delta times
 */
{
	int	i;
	time	curtime, delta;

	tl_GetTime (&curtime);
/*
 * Update the delta for all of the interesting platforms
 */
	LabelString[0] = '\0';

	for (i = 0; i < MAXOURS; i++)
	{
		if (! *StatusStr[i])
			continue;

		ud_sub_date (&curtime, &SaveTime[i], &delta);

		sprintf (StatusStr[i] + 51, "%4d:%02d:%02d\n",
			delta.ds_hhmmss/10000, (delta.ds_hhmmss/100) % 100, 
			delta.ds_hhmmss % 100);

		strcat (LabelString, StatusStr[i]);
	}

	SetLabel (StatusLabel, LabelString);
	DoXevent ();
}
