/*
 * The data available menu.
 */
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/SimpleMenu.h>
# include <X11/Xaw/SmeBSB.h>
# include <X11/Shell.h>
# include <X11/Xaw/SmeLine.h>

# include <defs.h>
# include <message.h>
# include <pd.h>
# include <DataStore.h>
# include <ui_date.h>
# include "GraphProc.h"


/*
 * Stuff for the SME menu.
 */
# define MAXENTRY 20
static Widget Menu, Entries[MAXENTRY];
static time Times[MAXENTRY];
static char EPlats[40][MAXENTRY];
static int NManaged;

static stbl VTable;

static char IComp[60];

# ifdef __STDC__
	static void EntryCallback (Widget, int, char *);
	static void PopupCallback (Widget, char *, char *);
	static int SetupPlats (void);
	static int FunkyPlat (char *);
	static int AddPlatform (char *, int, time *);
	static void ToRealTime (void);
# else
# endif



void
InitDataMenu ()
/*
 * Get the data menu set up.
 */
{
	Arg args[2];
	int i, n;
	Widget rt;

	VTable = usy_g_stbl ("ui$variable_table");
/*
 * Create a shell for the thing.
 */
	XtSetArg (args[0], XtNlabel, "Data available menu");
	Menu = XtCreatePopupShell ("DataAvailable", simpleMenuWidgetClass,
		Top, args, 1);
	XtAddCallback (Menu, XtNpopupCallback, PopupCallback, Menu);
	XtCreateManagedWidget ("Line", smeLineObjectClass, Menu, NULL, 0);
/*
 * Real time mode.
 */
	XtSetArg (args[0], XtNlabel, "Real time mode");
	rt = XtCreateManagedWidget ("realtime", smeBSBObjectClass, Menu,
		args, 1);
	XtAddCallback (rt, XtNcallback, ToRealTime, 0);
	XtCreateManagedWidget ("Line", smeLineObjectClass, Menu, NULL, 0);
/*
 * Create all of the entries, but don't manage them now.
 */
	n = 0;
	XtSetArg (args[0], XtNlabel, "(nuttin)");	n++;
	for (i = 0; i < MAXENTRY; i++)
	{
		Entries[i] = XtCreateWidget ("DAEntry", smeBSBObjectClass,
			Menu, args, 1);
		XtAddCallback (Entries[i], XtNcallback, EntryCallback, i);
	}
	NManaged = 0;
}






static void
EntryCallback (w, which, junk)
Widget w;
int which;
char *junk;
/*
 * One of the entries has been selected.
 */
{
	SValue v;
	int type;
	char cbuf[200];
	char *qual;
/*
 * Here we just put together the command and go.  Start by searching for
 * a command to execute.
 */
# ifdef notdef
	if (! pd_Retrieve (Pd, IComp, "platform", plat, SYMT_STRING))
		qual = NULL;
# endif
	qual = EPlats[which];
	if (! pda_Search (Pd, IComp, "data-available-command", qual, 
			cbuf, SYMT_STRING))
	{
		msg_ELog (EF_PROBLEM, "No command for dataAvailable");
		return;
	}
/*
 * Now format the rest of the command with the platform and time.
 */
	strcat (cbuf, " ");
	strcpy (cbuf + strlen (cbuf), EPlats[which]);
	strcat (cbuf, " ");
	ud_format_date (cbuf + strlen (cbuf), Times + which, UDF_FULL);

	msg_ELog (EF_DEBUG, "DAvail cmd '%s'", cbuf);
	ui_perform (cbuf);
}





static void
PopupCallback (w, junk, junk1)
Widget w;
char *junk, *junk1;
/*
 * We're being popped up.  Set the entries accordingly.
 */
{
	int nentry, i;
	Arg args[2];
	char string[80];
/*
 * Get the platforms set.
 */
	nentry = SetupPlats ();
/*
 * Go through and make the labels for each one.
 */
	for (i = 0; i < nentry; i++)
	{
	/*
	 * Add the text.
	 */
		sprintf (string, "%-15s ", EPlats[i]);
		ud_format_date (string + 15, Times + i, UDF_FULL);
		XtSetArg (args[0], XtNlabel, string);
		XtSetValues (Entries[i], args, 1);
	/*
	 * If this one isn't managed yet, make it so now.
	 */
	 	if (i >= NManaged)
		{
			XtManageChild (Entries[i]);
			NManaged++;
		}
	}
/*
 * Clean out extras if need be.
 */
 	for (i = nentry; i < NManaged; i++)
		XtUnmanageChild (Entries[i]);
	NManaged = nentry;
}





static int
SetupPlats ()
/*
 * Figure out what should appear in this menu where.
 */
{
	char platform[80], *plats[MAXENTRY];
	SValue v;
	int type, nplat, nentry = 0, plat;
	time t;
/*
 * See which is our component and platform.
 */
	if (! usy_g_symbol (VTable, "icon_component", &type, &v))
		return (FunkyPlat ("no icon component"));
	strcpy (IComp, v.us_v_ptr);
	if (! pd_Retrieve (Pd, IComp, "menu-platform", platform,SYMT_STRING) &&
		  ! pd_Retrieve (Pd, IComp, "platform", platform, SYMT_STRING))
		return (FunkyPlat ("No platform"));
/*
 * Split things apart.
 */
	nplat = CommaParse (platform, plats);
/*
 * Add stuff from each platform.
 */
	tl_GetTime (&t);
	for (plat = 0; plat < nplat; plat++)
		nentry = AddPlatform (plats[plat], nentry, &t);
	return (nentry);
}





static int
FunkyPlat (s)
char *s;
/*
 * Return such that the given gripe appears in the menu.
 */
{
	strcpy (EPlats[0], s);
	return (1);
}





static int 
AddPlatform (plat, nsofar, t)
char *plat;
int nsofar;
time *t;
/*
 * Try to add stuff from this plat.
 */
{
	time dtimes[MAXENTRY];
	PlatformId pid = ds_LookupPlatform (plat);
	int dt, ntime, i, ent;
	char *attr = NULL, cattr[200];
/*
 * Check out our platform.
 */
	if (pid == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad platform %s", plat);
		return;
	}
/*
 * Check out filter attributes.
 */
	if (pda_Search (Pd, "global", "filter-attribute", plat, cattr,
						SYMT_STRING))
		attr = cattr;
/*
 * See what's available.
 */
	if ((ntime = ds_GetObsTimes (pid, t, dtimes, 2*MAXENTRY/3, attr)) <= 0)
		return (nsofar);
/*
 * Now sort them into the list.
 */
	ent = 0;
	for (dt = 0; dt < ntime && ent < MAXENTRY; dt++)
	{
	/*
	 * Search forward through the existing entries for the first one
	 * which is before this time.
	 */
	 	for (; ent < nsofar; ent++)
			if (DLT (Times[ent], dtimes[dt]))
				break;

		if (ent == MAXENTRY)
			break;
	/*
	 * Move the rest of them forward.
	 */
		if (nsofar < MAXENTRY)
			nsofar++;
		for (i = (nsofar >= MAXENTRY) ? MAXENTRY - 2 : nsofar - 1;
				i > ent; i--)
		{
			Times[i] = Times[i - 1];
			strcpy (EPlats[i], EPlats[i - 1]);
		}
	/*
	 * Store this one.
	 */
	 	Times[ent] = dtimes[dt];
		strcpy (EPlats[ent], plat);
		ent++;
	}
	return (nsofar);
}






static void
ToRealTime ()
/*
 * Put this window into real time mode.
 */
{
	parameter ("global", "plot-mode", "real-time");
}
