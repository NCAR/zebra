/*
 * The data available menu.
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
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/SimpleMenu.h>
# include <X11/Xaw/SmeBSB.h>
# include <X11/Shell.h>
# include <X11/Xaw/SmeLine.h>

# include <ui.h>
# include <defs.h>
# include <message.h>
# include <timer.h>
# include <pd.h>
# include <DataStore.h>
# include <ui_date.h>
# include "GraphProc.h"

RCSID ("$Id: DataMenu.c,v 2.17 1996-11-19 07:28:43 granger Exp $")


/*
 * Stuff for the SME menu.
 */
# define MAXENTRY 20
static Widget Menu, Entries[MAXENTRY];
static ZebTime Times[MAXENTRY];
static char EPlats[40][MAXENTRY];
static int NManaged;

static stbl VTable;

static char IComp[60];

static void EntryCallback FP ((Widget, XtPointer, XtPointer));
static void PopupCallback FP ((Widget, XtPointer, XtPointer));
static int SetupPlats FP ((void));
static int FunkyPlat FP ((char *));
static int AddPlatform FP ((char *, int, ZebTime *));
static void ToRealTime FP ((Widget, XtPointer, XtPointer));



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
	XtAddCallback (Menu, XtNpopupCallback, (XtCallbackProc) PopupCallback, 
		(XtPointer) Menu);
	XtCreateManagedWidget ("Line", smeLineObjectClass, Menu, NULL, 0);
/*
 * Real time mode.
 */
	XtSetArg (args[0], XtNlabel, "Real time mode");
	rt = XtCreateManagedWidget ("realtime", smeBSBObjectClass, Menu,
		args, 1);
	XtAddCallback (rt, XtNcallback, (XtCallbackProc) ToRealTime, 
		(XtPointer) 0);
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
		XtAddCallback (Entries[i], XtNcallback, 
			(XtCallbackProc) EntryCallback, (XtPointer) i);
	}
	NManaged = 0;
}






/* ARGSUSED */
static void
EntryCallback (w, xwhich, junk)
Widget w;
XtPointer xwhich, junk;
/*
 * One of the entries has been selected.
 */
{
	int which = (int) xwhich;
	char cbuf[200];
	char *qual;
	UItime uitime;
/*
 * Here we just put together the command and go.  Start by searching for
 * a command to execute.
 */
# ifdef notdef
	if (! pd_Retrieve (Pd, IComp, "platform", plat, SYMT_STRING))
		qual = NULL;
# endif
	qual = EPlats[which];
	if (! pda_Search (Pd, IComp, "data-available-command", qual, cbuf,
			  SYMT_STRING)  &&
	    ! pda_Search (Pd, IComp, "data-available-command", NULL, cbuf,
			  SYMT_STRING))
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
	TC_ZtToUI (Times + which, &uitime);
	ud_format_date (cbuf + strlen (cbuf), &uitime, UDF_FULL);

	msg_ELog (EF_DEBUG, "DAvail cmd '%s'", cbuf);
	ui_perform (cbuf);
}





static void
PopupCallback (w, junk, junk1)
Widget w;
XtPointer junk, junk1;
/*
 * We're being popped up.  Set the entries accordingly.
 */
{
	int nentry, i;
	Arg args[2];
	char string[80];
	UItime uitime;
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
		TC_ZtToUI (Times + i, &uitime);
		ud_format_date (string + 15, &uitime, UDF_FULL);
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
/*
 * Reposition the menu based on our new size.
 */
	I_RepositionMenu (w);
}





static int
SetupPlats ()
/*
 * Figure out what should appear in this menu where.
 */
{
	char platform[PlatformListLen];
	char *plats[MaxPlatforms], adjust_str[40];
	SValue v;
	int type, nplat, nentry = 0, plat, adjust;
	ZebTime t;
/*
 * See which is our component and platform.
 */
	if (! usy_g_symbol (VTable, "icon_component", &type, &v))
		return (FunkyPlat ("no icon component"));
	strcpy (IComp, v.us_v_ptr);
	if (! pd_Retrieve (Pd, IComp, "menu-platform", platform,SYMT_STRING) &&
		  ! pd_Retrieve (Pd, IComp, "platform", platform, SYMT_STRING))
		return (FunkyPlat ("No platform or menu-platform"));
/*
 * Split things apart.
 */
	nplat = CommaParse (platform, plats);
/*
 * Add stuff from each platform.
 */
	if (PostProcMode)
	{
		t = PostProcTime;
		if (! pda_Search (Pd, "global", "pp-dm-time-adjust", NULL,
			adjust_str, SYMT_STRING))
				adjust = 600;
		else if ((adjust = pc_TimeTrigger (adjust_str)) == 0)  
		{
			msg_ELog (EF_PROBLEM, "Unparsable time adjustment %s",
				adjust_str);
			adjust = 600;
		}
# ifdef notdef
		dsadjust = (adjust/3600)*10000 + ((adjust/60) % 60)*100 +
			adjust % 60;
		pmu_dadd (&t.ds_yymmdd, &t.ds_hhmmss, dsadjust);
# endif
		t.zt_Sec += adjust;
	}
	else
		tl_Time (&t);
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
char	*plat;
int	nsofar;
ZebTime *t;
/*
 * Try to add stuff from this plat.
 */
{
	ZebTime dtimes[MAXENTRY];
	PlatformId pid = ds_LookupPlatform (plat);
	int dt, ntime, i, ent;
	char *attr = NULL, cattr[200];
/*
 * Check out our platform.
 */
	if (pid == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad platform %s", plat);
		return (0);
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
			if (Times[ent].zt_Sec < dtimes[dt].zt_Sec)
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





/* ARGSUSED */
static void
ToRealTime (w, xwhich, junk)
Widget w;
XtPointer xwhich, junk;
/*
 * Put this window into real time mode.
 */
{
	int	which = (int) xwhich;
	char	cbuf[100], *plat;

	parameter ("global", "plot-mode", "real-time");
/*
 * Allow for user-defined stuff when we go into real time mode
 */
	plat = EPlats[which];
	if (pda_Search (Pd, IComp, "real-time-hook", plat, cbuf, SYMT_STRING))
	{
		msg_ELog (EF_DEBUG, "Real-time hook: %s", cbuf);
		ui_perform (cbuf);
		return;
	}
}
