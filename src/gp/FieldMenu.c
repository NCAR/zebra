/*
 * The available field menu.  This is a carved up version of the data 
 * available menu....
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
# include <string.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/SimpleMenu.h>
# include <X11/Xaw/SmeBSB.h>
# include <X11/Shell.h>
# include <X11/Xaw/SmeLine.h>

# include <defs.h>
# include <message.h>
# include <pd.h>
# include <ds_fields.h>
# include <DataStore.h>
# include <ui_date.h>
# include "GraphProc.h"
MAKE_RCSID ("$Id: FieldMenu.c,v 2.10 1994-05-20 20:04:25 corbet Exp $")


/*
 * Stuff for the SME menu.
 */
# define MAXENTRY 100
static Widget Menu, Entries[MAXENTRY];
static char Platform[40];	/* Platform of interest	*/
static FieldId Fields[MAXENTRY];
static int NManaged, NField, NExtra;
static char Extras[256], *PExtras[20];

static stbl VTable;

static char IComp[60];

static void EntryCallback FP ((Widget, XtPointer, XtPointer));
static void PopupCallback FP ((Widget, XtPointer, XtPointer));
static int SetupFields FP ((void));
static int Funky FP ((char *));
static int AddPlatform FP ((char *, int, ZebTime *));
static void ToRealTime FP ((Widget, XtPointer, XtPointer));

/*
 * XXX Ugly snarfed out of ui_wPulldown.c.
 */
static int	Star_width = 16, Star_height = 16;
unsigned static char Star_bits[] = {
	0x00, 0x00, 0x80, 0x00, 0x80, 0x00, 0x88, 0x08, 0x90, 0x04, 
	0xa0, 0x02, 0x40, 0x01, 0x3e, 0x3e, 0x40, 0x01, 0xa0, 0x02, 
	0x90, 0x04, 0x88, 0x08, 0x80, 0x00, 0x80, 0x00, 0x00, 0x00, 
	0x00, 0x00};
static Pixmap Star;


void
InitFieldMenu ()
/*
 * Get the field menu set up.
 */
{
	Arg args[2];
	int i, n;
	Widget rt;

	VTable = usy_g_stbl ("ui$variable_table");
/*
 * Create a shell for the thing.
 */
	XtSetArg (args[0], XtNlabel, "Field selection menu");
	Menu = XtCreatePopupShell ("FieldMenu", simpleMenuWidgetClass,
		Top, args, 1);
	XtAddCallback (Menu, XtNpopupCallback, (XtCallbackProc) PopupCallback, 
		(XtPointer) Menu);
	XtCreateManagedWidget ("Line", smeLineObjectClass, Menu, NULL, 0);
/*
 * Create all of the entries, but don't manage them now.
 */
	n = 0;
	XtSetArg (args[0], XtNlabel, "(nuttin)");	n++;
	XtSetArg (args[1], XtNleftMargin, 20);		n++;
	for (i = 0; i < MAXENTRY; i++)
	{
		Entries[i] = XtCreateWidget ("DAEntry", smeBSBObjectClass,
			Menu, args, n);
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
	char cbuf[200], *fname;
	UItime uitime;
/*
 * See which field name we should use.
 */
	fname = (which < NField) ? F_GetName (Fields[which]) :
		PExtras[which - NField];
/*
 * Here we just put together the command and go.  Start by searching for
 * a command to execute.
 */
	if (! pda_Search (Pd, IComp, "field-select-command", Platform, 
			cbuf, SYMT_STRING))
	{
		parameter (IComp, "field", fname);
		return;
	}
/*
 * Now format the rest of the command with the field.
 */
	strcat (cbuf, " ");
	strcpy (cbuf + strlen (cbuf), fname);

	msg_ELog (EF_DEBUG, "FMenu cmd '%s'", cbuf);
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
	int nentry, i, type;
	Arg args[2];
	char string[80], field[40];
	SValue v;
/*
 * Get the platforms set.
 */
	NField = nentry = SetupFields ();
/*
 * If we don't have the star pixmap, get it now.
 */
	if (! Star)
		Star = XCreateBitmapFromData (Disp, XtWindow (Menu), Star_bits,
			Star_width, Star_height);
	pd_Retrieve (Pd, IComp, "field", field, SYMT_STRING);
/*
 * Go through and make the labels for each one.
 */
	for (i = 0; i < nentry; i++)
	{
		char *name = F_GetName (Fields[i]);
	/*
	 * Add the text.
	 */
		sprintf (string, "%s", F_GetDesc (Fields[i]));
		if (strcmp (string, name))
			sprintf (string + strlen (string), " (%s)", name);
		XtSetArg (args[0], XtNlabel, string);
		XtSetArg (args[1], XtNleftBitmap, 
			strcmp (field, name) ? None : Star);
		XtSetValues (Entries[i], args, 2);
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
 * If they have additional junk to add, do it now.
 */
	usy_g_symbol (Vtable, "area_type", &type, &v);
	sprintf (string, "%s-field-menu-extras", v.us_v_ptr);
	if (pda_Search (Pd, IComp, string, Platform, Extras, SYMT_STRING) ||
	    pda_Search (Pd, IComp, "field-menu-extras", Platform, Extras,
			SYMT_STRING))
	{
		NExtra = CommaParse (Extras, PExtras);
		for (i = 0; i < NExtra; i++)
		{
			char *bar = strchr (PExtras[i], '|');
		/*
		 * Set up the entry text.
		 */
			if (bar)
			{
				*bar++ = '\0';
				sprintf (string, "%s (%s)", bar, PExtras[i]);
			}
			else
				strcpy (string, PExtras[i]);
			XtSetArg (args[0], XtNlabel, string);
			XtSetArg (args[1], XtNleftBitmap, None);
			XtSetValues (Entries[i + nentry], args, 2);
		/*
		 * If this one isn't managed yet, make it so now.
		 */
			if ((i + nentry) >= NManaged)
			{
				XtManageChild (Entries[i + nentry]);
				NManaged++;
			}
		}
		nentry += NExtra;
	}
/*
 * Clean out extras if need be.
 */
 	for (i = nentry; i < NManaged; i++)
		XtUnmanageChild (Entries[i]);
	NManaged = nentry;
}





static int
SetupFields ()
/*
 * Figure out what should appear in this menu where.
 */
{
	SValue v;
	int type, nfield = MAXENTRY;
	PlatformId pid;
	char *comma;
/*
 * See which is our component and platform.
 */
	if (! usy_g_symbol (VTable, "icon_component", &type, &v))
		return (Funky ("no icon component"));
	strcpy (IComp, v.us_v_ptr);
/*
 * Assume that if our area type is not "icon" we need to see what our
 * real platform should be.
 */
	usy_g_symbol (VTable, "area_type", &type, &v);
	if (! strcmp (v.us_v_ptr, "icon"))
	{
		if (! pd_Retrieve (Pd, IComp, "platform", Platform,
				SYMT_STRING))
			return (Funky ("No platform!"));
	}
	else
	{
		usy_g_symbol (VTable, "icon_platform", &type, &v);
		strcpy (Platform, v.us_v_ptr);
	/*
	 * XXX: If we're running in the station plot annotation area, it
	 * 	has "quadN" kludged into the platform field.  Kludge our
	 *	way back to the real platform.  Ugly.
	 */
		if (! strncmp (Platform, "quad", 4) && Platform[5] == '\0')
			pd_Retrieve (Pd, IComp, "platform", Platform,
					SYMT_STRING);
	}
/*
 * If we are dealing with a comma-separated list of platforms, just
 * use the first.  This may not be the ideal behavior, but then, what is?
 */
	if (comma = strchr (Platform, ','))
		*comma = '\0';
/*
 * Query the fields that are available.
 */
	if ((pid = ds_LookupPlatform (Platform)) == BadPlatform)
		return (Funky ("Bad platform name"));
	ds_GetFields (pid, &PlotTime, &nfield, Fields);
	return (nfield);
}





static int
Funky (s)
char *s;
/*
 * Return such that the given gripe appears in the menu.
 */
{
	msg_ELog (EF_PROBLEM, "Field menu problem: %s", s);
	return (0);
}
