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
# include <string.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/SimpleMenu.h>
# include <X11/Xaw/SmeBSB.h>
# include <X11/Shell.h>
# include <X11/Xaw/SmeLine.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Viewport.h>

# include <ui.h>
# include <ui_date.h>

# include <defs.h>
# include <copyright.h>
# include <message.h>
# include <pd.h>
# include <ds_fields.h>
# include <DataStore.h>
# include "GraphProc.h"

RCSID ("$Id: FieldMenu.c,v 2.21 1998-11-20 16:08:54 burghart Exp $")

/*
 * Stuff for the SME menu.
 */
# define MAXENTRY 200
static Widget Menu, Entries[MAXENTRY];
static char Platform[PlatformListLen];	/* Platform of interest	*/
static FieldId Fields[MAXENTRY];
static int NManaged, NField, NExtra;
static char Extras[256], *PExtras[20];

static stbl VTable;

static char IComp[60];

static void EntryCallback FP ((Widget, XtPointer, XtPointer));
static void PopupCallback FP ((Widget, XtPointer, XtPointer));
static int SetupFields FP ((void));
static int Funky FP ((char *));
/*
 * Field chooser stuff.
 */
static void InitFChooser FP ((void));
static Widget FC_Create FP ((char *, Widget, XtAppContext));
static void FC_Callback FP ((Widget, XtPointer, XtPointer));

/*
 * XXX Ugly snarfed out of ui_wPulldown.c.
 */
static int	Star_width = 16, Star_height = 16;
static unsigned char Star_bits[] = {
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
/*
 * Also initialize the file chooser.
 */
	InitFChooser ();
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
	char cbuf[512], *fname;
/*
 * See which field name we should use.
 */
	fname = (which < (NManaged - NExtra)) ? F_GetFullName (Fields[which]) :
		PExtras[which + NExtra - NManaged];
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
	sprintf (cbuf + strlen (cbuf), " '%s'", fname);

	msg_ELog (EF_DEBUG, "FMenu cmd '%s'", cbuf);
	ui_perform (cbuf);
}



static void
PopupCallback (w, junk, junk1)
Widget w;
XtPointer junk, junk1;
/*
 * We're being popped up.  Set the entries accordingly.  At the time the
 * callback is called, the popup-shell window has been sized, located, and
 * mapped, but the shell widget has not been realized or raised.  Since our
 * size will change depending upon the entries, we must re-calculate our
 * size and location to fit on the screen.
 */
{
	int nentry, i, type;
	Arg args[2];
	char string[256], field[FieldListLen];
	SValue v;
/*
 * Get the platforms set.
 */
	NField = nentry = SetupFields ();
/*
 * If we don't have the star pixmap, get it now.
 */
	if (! Star)
		Star = XCreateBitmapFromData (Disp, XtWindow (Menu), 
		      (const char *)Star_bits, Star_width, Star_height);
	pd_Retrieve (Pd, IComp, "field", field, SYMT_STRING);
/*
 * If they have additional junk to add, find out about it now.
 */
	NExtra = 0;
	usy_g_symbol (Vtable, "area_type", &type, &v);
	sprintf (string, "%s-field-menu-extras", v.us_v_ptr);
	if (pda_Search (Pd, IComp, string, Platform, Extras, SYMT_STRING) ||
	    pda_Search (Pd, IComp, "field-menu-extras", Platform, Extras,
			SYMT_STRING))
	{
		NExtra = ParseFieldList (Extras, PExtras);
	}
/*
 * Go through and make the labels for each one.
 */
	for (i = 0; (i < nentry) && (i < MAXENTRY - NExtra); i++)
	{
		char *name = F_GetName (Fields[i]);
		char *units = F_GetUnits (Fields[i]);
	/*
	 * Add the text.
	 */
		strncpy (string, F_GetDesc (Fields[i]), 60);
		string[60] = 0;
		if (strcmp (string, name))
			sprintf (string + strlen (string), " (%s)", name);
		if (strcmp (units, "none"))
			sprintf (string + strlen (string), " (%s)", units);
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
	nentry = i; 	/* in case we were cut off by MAXENTRY */
/*
 * Add the extra entires
 */
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
/*
 * Clean out extras if need be.
 */
 	for (i = nentry; i < NManaged; i++)
		XtUnmanageChild (Entries[i]);
	NManaged = nentry;
/*
 * Re-position the pop-up shell now that our size is correct.
 */
	I_RepositionMenu (w);
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
	if ((comma = strchr (Platform, ',')))
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
 * Send notice of the given gripe.
 */
{
	msg_ELog (EF_PROBLEM, "Field menu problem: %s", s);
	return (0);
}




/*
 * Stuff from here on down is specific to the "field chooser" widget --
 * a popup, stay-around version of the field menu.
 */




static void
InitFChooser ()
/*
 * Initialize the field chooser widget.
 */
{
	char title[80];

	sprintf (title, "Field chooser for %s", dm_WindowName ());
	uw_def_widget ("fchooser", title, FC_Create, 0, 0);
}




static Widget
FC_Create (junk, parent, appc)
char *junk;
Widget parent;
XtAppContext appc;
/*
 * Create the field chooser.
 */
{
	Arg args[15];
	int n, field;
	Widget vp, vpform, above;

	NField = SetupFields ();
/*
 * We know that UI will create us with a Form widget parent and a TopLevelShell
 * as a grandparent.  We *need* a resizable shell, so we force it..
 */
	n = 0;
	XtSetArg (args[n], XtNallowShellResize, True); n++;
	XtSetValues (XtParent(parent), args, n);

	n = 0;
 	XtSetArg (args[n], XtNdefaultDistance, 5); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetValues (parent, args, n);
/*
 * Make a viewport to hold all the buttons.
 */
	n = 0;
	XtSetArg (args[n], XtNallowVert, True);			n++;
	XtSetArg (args[n], XtNheight, 300);			n++;
	XtSetArg (args[n], XtNwidth, 450);			n++;
/*	XtSetArg (args[n], XtNfromVert, above);			n++; */
	vp = XtCreateManagedWidget ("pvp", viewportWidgetClass, parent,
			args, n);
	vpform = XtCreateManagedWidget ("pform", formWidgetClass, vp, 0, 0);
/*
 * Lotsa buttons.
 */
	above = NULL;
	for (field = 0; field < NField; field++)
	{
		char *name = F_GetName (Fields[field]);
		char *units = F_GetUnits (Fields[field]);
		char string[120];
	/*
	 * Format up the label.
	 */
		strncpy (string, F_GetDesc (Fields[field]), 60);
		string[60] = 0;
		if (strcmp (string, name))
			sprintf (string + strlen (string), " (%s)", name);
		if (strcmp (units, "none"))
			sprintf (string + strlen (string), " (%s)", units);
	/*
	 * Put together the args and make the button.
	 */
		n = 0;
		XtSetArg (args[n], XtNlabel, string);		n++;
		XtSetArg (args[n], XtNfromVert, above);		n++;
		XtSetArg (args[n], XtNwidth, 300);		n++;
		XtSetArg (args[n], XtNjustify, XtJustifyLeft);	n++;
		XtSetArg (args[n], XtNborderWidth, 0);		n++;
		above = XtCreateManagedWidget (name, commandWidgetClass,
				vpform, args, n);
		XtAddCallback (above, XtNcallback, FC_Callback,
				(XtPointer) name);
	}
/*
 * Done.
 */
	return (vp);
}
		
		



static void
FC_Callback (w, xfield, junk)
Widget w;
XtPointer xfield, junk;
/*
 * They have punched a button.
 */
{
	char *field = (char *) xfield, cbuf[512];
/*
 * Look and see if there is a command to execute.  If not, just set the
 * field name and be done with it.
 */
	if (! pda_Search (Pd, IComp, "field-select-command", Platform, 
			  cbuf, SYMT_STRING))
		parameter (IComp, "field", field);
/*
 * OK, format up the command and set it loose.
 */
	else
	{
	    sprintf (cbuf + strlen (cbuf), " '%s'", field);
	    msg_ELog (EF_DEBUG, "FChooser cmd '%s'", cbuf);
	    ui_perform (cbuf);
	}
}
