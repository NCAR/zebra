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
# include "FieldMenu.h"

RCSID ("$Id: FieldMenu.c,v 2.25 2001-04-24 23:42:25 granger Exp $")

/*
 * Establish a single interface style for dynamic field selection
 * widgets and menus.  The specification follows the form of a 
 * comma-separated list of key=value pairs, so that parameters can be
 * easily added and only those relevant need to be specified.  This
 * specification line is parsed before popping up the FieldMenu or the
 * field selection shell.
 *
 * Here are the parameters so far:
 *
 * param	The field parameter to change, default is 'field'
 * extras	The extras string, which maintains its original format
 *		of 'field pseudo-name|menu entry label,...'
 * command      Field selection command which overrides the default action
 *		using any specified 'fp' value.
 * title	The title to use for the menu, else its 'Select <param>'
 *
 * The same spec string can be used for both the FieldMenu(<spec>)
 * call from an icon menu and for the selectfield ui command.
 */


/*
 * Stuff for the SME FieldMenu.
 */
# define MAXMENU 50	/* maximum entries allowed in the popup field menu */
static Widget Menu, Entries[MAXMENU];
static int NManaged;
static fm_Context FieldMenuContext;

static struct fm_Callback FieldMenuCallbacks[FM_MAXENTRY];

static stbl VTable;

static void InitFieldMenu ();
static void PopupCallback FP ((Widget, XtPointer, XtPointer));
static int Funky FP ((char *));

/*
 * Field chooser stuff.
 */
static void InitFChooser FP ((void));
static Widget FC_Create FP ((char *, Widget, XtAppContext));
static void fm_EntryCallback FP ((Widget, XtPointer, XtPointer));
static void fm_PopupSelector (Widget w, XtPointer xfield, XtPointer junk);

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


static void
fm_ParseMenuSpec (fm_Context *fmc, const char *inspec)
{
    char spec[256];
    char *parms[32];
    char *values[32];
    int nparms;
    int ngood;
    int i;

    /* preserve the input string before parsing it */
    strcpy (spec, inspec);
    nparms = CommaParse (spec, parms);

    /* loop through the parms splitting name and value, skipping
       any without an equal sign */
    ngood = 0;
    for (i = 0; i < nparms; ++i)
    {
	char *cp = strchr (parms[i], '=');
	if (cp)
	{
	    *cp = '\0';
	    values[ngood] = cp + 1;
	    parms[ngood] = parms[i];
	    ++ngood;
	}
    }
    nparms = ngood;

    /* now we can actually act on the parameters */
    for (i = 0; i < nparms; ++i)
    {
	if (strncmp (parms[i], "param", 5) == 0)
	{
	    strcpy (fmc->fparam, values[i]);
	}
	else if (strcmp (parms[i], "title") == 0)
	{
	    strcpy (fmc->title, values[i]);
	}
	else if (strncmp (parms[i], "extras", 5) == 0)
	{
	    strcpy (fmc->extras, values[i]);
	}
	else if (strncmp (parms[i], "command", 3) == 0)
	{
	    strcpy (fmc->fsc, values[i]);
	}
    }
}


/*
 * Parse a menu string and check whether it's one of our menus.  Store
 * the spec parameters in case the menu gets popped up, and return the
 * canonical menu name which should be used to pop up the menu.
 */
char *
fm_SetupFieldMenu (char *spec)
{
    if (strncmp (spec, "FieldMenu", 9))
	return 0;
    if (fm_GetContext (&FieldMenuContext, spec+9))
	return "FieldMenu";
    return 0;
}




void
fm_Init ()
{
    VTable = usy_g_stbl ("ui$variable_table");
    InitFieldMenu ();
/*
 * Also initialize the file chooser.
 */
    InitFChooser ();
}



static void
fm_InitBitmaps ()
{
    /*
     * If we don't have the star pixmap, get it now.
     */
    if (! Star)
	Star = XCreateBitmapFromData (Disp, XtWindow (Graphics), 
				      (const char *)Star_bits, 
				      Star_width, Star_height);
}



static void
InitFieldMenu ()
/*
 * Get the field menu set up.
 */
{
	Arg args[5];
	int i, n;
	Widget sme;
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
 * Always have an entry to popup the field selector on the current context.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Field Selector..."); ++n;
	sme = XtCreateManagedWidget ("popupfs", smeBSBObjectClass,
				     Menu, args, n);
	XtAddCallback (sme, XtNcallback, fm_PopupSelector, 0);
	XtCreateManagedWidget ("Line", smeLineObjectClass, Menu, NULL, 0);
/*
 * Create all of the entries, but don't manage them now.
 */
	n = 0;
	XtSetArg (args[0], XtNlabel, "(nuttin)");	n++;
	XtSetArg (args[1], XtNleftMargin, 20);		n++;
	for (i = 0; i < MAXMENU; i++)
	{
		Entries[i] = XtCreateWidget ("DAEntry", smeBSBObjectClass,
					     Menu, args, n);
	}
	NManaged = 0;
}



int
fm_NumEntries (fm_Context *fmc)
{
    return fmc->nfield + fmc->nextra;
}



/*
 * Given a context and a list of entry widgets, set the labels and
 * callbacks for each.  Return the number of entries setup.
 */
static int
fm_SetupEntries (fm_Context *fmc, struct fm_Callback *callbacks,
		 XtCallbackProc callback, 
		 Widget *entries, int maxentry)
{
    Arg args[10];
    int i;
    FieldId *fields = fmc->fields;
    char cbuf[256];
    int nentry;

    fm_InitBitmaps ();
    /*
     * Go through and make the labels for each one.
     */
    fmc->fentry = 0;
    nentry = 0;
    for (i = 0; (i < fmc->nfield) && (i < maxentry - fmc->nextra); i++)
    {
	char *name = F_GetName (fields[i]);
	char *fullname = F_GetFullName (fields[i]);
	char *units = F_GetUnits (fields[i]);
	int fld_is_displayed = 
	    ! strcmp (fmc->fcurrent, name) ||
	    ! strcmp (fmc->fcurrent, fullname);
	/*
	 * Add the text.
	 */
	strncpy (cbuf, F_GetDesc (fields[i]), 60);
	cbuf[60] = 0;
	if (strcmp (cbuf, name))
	    sprintf (cbuf + strlen (cbuf), " (%s)", name);
	if (strcmp (units, "none"))
	    sprintf (cbuf + strlen (cbuf), " (%s)", units);
	XtSetArg (args[0], XtNlabel, cbuf);
	XtSetArg (args[1], XtNleftBitmap, None);
	if (!fmc->fentry && fld_is_displayed)
	{
	    XtSetArg (args[1], XtNleftBitmap, Star);
	    fmc->fentry = entries[i];
	}
	XtSetValues (entries[i], args, 2);

	callbacks[i].fid = fields[i];
	callbacks[i].extra = 0;
	callbacks[i].refdata = 0;
	callbacks[i].fm_context = fmc;
	callbacks[i].i = i;
	XtRemoveAllCallbacks (entries[i], XtNcallback);
	XtAddCallback (entries[i], XtNcallback, callback,
		       (XtPointer) (callbacks + i));
	++nentry;
    }
    /*
     * Setup the extra entires
     */
    for (i = 0; i < fmc->nextra; i++)
    {
	char *bar = strchr (fmc->pextras[i], '|');
	/*
	 * Set up the entry text.
	 */
	if (bar)
	{
	    *bar++ = '\0';
	    sprintf (cbuf, "%s (%s)", bar, fmc->pextras[i]);
	}
	else
	    strcpy (cbuf, fmc->pextras[i]);
	XtSetArg (args[0], XtNlabel, cbuf);
	XtSetArg (args[1], XtNleftBitmap, None);
	if (!fmc->fentry && !strcmp (fmc->fcurrent, fmc->pextras[i]))
	{
	    XtSetArg (args[1], XtNleftBitmap, Star);
	    fmc->fentry = entries[nentry];
	}
	XtSetValues (entries[nentry], args, 2);

	callbacks[nentry].fid = BadField;
	callbacks[nentry].extra = fmc->pextras[i];
	callbacks[nentry].refdata = 0;
	callbacks[nentry].i = nentry;
	callbacks[nentry].fm_context = fmc;
	XtRemoveAllCallbacks (entries[nentry], XtNcallback);
	XtAddCallback (entries[nentry], XtNcallback, callback,
		       (XtPointer) (callbacks + nentry));
	++nentry;
    }
    return nentry;
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
    Arg args[5];
    int n;
    int nentry;
    fm_Context *fmc = &FieldMenuContext;

    n = 0;
    XtSetArg (args[n], XtNlabel, fmc->title); n++;
    XtSetValues (Menu, args, n);
/*
 * Go through and make the labels for each one.
 */
    nentry = fm_SetupEntries (fmc, FieldMenuCallbacks, fm_EntryCallback,
			      Entries, MAXMENU);
/*
 * Manage any that were not managed before, or clean out extras if need be.
 */
    if (nentry > NManaged)
    {
	XtManageChildren (Entries + NManaged, nentry - NManaged);
    }
    else if (NManaged > nentry)
    {
	XtUnmanageChildren (Entries + nentry, NManaged - nentry);
    }
    NManaged = nentry;
/*
 * Re-position the pop-up shell now that our size is correct.
 */
    I_RepositionMenu (w);
}




int
fm_GetContext (fm_Context *fmc, char *spec)
/*
 * Figure out what should appear in this menu where.
 * Return non-zero if successful.
 */
{
	SValue v;
	int type;
	PlatformId pid;
	char *comma;
	char buf[256];

/*
 * Initialization.
 */
	fmc->nfield = 0;
	strcpy (fmc->platform, "null");
	fmc->extras[0] = '\0';
	fmc->nextra = 0;
	strcpy (fmc->icomp, "unknown");
	strcpy (fmc->area_type, "unknown");
	strcpy (fmc->icon_platform, "unknown");
	fmc->fsc[0] = '\0';
	fmc->title[0] = '\0';
	strcpy (fmc->fparam, "field");
	strcpy (fmc->fcurrent, "unknown");
	fmc->fentry = 0;
/*
 * See which is our component and platform.
 */
	if (! usy_g_symbol (VTable, "icon_component", &type, &v))
	{
		Funky ("no icon component");
		return 0;
	}
	strcpy (fmc->icomp, v.us_v_ptr);
/*
 * Assume that if our area type is not "icon" we need to see what our
 * real platform should be.
 */
	usy_g_symbol (VTable, "area_type", &type, &v);
	strcpy (fmc->area_type, v.us_v_ptr);
	if (! strcmp (fmc->area_type, "icon"))
	{
		if (! pd_Retrieve (Pd, fmc->icomp, "platform", fmc->platform,
				   SYMT_STRING))
		{
		    Funky ("No platform!");
		    return 0;
		}
	}
	else
	{
		usy_g_symbol (VTable, "icon_platform", &type, &v);
		strcpy (fmc->icon_platform, v.us_v_ptr);
		strcpy (fmc->platform, v.us_v_ptr);
	/*
	 * XXX: If we're running in the station plot annotation area, it
	 * 	has "quadN" kludged into the platform field.  Kludge our
	 *	way back to the real platform.  Ugly.
	 */
		if (! strncmp (fmc->platform, "quad", 4) && 
		    fmc->platform[5] == '\0')
			pd_Retrieve (Pd, fmc->icomp, "platform", 
				     fmc->platform, SYMT_STRING);
	}
/*
 * If we are dealing with a comma-separated list of platforms, just
 * use the first.  This may not be the ideal behavior, but then, what is?
 */
	if ((comma = strchr (fmc->platform, ',')))
		*comma = '\0';
/*
 * If they have additional junk to add, find out about it now.
 */
	sprintf (buf, "%s-field-menu-extras", fmc->area_type);
	if (! pda_Search (Pd, fmc->icomp, buf, fmc->platform, 
			  fmc->extras, SYMT_STRING))
	{
	    pda_Search (Pd, fmc->icomp, "field-menu-extras", 
			fmc->platform, fmc->extras, SYMT_STRING);
	}
/*
 * Look for an explicit command to perform.
 */
	pda_Search (Pd, fmc->icomp, "field-select-command", 
		    fmc->platform, fmc->fsc, SYMT_STRING);
/*
 * Now load the spec string, if any, so that parameters there will override
 * any retrieved above.
 */
	if (spec)
	{
	    fm_ParseMenuSpec (fmc, spec);
	}
	fmc->nextra = ParseFieldList (fmc->extras, fmc->pextras);
	if (! fmc->title[0])
	    sprintf (fmc->title, "Select %s", fmc->fparam);
/*
 * Find out what the current field is.
 */
	if (fmc->fparam[0])
	    pd_Retrieve (Pd, fmc->icomp, fmc->fparam, fmc->fcurrent, 
			 SYMT_STRING);
/*
 * Query the fields that are available.
 */
	if ((pid = ds_LookupPlatform (fmc->platform)) == BadPlatform)
	{
	    Funky ("Bad platform name");
	    return 0;
	}
	fmc->nfield = MAXFIELD;
	ds_GetFields (pid, &PlotTime, &fmc->nfield, fmc->fields);
	return (1);
}



/*
 * Basically, just make sure the UI variables queried to get this context
 * are restored.
 */
void
fm_StoreContext (fm_Context *fmc)
{
    SValue v;

    v.us_v_ptr = fmc->icomp;
    usy_s_symbol (VTable, "icon_component", SYMT_STRING, &v);
    v.us_v_ptr = fmc->area_type;
    usy_s_symbol (VTable, "area_type", SYMT_STRING, &v);
    v.us_v_int = ! strcmp (fmc->area_type, "posicon");
    usy_s_symbol (VTable, "position_icon", SYMT_BOOL, &v);
    v.us_v_ptr = fmc->icon_platform;
    usy_s_symbol (VTable, "icon_platform", SYMT_STRING, &v);
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



/*
 * UI app widget creation routine for Field Chooser
 */
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
    int n;
    fm_Context *fmc = &FieldMenuContext;
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

    fm_GetContext (fmc, 0);
    return fm_CreateFieldChooser (fmc, parent, 
				  fm_EntryCallback, NULL, FieldMenuCallbacks);
}
		
		


Widget
fm_CreateFieldChooser (fm_Context *fmc, Widget parent, 
		       XtCallbackProc callback, void *refdata,
		       struct fm_Callback *callbacks)
/*
 * Create the field chooser.
 */
{
	Arg args[15];
	int n;
	int i, nentry;
	Widget vp, vpform, above;
	Widget entries[FM_MAXENTRY];
	char name[15];
/*
 * Make a viewport to hold all the buttons.
 */
	n = 0;
	XtSetArg (args[n], XtNallowVert, True);			n++;
	XtSetArg (args[n], XtNheight, 300);			n++;
	XtSetArg (args[n], XtNwidth, 450);			n++;
	vp = XtCreateManagedWidget ("pvp", viewportWidgetClass, parent,
				    args, n);
	vpform = XtCreateManagedWidget ("pform", formWidgetClass, vp, 0, 0);
/*
 * Create all the buttons, but defer the bulk of the setup.
 */
	above = NULL;
	nentry = fm_NumEntries (fmc);
	for (i = 0; i < nentry; ++i)
	{
	    	sprintf (name, "entry%i", i);
	/*
	 * Put together the args to make the button.
	 */
		n = 0;
		XtSetArg (args[n], XtNfromVert, above);		n++;
		XtSetArg (args[n], XtNwidth, 300);		n++;
		XtSetArg (args[n], XtNjustify, XtJustifyLeft);	n++;
		XtSetArg (args[n], XtNborderWidth, 0);		n++;
		entries[i] = XtCreateManagedWidget (name, commandWidgetClass,
						    vpform, args, n);
		above = entries[i];
	}
	nentry = fm_SetupEntries (fmc, callbacks, callback, entries, nentry);
/*
 * Now add the refdata to the callbacks.
 */
	for (i = 0; i < nentry; ++i)
	{
	    callbacks[i].refdata = refdata;
	}
	return (vp);
}
		
		



void
fm_SelectField (fm_Context *fmc, char *field)
/*
 * They have punched a button.
 */
{
	char cbuf[512];
/*
 * Look and see if there is a command to execute.  If not, just set the
 * field name and be done with it.
 */
	if (! fmc->fsc[0])
	{
	    parameter (fmc->icomp, fmc->fparam, field);
	}
/*
 * OK, format up the command and set it loose.  This expects that the
 * caller has already setup the context for the command as necessary.
 */
	else
	{
	    sprintf (cbuf, "%s '%s'", fmc->fsc, field);
	    msg_ELog (EF_DEBUG, "running field-select-command: '%s'", cbuf);
	    ui_perform (cbuf);
	}
}



static void
fm_EntryCallback (w, xfield, junk)
Widget w;
XtPointer xfield, junk;
{
    struct fm_Callback *cb = (struct fm_Callback *) xfield;
    fm_Context *fmc = cb->fm_context;
    char *field = (cb->extra ? cb->extra : F_GetFullName (cb->fid));

    fm_SelectEntry (fmc, w);
    fm_SelectField (fmc, field);
}



static void
fm_PopupSelector (w, xfield, junk)
Widget w;
XtPointer xfield, junk;
{
    fs_CreateWithContext (&FieldMenuContext);
}



void
fm_SelectEntry (fm_Context *fmc, Widget w)
{
    Arg args[5];

    if (fmc->fentry)
    {
	XtSetArg (args[0], XtNleftBitmap, None);
	XtSetValues ((Widget)fmc->fentry, args, 1);
	fmc->fentry = 0;
    }
	
    if (w)
    {
	XtSetArg (args[0], XtNleftBitmap, Star);
	XtSetValues (w, args, 1);
	fmc->fentry = w;
    }
}
