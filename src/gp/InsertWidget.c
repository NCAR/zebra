/*
 * Data insertion widget.
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
# include <math.h>

# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/SimpleMenu.h>
# include <X11/Xaw/SmeBSB.h>
# include <X11/Xaw/SmeLine.h>
# include <X11/Xaw/MenuButton.h>

# include <ui.h>
# include <ui_date.h>

# include <defs.h>
# include <timer.h>
# include <message.h>
# include <pd.h>
# include <GraphicsW.h>
# include <DataStore.h>
# include "GraphProc.h"
# include "PixelCoord.h"
# include "EventQueue.h"

RCSID("$Id: InsertWidget.c,v 1.16 2001-01-16 22:27:35 granger Exp $")

# ifndef PI
# define PI		3.141592654
# endif
# define STRLEN		40
# define MAXENTRY	20
# define MAXSUBPLAT	20

/*
 * Yes and No.
 */
# define NO		0
# define YES		1

/*
 * Possible Modes of Operation.
 */
# define NOTHING	0
# define INSERT		1
# define REMOVE		2
# define CHANGE		3
# define NUMMODE	4

/*
 * What type of data are we inserting?
 */
# define OUTLINE	0
# define LOCATION	1
# define NUMTYPE	2

/*
 * Steps in the outline insertion process.
 */
# define I_LINE_DRAW	0
# define I_LINE_CENTER	1
# define I_LINE_STORE	2
/*
 * Total number of steps.
 */
# define I_LINE_STEPS	3

/*
 * Steps in the location insertion process.
 */
# define I_LOC_ENTER	0
# define I_LOC_STORE	1
/*
 * Total number of steps.
 */
# define I_LOC_STEPS	2

/*
 * Steps in changing an outline.
 */
# define C_LINE_PICK	0
# define C_LINE_DRAW	1
# define C_LINE_CENTER	2
# define C_LINE_STORE	3
/*
 * Total number of steps.
 */
# define C_LINE_STEPS	4

/*
 * Steps in changing a location.
 */
# define C_LOC_PICK	0
# define C_LOC_ENTER	1
# define C_LOC_STORE	2
/*
 * Total number of steps.
 */
# define C_LOC_STEPS	3

/*
 * Data removal steps.
 */
# define REMOVE_RM	0
/*
 * Total number of steps.
 */
# define REMOVE_STEPS	1

/*
 * Maximum number of steps in any type.
 */
# define MAXSTEPS	4


/*
 * Global stuff.
 */
/*
 * Widgets and associated stuff.
 */
static Widget 	InstrLabel;		/* Instructions label widget	*/
static Widget 	HelpLabel;		/* Help label widget		*/
static Widget 	TimeText;		/* Time ascii text widget	*/
static Widget 	InsertMenu;		/* Menu of insertable platforms */
static Widget 	InsertEntries[MAXENTRY];/* Entries in the InsertMenu	*/
static int	InsertNManaged;		/* Number of entries managed	*/
#ifdef notdef
static Widget 	IconMenu;		/* Menu of toggle-able icons	*/
static Widget 	IconEntries[MAXENTRY];	/* Entries in the IconMenu	*/
static int	IconNManaged;		/* Number of entries managed	*/
#endif
static Widget	AttrMenu;		/* Select attributes menu.	*/
static Widget	AttrEntries[MAXENTRY];	/* Entried in the AttrMenu.	*/
static int	AttrNManaged;		/* Number of entries managed.	*/
static Widget	ChangeMenu;		/* Select change menu.		*/
static Widget	ChangeEntries[MAXENTRY];/* Entried in the ChangeMenu.	*/
static int	ChangeNManaged;		/* Number of entries managed.	*/
static char	ValidTime[STRLEN];	/* User entered valid time.	*/

/*
 * What are we doing?
 */
static int 	Mode = NOTHING;		/* What are we trying to do?	*/
static int	Type;			/* Type of data to deal with.	*/
static int	Step = 0;		/* Step in the process.		*/
static long	Entry = -1;		/* Insert menu entry selected.	*/
static long	AttrEntry = -1;		/* Attribute menu entry selected*/
static char	IconComponent[STRLEN];	/* Selected component name. 	*/
static char	IconPlatform[STRLEN];	/* Selected platform name. 	*/
static date	IconTime;		/* Selected data name. 		*/
static char	LocFormat[STRLEN];	/* Location format.	 	*/
static char	LocOrigin[STRLEN];	/* Location origin.	 	*/
static char	LocLatX[STRLEN];	/* Location latitude or x. 	*/
static char	LocLonY[STRLEN];	/* Location longitude or y. 	*/
static char	LocAlt[STRLEN];		/* Location altitude. 		*/
static int	SubPlatform = 0;	/* Sub-platform number.		*/

/*
 * Platforms we know about and their type.
 */
static char	IPlatform[MAXENTRY][20];/* User insertable platforms.	*/
static int	IType[MAXENTRY];	/* Platform type.		*/
static int	ISub[MAXENTRY];		/* Max number of subplatforms.	*/
static int	NumPlatforms;		/* Number of platforms		*/

/*
 * Attributes we know about.
 */
static char	Attr[MAXENTRY][20];	/* Selectable attributes.	*/
static int	NumAttr;		/* Number of attributes.	*/

#ifdef notdef
/*
 * Platforms that we know about for the icons menu.
 */
static char	IconPlats[MAXENTRY][20];/* User insertable platforms.	*/
static int	NumIconPlats;		/* Number of platforms.		*/
#endif

/*
 * Data to be stored.
 */
static ZebTime	DataTime;		/* Data time to store.		*/
static char	DataPlatform[STRLEN];	/* Platform name to store.	*/
static Location	DataLocation;		/* Location to be stored.	*/

/*
 * Arrays of strings and routines.
 */
static char	*NoStrings[NUMMODE][NUMTYPE][MAXSTEPS]; 
					/* Help strings for 'No' button.*/
static void	(*Routines[NUMMODE][NUMTYPE])();
					/* Routines that do stuff.	*/
static void	(*YesRoutines[NUMMODE][NUMTYPE])();
					/* Routines that do stuff on yes.*/

/*
 * Forwards.
 */
void		iw_Initialize FP ((void));
Widget		iw_CreateWidget FP((char *, Widget, XtAppContext));
static void	Insert FP ((void));
static void	Remove FP ((void));
static void	Change FP ((void));
#ifdef notdef
static void	Icons FP ((void));
static void	ChangeHour FP ((Widget, XtPointer, XtPointer));
static void	ChangeMin FP ((Widget, XtPointer, XtPointer));
static void	InitIconMenu FP ((Widget));
static void	IconToggle FP ((Widget, XtPointer, XtPointer));
#endif
static void	AbortIt FP ((void));
static void	ResetTime FP ((void));
static void	YesNo FP ((Widget, XtPointer, XtPointer));
static void	Attributes FP ((void));
static void	SetHelp FP ((char *));
static void	SetInstr FP ((char *));
static void	SetTime FP ((void));
static void	insertHelpAction FP (());
static void	removeHelpAction FP (());
static void	changeHelpAction FP (());
static void	abortHelpAction FP (());
static void	yesHelpAction FP (());
static void	noHelpAction FP (());
static void	attrHelpAction FP (());
static void	iconHelpAction FP (());
static void	resetTimeHelpAction FP (());
static void	InitInsertMenu FP ((Widget));
static void	InitAttrMenu FP ((Widget));
static void	InitChangeMenu FP ((Widget));
static void	InsertData FP ((Widget, XtPointer, XtPointer));
static void	InsertLine FP ((int));
static void	InsertLoc FP ((int));
static void	SelectChange FP ((Widget, XtPointer, XtPointer));
static void	ChangeLine FP ((int));
static void	ChangeLoc FP ((int));
static void	SelectAttr FP ((Widget, XtPointer, XtPointer));
static void	YesInsertLine FP ((void));
static void	YesInsertLoc FP ((void));
static void	YesRemove FP ((void));
static void	YesChangeLine FP ((void));
static void	YesChangeLoc FP ((void));
static void	set_help_strings FP ((void));
static void	set_routines FP ((void));
static void	cleanup FP ((void));

/*
 * Rubber band routines that get used.
 */
extern void	rb_Outline ();
extern void	rb_Point ();
extern void	rb_Done ();

extern XPoint	OL[], Point;
extern int	N_OLSeg;


void
iw_Initialize ()
/*
 * Tell UI about the data insertion widget.
 */
{
/*
 * Take care of the widget.
 */
	uw_def_widget ("insert", "Data Insertion", iw_CreateWidget, 0, 0);
/*
 * Our indirect variables.
 */
	usy_c_indirect (Vtable, "compname", IconComponent, SYMT_STRING, STRLEN);
	usy_c_indirect (Vtable, "platform", IconPlatform, SYMT_STRING, STRLEN);
	usy_c_indirect (Vtable, "datatime", &IconTime, SYMT_DATE, 0);
	usy_c_indirect (Vtable, "locformat", LocFormat, SYMT_STRING, STRLEN);
	usy_c_indirect (Vtable, "locorigin", LocOrigin, SYMT_STRING, STRLEN);
	usy_c_indirect (Vtable, "loclatx", LocLatX, SYMT_STRING, STRLEN);
	usy_c_indirect (Vtable, "loclony", LocLonY, SYMT_STRING, STRLEN);
	usy_c_indirect (Vtable, "localt", LocAlt, SYMT_STRING, STRLEN);
/*
 * Set some help strings.
 */
	set_help_strings ();
/*
 * Set up the routines.
 */
	set_routines ();
/*
 * Initialize some variables. 
 */
	IconPlatform[0] = '\0';
}


Widget
iw_CreateWidget (junk, parent, actx)
char 		*junk;
Widget 		parent;
XtAppContext 	actx;
/*
 * Actually create the data insertion widget.
 */
{
	Widget	form, left, above;
	Arg	args[20];
	ZebTime	zt;
	int	n;

	static XtActionsRec actions[] = {
		{"insertHelp",	insertHelpAction},
		{"removeHelp",	removeHelpAction},
		{"changeHelp",	changeHelpAction},
		{"abortHelp",	abortHelpAction},
		{"yesHelp",	yesHelpAction},
		{"noHelp",	noHelpAction},
		{"attrHelp",	attrHelpAction},
		{"iconHelp",	iconHelpAction},
		{"resetTimeHelp",	resetTimeHelpAction},
	};
	static char *translation;
	XtTranslations table;
	
	XtAppAddActions (actx, actions, 9);
/*
 * Build the left and right arrow bitmaps.
 */
# ifdef notdef
	bm_BuildBitmaps (parent);
# endif
/*
 * A form to hold everything.
 */
	n = 0;
	XtSetArg (args[n], XtNdefaultDistance, 5);	n++;
	XtSetArg (args[n], XtNborderWidth, 0);		n++;
	form = XtCreateManagedWidget ("insertform", formWidgetClass, parent,
		args, n);
/*
 * Control Buttons.
 */
/*
 * Insert button.
 */
	translation = "<EnterWindow>:	highlight()insertHelp() \n\
		       <LeaveWindow>:	reset() \n\
		       <BtnDown>:	reset()PopupMenu()";
	table = XtParseTranslationTable (translation);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNmenuName, "DataInsert");	n++;
	XtSetArg (args[n], XtNtranslations, table);	n++;
	left = XtCreateManagedWidget ("Insert", menuButtonWidgetClass,
		form, args, n);

	InitInsertMenu (Top);
/*
 * Remove button.
 */
# ifdef notdef  /* Data store doesn't do removing at this time. */
	translation = "<EnterWindow>:	highlight()removeHelp() \n\
		       <LeaveWindow>:	reset() \n\
		       <Btn1Up>:	notify()unset() \n\
		       <Btn1Down>:	set()";
	table = XtParseTranslationTable (translation);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNtranslations, table);	n++;
	left = XtCreateManagedWidget ("Remove", commandWidgetClass,
		form, args, n);
	XtAddCallback (left, XtNcallback, (XtCallbackProc) Remove, 
		(XtPointer) 0);
# endif
/*
 * Change button.
 */
	translation = "<EnterWindow>:	highlight()changeHelp() \n\
		       <LeaveWindow>:	reset() \n\
		       <BtnDown>:	reset()PopupMenu()";
	table = XtParseTranslationTable (translation);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNmenuName, "ChangeMenu");	n++;
	XtSetArg (args[n], XtNtranslations, table);	n++;
	left = XtCreateManagedWidget ("Change", menuButtonWidgetClass,
		form, args, n);

	InitChangeMenu (Top);
/*
 * Abort button.
 */
	translation = "<EnterWindow>:	highlight()abortHelp() \n\
		       <LeaveWindow>:	reset() \n\
		       <Btn1Up>:	notify()unset() \n\
		       <Btn1Down>:	set()";
	table = XtParseTranslationTable (translation);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNtranslations, table);	n++;
	above = XtCreateManagedWidget ("Abort", commandWidgetClass,
		form, args, n);
	XtAddCallback (above, XtNcallback, (XtCallbackProc) AbortIt, 
		(XtPointer) 0);
/*
 * Instructions.
 */
/*
 * The text window which displays the instructions.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	XtSetArg (args[n], XtNjustify, XtJustifyLeft);	n++;
	XtSetArg (args[n], XtNresize, True);		n++;
	XtSetArg (args[n], XtNwidth, 450);		n++;
	XtSetArg (args[n], XtNheight, 120);		n++;
	above = InstrLabel = XtCreateManagedWidget ("Instructions", 
		labelWidgetClass, form, args, n);
/*
 * The 'Yes' button.
 */
	translation = "<EnterWindow>:	highlight()yesHelp() \n\
		       <LeaveWindow>:	reset() \n\
		       <Btn1Up>:	notify()unset() \n\
		       <Btn1Down>:	set()";
	table = XtParseTranslationTable (translation);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	XtSetArg (args[n], XtNtranslations, table);	n++;
	left = XtCreateManagedWidget ("Yes", commandWidgetClass,
		form, args, n);
	XtAddCallback (left, XtNcallback, (XtCallbackProc) YesNo, 
		(XtPointer) YES);
/*
 * The 'No' button.
 */
	translation = "<EnterWindow>:	highlight()noHelp() \n\
		       <LeaveWindow>:	reset() \n\
		       <Btn1Up>:	notify()unset() \n\
		       <Btn1Down>:	set()";
	table = XtParseTranslationTable (translation);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	XtSetArg (args[n], XtNtranslations, table);	n++;
	left = XtCreateManagedWidget ("No", commandWidgetClass,
		form, args, n);
	XtAddCallback (left, XtNcallback, (XtCallbackProc) YesNo, 
		(XtPointer) NO);
/*
 * The attributes menu.
 */
	translation = "<EnterWindow>:	highlight()attrHelp() \n\
		       <LeaveWindow>:	reset() \n\
		       <BtnDown>:	reset()PopupMenu()";
	table = XtParseTranslationTable (translation);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	XtSetArg (args[n], XtNmenuName, "SelectAttr");	n++;
	XtSetArg (args[n], XtNtranslations, table);	n++;
	above = XtCreateManagedWidget ("Attributes", menuButtonWidgetClass,
		form, args, n);

	InitAttrMenu (Top);
/*
 * Icon enable/disable button.
 */
# ifdef notdef
	translation = "<EnterWindow>:	highlight()iconHelp() \n\
		       <LeaveWindow>:	reset() \n\
		       <BtnDown>:	reset()PopupMenu()";
	table = XtParseTranslationTable (translation);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	XtSetArg (args[n], XtNtranslations, table);	n++;
	XtSetArg (args[n], XtNmenuName, "IconToggle");	n++;
	above = XtCreateManagedWidget ("Icons", menuButtonWidgetClass,
		form, args, n);

	InitIconMenu (Top);
# endif
/*
 * The time window.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	XtSetArg (args[n], XtNborderWidth, 0);		n++;
	left = XtCreateManagedWidget ("Valid Time:", labelWidgetClass,
		form, args, n);

	tl_Time (&zt);
	TC_EncodeTime (&zt, TC_Full, ValidTime);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	XtSetArg (args[n], XtNdisplayPosition, 0);	n++;
	XtSetArg (args[n], XtNinsertPosition, 0);	n++;
	XtSetArg (args[n], XtNlength, STRLEN);		n++;
	XtSetArg (args[n], XtNresize, XawtextResizeNever);	n++;
	XtSetArg (args[n], XtNwidth, 140);		n++;
	XtSetArg (args[n], XtNstring, ValidTime);	n++;
	XtSetArg (args[n], XtNtype, XawAsciiString);	n++;
	XtSetArg (args[n], XtNuseStringInPlace, True);	n++;
	XtSetArg (args[n], XtNleftMargin, 5);		n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit);	n++;
	above = TimeText = XtCreateManagedWidget ("time", asciiTextWidgetClass,
		form, args, n);
/*
 * The reset time button.
 */
# ifdef notdef
	translation = "<EnterWindow>:	highlight()resetTimeHelp() \n\
		       <LeaveWindow>:	reset() \n\
		       <Btn1Up>:	notify()unset() \n\
		       <Btn1Down>:	set()";
	table = XtParseTranslationTable (translation);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	XtSetArg (args[n], XtNtranslations, table);	n++;
	above = XtCreateManagedWidget ("Reset Time", commandWidgetClass,
		form, args, n);
	XtAddCallback (above, XtNcallback, (XtCallbackProc) ResetTime, 
		(XtPointer) 0);
# endif
/*
 * Left/right buttons for hour, minute.
 */
# ifdef notdef
        n = 0;
        XtSetArg (args[n], XtNfromHoriz, left);         n++;
        XtSetArg (args[n], XtNfromVert, above);		n++;
        left = XtCreateManagedWidget ("Hours", labelWidgetClass, form,
                args, n);

        button = LeftRightButtons (form, ChangeHour, NULL);

        n = 0;
        XtSetArg (args[n], XtNfromHoriz, left);         n++;
        XtSetArg (args[n], XtNfromVert, above);		n++;
        XtSetValues (button, args, n);

        n = 0;
        XtSetArg (args[n], XtNfromHoriz, button);	n++;
        XtSetArg (args[n], XtNfromVert, above);		n++;
        left = XtCreateManagedWidget ("Minutes", labelWidgetClass, form,
                args, n);

        button = LeftRightButtons (form, ChangeMin, NULL);

        n = 0;
        XtSetArg (args[n], XtNfromHoriz, left);		n++;
        XtSetArg (args[n], XtNfromVert, above);		n++;
        XtSetValues (button, args, n);
# endif

/*
 * The Help line.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	XtSetArg (args[n], XtNwidth, 450);		n++;
	HelpLabel = XtCreateManagedWidget ("Help", labelWidgetClass,
		form, args, n);

	return (form);
}


static void
insertHelpAction ()
/*
 * Display help information for the 'Insert' button.
 */
{
	SetHelp ("Insert:  Begin the process of entering data.");
}


static void
removeHelpAction ()
/*
 * Display help information for the 'Remove' button.
 */
{
	SetHelp ("Remove:  Delete previously entered data.");
}


static void
changeHelpAction ()
/*
 * Display help information for the 'Change' button.
 */
{
	SetHelp ("Change:  Modify previously entered data.");
}


static void
abortHelpAction ()
/*
 * Display help information for the 'Abort' button.
 */
{
	SetHelp ("Abort:  Abandon the current operation.");
}


static void
yesHelpAction ()
/*
 * Display help information for the 'Yes' button.
 */
{
	SetHelp ("Yes:  Accept the information.");
}


static void
noHelpAction ()
/*
 * Display help information for the 'No' button.
 */
{
	SetHelp ("No:  Reject the information.");
}


static void
attrHelpAction ()
/*
 * Display help information for the 'Select Attributes' button.
 */
{
	SetHelp ("Attributes:  Select an attribute.");
}


static void
iconHelpAction ()
/*
 * Display help information for the 'Icon' button.
 */
{
	SetHelp ("Icon:  Enable or disable icons in the graphics window.");
}


static void
resetTimeHelpAction ()
/*
 * Display help information for the 'Reset Time' button.
 */
{
	SetHelp ("Reset Time:  Reset the valid time.");
}


static void
Attributes ()
/*
 * Popping up the select attributes menu.
 */
{
	int i, numattr;
	char platform[40], attributes[200], *attrlist[200];
	Arg arg;
/*
 * Has a platform been selected? 
 */
	if ((Entry == -1) && (IconPlatform[0] != '\0'))
		strcpy (platform, IconPlatform);
	else if (Entry >= 0 && Entry < NumPlatforms)
		strcpy (platform, IPlatform[Entry]);
	else
	{
		msg_ELog (EF_PROBLEM, "Can't get a good platform name.");
		platform[0] = '\0';
	}
/*
 * Read in and parse the attributes for this platform.
 */
	NumAttr = 0;
	if (! platform[0])
	{
		msg_ELog (EF_PROBLEM, "No platform active for attributes");
	}
	else if (! pda_Search (Pd, "global", "attributes", platform, 
			       attributes, SYMT_STRING))
	{
		msg_ELog (EF_PROBLEM, "No attributes for %s", platform);
	}
	else
	{
		numattr = CommaParse (attributes, attrlist);
	/*
 	 * Fill in each attribute menu entry.
 	 */
		for (i = 0, NumAttr = 0; i < numattr; i++)
		{
			strcpy (Attr[NumAttr], attrlist[i]);
		/*
	 	 * Set the label.
	 	 */
			XtSetArg (arg, XtNlabel, attrlist[i]);
			XtSetValues (AttrEntries[NumAttr], &arg, 1);
		/*
		 * If its not managed, manage it.
	 	 */
			if (NumAttr >= AttrNManaged)
			{
				XtManageChild (AttrEntries[NumAttr]);
					AttrNManaged++;
			}
		/*
	 	 * Increment the number of good platforms.
	 	 */
			NumAttr++;
		}
	}
	if (NumAttr == 0)
		SetHelp ("No attributes.");
/*
 * Clean out extras.
 */
	for (i = NumAttr; i < AttrNManaged; i++)
		XtUnmanageChild (AttrEntries[i]);
	AttrNManaged = NumAttr;
}


static void
SelectAttr (w, entry, junk)
Widget		w;
XtPointer	entry, junk;
/*
 * An attribute has been selected.
 */
{
/*
 * Store the entry number.
 */
	AttrEntry = (long) entry;
	msg_ELog (EF_DEBUG, "Selected attribute %s", Attr[AttrEntry]);
}


static void
Insert ()
/*
 * Activate the insert menu.
 */
{
	int i, numplat, sub;
	char platforms[PlatformListLen];
	char *platlist[MaxPlatforms];
	Arg arg;
	PlatformId pid;
	DataOrganization org;
/*
 * Read in and parse the user insertable platforms.
 */
	if (! pda_Search (Pd, "global", "user-insert-platforms", NULL,
		platforms, SYMT_STRING))
	{
		NumPlatforms = 0;
		msg_ELog (EF_PROBLEM, "No user insertable platforms.");
		SetHelp ("No user insertable platforms.");
	}
	else
	{
		numplat = CommaParse (platforms, platlist);
	/*
	 * Fill in each platform menu entry.
 	*/
		for (i = 0, NumPlatforms = 0; i < numplat; i++)
		{
		/*
		 * Test the platform.
		 */
			if ((pid = ds_LookupPlatform (platlist[i])) == 
				BadPlatform)
			{
				msg_ELog (EF_PROBLEM, "Unknown platform '%s'.",
					platlist[i]);
				continue;
			}
			strcpy (IPlatform[NumPlatforms], platlist[i]);
		/*
		 * Get its organization.
		 */
			org = ds_PlatformDataOrg (pid);
			switch (org)
			{
				case OrgOutline:
					IType[NumPlatforms] = OUTLINE;
					break;
				case OrgScalar:
					IType[NumPlatforms] = LOCATION;
					break;
				default:
					msg_ELog (EF_PROBLEM,
						  "Bad data organization %d", 
						  org);
					continue;
			}
		/*
		 * Get the number of subplatforms.
		 */
			if (! pda_Search (Pd, "global", "max-sub-plat", 
					platlist[i], (char *) &sub, SYMT_INT))
				ISub[NumPlatforms] = 0;
			else if (sub >= 0 && sub < MAXSUBPLAT)
			{
				ISub[NumPlatforms] = sub;
			}
			else ISub[NumPlatforms] = 0;
		/*
		 * Set the label.
		 */
			XtSetArg (arg, XtNlabel, platlist[i]);
			XtSetValues (InsertEntries[NumPlatforms], &arg, 1);
		/*
		 * If its not managed, manage it.
		 */
			if (NumPlatforms >= InsertNManaged)
			{
				XtManageChild (InsertEntries[NumPlatforms]);
				InsertNManaged++;
			}
		/*
		 * Increment the number of good platforms.
		 */
			NumPlatforms++;
		}
	}
/*
 * Clean out extras.
 */
	for (i = NumPlatforms; i < InsertNManaged; i++)
		XtUnmanageChild (InsertEntries[i]);
	InsertNManaged = NumPlatforms;
}


static void
InsertData (w, entry, junk)
Widget		w;
XtPointer	entry, junk;
/*
 * Enter data for the platform indicated by entry.
 */
{
/*
 * See what mode we're in and if its possible to insert.
 */	
	if (Mode != NOTHING)
	{
		SetHelp ("Abort current operation before changing modes.");
	}	
	else
	{
	/*
	 * Store the entry number.
	 */
		Entry = (long) entry;
	/*
	 * Check that the entry number is valid.
	 */
		if (Entry < 0 || Entry >= NumPlatforms)
			return;
	/*
	 * Set the Mode to INSERT.
	 */
		Mode = INSERT;
	/*
 	 * Reset the step counter.
	 */
		Step = 0;
	/*
	 * Store the data platform.
	 */
		strcpy (DataPlatform, IPlatform[Entry]);
	/*
	 * Call the insertion procedure depending on data type.
	 */
		Type = IType[Entry];
		(*Routines[Mode][Type]) (Entry);
	}
}


static void
InsertLine (entry)
int	entry;
/*
 * Insert a outline.
 */
{
	char	string[1000];
/*
 * Tell the user we're doing it.
 */
	if (Step == 0)
	{
		sprintf (string, "Inserting a %s outline.", IPlatform[entry]);
		SetHelp (string);
	}
/*
 * What step of the process are we in?
 */
	switch (Step)
	{
	/*
	 * Draw the outline.
	 */
  	  case I_LINE_DRAW:
	  /*
	   * Display the instructions for drawing the outline.
	   */
		sprintf (string, "Draw the outline of the %s.\n\n",
			IPlatform[entry]);
		strcat (string, "LEFT = enter, MIDDLE = delete");
		strcat (string, "\n\nYes - Accept the outline.");
		strcat (string, "\nNo  - Reject the outline.");
		SetInstr (string);
	  /*
	   * Call the polyline drawing routine.
	   */
		rb_Outline ();
		break;
	/*
	 * Center picking.
	 */
	  case I_LINE_CENTER:
	  /*
	   * Display the instructions for picking the center.
	   */
		sprintf (string, "Pick the center of the %s.\n\n", 
			IPlatform[entry]);
		strcat (string, "LEFT = enter");
		strcat (string, "\n\nYes - Accept the center.");
		strcat (string, "\nNo  - Reject the center.");
		SetInstr (string);
	  /*
	   * Call the point picking routine.
	   */
		rb_Point ();
		break;
	/*
	 * Store the data.
	 */
	  case I_LINE_STORE:
	  /*
	   * Display the instructions for storing the data.
	   */
		strcpy (string,"1.  Link to data in the graphics window.\n");
		strcat (string,"2.  Select an attribute.\n");
		strcat (string,"3.  Set a valid time.");

		strcat (string, "\n\nYes - Accept the data and store it.");
		strcat (string, "\nNo  - Reject the data.");
		SetInstr (string);

	/*
	 * ResetTime is described as a "RAPS kludge."  In any case, it
	 * annoys Wilson.  Someday we'll do something smarter here...
	 */
		/* ResetTime (); */

		break;
	  default:
		msg_ELog (EF_PROBLEM, "Weird step %d", Step);
	}
}


static void
YesInsertLine ()
/*
 * What to do in each step when 'Yes' is clicked.
 */
{
	date		t;
	DataChunk	*dc;
	PlatformId	pid;
	Location	*pts, center;
	int		i;
	zbool		sub = FALSE;
	char		temp[STRLEN], param[STRLEN];
	char		string[1000];

	switch (Step)
	{
		case I_LINE_DRAW:
		/*
		 * Tell them what they did.
		 */
			SetHelp ("Outline accepted.");
			rb_Done ();
			break;
		case I_LINE_CENTER:
		/*
		 * Tell them what they did.
		 */
			SetHelp ("Center entered.");
			rb_Done ();
			break;
		case I_LINE_STORE:
		/*
		 * Get the platform name.
		 */
			sprintf (param, "%s-sub", IPlatform[Entry]);
			pda_Search (Pd, "global", param, 0, (char *) &sub,
				SYMT_BOOL); 
			if (sub) /* If this platform has subplatforms. */
			{
			    if (IconPlatform[0] == '\0')
			    {
				sprintf (DataPlatform, "%s.%d", 
					IPlatform[Entry], SubPlatform + 1); 
				SubPlatform++;
				SubPlatform %= ISub[Entry];
			    }
			    else if (strstr (IconPlatform, IPlatform[Entry]) 
				== NULL)
			    {
				msg_ELog (EF_PROBLEM, 
					  "Linked with bad platform %s.",
					  IconPlatform);
				SetHelp ("Linked with bad platform.");
				Step--;
				return;
			    }
			    else
				strcpy (DataPlatform, IconPlatform);
			}
			else
				strcpy (DataPlatform, IPlatform[Entry]);
		/*
		 * Get the time the data is stored at.
		 */
			if (PlotMode == History)
				DataTime = PlotTime;
			else
				tl_Time (&DataTime);
			DataTime.zt_MicroSec = 0;
			TC_ZtToUI (&DataTime, &t);
			msg_ELog (EF_DEBUG, "Inserting data at %d %d",
					t.ds_yymmdd, t.ds_hhmmss);
		/*
		 * Create the data chunk. 
		 */
			msg_ELog (EF_DEBUG, "%d segments in the outline",
				N_OLSeg);
			if (N_OLSeg <= 0)
			    msg_ELog (EF_PROBLEM, "No points in the outline.");
			else
			{
			    dc = dc_CreateDC (DCC_Boundary);
			    pid = ds_LookupPlatform (DataPlatform);
			    dc->dc_Platform = pid;
			/*
			 * Store the outline in the data chunk.
			 */
			    pts = (Location *) malloc ((N_OLSeg + 1) * 
						sizeof (Location));
			    for (i = 0; i < N_OLSeg + 1; i++)
			    {
				prj_Reverse (XUSER (OL[i].x), YUSER (OL[i].y),
					&pts[i].l_lat, &pts[i].l_lon);
				pts[i].l_alt = 0;	
			    }
			    dc_BndAdd (dc, &DataTime, pid, pts, N_OLSeg + 1);
			/*
			 * Add the "type" attribute.
			 */
			    if (AttrEntry >= 0)
			    	dc_SetSampleAttr (dc, 0, "type", 
					Attr[AttrEntry]); 
			/*
			 * Store the valid time attribute.
			 */
			    dc_SetSampleAttr (dc, 0, "validtime", ValidTime);
			/*
			 * Add the center.
			 */
			    if (Point.x != -9999 || Point.y != -9999)
			    {
			    	prj_Reverse (XUSER (Point.x), YUSER (Point.y),
					&center.l_lat, &center.l_lon);

			    	sprintf (temp, "%.2f", center.l_lat); 
			    	dc_SetSampleAttr (dc, 0, "center_lat", temp); 

			    	sprintf (temp, "%.2f", center.l_lon); 
			    	dc_SetSampleAttr (dc, 0, "center_lon", temp); 
				msg_ELog (EF_DEBUG, "Point stored %f %f",
					center.l_lat, center.l_lon);
			    }
			/*
			 * Store the data chunk in the data store.
			 */
			    ds_Store (dc, FALSE, NULL, 0);
			/*
			 * Free memory.
			 */
			    dc_DestroyDC (dc);
			    free (pts);
			}
		/*
		 * Tell them what they did.
		 */
			sprintf (string, "%s data stored.", DataPlatform);
			SetHelp (string);
		/*
 		 * Get out of insert mode.
		 */
			cleanup ();
		/*
 		 * Do an update since we turned this off to draw the boundary.
		 */
			pc_PlotHandler ();
			break;
		default:
			break;
	}
}



static void
InsertLoc (entry)
int	entry;
/*
 * Insert a location.
 */
{
	char	string[1000];
/*
 * Tell the user we're doing it.
 */
	if (Step == 0)
	{
		sprintf (string, "Inserting a %s location.", 
			IPlatform[entry]);
		SetHelp (string);
	}
/*
 * What step of the process are we in?
 */
	switch (Step)
	{
	/*
	 * Enter a location.
	 */
	  case I_LOC_ENTER:
	  /*
	   * Display the instructions for entering a location.
	   */
		sprintf (string, "Enter the location of the %s.", 
			IPlatform[entry]);
		strcat (string, "\nBy clicking in the graphics window.");

		strcat (string, "\n\nYes - Accept the location.");
		strcat (string, "\nNo  - Reject the location.");
		SetInstr (string);
	  /*
	   * Pop up the location widget.
	   */
# ifdef notdef
		uw_popup ("enter-loc");
# endif
	  /*
	   * Set up for point picking also.
	   */
		rb_Point ();
		break;
	/*
	 * Store it.
	 */
	  case I_LOC_STORE:
	  /*
	   * Display the instructions for storing the data.
	   */
		strcpy (string, "Select an attribute.\n");

		strcat (string, "\n\nYes - Accept the data and store it.");
		strcat (string, "\nNo  - Reject the data.");
		SetInstr (string);

		ResetTime ();
	
		break;
	  default:
		msg_ELog (EF_PROBLEM, "Weird step %d", Step);
	}
}


static void
YesInsertLoc ()
/*
 * What to do for each step when 'Yes' is clicked.
 */
{
	date	t;
	float		value = 0.0;
	Location	origin;
	PlatformId	pid;
	FieldId		fid;
	DataChunk	*dc;
	char		string[1000];

	switch (Step)
	{
		case I_LOC_ENTER:
		/*
		 * If no point was picked, assume the location was entered
		 * in the widget.
		 */
			rb_Done ();

			if ((Point.x == -9999) && (Point.y == -9999))
			{
			/*
			 * Get the data out of the widget.
			 */
				SetHelp ("Location entered by widget.");

				if (strcmp (LocFormat, "latlon") == 0)
				{
				    DataLocation.l_lat = atof (LocLatX);
				    DataLocation.l_lon = atof (LocLonY);
				    DataLocation.l_alt = atof (LocAlt);
				}
				else if (strcmp (LocFormat, "km") == 0)
				{
				    if (! GetLocation (LocOrigin, &PlotTime, 
					&origin))
				    {
					msg_ELog (EF_PROBLEM, 
						"Unable to locate origin %s.", 
						LocOrigin);
					break;
				    }
				    cvt_Origin (origin.l_lat, origin.l_lon);
				    prj_Reverse (XUSER (atof (LocLatX)),
					YUSER (atof (LocLonY)), 
					&DataLocation.l_lat, 
					&DataLocation.l_lon);
				}
				else if (strcmp (LocFormat, "nm") == 0)
				{
					msg_ELog (EF_INFO, "Don't do nm yet.");
					break;
				}
				else
				{
					msg_ELog (EF_PROBLEM, 
						"Unknown format %s", LocFormat);
					break;
				}
			}
			else
			{
			/*
			 * Get the data out of the picked point.
			 */
				SetHelp ("Location entered by pointer.");
			    	prj_Reverse (XUSER (Point.x), YUSER (Point.y),
					&DataLocation.l_lat, 
					&DataLocation.l_lon);
				DataLocation.l_alt = 0.0;
			}
			break;
		case I_LOC_STORE:
		/*
		 * Get the time the data is stored at.
		 */
			if (PlotMode == History)
				DataTime = PlotTime;
			else
				tl_Time (&DataTime);
			DataTime.zt_MicroSec = 0;
			TC_ZtToUI (&DataTime, &t);
			msg_ELog (EF_DEBUG, "Inserting data at %d %d",
					t.ds_yymmdd, t.ds_hhmmss);
		/*
		 * Put it all into the data store.
		 */
			dc = dc_CreateDC (DCC_Scalar);
			pid = ds_LookupPlatform (DataPlatform);
			dc->dc_Platform = pid;
		/*
		 * Set up a phoney field.
		 */
			fid = F_Lookup ("temp");
			dc_SetScalarFields (dc, 1, &fid);
			dc_AddScalar (dc, &DataTime, 0, fid, &value);
		/*
		 * Set the location.
		 */
			dc_SetLoc (dc, 0, &DataLocation);
# ifdef notdef
			dc_LocAdd (dc, &DataTime, &DataLocation);
# endif
		/*
		 * Set the attribute.
		 */
			if (AttrEntry >= 0)
				dc_SetSampleAttr (dc, 0, "type", 
					Attr[AttrEntry]); 
		/*
		 * Store it.
		 */
			ds_Store (dc, FALSE, NULL, 0);
		/*
		 * Free memory.
		 */
			dc_DestroyDC (dc);
		/*
 		 * Get out of insert mode.
		 */
			sprintf (string, "%s data stored.", DataPlatform);
			SetHelp (string);
			cleanup ();
			break;
		default:
			break;
	}
}


static void
ChangeLine (entry)
int	entry;
/*
 * Change a previously entered outline.
 */
{
        char    string[2000];
/*
 * Tell the user we're doing it.
 */
        if (Step == 0) SetHelp ("Changing an outline.");
/*
 * What step of the process are we in?
 */
        switch (Step)
        {
	/*
	 * Pick which data to change.
	 */
	  case C_LINE_PICK:
          /*
           * Display the instructions for picking.
           */
                sprintf (string, "Pick the outline to change.\n\n");
                strcat (string, "\n\nYes - Accept the choice.");
                strcat (string, "\nNo  - Reject the choice.");
                SetInstr (string);
		break;
        /*
         * Draw the outline.
         */
          case C_LINE_DRAW:
          /*
           * Display the instructions for drawing the outline.
           */
                sprintf (string, "Draw the new outline of the %s.\n\n",
                        IconPlatform);
                strcat (string, "LEFT = enter, MIDDLE = delete");
                strcat (string, "\n\nYes - Accept the outline.");
                strcat (string, "\nNo  - Reject the outline.");
                SetInstr (string);
          /*
           * Call the polyline drawing routine.
           */
                rb_Outline ();
                break;
        /*
         * Center picking.
         */
          case C_LINE_CENTER:
          /*
           * Display the instructions for picking the center.
           */
                sprintf (string, "Pick the new center of the %s.\n\n",
                        IconPlatform);
                strcat (string, "LEFT = enter");
                strcat (string, "\n\nYes - Accept the center.");
                strcat (string, "\nNo  - Reject the center.");
                SetInstr (string);
          /*
           * Call the point picking routine.
           */
                rb_Point ();
                break;
        /*
         * Store the data.
         */
          case C_LINE_STORE:
          /*
           * Display the instructions for storing the data.
           */
                strcpy (string, "1.  Select a new attribute.\n");
                strcat (string, "2.  Set a new valid time.\n");

                strcat (string, "\n\nYes - Accept the data and store it.");
                strcat (string, "\nNo  - Reject the data.");
                SetInstr (string);

		ResetTime ();

                break;
          default:
                msg_ELog (EF_PROBLEM, "Weird step %d", Step);
        }
}


static void
YesChangeLine ()
/*
 * What to do in each step of changing a line when 'Yes' is clicked.
 */
{
        DataChunk       *dc;
        PlatformId      pid;
        Location        *pts, center;
        int             i;
        char            temp[STRLEN], timestring[STRLEN];
	char		string[1000];
	ZebTime		zt;

        switch (Step)
        {
	/*
	 * The data to change has been picked.
	 */
		case C_LINE_PICK:
		/*
		 * Is the platform name a good one.
		 */
			pid = ds_LookupPlatform (IconPlatform);
			if (ds_PlatformDataOrg (pid) != OrgOutline)
			{
				sprintf (string, "%s is not an outline!",
					IconPlatform);
                       		SetHelp (string);
				cleanup ();
			}
			else
			{
				TC_UIToZt (&IconTime, &zt);
				TC_EncodeTime (&zt, TC_Full, timestring);
				sprintf (string, "Changing a %s outline at %s.",
					IconPlatform, timestring);
                       		SetHelp (string);
			}
                        break;
	/*
	 * The new outline has been drawn.
	 */
                case C_LINE_DRAW:
                /*
                 * Tell them what they did.
                 */
                        SetHelp ("New outline accepted.");
                        rb_Done ();
                        break;
	/*
	 * The new center has been entered.
	 */
                case C_LINE_CENTER:
                /*
                 * Tell them what they did.
                 */
                        SetHelp ("New center entered.");
                        rb_Done ();
                        break;
	/*
	 * The new data has been stored.
	 */
                case C_LINE_STORE:
                /*
                 * Get the platform name.
                 */
                        strcpy (DataPlatform, IconPlatform);
                /*
                 * Get the time the data is stored at.
                 */
			TC_UIToZt (&IconTime, &DataTime);
			DataTime.zt_MicroSec = 0;
                /*
                 * Create the data chunk.
                 */
                        if (N_OLSeg <= 0)
                            msg_ELog (EF_PROBLEM, "No points in the outline.");
                        else
                        {
                            dc = dc_CreateDC (DCC_Boundary);
                            pid = ds_LookupPlatform (DataPlatform);
                            dc->dc_Platform = pid;
                        /*
                         * Store the outline in the data chunk.
                         */
                            pts = (Location *) malloc ((N_OLSeg + 1) *
                                                sizeof (Location));
                            for (i = 0; i < N_OLSeg + 1; i++)
                            {
                                prj_Reverse (XUSER (OL[i].x), YUSER (OL[i].y),
                                        &pts[i].l_lat, &pts[i].l_lon);
                                pts[i].l_alt = 0;
                            }
                            dc_BndAdd (dc, &DataTime, pid, pts, N_OLSeg + 1);
                        /*
                         * Add the "type" attribute.
                         */
                            if (AttrEntry >= 0)
                                dc_SetSampleAttr (dc, 0, "type", 
					Attr[AttrEntry]);
			/*
			 * Store the valid time attribute.
			 */
			    dc_SetSampleAttr (dc, 0, "validtime", ValidTime);
                        /*
                         * Add the center.
                         */
                            if (Point.x != -9999 || Point.y != -9999)
                            {
                                prj_Reverse (XUSER (Point.x), YUSER (Point.y),
                                        &center.l_lat, &center.l_lon);

                                sprintf (temp, "%.2f", center.l_lat);
                                dc_SetSampleAttr (dc, 0, "center_lat", temp);

                                sprintf (temp, "%.2f", center.l_lon);
                                dc_SetSampleAttr (dc, 0, "center_lon", temp);
                                msg_ELog (EF_DEBUG, "Point stored %f %f",
                                        center.l_lat, center.l_lon);
                            }
                        /*
                         * Store the data chunk in the data store.
                         */
                            ds_Store (dc, FALSE, NULL, 0);
			/*
			 * Free memory.
			 */
			    dc_DestroyDC (dc);
                            free (pts);
                        }
                /*
                 * Tell them what they did.
                 */
			sprintf (string, "%s data stored.", DataPlatform);
                        SetHelp (string);
                /*
                 * Get out of insert mode.
                 */
                        cleanup ();
                /*
                 * Do an update since we turned this off to draw the boundary.
                 */
                        pc_PlotHandler ();
                        break;
                default:
                        break;
        }
}


static void
ChangeLoc (entry)
int	entry;
/*
 * Change a previously entered location.
 */
{
        char    string[2000];
/*
 * Tell the user we're doing it.
 */
        if (Step == 0) SetHelp ("Changing a location.");
/*
 * What step of the process are we in?
 */
        switch (Step)
        {
        /*
         * Pick which data to change.
         */
          case C_LOC_PICK:
          /*
           * Display the instructions for picking.
           */
                sprintf (string, "Pick the location to change.\n\n");
                strcat (string, "\n\nYes - Accept the choice.");
                strcat (string, "\nNo  - Reject the choice.");
                SetInstr (string);
                break;
        /*
         * Enter a location.
         */
          case C_LOC_ENTER:
          /*
           * Display the instructions for entering a location.
           */
                sprintf (string, "Enter the new location of the %s.", 
                        IconPlatform);
                strcat (string, "\nBy clicking in the graphics window.");

                strcat (string, "\n\nYes - Accept the location.");
                strcat (string, "\nNo  - Reject the location.");
                SetInstr (string);
          /*
           * Pop up the location widget.
           */
# ifdef notdef
                uw_popup ("enter-loc");
# endif
          /*
           * Set up for point picking also.
           */
                rb_Point ();
                break;
        /*
         * Store it.
         */
          case C_LOC_STORE:
          /*
           * Display the instructions for storing the data.
           */
                strcpy (string, "Select a new attribute.\n");

                strcat (string, "\n\nYes - Accept the data and store it.");
                strcat (string, "\nNo  - Reject the data.");
                SetInstr (string);

		ResetTime ();

                break;
          default:
                msg_ELog (EF_PROBLEM, "Weird step %d", Step);
        }
}


static void
YesChangeLoc ()
{
	ZebTime	zt;
	char	timestring[40];
	char	string[1000];
	float	value = 0.0;
        Location        origin;
	PlatformId	pid;
	FieldId		fid;
        DataChunk       *dc;

        switch (Step)
        {
	/*
	 * The data to change has been picked.
	 */
		case C_LOC_PICK:
		/*
		 * Is the platform name a good one.
		 */
			pid = ds_LookupPlatform (IconPlatform);
			if (ds_PlatformDataOrg (pid) != OrgScalar)
			{
				sprintf (string, "%s is not a location!",
					IconPlatform);
                       		SetHelp (string);
				cleanup ();
			}
			else
			{
				TC_UIToZt (&IconTime, &zt);
				TC_EncodeTime (&zt, TC_Full, timestring);
				sprintf (string,"Changing a %s location at %s.",
					IconPlatform, timestring);
				SetHelp (string);
			}
			break;
	/*
	 * The new location has been picked.
	 */
                case C_LOC_ENTER:
                /*
                 * If no point was picked, assume the location was entered
                 * in the widget.
                 */
			rb_Done ();

                        if ((Point.x == -9999) && (Point.y == -9999))
                        {
                        /*
                         * Get the data out of the widget.
                         */
                                SetHelp ("Location entered by widget.");

                                if (strcmp (LocFormat, "latlon") == 0)
                                {
                                    DataLocation.l_lat = atof (LocLatX);
                                    DataLocation.l_lon = atof (LocLonY);
                                    DataLocation.l_alt = atof (LocAlt);
                                }
                                else if (strcmp (LocFormat, "km") == 0)
                                {
                                    if (! GetLocation (LocOrigin, &PlotTime, 
                                        &origin))
                                    {
                                        msg_ELog (EF_PROBLEM, 
                                                "Unable to locate origin %s.", 
                                                LocOrigin);
                                        break;
                                    }
                                    cvt_Origin (origin.l_lat, origin.l_lon);
                                    prj_Reverse (XUSER (atof (LocLatX)),
                                        YUSER (atof (LocLonY)), 
                                        &DataLocation.l_lat, 
                                        &DataLocation.l_lon);
                                }
                                else if (strcmp (LocFormat, "nm") == 0)
                                {
                                        msg_ELog (EF_INFO, "Don't do nm yet.");
                                        break;
                                }
                                else
                                {
                                        msg_ELog (EF_PROBLEM, 
                                                "Unknown format %s", LocFormat);
                                        break;
                                }
                        }
                       else
                        {
                        /*
                         * Get the data out of the picked point.
                         */
                                SetHelp ("Location entered by pointer.");
                                prj_Reverse (XUSER (Point.x), YUSER (Point.y),
                                        &DataLocation.l_lat, 
                                        &DataLocation.l_lon);
                                DataLocation.l_alt = 0.0;
                        }
                        break;
	/*
	 * The new data has been entered.
	 */
                case C_LOC_STORE:
		/*
		 * Get the platform name.
		 */
			strcpy (DataPlatform, IconPlatform);
                /*
                 * Get the time the data is stored at.
                 */
			TC_UIToZt (&IconTime, &DataTime);
			DataTime.zt_MicroSec = 0;
                /*
                 * Put it all into the data store.
                 */
			dc = dc_CreateDC (DCC_Scalar);
			pid = ds_LookupPlatform (DataPlatform);
			dc->dc_Platform = pid;
		/*
		 * Set up a phoney field.
		 */
			fid = F_Lookup ("temp");
			dc_SetScalarFields (dc, 1, &fid);
			dc_AddScalar (dc, &DataTime, 0, fid, &value);
		/*
		 * Set the location.
		 */
			dc_SetLoc (dc, 0, &DataLocation);
# ifdef notdef
			dc_LocAdd (dc, &DataTime, &DataLocation);
# endif
                /*
                 * Set the attribute.
                 */
                        if (AttrEntry >= 0)
                                dc_SetSampleAttr (dc, 0, "type", 
					Attr[AttrEntry]); 
                /*
                 * Store it.
                 */
                        ds_Store (dc, FALSE, NULL, 0);
		/*
		 * Free memory.
		 */
			dc_DestroyDC (dc);
                /*
                 * Get out of change mode.
                 */
			sprintf (string, "%s data stored.", DataPlatform);
                        SetHelp (string);
                        cleanup ();
                        break;
                default:
                        break;
        }

}



#ifdef notdef
static void
Icons ()
/*
 * Activate the icons enable/disable menu.
 */
{
        int	i, numplat;
        char	platforms[PlatformListLen];
	char	*platlist[MaxPlatforms];
        Arg	arg;
/*
 * Read in and parse the user insertable platforms.
 */
        if (! pda_Search (Pd, "global", "user-insert-platforms", NULL,
                platforms, SYMT_STRING))
        {
                NumIconPlats = 0;
                msg_ELog (EF_PROBLEM, "No user insertable platforms.");
                SetHelp ("No user insertable platforms.");
        }
        else
        {
                numplat = CommaParse (platforms, platlist);
        /*
         * Fill in each platform menu entry.
        */
                for (i = 0, NumIconPlats = 0; i < numplat; i++)
                {
                /*
                 * Test the platform.
                 */
                        if (ds_LookupPlatform (platlist[i]) == 
                                BadPlatform)
                        {
                                msg_ELog (EF_PROBLEM, "Unknown platform '%s'.",
                                        platlist[i]);
                                continue;
                        }
                        strcpy (IconPlats[NumIconPlats], platlist[i]);
                /*
                 * Set the label.
                 */
                        XtSetArg (arg, XtNlabel, platlist[i]);
                        XtSetValues (IconEntries[NumIconPlats], &arg, 1);
                /*
                 * If its not managed, manage it.
                 */
                        if (NumIconPlats >= IconNManaged)
                        {
                                XtManageChild (IconEntries[NumIconPlats]);
                                IconNManaged++;
                        }
                /*
                 * Increment the number of good platforms.
                 */
                        NumIconPlats++;
                }
	}
/*
 * Clean out extras.
 */
        for (i = NumIconPlats; i < IconNManaged; i++)
                XtUnmanageChild (IconEntries[i]);
        IconNManaged = NumIconPlats;
}



static void
IconToggle (w, entry, junk)
Widget		w;
XtPointer	entry, junk;
/*
 * Toggle the icons for the platform indicated by entry.
 */
{
	int	i = (int) entry;
	char	param[40];
	zbool	iconon;
/*
 * Is the icon currently on or off?
 */
	sprintf (param, "%s-show-icon", IconPlats[i]);
	iconon = TRUE;
	pda_Search (Pd, "global", param, NULL, (char *) &iconon, SYMT_BOOL);
/*
 * Toggle it.
 */
	if (iconon)
		iconon = FALSE;
	else
		iconon = TRUE;
	pd_Store (Pd, "global", param, (char *) &iconon, SYMT_BOOL);
/*
 * Queue up a replot and ship the PD back to the display manager.
 */
	Eq_AddEvent (PDisplay, pc_ParamChange, param, strlen (param) + 1, 
		Augment);
	Eq_AddEvent (PWhenever, eq_ReturnPD, 0, 0, Override);
	pdm_ScheduleUpdate ();
}
#endif /* notdef */



static void
InitInsertMenu (parent)
Widget	parent;
/*
 * Set up the insert menu.
 */
{
	int i, n;
	Arg args[2];
/*
 * Create a shell for the menu.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Data Insertion Menu");	n++;
	InsertMenu = XtCreatePopupShell ("DataInsert", simpleMenuWidgetClass,
		parent, args, n);
	XtAddCallback (InsertMenu, XtNpopupCallback, (XtCallbackProc) Insert,
		(XtPointer) 0);
	XtCreateManagedWidget ("Line", smeLineObjectClass, InsertMenu, NULL, 0);
/*
 * Create all of the entries, but don't manage them now.
 */
	n = 0;
	XtSetArg (args[0], XtNlabel, "(nuttin)");       n++;
	for (i = 0; i < MAXENTRY; i++)
	{
		InsertEntries[i] = XtCreateWidget ("InsertEntry", 
			smeBSBObjectClass, InsertMenu, args, n);
		XtAddCallback (InsertEntries[i], XtNcallback, 
			(XtCallbackProc) InsertData, (XtPointer)(long) i);
	}
	InsertNManaged = 0;
}


static void
InitAttrMenu (parent)
Widget	parent;
/*
 * Set up the select attribute menu.
 */
{
	int i, n;
	Arg args[2];
/*
 * Create a shell for the menu.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Select Attributes:");	n++;
	AttrMenu = XtCreatePopupShell ("SelectAttr", simpleMenuWidgetClass,
		parent, args, n);
	XtAddCallback (AttrMenu, XtNpopupCallback, (XtCallbackProc) Attributes,
		(XtPointer) 0);
	XtCreateManagedWidget ("Line", smeLineObjectClass, AttrMenu, NULL, 0);
/*
 * Create all of the entries, but don't manage them now.
 */
	n = 0;
	XtSetArg (args[0], XtNlabel, "(nuttin)");       n++;
	for (i = 0; i < MAXENTRY; i++)
	{
		AttrEntries[i] = XtCreateWidget ("AttrEntry", 
			smeBSBObjectClass, AttrMenu, args, n);
		XtAddCallback (AttrEntries[i], XtNcallback, 
			(XtCallbackProc) SelectAttr, (XtPointer)(long) i);
	}
	AttrNManaged = 0;
}


#ifdef notdef
static void
InitIconMenu (parent)
Widget	parent;
/*
 * Set up the icon menu.
 */
{
	int i, n;
	Arg args[2];
/*
 * Create a shell for the menu.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Icon Enable/Disable Menu");	n++;
	IconMenu = XtCreatePopupShell ("IconToggle", simpleMenuWidgetClass,
		parent, args, n);
	XtAddCallback (IconMenu, XtNpopupCallback, (XtCallbackProc) Icons,
		(XtPointer) 0);
	XtCreateManagedWidget ("Line", smeLineObjectClass, IconMenu, NULL, 0);
/*
 * Create all of the entries, but don't manage them now.
 */
	n = 0;
	XtSetArg (args[0], XtNlabel, "(nuttin)");       n++;
	for (i = 0; i < MAXENTRY; i++)
	{
		IconEntries[i] = XtCreateWidget ("IconEntry", 
			smeBSBObjectClass, IconMenu, args, n);
		XtAddCallback (IconEntries[i], XtNcallback, 
			(XtCallbackProc) IconToggle, (XtPointer) i);
	}
	IconNManaged = 0;
}
#endif /* notdef */


static void
InitChangeMenu (parent)
Widget	parent;
/*
 * Set up the change menu.
 */
{
	int i, n;
	Arg args[2];
/*
 * Create a shell for the menu.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Change ... ");	n++;
	ChangeMenu = XtCreatePopupShell ("ChangeMenu", simpleMenuWidgetClass,
		parent, args, n);
	XtAddCallback (ChangeMenu, XtNpopupCallback, (XtCallbackProc) Change,
		(XtPointer) 0);
	XtCreateManagedWidget ("Line", smeLineObjectClass, ChangeMenu, NULL, 0);
/*
 * Create all of the entries, but don't manage them now.
 */
	n = 0;
	XtSetArg (args[0], XtNlabel, "(nuttin)");       n++;
	for (i = 0; i < 2; i++)
	{
		ChangeEntries[i] = XtCreateWidget ("ChangeEntry", 
			smeBSBObjectClass, ChangeMenu, args, n);
		XtAddCallback (ChangeEntries[i], XtNcallback, 
			(XtCallbackProc) SelectChange, (XtPointer)(long) i);
	}
	ChangeNManaged = 0;
}


static void
Remove ()
/*
 * Remove some previously entered data.
 */
{
	char string[1000];
/*
 * See what mode we're in and if its possible to remove.
 */
	if (Mode != NOTHING)
	{
		SetHelp ("Abort current operation before removing.");
	}
	else
	{
		strcpy (string, "Remove data by selecting in the graphics window.");
		strcat (string, "\n\nYes - Remove the selected data.");
		strcat (string, "\nNo  - Do Not remove the selected data.");
		SetInstr (string);
		
		SetHelp ("Initiating Remove operation.");
		Step = 0;
		Type = 0;
		Mode = REMOVE;
	}
}


static void
YesRemove ()
{
	msg_ELog (EF_DEBUG, "Removing %s data at %d %d.", IconPlatform,
		IconTime.ds_yymmdd, IconTime.ds_hhmmss);
	SetHelp ("Data removed.");
	cleanup ();
}


static void
Change ()
/*
 * Activate the change menu.
 */
{
	int i, numentries;
        Arg arg;
	char	*entries[10];
/*
 * Set up the entries.
 */
	entries[OUTLINE] = "an outline.";
	entries[LOCATION] = "a location.";
	numentries = 2;
/*
 * Add the entries and manage them.
 */
	for (i = 0; i < numentries; i++)
	{
	/*
	 * Set the labels.
	 */
		XtSetArg (arg, XtNlabel, entries[i]);
		XtSetValues (ChangeEntries[i], &arg, 1);
	/*
	 * If its not managed, manage it.
	 */
		if (i >= ChangeNManaged)
		{
			XtManageChild (ChangeEntries[i]);
			ChangeNManaged++;
		}
	}
/*
 * Clean out extras.
 */
        for (i = numentries; i < ChangeNManaged; i++)
                XtUnmanageChild (ChangeEntries[i]);
        ChangeNManaged = numentries;

}


static void
SelectChange (w, entry, junk)
Widget		w;
XtPointer	entry, junk;
/*
 * Change the type of data indicated by the entry.
 */
{
	long	e = (long) entry;
/*
 * See what mode we're in and if its possible to insert.
 */	
	if (Mode != NOTHING)
	{
		SetHelp ("Abort current operation before changing modes.");
	}	
	else
	{
	/*
	 * Check that the entry number is valid.
	 */
		if (e < 0 || e >= 2)
			return;
	/*
	 * Set the Mode to CHANGE.
	 */
		Mode = CHANGE;
		Type = e;
	/*
 	 * Reset the step counter.
	 */
		Step = 0;
	/*
	 * Call the insertion procedure depending on data type.
	 */
		(*Routines[Mode][e]) (NULL);
	}
}


static void
ResetTime ()
/*
 * Totally kludgey way of setting the valid time which applies only to
 * nowcasting for RAPS92.
 */
{
        ZebTime zt;
        int     year, month, day, hour, min;
/*
 * Get the current time.
 */
        tl_Time (&zt);
/*
 * Round the time to the nearest hour of half hour.  
 */
        TC_ZtSplit (&zt, &year, &month, &day, &hour, &min, NULL, NULL);
	if (min < 20)
                TC_ZtAssemble (&zt, year, month, day, hour, 45, 0, 0);
	else if (min < 50)
                TC_ZtAssemble (&zt, year, month, day, hour + 1, 15, 0, 0);
	else
                TC_ZtAssemble (&zt, year, month, day, hour + 1, 45, 0, 0);
/*
 * Store it in the ValidTime.
 */
        TC_EncodeTime (&zt, TC_Full, ValidTime);
        SetTime ();
}


static void
AbortIt ()
/*
 * Abort the current operation.
 */
{
/*
 * See what mode we're in and abort it.
 */
	switch (Mode)
	{
		case NOTHING:
			SetHelp ("No operation to abort.");
			break;
		case INSERT:
			SetHelp ("Insert operation aborted.");
			if (Step == I_LINE_DRAW || Step == I_LINE_CENTER ||
				Step == I_LOC_ENTER)
				rb_Done ();
			break;
		case REMOVE:
			SetHelp ("Remove operation aborted.");
			break;
		case CHANGE:
			SetHelp ("Change operation aborted.");
			if (Step == C_LINE_DRAW || Step == C_LINE_CENTER ||
				Step == C_LOC_ENTER)
				rb_Done ();
			break;
		default:
			msg_ELog (EF_PROBLEM, "Weird mode %d", Mode);
	}
	cleanup ();
}


static void
YesNo (w, yesno, junk)
Widget		w;
XtPointer	yesno, junk;
/*
 * They clicked the 'Yes' or 'No' buttons.
 */
{
/*
 * Are we really trying to do something?
 */
	if (Mode != NOTHING)
	{
	/*
	 * If they clicked 'Yes' tell them what they did and increment the 
	 * Step, otherwise they clicked 'No' so tell them what they didn't do.
	 */
		if ((long) yesno == YES)
		{
			if (YesRoutines[Mode][Type])
				(*YesRoutines[Mode][Type]) ();
			Step++;
		}
		else
			SetHelp (NoStrings[Mode][Type][Step]);
	/*
	 * Call the appropriate routine.
	 */
		if (Routines[Mode][Type])
			(*Routines[Mode][Type]) (Entry);
	}
}


#ifdef notdef
static void
ChangeHour (w, change, junk)
Widget		w;
XtPointer	change, junk;
/*
 * Change the hour with the buttons.
 */
{
	date t;
	ZebTime zt;
/*
 * Convert the time.
 */
	uit_parse_date (ValidTime, &t, FALSE);
	TC_UIToZt (&t, &zt);
/*
 * Change the time.
 */
	if ((int) change == 1)
		zt.zt_Sec += 3600;
	else
		zt.zt_Sec -= 3600;
/*
 * Convert time to a string. 
 */
	TC_EncodeTime (&zt, TC_Full, ValidTime);
	SetTime();
}


static void
ChangeMin (w, change, junk)
Widget		w;
XtPointer	change, junk;
/*
 * Change the minutes with the buttons.
 */
{
	date t;
	ZebTime zt;
/*
 * Convert the time.
 */
	uit_parse_date (ValidTime, &t, FALSE);
	TC_UIToZt (&t, &zt);
/*
 * Change the time.
 */
	if ((int) change == 1)
		zt.zt_Sec += 60;
	else
		zt.zt_Sec -= 60;
/*
 * Convert it back.
 */
	TC_EncodeTime (&zt, TC_Full, ValidTime);
	SetTime();
}
#endif /* notdef */


static void
SetInstr (string)
char	*string;
/*
 * Set the text of the Instructions Label to string.
 */
{
	Arg arg;

	XtSetArg (arg, XtNlabel, string);
	XtSetValues (InstrLabel, &arg, 1);
}


static void
SetTime ()
/*
 * Set the insert time string.
 */
{
	Arg arg;

	XtSetArg (arg, XtNstring, ValidTime);
	XtSetValues (TimeText, &arg, 1);
}


static void
SetHelp (string)
char	*string;
/*
 * Set the text of the Help Label to string.
 */
{
	Arg arg;

	XtSetArg (arg, XtNlabel, string);
	XtSetValues (HelpLabel, &arg, 1);
}


static void
set_help_strings ()
/*
 * Fill the help string arrays used for the 'No' button.
 */
{
/*
 * Inserting...
 */
/*
 * The outline help strings.
 */
	NoStrings [INSERT][OUTLINE][I_LINE_DRAW] = "Outline rejected.";
	NoStrings [INSERT][OUTLINE][I_LINE_CENTER] = "Center rejected.";
	NoStrings [INSERT][OUTLINE][I_LINE_STORE] = "Data rejected.";
/*
 * The location help strings.
 */
	NoStrings [INSERT][LOCATION][I_LOC_ENTER] = "Location rejected.";
	NoStrings [INSERT][LOCATION][I_LOC_STORE] = "Data rejected.";
/*
 * Changing...
 */
/*
 * an outline.
 */
	NoStrings [CHANGE][OUTLINE][C_LINE_PICK] = "Pick rejected.";
	NoStrings [CHANGE][OUTLINE][C_LINE_DRAW] = "Outline rejected.";
	NoStrings [CHANGE][OUTLINE][C_LINE_CENTER] = "Center rejected.";
	NoStrings [CHANGE][OUTLINE][C_LINE_STORE] = "Data rejected.";
/*
 * Removing.
 */
	NoStrings [REMOVE][OUTLINE][REMOVE_RM] = "Data rejected.";
	NoStrings [REMOVE][LOCATION][REMOVE_RM] = "Data rejected.";
}


static void
set_routines ()
/*
 * Save routine names.
 */
{
/*
 * Insert routines.
 */
	Routines[INSERT][OUTLINE] = InsertLine;
	Routines[INSERT][LOCATION] = InsertLoc;
/*
 * Remove Routines.
 */
	Routines[REMOVE][0] = Remove;
/*
 * Change Routines.
 */
	Routines[CHANGE][OUTLINE] = ChangeLine;
	Routines[CHANGE][LOCATION] = ChangeLoc;
/*
 * Yes routines.  Routines that do stuff when 'Yes' is clicked.
 */
	YesRoutines[INSERT][OUTLINE] = YesInsertLine;
	YesRoutines[INSERT][LOCATION] = YesInsertLoc;
	YesRoutines[REMOVE][0] = YesRemove;
	YesRoutines[CHANGE][OUTLINE] = YesChangeLine;
	YesRoutines[CHANGE][LOCATION] = YesChangeLoc;
}


static void
cleanup ()
/*
 * Clean up when an operation is finished.
 */
{
	IconPlatform[0] = '\0';
	SetInstr ("");
	Entry = AttrEntry = -1;
	Step = 0;
	Mode = NOTHING;
}

