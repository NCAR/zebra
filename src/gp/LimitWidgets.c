/*
 * Widgets for changing plot limits.
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
# include <X11/Shell.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/Cardinals.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Scrollbar.h>
# include <X11/Xaw/Box.h>
# include <X11/Xaw/Toggle.h>

# include <ui.h>
# include <defs.h>
# include <message.h>
# include <DataStore.h>
# include <pd.h>
# include "GraphProc.h"

RCSID("$Id: LimitWidgets.c,v 2.27 2001-06-19 23:48:30 granger Exp $")

/*
 * Station widget static data.
 */
# define MAXSTA 50

static int Sw_Nsta;			/* Number of stations		*/
static Widget Sw_Swidgets[MAXSTA];	/* Per-station toggles		*/
static Widget Sw_Form;			/* Form that holds the stations	*/
static zbool Sw_Sset[MAXSTA];		/* Is this station selected?	*/
static char Sw_RetSta[40*MAXSTA];	/* What we return		*/
static char Sw_Plat[40], SavePlat[40*MAXSTA];
static int SwNManaged;		/* Number of managed sw boxes	*/

/*
 * The types of widgets we know.
 */
typedef enum {
	SingleFloatWidget = 0,
	DoubleFloatWidget = 1,
	SingleStringWidget = 2,
	TimeSeriesWidget = 3,
	SingleIntWidget = 4,
	StationWidget = 5
} WidgetType;
# define N_WIDGET_TYPES 6	/* Keep this updated!	*/

# define LW_FORM "LimitForm"	/* For keeping resource files manageable */

/*
 * Widget queues -- this is how we keep track of which widgets are out
 * there.
 */
typedef struct _WidgetQueue_
{
	Widget	*wq_widget;		/* The actual widget		*/
	struct _WidgetQueue_ *wq_next;	/* Next entry in the queue	*/
	char	wq_name[32];		/* Name of this widget		*/
	void	*wq_wdata;		/* Widget-specific data		*/
	WidgetType	wq_type;	/* Type of this widget		*/
	char	wq_comp[32];		/* Component			*/
	char	wq_param[2][32];	/* Parameter			*/
} WidgetQueue;

/*
 * The free and used queues.
 */
static WidgetQueue *FreeQueue[N_WIDGET_TYPES];
static WidgetQueue *BusyQueue[N_WIDGET_TYPES];

/*
 * Here we describe the individual widgets.
 */
typedef struct _WidgetDesc_
{
	char	*wd_name;	/* Name of this widget type	*/
	WidgetType we_type;	/* The actual type		*/
	Widget	(*wd_create)();	/* Create routine		*/
	void	(*wd_setup)();	/* Setup routine		*/
	int	wd_seq;		/* Sequence number		*/
} WidgetDesc;

/*
 * Routines that manipulate specific widgets.
 */
static Widget	lw_SFCreate FP ((char *, Widget, XtAppContext));
static void	lw_SFSetup FP ((WidgetQueue *, struct ui_command *));
static void	lw_SFStore FP ((Widget, WidgetQueue *, XtPointer));
static Widget	lw_DFCreate FP ((char *, Widget, XtAppContext));
static void	lw_DFSetup FP ((WidgetQueue *, struct ui_command *));
static void	lw_DFStore FP ((Widget, WidgetQueue *, XtPointer));
static Widget	lw_SSCreate FP ((char *, Widget, XtAppContext));
static void	lw_SSSetup FP ((WidgetQueue *, struct ui_command *));
static void	lw_SSStore FP ((Widget, WidgetQueue *, XtPointer));
static Widget	lw_TSCreate FP ((char *, Widget, XtAppContext));
static void	lw_TSSetup FP ((WidgetQueue *, struct ui_command *));
static void	lw_TSStore FP ((Widget, WidgetQueue *, XtPointer));
static Widget	lw_SICreate FP ((char *, Widget, XtAppContext));
static void	lw_SISetup FP ((WidgetQueue *, struct ui_command *));
static void	lw_SIStore FP ((Widget, WidgetQueue *, XtPointer));
static Widget	lw_SWCreate FP ((char *, Widget, XtAppContext));
static void	lw_SWSetup FP ((WidgetQueue *, struct ui_command *));
static void	lw_SWStore FP ((Widget, WidgetQueue *, XtPointer));


static WidgetDesc WTable[N_WIDGET_TYPES] =
{
	{ "SingleFloat",	SingleFloatWidget,	lw_SFCreate,
	  lw_SFSetup,		0},
	{ "DoubleFloat",	DoubleFloatWidget,	lw_DFCreate,
	  lw_DFSetup,		0},
	{ "SingleString",	SingleStringWidget,	lw_SSCreate,
	  lw_SSSetup,		0},
	{ "TimeSeries",		TimeSeriesWidget,	lw_TSCreate,
	  lw_TSSetup,		0},
	{ "SingleInt",		SingleIntWidget,	lw_SICreate,
	  lw_SISetup,		0},
	{ "StationWidget",	StationWidget,		lw_SWCreate,
	  lw_SWSetup,		0},
};


/*
 * Widget-specific structures.
 */
struct SFWData
{
	Widget d_label;
	Widget d_value;
	char d_vstring[50];
};

struct DFWData
{
	Widget d_label[2];
	Widget d_value[2];
	char d_vstring[2][50];
};

/*
 * Other local routines.
 */
static void	 lw_Popup FP ((WidgetQueue *));
static void	 lw_CBPopdown FP ((Widget, WidgetQueue *, XtPointer));
static void	 lw_Popdown FP ((WidgetQueue *));
static void	 lw_Setup FP ((WidgetQueue *, int, struct ui_command *));
static WidgetQueue	 *lw_GetWidget FP ((WidgetType));
static void	 lw_SwCb FP ((Widget, int, int));
static void	 CopyBackground FP ((Widget soure, Widget dest));


void
lw_InitWidgets ()
/*
 * Initialize all of our widgets.
 */
{
	int i;

	for (i = 0; i < N_WIDGET_TYPES; i++)
		FreeQueue[i] = BusyQueue[i] = (WidgetQueue *) 0;
}






void
lw_ActivateWidget (type, cmds)
int type;
struct ui_command *cmds;
/*
 * Activate a widget of this type.
 */
{
	WidgetQueue *w;
/*
 * Get a widget.
 */
	w = lw_GetWidget ((WidgetType) type);

	if (type == StationWidget)
	{
		pda_Search (Pd, UPTR(cmds[0]), "num-stations", NULL, 
			(char *) &Sw_Nsta, SYMT_INT);
		if (Sw_Nsta > MAXSTA) Sw_Nsta = MAXSTA;
		msg_ELog (EF_DEBUG, "Setting Sw_Nsta to %d", Sw_Nsta);
	}
/*
 * Put it on the screen.  We do this first because we have to guarantee that
 * it exists before it can be configured.
 */
	lw_Popup (w);
/*
 * Configure it and put it onto the busy queue.
 */
	lw_Setup (w, type, cmds);
# ifdef notdef
	w->wq_next = BusyQueue[type];
	BusyQueue[type] = w;
# endif
}




static WidgetQueue *
lw_GetWidget (type)
WidgetType type;
/*
 * Get a widget of this type.
 */
{
	WidgetQueue *ret;
	WidgetDesc *wd = WTable + type;
/*
 *  If there is one sitting on the free queue, we just remove and
 * return it.
 */
	if (FreeQueue[type])
	{
		ret = FreeQueue[type];
		FreeQueue[type] = ret->wq_next;
		return (ret);
	}
/*
 * Nope, allocate a new entry.
 */
	ret = ALLOC (WidgetQueue);
	ret->wq_next = (WidgetQueue *) 0;
	ret->wq_type = type;
	sprintf (ret->wq_name, "%s%d", wd->wd_name, wd->wd_seq++);
/*
 * Make it known to UI.
 */
	uw_def_widget (ret->wq_name, "", wd->wd_create, 0, (char *) ret);
	uw_NoHeader (ret->wq_name);
# ifdef notdef
	uw_ForceOverride (ret->wq_name);
# endif
/*
 * All done -- send it back.
 */
	return (ret);
}




static void
lw_Popup (w)
WidgetQueue *w;
/*
 * Put this one on the screen.
 */
{
	int width,height;
	int swidth,sheight;
	Window wjunk;
	int junk, x, y;
/*
 * Find out where the pointer is.
 */
	XQueryPointer (Disp, XtWindow (Graphics), &wjunk, &wjunk, &x, &y,
			&junk, &junk, (unsigned int *) &junk);
	msg_ELog (EF_DEBUG, "Popup, ptr at %d %d", x, y);
/*
 * Set the geometry of this widget to be near the pointer.
 * But make sure the whole widget is on the screen
 */
	if ((x -= 50) < 0)
		x = 0;
	if ((y -= 10) < 0)
		y = 0;
# ifdef notdef
/*
 * This doesn't work because wq_widget is never assigned a value.
 */
	n = 0;
	XtSetArg (args[n], XtNwidth, &width);		n++;
	XtSetArg (args[n], XtNheight, &height);		n++;
/*  this was bombing, so hope 150 is enough...
	XtGetValues (w->wq_widget, args, n);*/
# endif
	width = 500;
	height = 100;
	swidth = XWidthOfScreen(XtScreen(Top));
	sheight = XHeightOfScreen(XtScreen(Top));
	if ((x + width) > swidth)
		x = swidth - width;
	if ((y + height) > sheight)
		y = sheight - height;
	uw_SetGeometry (w->wq_name, x, y, 0, 0);
/*
 * Now force it up on the screen.
 */
	uw_popup (w->wq_name);
}



static void
CopyBackground(source,dest)
Widget source;
Widget dest;
/*
 * Copies the background color of the source widget to the dest widget
 */
{
	Pixel color;
	Arg arg;

	XtSetArg (arg, XtNbackground, &color);
	XtGetValues (source, &arg, (Cardinal)1);

	XtSetArg (arg, XtNbackground, color);
	XtSetValues (dest, &arg, (Cardinal)1);
}




static Widget
lw_SSCreate (tag, parent, actx)
char *tag;
Widget parent;
XtAppContext actx;
/*
 * Create a SingleString widget.
 */
{
	Widget form, w, above;
	Arg args[40];
	int n;
	struct SFWData *wdata = ALLOC (struct SFWData);
	WidgetQueue *wq = (WidgetQueue *) tag;
/*
 * There is the inevitable form to hold everything.
 */
	n = 0;
 	XtSetArg (args[n], XtNdefaultDistance, 5); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	form = XtCreateManagedWidget (LW_FORM, formWidgetClass, parent,
		args, n);
/*
 * Give our parent the same background, since we'll be its only child
 */
	CopyBackground(form, parent);
/*
 * Install any accelerators that have been set in our resources in the
 * the entire form.
 */
	XtInstallAllAccelerators (form, form);
/*
 * The label to describe this string.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNlabel, "Gimme something:   "); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	above = wdata->d_label = XtCreateManagedWidget ("SSLabel",
		labelWidgetClass, form, args, n);
/*
 * Next, the text widget to hold the */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, wdata->d_label); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNdisplayPosition, 0); n++;
	XtSetArg (args[n], XtNinsertPosition, 0); n++;
	XtSetArg (args[n], XtNresize, XawtextResizeNever); n++;
	XtSetArg (args[n], XtNwidth, 40); n++;
	XtSetArg (args[n], XtNtype, XawAsciiString); n++;
	XtSetArg (args[n], XtNuseStringInPlace, True); n++;
	XtSetArg (args[n], XtNlength, 40); n++;
	XtSetArg (args[n], XtNstring, wdata->d_vstring); n++;
	XtSetArg (args[n], XtNleftMargin, 5); n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit); n++;
	wdata->d_value = XtCreateManagedWidget ("SSValue",asciiTextWidgetClass,
		form, args, n);
	wq->wq_wdata = (void *) wdata;
/*
 * The help button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Help"); n++;
	w = XtCreateManagedWidget ("help", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, (XtCallbackProc) HelpCallback, 
		(XtPointer) GP_HELP_LIMITS);
/*
 * The "store" button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Store"); n++;
	w = XtCreateManagedWidget ("store", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, (XtCallbackProc) lw_SSStore, 
		(XtPointer) wq);
/*
 * The "cancel" button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Cancel"); n++;
	w = XtCreateManagedWidget ("cancel", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, (XtCallbackProc) lw_CBPopdown, 
		(XtPointer) wq);
	return (form);
}


static Widget
lw_SICreate (tag, parent, actx)
char *tag;
Widget parent;
XtAppContext actx;
/*
 * Create a SingleInt widget.
 */
{
	Widget form, w, above;
	Arg args[20];
	int n;
	struct SFWData *wdata = ALLOC (struct SFWData);
	WidgetQueue *wq = (WidgetQueue *) tag;
/*
 * There is the inevitable form to hold everything.
 */
	n = 0;
 	XtSetArg (args[n], XtNdefaultDistance, 5); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	form = XtCreateManagedWidget (LW_FORM, formWidgetClass, parent,
		args, n);
/*
 * Give our parent the same background, since we'll be its only child
 */
	CopyBackground(form, parent);
/*
 * Install any accelerators that have been set in our resources in the
 * the entire form.
 */
	XtInstallAllAccelerators (form, form);
/*
 * The label to describe this string.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNlabel, "Gimme something:   "); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	above = wdata->d_label = XtCreateManagedWidget ("SILabel",
		labelWidgetClass, form, args, n);
/*
 * Next, the text widget to hold the */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, wdata->d_label); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNdisplayPosition, 0); n++;
	XtSetArg (args[n], XtNinsertPosition, 0); n++;
	XtSetArg (args[n], XtNresize, XawtextResizeNever); n++;
	XtSetArg (args[n], XtNwidth, 60); n++;
	XtSetArg (args[n], XtNtype, XawAsciiString); n++;
	XtSetArg (args[n], XtNuseStringInPlace, True); n++;
	XtSetArg (args[n], XtNlength, 40); n++;
	XtSetArg (args[n], XtNstring, wdata->d_vstring); n++;
	XtSetArg (args[n], XtNleftMargin, 5); n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit); n++;
	wdata->d_value = XtCreateManagedWidget ("SIValue",asciiTextWidgetClass,
		form, args, n);
	wq->wq_wdata = (void *) wdata;
/*
 * The help button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Help"); n++;
	w = XtCreateManagedWidget ("help", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, (XtCallbackProc) HelpCallback, 
		(XtPointer) GP_HELP_LIMITS);
/*
 * The "store global" button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Store"); n++;
	w = XtCreateManagedWidget ("store", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, (XtCallbackProc) lw_SIStore, 
		(XtPointer) wq);
# ifdef notdef
/*
 * The "store local" button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Store local"); n++;
	w = XtCreateManagedWidget ("local", commandWidgetClass, form,
		args, n);
	/* XtAddCallback (w, XtNcallback, (XtCallbackProc) mc_MovieRun, 
		(XtPointer) 0); */
# endif
/*
 * The "cancel" button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Cancel"); n++;
	w = XtCreateManagedWidget ("cancel", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, (XtCallbackProc) lw_CBPopdown, 
		(XtPointer) wq);
	return (form);
}


static Widget
lw_SFCreate (tag, parent, actx)
char *tag;
Widget parent;
XtAppContext actx;
/*
 * Create a SingleFloat widget.
 */
{
	Widget form, w, above;
	Arg args[20];
	int n;
	struct SFWData *wdata = ALLOC (struct SFWData);
	WidgetQueue *wq = (WidgetQueue *) tag;
/*
 * There is the inevitable form to hold everything.
 */
	n = 0;
 	XtSetArg (args[n], XtNdefaultDistance, 5); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	form = XtCreateManagedWidget (LW_FORM, formWidgetClass, parent,
		args, n);
/*
 * Give our parent the same background, since we'll be its only child
 */
	CopyBackground(form, parent);
/*
 * Install any accelerators that have been set in our resources in the
 * the entire form.
 */
	XtInstallAllAccelerators (form, form);
/*
 * The label to describe this floating point quantity.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNlabel, "Gimme something:   "); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	above = wdata->d_label = XtCreateManagedWidget ("SFLabel",
		labelWidgetClass, form, args, n);
/*
 * Next, the text widget to hold the */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, wdata->d_label); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNdisplayPosition, 0); n++;
	XtSetArg (args[n], XtNinsertPosition, 0); n++;
	XtSetArg (args[n], XtNresize, XawtextResizeNever); n++;
	XtSetArg (args[n], XtNwidth, 60); n++;
	XtSetArg (args[n], XtNtype, XawAsciiString); n++;
	XtSetArg (args[n], XtNuseStringInPlace, True); n++;
	XtSetArg (args[n], XtNlength, 40); n++;
	XtSetArg (args[n], XtNstring, wdata->d_vstring); n++;
	XtSetArg (args[n], XtNleftMargin, 5); n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit); n++;
	wdata->d_value = XtCreateManagedWidget ("SFValue",asciiTextWidgetClass,
		form, args, n);
	wq->wq_wdata = (void *) wdata;
/*
 * The help button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Help"); n++;
	w = XtCreateManagedWidget ("help", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, (XtCallbackProc) HelpCallback, 
		(XtPointer) GP_HELP_LIMITS);
/*
 * The "store global" button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Store"); n++;
	w = XtCreateManagedWidget ("store", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, (XtCallbackProc) lw_SFStore, 
		(XtPointer) wq);
# ifdef notdef
/*
 * The "store local" button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Store local"); n++;
	w = XtCreateManagedWidget ("local", commandWidgetClass, form,
		args, n);
	/* XtAddCallback (w, XtNcallback, (XtCallbackProc) mc_MovieRun, 
		(XtPointer) 0); */
# endif
/*
 * The "cancel" button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Cancel"); n++;
	w = XtCreateManagedWidget ("cancel", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, (XtCallbackProc) lw_CBPopdown, 
		(XtPointer) wq);
	return (form);
}


static Widget
lw_DFCreate (tag, parent, actx)
char *tag;
Widget parent;
XtAppContext actx;
/*
 * Create a DoubleFloat widget.
 */
{
	Widget form, w, above;
	Arg args[20];
	int n;
	struct DFWData *wdata = ALLOC (struct DFWData);
	WidgetQueue *wq = (WidgetQueue *) tag;
/*
 * There is the inevitable form to hold everything.
 */
	n = 0;
 	XtSetArg (args[n], XtNdefaultDistance, 5); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	form = XtCreateManagedWidget (LW_FORM, formWidgetClass, parent,
		args, n);
/*
 * Give our parent the same background, since we'll be its only child
 */
	CopyBackground(form, parent);
/*
 * Install any accelerators that have been set in our resources in the
 * the entire form.
 */
	XtInstallAllAccelerators (form, form);
/*
 * The label to describe the first floating point quantity.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNlabel, "Gimme something:   "); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	above = wdata->d_label[0] = XtCreateManagedWidget ("DFLabel1",
		labelWidgetClass, form, args, n);
/*
 * Next, the text widget to hold the 
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, wdata->d_label[0]); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNdisplayPosition, 0); n++;
	XtSetArg (args[n], XtNinsertPosition, 0); n++;
	XtSetArg (args[n], XtNresize, XawtextResizeNever); n++;
	XtSetArg (args[n], XtNwidth, 60); n++;
	XtSetArg (args[n], XtNtype, XawAsciiString); n++;
	XtSetArg (args[n], XtNuseStringInPlace, True); n++;
	XtSetArg (args[n], XtNlength, 40); n++;
	XtSetArg (args[n], XtNstring, wdata->d_vstring[0]); n++;
	XtSetArg (args[n], XtNleftMargin, 5); n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit); n++;
	wdata->d_value[0] = XtCreateManagedWidget ("DFValue1",
		asciiTextWidgetClass, form, args, n);
/*
 * The label to describe the second floating point quantity.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Gimme something:   "); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	wdata->d_label[1] = XtCreateManagedWidget ("DFLabel2",
		labelWidgetClass, form, args, n);
/*
 * Next, the text widget to hold the 
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, wdata->d_label[1]); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNdisplayPosition, 0); n++;
	XtSetArg (args[n], XtNinsertPosition, 0); n++;
	XtSetArg (args[n], XtNresize, XawtextResizeNever); n++;
	XtSetArg (args[n], XtNwidth, 60); n++;
	XtSetArg (args[n], XtNtype, XawAsciiString); n++;
	XtSetArg (args[n], XtNuseStringInPlace, True); n++;
	XtSetArg (args[n], XtNlength, 40); n++;
	XtSetArg (args[n], XtNstring, wdata->d_vstring[1]); n++;
	XtSetArg (args[n], XtNleftMargin, 5); n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit); n++;
	above = wdata->d_value[1] = XtCreateManagedWidget ("DFValue2",
		asciiTextWidgetClass, form, args, n);
	wq->wq_wdata = (void *) wdata;
/*
 * The help button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Help"); n++;
	w = XtCreateManagedWidget ("help", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, (XtCallbackProc) HelpCallback, 
		(XtPointer) GP_HELP_LIMITS);
/*
 * The "store global" button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Store"); n++;
	w = XtCreateManagedWidget ("store", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, (XtCallbackProc) lw_DFStore, 
		(XtPointer) wq);
# ifdef notdef
/*
 * The "store local" button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Store local"); n++;
	w = XtCreateManagedWidget ("local", commandWidgetClass, form,
		args, n);
	/* XtAddCallback (w, XtNcallback, (XtCallbackProc) mc_MovieRun, 
		(XtPointer) 0); */
# endif
/*
 * The "cancel" button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Cancel"); n++;
	w = XtCreateManagedWidget ("cancel", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, (XtCallbackProc) lw_CBPopdown, 
		(XtPointer) wq);
	return (form);
}


static Widget
lw_TSCreate (tag, parent, actx)
char *tag;
Widget parent;
XtAppContext actx;
/*
 * Create a TimeSeries widget.
 */
{
	Widget form, w, above;
	Arg args[20];
	int n;
	struct DFWData *wdata = ALLOC (struct DFWData);
	WidgetQueue *wq = (WidgetQueue *) tag;
/*
 * There is the inevitable form to hold everything.
 */
	n = 0;
 	XtSetArg (args[n], XtNdefaultDistance, 5); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	form = XtCreateManagedWidget (LW_FORM, formWidgetClass, parent,
		args, n);
/*
 * Give our parent the same background, since we'll be its only child
 */
	CopyBackground(form, parent);
/*
 * Install any accelerators that have been set in our resources in the
 * the entire form.
 */
	XtInstallAllAccelerators (form, form);
/*
 * The label to describe the first floating point quantity.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNlabel, "Gimme something:   "); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	above = wdata->d_label[0] = XtCreateManagedWidget ("TSLabel1",
		labelWidgetClass, form, args, n);
/*
 * Next, the text widget to hold the 
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, wdata->d_label[0]); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNdisplayPosition, 0); n++;
	XtSetArg (args[n], XtNinsertPosition, 0); n++;
	XtSetArg (args[n], XtNresize, XawtextResizeNever); n++;
	XtSetArg (args[n], XtNwidth, 60); n++;
	XtSetArg (args[n], XtNtype, XawAsciiString); n++;
	XtSetArg (args[n], XtNuseStringInPlace, True); n++;
	XtSetArg (args[n], XtNlength, 40); n++;
	XtSetArg (args[n], XtNstring, wdata->d_vstring[0]); n++;
	XtSetArg (args[n], XtNleftMargin, 5); n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit); n++;
	wdata->d_value[0] = XtCreateManagedWidget ("TSValue1",
		asciiTextWidgetClass, form, args, n);
/*
 * The label to describe the second floating point quantity.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Gimme something:   "); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	wdata->d_label[1] = XtCreateManagedWidget ("TSLabel2",
		labelWidgetClass, form, args, n);
/*
 * Next, the text widget to hold the 
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, wdata->d_label[1]); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNdisplayPosition, 0); n++;
	XtSetArg (args[n], XtNinsertPosition, 0); n++;
	XtSetArg (args[n], XtNresize, XawtextResizeNever); n++;
	XtSetArg (args[n], XtNwidth, 60); n++;
	XtSetArg (args[n], XtNtype, XawAsciiString); n++;
	XtSetArg (args[n], XtNuseStringInPlace, True); n++;
	XtSetArg (args[n], XtNlength, 40); n++;
	XtSetArg (args[n], XtNstring, wdata->d_vstring[1]); n++;
	XtSetArg (args[n], XtNleftMargin, 5); n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit); n++;
	above = wdata->d_value[1] = XtCreateManagedWidget ("TSValue2",
		asciiTextWidgetClass, form, args, n);
	wq->wq_wdata = (void *) wdata;
/*
 * The help button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Help"); n++;
	w = XtCreateManagedWidget ("help", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, (XtCallbackProc) HelpCallback, 
		(XtPointer) GP_HELP_TSERIES);
/*
 * The "store global" button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Store"); n++;
	w = XtCreateManagedWidget ("store", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, (XtCallbackProc) lw_TSStore, 
		(XtPointer) wq);
# ifdef notdef
/*
 * The "store local" button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Store local"); n++;
	w = XtCreateManagedWidget ("local", commandWidgetClass, form,
		args, n);
	/* XtAddCallback (w, XtNcallback, (XtCallbackProc) mc_MovieRun, 
		(XtPointer) 0); */
# endif
/*
 * The "cancel" button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Cancel"); n++;
	w = XtCreateManagedWidget ("cancel", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, (XtCallbackProc) lw_CBPopdown, 
		(XtPointer) wq);
	return (form);
}




static void
lw_CBPopdown (w, wq, junk)
Widget w;
WidgetQueue *wq;
XtPointer junk;
/*
 * Popdown callback.
 */
{
	lw_Popdown (wq);
}



static void
lw_Popdown (wq)
WidgetQueue *wq;
/*
 * Pop down this widget.
 */
{
	uw_popdown (wq->wq_name);
	wq->wq_next = FreeQueue[wq->wq_type];
	FreeQueue[wq->wq_type] = wq;
}




static void
lw_SSStore (w, wq, junk)
Widget w;
WidgetQueue *wq;
XtPointer junk;
/*
 * Store the value in our widget.
 */
{
	struct SFWData *wd = (struct SFWData *) wq->wq_wdata;
/*
 * Get the string out of the widget and store it.
 */
	msg_ELog (EF_DEBUG, "New value is '%s'", wd->d_vstring);
	parameter (wq->wq_comp, wq->wq_param[0], wd->d_vstring);
	lw_Popdown (wq);
}


static void
lw_SIStore (w, wq, junk)
Widget w;
WidgetQueue *wq;
XtPointer junk;
/*
 * Store the value in our widget.
 */
{
	struct SFWData *wd = (struct SFWData *) wq->wq_wdata;
/*
 * Get the string out of the widget and store it.
 */
	msg_ELog (EF_DEBUG, "New value is '%s'", wd->d_vstring);
	parameter (wq->wq_comp, wq->wq_param[0], wd->d_vstring);
	lw_Popdown (wq);
}


static void
lw_SFStore (w, wq, junk)
Widget w;
WidgetQueue *wq;
XtPointer junk;
/*
 * Store the value in our widget.
 */
{
	struct SFWData *wd = (struct SFWData *) wq->wq_wdata;
/*
 * Get the string out of the widget and store it.
 */
	msg_ELog (EF_DEBUG, "New value is '%s'", wd->d_vstring);
	parameter (wq->wq_comp, wq->wq_param[0], wd->d_vstring);
	lw_Popdown (wq);
}


static void
lw_DFStore (w, wq, junk)
Widget w;
WidgetQueue *wq;
XtPointer junk;
/*
 * Store the value in our widget.
 */
{
	struct DFWData *wd = (struct DFWData *) wq->wq_wdata;
/*
 * Get the string out of the widget and store it.
 */
	msg_ELog (EF_DEBUG, "New value is '%s' '%s'", wd->d_vstring[0],
		wd->d_vstring[1]);
	parameter (wq->wq_comp, wq->wq_param[0], wd->d_vstring[0]);
	parameter (wq->wq_comp, wq->wq_param[1], wd->d_vstring[1]);
	lw_Popdown (wq);
}


static void
lw_TSStore (w, wq, junk)
Widget w;
WidgetQueue *wq;
XtPointer junk;
/*
 * Store the value in our widget.
 */
{
	struct DFWData *wd = (struct DFWData *) wq->wq_wdata;
/*
 * Get the string out of the widget and store it.
 */
	msg_ELog (EF_DEBUG, "New value is '%s' '%s'", wd->d_vstring[0],
		wd->d_vstring[1]);

	pd_Store(Pd, "global", wq->wq_param[0], wd->d_vstring[0], SYMT_STRING);
	pd_Store(Pd, "global", wq->wq_param[1], wd->d_vstring[1], SYMT_STRING);
	parameter (wq->wq_comp, wq->wq_param[0], wd->d_vstring[0]);
	parameter (wq->wq_comp, wq->wq_param[1], wd->d_vstring[1]);
	lw_Popdown (wq);
}



static void
lw_Setup (wq, type, cmds)
WidgetQueue *wq;
int type;
struct ui_command *cmds;
/*
 * Get this widget ready to go.
 */
{
/*
 * Stash the component name.
 */
	strcpy (wq->wq_comp, UPTR (cmds[0]));
/*
 * Simply farm it out to the widget-specific code.
 */
	if (WTable[type].wd_setup)
		(*WTable[type].wd_setup) (wq, cmds + 1);
}


static void
lw_SSSetup (wq, cmds)
WidgetQueue *wq;
struct ui_command *cmds;
/*
 * Set up a single string widget.
 */
{
	Arg args[5];
	int n;
	struct SFWData *wd = (struct SFWData *) wq->wq_wdata;
/*
 *  Stash the parameter name.
 */
	strcpy (wq->wq_param[0], UPTR (cmds[0]));
/*
 * Stash the label into the widget.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, UPTR (cmds[1]));		n++;
	XtSetValues (wd->d_label, args, n);
/*
 * Set the initial value.
 */
	n = 0;
	strcpy (wd->d_vstring, cmds[2].uc_text);
	XtSetArg (args[n], XtNstring, wd->d_vstring);		n++;
	XtSetValues (wd->d_value, args, n);
}


static void
lw_SISetup (wq, cmds)
WidgetQueue *wq;
struct ui_command *cmds;
/*
 * Set up a single integer widget.
 */
{
	Arg args[5];
	int n;
	struct SFWData *wd = (struct SFWData *) wq->wq_wdata;
/*
 *  Stash the parameter name.
 */
	strcpy (wq->wq_param[0], UPTR (cmds[0]));
/*
 * Stash the label into the widget.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, UPTR (cmds[1]));		n++;
	XtSetValues (wd->d_label, args, n);
/*
 * Set the initial value.
 */
	n = 0;
	strcpy (wd->d_vstring, cmds[2].uc_text);
	XtSetArg (args[n], XtNstring, wd->d_vstring);		n++;
	XtSetValues (wd->d_value, args, n);
}


static void
lw_SFSetup (wq, cmds)
WidgetQueue *wq;
struct ui_command *cmds;
/*
 * Set up a single float widget.
 */
{
	char string[50];
	Arg args[5];
	int n;
	struct SFWData *wd = (struct SFWData *) wq->wq_wdata;
/*
 *  Stash the parameter name.
 */
	strcpy (wq->wq_param[0], UPTR (cmds[0]));
/*
 * Stash the label into the widget.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, UPTR (cmds[1]));		n++;
	XtSetValues (wd->d_label, args, n);
/*
 * Set the initial value.
 */
	n = 0;
	sprintf (string, "%.3f", UFLOAT (cmds[2]));
	strcpy (wd->d_vstring, string);
	XtSetArg (args[n], XtNstring, wd->d_vstring);		n++;
	XtSetValues (wd->d_value, args, n);
}


static void
lw_DFSetup (wq, cmds)
WidgetQueue *wq;
struct ui_command *cmds;
/*
 * Set up a double float widget.
 */
{
	char string[50];
	Arg args[5];
	int n;
	struct DFWData *wd = (struct DFWData *) wq->wq_wdata;
/*
 *  Stash the parameter name.
 */
	strcpy (wq->wq_param[0], UPTR (cmds[0]));
	strcpy (wq->wq_param[1], UPTR (cmds[3]));
/*
 * Stash the label into the widget.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, UPTR (cmds[1]));		n++;
	XtSetValues (wd->d_label[0], args, n);
/*
 * Set the initial value.
 */
	n = 0;
	sprintf (string, "%.1f", UFLOAT (cmds[2]));
	strcpy (wd->d_vstring[0], string);
	XtSetArg (args[n], XtNstring, wd->d_vstring[0]);		n++;
	XtSetValues (wd->d_value[0], args, n);
/*
 * Stash the label into the widget.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, UPTR (cmds[4]));		n++;
	XtSetValues (wd->d_label[1], args, n);
/*
 * Set the initial value.
 */
	n = 0;
	sprintf (string, "%.1f", UFLOAT (cmds[5]));
	strcpy (wd->d_vstring[1], string);
	XtSetArg (args[n], XtNstring, wd->d_vstring[1]);		n++;
	XtSetValues (wd->d_value[1], args, n);
}


static void
lw_TSSetup (wq, cmds)
WidgetQueue *wq;
struct ui_command *cmds;
/*
 * Set up a time series widget.
 */
{
	Arg args[5];
	int n, nfld;
	struct DFWData *wd = (struct DFWData *) wq->wq_wdata;
	char	fields[120], *fnames[120];
/*
 * Get the field names.
 */
	pd_Retrieve (Pd, wq->wq_comp, "field", fields, SYMT_STRING);
	nfld = ParseFieldList (fields, fnames);
/*
 *  Stash the parameter name.
 */
	if (strcmp (UPTR (cmds[0]), "left") == 0)
	{
		sprintf (wq->wq_param[0], "%s-%s", 
			 SimpleFieldName (F_Lookup (fnames[0])), 
			 UPTR (cmds[1]));
		sprintf (wq->wq_param[1], "%s-%s", 
			 SimpleFieldName (F_Lookup (fnames[0])), 
			 UPTR (cmds[3]));
	}
	else 
	{
		if (nfld >= 2)
		{	
		    sprintf (wq->wq_param[0], "%s-%s", 
			     SimpleFieldName (F_Lookup (fnames[1])), 
			     UPTR (cmds[1]));
		    sprintf (wq->wq_param[1], "%s-%s", 
			     SimpleFieldName (F_Lookup (fnames[1])), 
			     UPTR (cmds[3]));
		}
		else 
		{
		    sprintf (wq->wq_param[0], "%s-%s", 
			     SimpleFieldName (F_Lookup (fnames[0])), 
			     UPTR (cmds[1]));
		    sprintf (wq->wq_param[1], "%s-%s", 
			     SimpleFieldName (F_Lookup (fnames[0])), 
			     UPTR (cmds[3]));
		}
	}
/*
 * Get the parameter values.
 */
	pda_Search (Pd, wq->wq_comp, wq->wq_param[0], "tseries", 
		wd->d_vstring[0], SYMT_STRING);
	pda_Search (Pd, wq->wq_comp, wq->wq_param[1], "tseries",
		wd->d_vstring[1], SYMT_STRING);
/*
 * Stash the label into the widget.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, UPTR (cmds[2]));		n++;
	XtSetValues (wd->d_label[0], args, n);
/*
 * Set the initial value.
 */
	n = 0;
	XtSetArg (args[n], XtNstring, wd->d_vstring[0]);		n++;
	XtSetValues (wd->d_value[0], args, n);
/*
 * Stash the label into the widget.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, UPTR (cmds[4]));		n++;
	XtSetValues (wd->d_label[1], args, n);
/*
 * Set the initial value.
 */
	n = 0;
	XtSetArg (args[n], XtNstring, wd->d_vstring[1]);		n++;
	XtSetValues (wd->d_value[1], args, n);
}



/*
 * Sub-platform selector widget.
 */

static Widget
lw_SWCreate (tag, parent, appc)
char *		tag;
Widget		parent;
XtAppContext	appc;
/*
 * Create the station widget.
 */
{
	Widget form, box, w;
	Arg args[10];
	int sta, n;
	char name[40];
	WidgetQueue *wq = (WidgetQueue *) tag;
/*
 * Create the form widget to hold everything.
 */
	n = 0;
	XtSetArg (args[n], XtNdefaultDistance, 5);	n++;
	XtSetArg (args[n], XtNborderWidth, 0);		n++;
	form = XtCreateWidget (LW_FORM, formWidgetClass, parent,
		args, n);
	Sw_Form = form;
/*
 * Give our parent the same background, since we'll be its only child
 */
	CopyBackground(form, parent);
/*
 * Install any accelerators that have been set in our resources in the
 * the entire form.
 */
	XtInstallAllAccelerators (form, form);
/*
 * Make a box to hold the stations.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNwidth, 400);		n++;
	box = XtCreateManagedWidget ("stationbox", boxWidgetClass, form,
		args, n);
/*
 * Now add the maximum number of stations to the box.
 */
	for (sta = 0; sta < MAXSTA; sta++)
	{
		sprintf (name, "%d", sta + 1);
		XtSetArg (args[0], XtNlabel, name);
		Sw_Swidgets[sta] = XtCreateManagedWidget (name,
			toggleWidgetClass, box, args, 1);
		XtAddCallback (Sw_Swidgets[sta], XtNcallback, 
			(XtCallbackProc) lw_SwCb, (XtPointer)(long) sta);
		Sw_Sset[sta] = FALSE;
	}
	SwNManaged = MAXSTA;
/*
 * Store and Cancel buttons.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, box);		n++;
	XtSetArg (args[n], XtNlabel, "Store");		n++;
	w = XtCreateManagedWidget ("store", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, (XtCallbackProc) lw_SWStore, 
		(XtPointer) wq);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w);		n++;
	XtSetArg (args[n], XtNfromVert, box);		n++;
	XtSetArg (args[n], XtNlabel, "Cancel");		n++;
	w = XtCreateManagedWidget ("cancel", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, (XtCallbackProc) lw_CBPopdown, 
		(XtPointer) wq);

	return (form);
}


static void
lw_SWSetup (wq, cmds)
WidgetQueue	*wq;
struct ui_command	*cmds;
/*
 * Set up the station widget.
 */
{
	char	platforms[PlatformListLen];
	char	*pnames[MaxPlatforms];
	char	number[40];
	int	i, sta, nump;
	Arg	args[2];
/*
 *  Stash the platform name.
 */
	strcpy (Sw_Plat, UPTR (cmds[0]));
	strcpy (wq->wq_param[0], UPTR (cmds[0]));
/*
 * Unmanage and/or manage the correct number of buttons.
 */
	XawFormDoLayout (Sw_Form, False);

	for (i = SwNManaged; i < Sw_Nsta; i++)
		if (! XtIsManaged (Sw_Swidgets[i]))
		{
			XtManageChild (Sw_Swidgets[i]);
			SwNManaged++;
		}
	for (i = Sw_Nsta; i < MAXSTA; i++)
		if (XtIsManaged (Sw_Swidgets[i]))
		{
			XtUnmanageChild (Sw_Swidgets[i]);
			SwNManaged--;
		}
/*
 * Unset all the buttons.
 */
	for (i = 0; i < Sw_Nsta; i++)
	{
		XtSetArg (args[0], XtNstate, False);
		XtSetValues (Sw_Swidgets[i], args, 1);
		Sw_Sset[i] = FALSE;
	}

/*
 * Save the other platforms.
 */
	pd_Retrieve (Pd, wq->wq_comp, "platform", platforms, SYMT_STRING);
	nump = CommaParse (platforms, pnames);
	SavePlat[0] = '\0';
	Sw_RetSta[0] = '\0';
	for (i = 0; i < nump; i++)
	{
		msg_ELog (EF_DEBUG, "compare %s", pnames[i]);
		if (strncmp(pnames[i], wq->wq_param[0], strlen(wq->wq_param[0]))
			!= 0)
		{
			strcat (SavePlat, pnames[i]);
			strcat (SavePlat, ",");
		}
		else
		{
			strcpy (number, pnames[i] + strlen (wq->wq_param[0])+1);
			sta = atoi (number) - 1;
			msg_ELog (EF_DEBUG, "found %s %d", pnames[i], sta);
			XtSetArg (args[0], XtNstate, True);
			XtSetValues (Sw_Swidgets[sta], args, 1);
			strcat (Sw_RetSta, pnames[i]);
			strcat (Sw_RetSta, ",");
			Sw_Sset[sta] = TRUE;
		}
	}

	XawFormDoLayout (Sw_Form, True);

	msg_ELog (EF_DEBUG, "save platforms %s", SavePlat);
}


static void
lw_SWStore (w, wq, junk)
Widget	w;
WidgetQueue	*wq;
XtPointer	junk;
/*
 * Store the return stations string (Sw_RetSta).
 */
{
	parameter (wq->wq_comp, "platform", strcat (SavePlat, Sw_RetSta));
	lw_Popdown (wq);	
}


static void
lw_SwCb (w, sta, new)
Widget w;
int sta, new;
/*
 * The station widget callback.  Creates the return stations string.
 */
{
	int	i;
	char	name[40];
	
	Sw_Sset[sta] = new;
	Sw_RetSta[0] = '\0';
	for (i = 0; i < Sw_Nsta; i++)
		if (Sw_Sset[i])
		{
			sprintf (name, "%s/%d", Sw_Plat, i + 1);
			if (ds_LookupPlatform (name) == BadPlatform)
				sprintf (name, "%s.%d", Sw_Plat, i + 1);
			strcat (Sw_RetSta, name);
			strcat (Sw_RetSta, ",");
		}
	msg_ELog (EF_DEBUG, "In SwCb Sw_RetSta: %s", Sw_RetSta);
}
