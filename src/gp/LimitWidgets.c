/*
 * Widgets for changing plot limits.
 */
static char *rcsid = "$Id: LimitWidgets.c,v 1.4 1991-05-07 20:59:11 kris Exp $";

# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/Cardinals.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Scrollbar.h>

# include "../include/defs.h"
# include "../include/message.h"
# include "../include/pd.h"
# include "GraphProc.h"


/*
 * The types of widgets we know.
 */
typedef enum {
	SingleFloatWidget = 0,
	DoubleFloatWidget = 1,
	SingleStringWidget = 2,
	TimeSeriesWidget = 3,
	SingleIntWidget = 4,
} WidgetType;
# define N_WIDGET_TYPES 5	/* Keep this updated!	*/

typedef char ParamStr[32];

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
	ParamStr	*wq_param;	/* Parameter			*/
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
	WidgetType wd_type;	/* The actual type		*/
	Widget	(*wd_create)();	/* Create routine		*/
	void	(*wd_setup)();	/* Setup routine		*/
	int	wd_seq;		/* Sequence number		*/
} WidgetDesc;

/*
 * Routines that manipulate specific widgets.
 */
# ifdef __STDC__
	static Widget lw_SFCreate (char *, Widget, XtAppContext);
	static void lw_SFSetup (WidgetQueue *, struct ui_command *);
	static void lw_SFStore (Widget, WidgetQueue *, XtPointer);
	static Widget lw_DFCreate (char *, Widget, XtAppContext);
	static void lw_DFSetup (WidgetQueue *, struct ui_command *);
	static void lw_DFStore (Widget, WidgetQueue *, XtPointer);
	static Widget lw_SSCreate (char *, Widget, XtAppContext);
	static void lw_SSSetup (WidgetQueue *, struct ui_command *);
	static void lw_SSStore (Widget, WidgetQueue *, XtPointer);
	static Widget lw_TSCreate (char *, Widget, XtAppContext);
	static void lw_TSSetup (WidgetQueue *, struct ui_command *);
	static void lw_TSStore (Widget, WidgetQueue *, XtPointer);
	static Widget lw_SICreate (char *, Widget, XtAppContext);
	static void lw_SISetup (WidgetQueue *, struct ui_command *);
	static void lw_SIStore (Widget, WidgetQueue *, XtPointer);
	static Widget lw_OvCreate (char *, Widget, XtAppContext);
# endif



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
 * Overlay status widget info.
 */
static Widget OvLabel = NULL;
static char OvStatus[1024];

/*
 * Other local routines.
 */
# ifdef __STDC__
	static void lw_Popup (WidgetQueue *);
	static void lw_CBPopdown (Widget, WidgetQueue *, XtPointer);
	static void lw_Popdown (WidgetQueue *);
	static void lw_Setup (WidgetQueue *, int, struct ui_command *);
	static WidgetQueue *lw_GetWidget (int);
	static void lw_InitOverlay ();
# endif


void
lw_InitWidgets ()
/*
 * Initialize all of our widgets.
 */
{
	int i;

	for (i = 0; i < N_WIDGET_TYPES; i++)
		FreeQueue[i] = BusyQueue[i] = (WidgetQueue *) 0;
	lw_InitOverlay ();
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
	w = lw_GetWidget (type);
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
int type;
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
	ret->wq_next = 0;
	ret->wq_type = type;
	sprintf (ret->wq_name, "%s%d", wd->wd_name, wd->wd_seq++);
/*
 * Make it known to UI.
 */
	uw_def_widget (ret->wq_name, "title", wd->wd_create, 0, (char *) ret);
	uw_ForceOverride (ret->wq_name);
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
 */
	if ((x -= 150) < 0)
		x = 0;
	if ((y -= 50) < 0)
		y = 0;
	uw_SetGeometry (w->wq_name, x, y, 0, 0);
/*
 * Now force it up on the screen.
 */
	uw_popup (w->wq_name);
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
	form = XtCreateManagedWidget ("SingleString", formWidgetClass, parent,
		args, n);
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
	XtSetArg (args[n], XtNheight, 20); n++;
	XtSetArg (args[n], XtNtype, XawAsciiString); n++;
	XtSetArg (args[n], XtNuseStringInPlace, True); n++;
	XtSetArg (args[n], XtNlength, 40); n++;
	XtSetArg (args[n], XtNstring, wdata->d_vstring); n++;
	XtSetArg (args[n], XtNleftMargin, 5); n++;
	XtSetArg (args[n], XtNeditType, XawtextAppend); n++;
	wdata->d_value = XtCreateManagedWidget ("SSValue",asciiTextWidgetClass,
		form, args, n);
	wq->wq_wdata = (void *) wdata;
/*
 * The "store global" button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Store"); n++;
	w = XtCreateManagedWidget ("store", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, lw_SSStore, wq);
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
	/* XtAddCallback (w, XtNcallback, mc_MovieRun, 0); */
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
	XtAddCallback (w, XtNcallback, lw_CBPopdown, wq);
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
	form = XtCreateManagedWidget ("SingleInt", formWidgetClass, parent,
		args, n);
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
	XtSetArg (args[n], XtNwidth, 40); n++;
	XtSetArg (args[n], XtNheight, 20); n++;
	XtSetArg (args[n], XtNtype, XawAsciiString); n++;
	XtSetArg (args[n], XtNuseStringInPlace, True); n++;
	XtSetArg (args[n], XtNlength, 40); n++;
	XtSetArg (args[n], XtNstring, wdata->d_vstring); n++;
	XtSetArg (args[n], XtNleftMargin, 5); n++;
	XtSetArg (args[n], XtNeditType, XawtextAppend); n++;
	wdata->d_value = XtCreateManagedWidget ("SIValue",asciiTextWidgetClass,
		form, args, n);
	wq->wq_wdata = (void *) wdata;
/*
 * The "store global" button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Store"); n++;
	w = XtCreateManagedWidget ("store", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, lw_SIStore, wq);
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
	/* XtAddCallback (w, XtNcallback, mc_MovieRun, 0); */
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
	XtAddCallback (w, XtNcallback, lw_CBPopdown, wq);
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
	form = XtCreateManagedWidget ("SingleFloat", formWidgetClass, parent,
		args, n);
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
	XtSetArg (args[n], XtNwidth, 40); n++;
	XtSetArg (args[n], XtNheight, 20); n++;
	XtSetArg (args[n], XtNtype, XawAsciiString); n++;
	XtSetArg (args[n], XtNuseStringInPlace, True); n++;
	XtSetArg (args[n], XtNlength, 40); n++;
	XtSetArg (args[n], XtNstring, wdata->d_vstring); n++;
	XtSetArg (args[n], XtNleftMargin, 5); n++;
	XtSetArg (args[n], XtNeditType, XawtextAppend); n++;
	wdata->d_value = XtCreateManagedWidget ("SFValue",asciiTextWidgetClass,
		form, args, n);
	wq->wq_wdata = (void *) wdata;
/*
 * The "store global" button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Store"); n++;
	w = XtCreateManagedWidget ("store", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, lw_SFStore, wq);
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
	/* XtAddCallback (w, XtNcallback, mc_MovieRun, 0); */
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
	XtAddCallback (w, XtNcallback, lw_CBPopdown, wq);
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
	form = XtCreateManagedWidget ("DoubleFloat", formWidgetClass, parent,
		args, n);
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
	XtSetArg (args[n], XtNwidth, 40); n++;
	XtSetArg (args[n], XtNheight, 20); n++;
	XtSetArg (args[n], XtNtype, XawAsciiString); n++;
	XtSetArg (args[n], XtNuseStringInPlace, True); n++;
	XtSetArg (args[n], XtNlength, 40); n++;
	XtSetArg (args[n], XtNstring, wdata->d_vstring[0]); n++;
	XtSetArg (args[n], XtNleftMargin, 5); n++;
	XtSetArg (args[n], XtNeditType, XawtextAppend); n++;
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
	XtSetArg (args[n], XtNwidth, 40); n++;
	XtSetArg (args[n], XtNheight, 20); n++;
	XtSetArg (args[n], XtNtype, XawAsciiString); n++;
	XtSetArg (args[n], XtNuseStringInPlace, True); n++;
	XtSetArg (args[n], XtNlength, 40); n++;
	XtSetArg (args[n], XtNstring, wdata->d_vstring[1]); n++;
	XtSetArg (args[n], XtNleftMargin, 5); n++;
	XtSetArg (args[n], XtNeditType, XawtextAppend); n++;
	above = wdata->d_value[1] = XtCreateManagedWidget ("DFValue2",
		asciiTextWidgetClass, form, args, n);
	wq->wq_wdata = (void *) wdata;
/*
 * The "store global" button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Store"); n++;
	w = XtCreateManagedWidget ("store", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, lw_DFStore, wq);
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
	/* XtAddCallback (w, XtNcallback, mc_MovieRun, 0); */
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
	XtAddCallback (w, XtNcallback, lw_CBPopdown, wq);
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
	form = XtCreateManagedWidget ("TimeSeries", formWidgetClass, parent,
		args, n);
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
	XtSetArg (args[n], XtNwidth, 40); n++;
	XtSetArg (args[n], XtNheight, 20); n++;
	XtSetArg (args[n], XtNtype, XawAsciiString); n++;
	XtSetArg (args[n], XtNuseStringInPlace, True); n++;
	XtSetArg (args[n], XtNlength, 40); n++;
	XtSetArg (args[n], XtNstring, wdata->d_vstring[0]); n++;
	XtSetArg (args[n], XtNleftMargin, 5); n++;
	XtSetArg (args[n], XtNeditType, XawtextAppend); n++;
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
	XtSetArg (args[n], XtNwidth, 40); n++;
	XtSetArg (args[n], XtNheight, 20); n++;
	XtSetArg (args[n], XtNtype, XawAsciiString); n++;
	XtSetArg (args[n], XtNuseStringInPlace, True); n++;
	XtSetArg (args[n], XtNlength, 40); n++;
	XtSetArg (args[n], XtNstring, wdata->d_vstring[1]); n++;
	XtSetArg (args[n], XtNleftMargin, 5); n++;
	XtSetArg (args[n], XtNeditType, XawtextAppend); n++;
	above = wdata->d_value[1] = XtCreateManagedWidget ("TSValue2",
		asciiTextWidgetClass, form, args, n);
	wq->wq_wdata = (void *) wdata;
/*
 * The "store global" button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, above); n++;
	XtSetArg (args[n], XtNlabel, "Store"); n++;
	w = XtCreateManagedWidget ("store", commandWidgetClass, form,
		args, n);
	XtAddCallback (w, XtNcallback, lw_TSStore, wq);
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
	/* XtAddCallback (w, XtNcallback, mc_MovieRun, 0); */
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
	XtAddCallback (w, XtNcallback, lw_CBPopdown, wq);
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
	char field[20], param[40];
	struct SFWData *wd = (struct SFWData *) wq->wq_wdata;
/*
 * Get the string out of the widget and store it.
 */
	msg_ELog (EF_DEBUG, "New value is '%s'", wd->d_vstring);
	parameter (wq->wq_comp, wq->wq_param, wd->d_vstring);
	lw_Popdown (wq);
	free (wq->wq_param);
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
	char field[20], param[40];
	struct SFWData *wd = (struct SFWData *) wq->wq_wdata;
/*
 * Get the string out of the widget and store it.
 */
	msg_ELog (EF_DEBUG, "New value is '%s'", wd->d_vstring);
	parameter (wq->wq_comp, wq->wq_param, wd->d_vstring);
	lw_Popdown (wq);
	free (wq->wq_param);
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
	char field[20], param[40];
	struct SFWData *wd = (struct SFWData *) wq->wq_wdata;
/*
 * Get the string out of the widget and store it.
 */
	msg_ELog (EF_DEBUG, "New value is '%s'", wd->d_vstring);

	if(pd_Retrieve(Pd, wq->wq_comp, "field", field, SYMT_STRING))
	{
		sprintf(param, "%s-%s", field, wq->wq_param);
		pd_Store(Pd, "global", param, wd->d_vstring, SYMT_STRING);
	}
	parameter (wq->wq_comp, wq->wq_param, wd->d_vstring);
	lw_Popdown (wq);
	free (wq->wq_param);
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
	Arg args[2];
	char field[20], param1[40], param2[40];
	struct DFWData *wd = (struct DFWData *) wq->wq_wdata;
/*
 * Get the string out of the widget and store it.
 */
	msg_ELog (EF_DEBUG, "New value is '%s' '%s'", wd->d_vstring[0],
		wd->d_vstring[1]);

	if(pd_Retrieve(Pd, wq->wq_comp, "field", field, SYMT_STRING))
	{
		sprintf(param1, "%s-%s", field, wq->wq_param[0]);
		sprintf(param2, "%s-%s", field, wq->wq_param[1]);
		pd_Store(Pd, "global", param1, wd->d_vstring[0], SYMT_STRING);
		pd_Store(Pd, "global", param2, wd->d_vstring[1], SYMT_STRING);
	}
	parameter (wq->wq_comp, wq->wq_param[0], wd->d_vstring[0]);
	parameter (wq->wq_comp, wq->wq_param[1], wd->d_vstring[1]);
	lw_Popdown (wq);
	free (wq->wq_param);
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
	Arg args[2];
	char field[20], param1[40], param2[40];
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
	free (wq->wq_param);
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
	if((wq->wq_param = (ParamStr *) malloc(sizeof(ParamStr))) == NULL)
	{
		msg_ELog(EF_PROBLEM, "Can't get memory for param string.");
		return;
	}
	strcpy (wq->wq_param, UPTR (cmds[0]));
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
	if((wq->wq_param = (ParamStr *) malloc(sizeof(ParamStr))) == NULL)
	{
		msg_ELog(EF_PROBLEM, "Can't get memory for param string.");
		return;
	}
	strcpy (wq->wq_param, UPTR (cmds[0]));
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
	Arg args[5];
	int n;
	struct SFWData *wd = (struct SFWData *) wq->wq_wdata;
/*
 *  Stash the parameter name.
 */
	if((wq->wq_param = (ParamStr *) malloc(sizeof(ParamStr))) == NULL)
	{
		msg_ELog(EF_PROBLEM, "Can't get memory for param string.");
		return;
	}
	strcpy (wq->wq_param, UPTR (cmds[0]));
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
lw_DFSetup (wq, cmds)
WidgetQueue *wq;
struct ui_command *cmds;
/*
 * Set up a double float widget.
 */
{
	Arg args[5];
	int n;
	struct DFWData *wd = (struct DFWData *) wq->wq_wdata;
/*
 *  Stash the parameter name.
 */
	if((wq->wq_param = (ParamStr *) malloc(2 * sizeof(ParamStr))) == NULL)
	{
		msg_ELog(EF_PROBLEM, "Can't get memory for param string.");
		return;
	}
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
	strcpy (wd->d_vstring[0], cmds[2].uc_text);
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
	strcpy (wd->d_vstring[1], cmds[5].uc_text);
	XtSetArg (args[n], XtNstring, wd->d_vstring[1]);		n++;
	XtSetValues (wd->d_value[1], args, n);
}


static void
lw_TSSetup (wq, cmds)
WidgetQueue *wq;
struct ui_command *cmds;
/*
 * Set up a double float widget.
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
	nfld = CommaParse (fields, fnames);
/*
 *  Stash the parameter name.
 */
	if((wq->wq_param = (ParamStr *) malloc(2 * sizeof(ParamStr))) == NULL)
	{
		msg_ELog(EF_PROBLEM, "Can't get memory for param string.");
		return;
	}
	if (strcmp (UPTR (cmds[0]), "left") == 0)
	{
		sprintf (wq->wq_param[0], "%s-%s", fnames[0], UPTR (cmds[1]));
		sprintf (wq->wq_param[1], "%s-%s", fnames[0], UPTR (cmds[3]));
	}
	else 
		if (nfld >= 2)
		{
			sprintf (wq->wq_param[0], "%s-%s", fnames[1], 
				UPTR (cmds[1]));
			sprintf (wq->wq_param[1], "%s-%s", fnames[1], 
				UPTR (cmds[3]));
		}
		else 
		{
			sprintf (wq->wq_param[0], "%s-%s", fnames[0], 
				UPTR (cmds[1]));
			sprintf (wq->wq_param[1], "%s-%s", fnames[0], 
				UPTR (cmds[3]));
		}
/*
 * Get the parameter values.
 */
	pd_Retrieve (Pd, wq->wq_comp, wq->wq_param[0], wd->d_vstring[0],
		SYMT_STRING);
	pd_Retrieve (Pd, wq->wq_comp, wq->wq_param[1], wd->d_vstring[1],
		SYMT_STRING);
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



/************
 * Overlay status widget stuff.  This is a bit of a hack, but it works.
 */


static void
lw_InitOverlay ()
/*
 * The overlay times widget.
 */
{
	uw_def_widget ("overlay", "Plot overlay status", lw_OvCreate, 0, 0);
	strcpy (OvStatus, "No information.");
}




static Widget
lw_OvCreate (junk, parent, actx)
char *junk;
Widget parent;
XtAppContext actx;
/*
 * Actually create the overlay status widget.
 */
{
	Widget form;
	Arg args[10];
	int n;
/*
 * See what happens if we just make the label directly.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, OvStatus);		n++;
	XtSetArg (args[n], XtNjustify, XtJustifyLeft);		n++;
	XtSetArg (args[n], XtNresize, True);			n++;
	OvLabel = XtCreateManagedWidget ("OverlayStatus", labelWidgetClass,
		parent, args, n);
	return (OvLabel);
}




void
lw_SetOvLabel (text)
char *text;
/*
 * Change the overlay status widget.
 */
{
	Arg args[5];

	if (OvLabel)
	{
		XtSetArg (args[0], XtNlabel, text);
		XtSetValues (OvLabel, args, 1);
	}
}






void
lw_OvInit (s)
char *s;
/*
 * Initialize the overlay control widget to this string.
 */
{
	strcpy (OvStatus, s);
}



void 
lw_OvAddString (s)
char *s;
/*
 * Add this to our status.
 */
{
	strcat (OvStatus, s);
}




void
lw_LoadStatus ()
/*
 * Put the status info into the widget.
 */
{
	lw_SetOvLabel (OvStatus);
}


void
lw_TimeStatus (comp, t)
char *comp;
time *t;
/*
 * Add a status line.
 */
{
	char *cp = OvStatus + strlen (OvStatus);
	char plat[40], fld[40];

	strcpy (plat, "--");

	pd_Retrieve (Pd, comp, "platform", plat, SYMT_STRING);
	if (! pd_Retrieve (Pd, comp, "field", fld, SYMT_STRING) &&
	    ! pd_Retrieve (Pd, comp, "color-code-field", fld, SYMT_STRING) &&
	    ! pd_Retrieve (Pd, comp, "arrow-type", fld, SYMT_STRING))
		strcpy (fld, " ");
	sprintf (cp, "%-15s%-11s%-12s%2d:%02d\n", comp, plat, fld,
		t->ds_hhmmss/10000, (t->ds_hhmmss/100) % 100);
}




