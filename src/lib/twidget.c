/*
 * Time widget code.
 */
static char *rcsid = "$Id: twidget.c,v 2.0 1991-07-18 23:07:02 corbet Exp $";

# include <sys/types.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>

# include <X11/Xaw/Form.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/Toggle.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Cardinals.h>

# include <ui.h>
# include <ui_date.h>
# include "defs.h"
# include "../include/message.h"
# include "../include/timer.h"


# define TIMEOUT 5000

/*
 * Date/time button action codes.
 */
# define DATEUP		1
# define DATEDOWN	2
# define TIMEMINUP	3
# define TIMEMINDOWN	4
# define TIMEHOURUP	5
# define TIMEHOURDOWN	6

/*
 * Default resources.
 */
String Resources[] = {
	"	*input:		True",
	"	*Label*font:	-*-helvetica-bold-r-*-*-*-120-*-*-*-*-*-*",
	"	*Toggle*font:	-*-helvetica-medium-r-*-*-*-120-*-*-*-*-*-*",
	"	*Text*font:	-*-helvetica-medium-r-*-*-*-120-*-*-*-*-*-*",
	"	*title*font:	-*-times-bold-r-*-*-180-*-*-*-*-*-*",
	"	*arrow*font:	-*-symbol-*-*-*-*-*-120-*-*-*-*-*-*",
	"	*Text*height:	20",
	"	*title*borderWidth: 0",
	"	*Command*font:	-*-times-medium-i-*-*-*-120-*-*-*-*-*-*",
	0,
};

/*
 * Global stuff
 */
static Widget Form, Ctime, Hdate, Htime, Htext;
static Widget DUarrow, DDarrow, HUarrow, HDarrow, MUarrow, MDarrow;

/*
 * Histdate is the history date that appears in the window.  Maxdate is
 * the highest date we've ever seen, used to keep the date from being
 * moved into the future.
 */
static date Histdate, Maxdate;
static char Ahistdate[80];
static char Title[200];
static int Tslot = -1;	/* Timeout slot		*/
static int (*Tw_Callback) () = 0;

static Widget tw_WCreate ();
static int tw_WCallback ();
static void finish_arrow ();



void
tw_DefTimeWidget (callback, title)
int (*callback) ();
char *title;
/*
 * Hook the time widget into the UI.
 */
{
	tl_GetTime (&Histdate);
	Histdate.ds_hhmmss -= Histdate.ds_hhmmss % 100;
	strcpy (Title, title);
	Tw_Callback = callback;
	uw_def_widget ("time", "Time control widget", tw_WCreate, 0, 0);
}



static Widget
tw_WCreate (junk, parent, appc)
int junk;
Widget parent;
XtAppContext appc;
{
	Arg args[10];
	Widget form, title, left, f, hist, hist_w ();
	int timeint ();
	static char *ttrans = "<Btn1Down>,<Btn1Up>: 	set()notify()";
	XtTranslations ttable;
/*
 * Translations.
 */
	ttable = XtParseTranslationTable (ttrans);
/*
 * Put a form inside it.
 */
	XtSetArg (args[0], XtNdefaultDistance, 5);
	form = XtCreateManagedWidget ("form", formWidgetClass, parent,args, 1);
/*
 * Put the top label on the form.
 */
	XtSetArg (args[0], XtNlabel, Title);
	XtSetArg (args[1], XtNfromHoriz, NULL);
	XtSetArg (args[2], XtNfromVert, NULL);
	title = XtCreateManagedWidget ("title", labelWidgetClass, form,
		args, 3);
/*
 * Throw in the plot mode toggle.
 */
	XtSetArg (args[0], XtNborderWidth, 2);
	XtSetArg (args[1], XtNfromHoriz, NULL);
	XtSetArg (args[2], XtNfromVert, title);
	XtSetArg (args[3], XtNdefaultDistance, 5);
	f = XtCreateManagedWidget ("pmform", formWidgetClass, form, args, 4);

	XtSetArg (args[0], XtNlabel, "Plot mode:");
	XtSetArg (args[1], XtNfromHoriz, NULL);
	XtSetArg (args[2], XtNfromVert, NULL);
	XtSetArg (args[3], XtNborderWidth, 0);
	left = XtCreateManagedWidget ("plotmode", labelWidgetClass, f, args,4);

	XtSetArg (args[0], XtNlabel, "Real time");
	XtSetArg (args[1], XtNfromHoriz, left);
	XtSetArg (args[2], XtNfromVert, NULL);
	XtSetArg (args[3], XtNborderWidth, 0);
	XtSetArg (args[4], XtNstate, True);
	left = XtCreateManagedWidget ("rt", toggleWidgetClass, f, args, 5);
	XtOverrideTranslations (left, ttable);
	XtAddCallback (left, XtNcallback, tw_WCallback, RealTime);

	XtSetArg (args[0], XtNlabel, "History");
	XtSetArg (args[1], XtNfromHoriz, left);
	XtSetArg (args[2], XtNfromVert, NULL);
	XtSetArg (args[3], XtNradioGroup, left);
	XtSetArg (args[4], XtNborderWidth, 0);
	left = XtCreateManagedWidget ("history", toggleWidgetClass, f, args,5);
	XtOverrideTranslations (left, ttable);
	XtAddCallback (left, XtNcallback, tw_WCallback, History);

	XtSetArg (args[0], XtNlabel, "Movie");
	XtSetArg (args[1], XtNfromHoriz, left);
	XtSetArg (args[2], XtNfromVert, NULL);
	XtSetArg (args[3], XtNradioGroup, left);
	XtSetArg (args[4], XtNborderWidth, 0);
	left = XtCreateManagedWidget ("movie", toggleWidgetClass, f, args, 5);
	XtOverrideTranslations (left, ttable);
# ifdef notdef
	XtAddCallback (left, XtNcallback, tw_WCallback, Movie);
# endif

# ifdef notdef
/*
 * Current time.
 */
	XtSetArg (args[0], XtNlabel, "Current time: ");
	XtSetArg (args[1], XtNfromHoriz, NULL);
	XtSetArg (args[2], XtNfromVert, f);
	XtSetArg (args[3], XtNborderWidth, 0);
	left = XtCreateManagedWidget ("ctlabel", labelWidgetClass, form,
		args,4);
	XtSetArg (args[0], XtNlabel, "(Today's date goes here)");
	XtSetArg (args[1], XtNfromHoriz, left);
	XtSetArg (args[2], XtNfromVert, f);
	XtSetArg (args[3], XtNborderWidth, 1);
	XtSetArg (args[4], XtNresize, False);
	Ctime = XtCreateManagedWidget ("ctime", labelWidgetClass, form,
		args, 5);
/*
 * The current time update routine.
 */
	XtAppAddTimeOut (Actx, TIMEOUT, timeint, 0);
# endif
/*
 * A separate box for the history time.
 */
	hist = hist_w (form, appc);
	XtSetArg (args[0], XtNfromVert, f);
	XtSetArg (args[1], XtNfromHoriz, NULL);
	XtSetValues (hist, args, 2);
	XtManageChild (hist, appc);
/*
 * See what we get.
 */
	set_dt ();
	return (form);
}






static int
tw_WCallback (w, mode, value)
Widget w;
enum pmode mode;
int value;
/*
 * The toggle callback.
 */
{
	if (value)
		(*Tw_Callback) (mode, &Histdate);
}




Widget
make_label (name, label, border, parent, horiz, vert)
char *name, *label;
int border;
Widget parent, horiz, vert;
/*
 * Make a label widget.
 */
{
	Arg args[5];

	XtSetArg (args[0], XtNlabel, label);
	XtSetArg (args[1], XtNborderWidth, border);
	XtSetArg (args[2], XtNfromHoriz, horiz);
	XtSetArg (args[3], XtNfromVert, vert);
	return (XtCreateManagedWidget (name, labelWidgetClass, parent,
		args, FOUR));
}




Widget
make_button (name, label, border, parent, horiz, vert, callback, cbdata)
char *name, *label;
int border, (*callback) (), cbdata;
Widget parent, horiz, vert;
/*
 * Make a command button widget.
 */
{
	Arg args[5];
	Widget ret;

	XtSetArg (args[0], XtNlabel, label);
	XtSetArg (args[1], XtNborderWidth, border);
	XtSetArg (args[2], XtNfromHoriz, horiz);
	XtSetArg (args[3], XtNfromVert, vert);
	ret =  XtCreateManagedWidget (name, commandWidgetClass, parent,
		args, FOUR);
	XtAddCallback (ret, XtNcallback, callback, cbdata);
	return (ret);
}




Widget
hist_w (parent, appc)
Widget parent;
XtAppContext appc;
/*
 * Create the history widget.
 */
{
	int datebutton ();
	Widget form, above, left, w;
	Arg args[15];
	static XtActionsRec actions[] = {
		{ "dateadj", datebutton },
		{ "finishadj", finish_arrow }
	};
	static char *atrans =
		"<Btn1Down>: 	set()dateadj() \n\
		 <Btn1Up>: 	finishadj()unset()";
	XtTranslations ttable;
/*
 * Declare our actions, and parse the translations.
 */
	XtAppAddActions (appc, actions, TWO);
	ttable = XtParseTranslationTable (atrans);
/*
 * As usual, there's a form to hold it all.
 */
	XtSetArg (args[0], XtNdefaultDistance, 3);
	form = XtCreateWidget ("Histform", formWidgetClass, parent, args, 1);
/*
 * Throw in the label at the top.
 */
 	XtSetArg (args[0], XtNlabel, "History/movie time:");
	XtSetArg (args[1], XtNborderWidth, 0);
	XtSetArg (args[2], XtNfromHoriz, NULL);
	XtSetArg (args[3], XtNfromVert, NULL);
	above = XtCreateManagedWidget ("histlabel", labelWidgetClass, form,
		args, 4);
/*
 * Make a text widget to hold the actual date.
 */
	XtSetArg (args[0], XtNdisplayPosition, 0);
	XtSetArg (args[1], XtNinsertPosition, 0);
	XtSetArg (args[2], XtNlength, 80);
	XtSetArg (args[3], XtNresize, XawtextResizeNever);
	XtSetArg (args[4], XtNwidth, 250);
	XtSetArg (args[5], XtNheight, 20);
	XtSetArg (args[6], XtNstring, Ahistdate);
	XtSetArg (args[7], XtNtype, XawAsciiString);
	XtSetArg (args[8], XtNuseStringInPlace, True);
	XtSetArg (args[9], XtNfromHoriz, above);
	XtSetArg (args[10], XtNfromVert, NULL);
	XtSetArg (args[11], XtNleftMargin, 5);
	Htext = XtCreateManagedWidget ("text", asciiTextWidgetClass, form,
		args, 12);
/*
 * The date line
 */
	left = above;
	above = Htext;
	w = left = make_label ("histdatel", "Date: ", 0, form, left, above);
	DUarrow = make_button ("arrow", "\335", 1, form, left, above,
		datebutton, DATEUP);
	XtOverrideTranslations (DUarrow, ttable);
	DDarrow = make_button ("arrow", "\337", 1, form, DUarrow, above,
		datebutton, DATEDOWN);
	XtOverrideTranslations (DDarrow, ttable);
/*
 * The time line.
 */
	left = make_label ("histhour", "hour:", 0, form, DDarrow, above);
	HUarrow = make_button ("arrow", "\335", 1, form, left, above,
		datebutton, TIMEHOURUP);
	XtOverrideTranslations (HUarrow, ttable);
	HDarrow = make_button ("arrow", "\337", 1, form, HUarrow, above,
		datebutton, TIMEHOURDOWN);
	XtOverrideTranslations (HDarrow, ttable);
	left = make_label ("histhour", "min:", 0, form, HDarrow, above);
	MUarrow = make_button ("arrow", "\335", 1, form, left, above,
		datebutton, TIMEMINUP);
	XtOverrideTranslations (MUarrow, ttable);
	MDarrow = make_button ("arrow", "\337", 1, form, MUarrow, above,
		datebutton, TIMEMINDOWN);
	XtOverrideTranslations (MDarrow, ttable);

	/* set_dt (); */
	return (form);
}



int
datebutton (w, event, params, nparam)
Widget w;
XEvent *event;
String *params;
int nparam;
{
	int code;
	void arrow_timeout ();
	bool norep = FALSE;
/*
 * Cancel if necessary.
 */
	/* msg_ELog (EF_DEBUG, "Button down, slot %d", Tslot); */
	if (Tslot >= 0)
	{
		finish_arrow ();
		norep = TRUE;
	}
/*
 * Figure out our widget.
 */
	if (w == DUarrow)
		code = DATEUP;
	else if (w == DDarrow)
		code = DATEDOWN;
	else if (w == HUarrow)
		code = TIMEHOURUP;
	else if (w == HDarrow)
		code = TIMEHOURDOWN;
	else if (w == MUarrow)
		code = TIMEMINUP;
	else if (w == MDarrow)
		code = TIMEMINDOWN;
/*
 * Do one adjustment immediately.
 */
	do_adjust (code);
/*
 * Add the timeout to start repeating.
 */
	if (! norep)
	{
		Tslot = tl_AddRelativeEvent(arrow_timeout, (char *) code, 5,1);
		/* msg_ELog (EF_DEBUG, "Event slot is %d", Tslot); */
	}
	uw_sync ();
}




void
arrow_timeout (t, code)
time *t;
int code;
/*
 * Deal with repeating arrows.
 */
{
	do_adjust (code);
	uw_sync ();
}




static void
finish_arrow ()
/*
 * Deal with the end of an arrow situation.
 */
{
	/* msg_ELog (EF_DEBUG, "Cancel %d", Tslot); */
	if (Tslot >= 0)
		tl_Cancel (Tslot);
	Tslot = -1;
}


do_adjust (which)
int which;
/*
 * Perform a clock adjustment.
 */
{
	switch (which)
	{
	   case DATEUP:
	   	pmu_dadd (&Histdate.ds_yymmdd, &Histdate.ds_hhmmss, 240000);
		break;

	   case DATEDOWN:
	   	pmu_dsub (&Histdate.ds_yymmdd, &Histdate.ds_hhmmss, 240000);
		break;

	   case TIMEHOURUP:
	   	pmu_dadd (&Histdate.ds_yymmdd, &Histdate.ds_hhmmss, 10000);
		break;

	   case TIMEHOURDOWN:
	   	pmu_dsub (&Histdate.ds_yymmdd, &Histdate.ds_hhmmss, 10000);
		break;
		
	   case TIMEMINUP:
	   	pmu_dadd (&Histdate.ds_yymmdd, &Histdate.ds_hhmmss, 100);
		break;

	   case TIMEMINDOWN:
	   	pmu_dsub (&Histdate.ds_yymmdd, &Histdate.ds_hhmmss, 100);
		break;
	}
	set_dt ();
}



void
tw_DialAdjust (dial, incr)
int dial, incr;
/*
 * Adjust the time from the dial box.
 */
{
	if (incr > 0)
		pmu_dadd (&Histdate.ds_yymmdd, &Histdate.ds_hhmmss, incr*100);
	else
		pmu_dsub (&Histdate.ds_yymmdd, &Histdate.ds_hhmmss, incr*-100);
	set_dt ();
	uw_sync ();
}




set_dt ()
/*
 * Set the value of the date and time widgets.
 */
{
	Arg args[2];
	char dbuf[40], *time, *strchr ();
/*
 * Format, then split, the date.
 */
	ud_format_date (dbuf, &Histdate, UDF_FULL);
	strcpy (Ahistdate, dbuf);
	time = strchr (dbuf, ',');
	*time++ = '\0';
/*
 * Text too.
 */
	XtSetArg (args[0], XtNstring, Ahistdate);
	XtSetValues (Htext, args, 1);

	/* uw_sync (); */
}



# ifdef notdef
timeint ()
/*
 * Timer interrupt.
 */
{
	long lt = time (0);
	struct tm *t = localtime (&lt);
	Arg args[2];
	char *atime = asctime (t);

	XtSetArg (args[0], XtNlabel, atime);
	XtSetValues (Ctime, args, ONE);
	XtAppAddTimeOut (Actx, TIMEOUT, timeint, 0);
	sync ();
}
# endif



