/*
 * Our status widget.
 */
static char *rcsid = "$Id: StatusWidget.c,v 1.1 1991-04-28 17:38:15 corbet Exp $";



# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Label.h>

# include <defs.h>
# include "HouseKeeping.h"
# include "radar_ingest.h"


static bool SWMade = FALSE;
static Widget AzLabel, ElLabel, FixedLabel, TimeLabel, GateLabel, ModeLabel;
static Widget BeamLabel, MissedLabel;


# ifdef __STDC__
	static Widget MakeStatusWidget (int, Widget, XtAppContext);
	static Widget MakeLabel (char *, Widget, Widget, Widget, char *,
			int, int);
# else
	static Widget MakeStatusWidget ();
	static Widget MakeLabel ();
# endif



DefineWidgets ()
/*
 * Make our widgets known to the system.
 */
{
	uw_def_widget ("status", "Rasterizer status", MakeStatusWidget, 0, 0);
}





static Widget
MakeStatusWidget (junk, parent, appc)
int junk;
Widget parent;
XtAppContext appc;
/*
 * Create the status widget.
 */
{
	Widget form, w, above, ww;
	Arg args[10];
	int n;

	SWMade = TRUE;
/*
 * Create the usual form to hold all this stuff.
 */
	n = 0;
	XtSetArg (args[n], XtNdefaultDistance, 5);	n++;
	XtSetArg (args[n], XtNborderWidth, 0);		n++;
	form = XtCreateWidget ("statusform", formWidgetClass, parent,
		args, n);
/*
 * Most of the rest is done with labels.
 */
	above = MakeLabel ("LastBeam", form, NULL, NULL, "Last beam: ", 0, 0);
	TimeLabel = MakeLabel ("Time", form, above, NULL, "--", 80, 1);

	ww = w = MakeLabel ("Az", form, NULL, above, "Az: ", 0, 0);
	AzLabel = MakeLabel ("AzLabel", form, w, above, "--", 60, 1);

	w = MakeLabel ("El", form, AzLabel, above, "El: ", 0, 0);
	ElLabel = MakeLabel ("ElLabel", form, w, above, "--", 60, 1);

	w = MakeLabel ("Fixed", form, ElLabel, above, "Fixed: ", 0, 0);
	FixedLabel = MakeLabel ("FxLabel", form, w, above, "--", 60, 1);

	above = ww;
	ww = MakeLabel ("Beams", form, NULL, above, "Beams read: ", 0, 0);
	BeamLabel = MakeLabel ("BLabel", form, ww, above, "--", 60, 1);
	w = MakeLabel ("Missed", form, BeamLabel, above,"Packets missed:",0,0);
	MissedLabel = MakeLabel ("MLabel", form, w, above, "--", 50, 1);


	return (form);
}






static Widget
MakeLabel (name, parent, horiz, vert, label, width, border)
char *name, *label;
Widget parent, horiz, vert;
int width, border;
/*
 * Create a label widget.
 */
{
	int n;
	Arg args[10];

	n = 0;
	XtSetArg (args[n], XtNlabel, label);		n++;
	XtSetArg (args[n], XtNfromHoriz, horiz);	n++;
	XtSetArg (args[n], XtNfromVert, vert);		n++;
	XtSetArg (args[n], XtNborderWidth, border);	n++;
	if (width > 0)
	{
		XtSetArg (args[n], XtNwidth, width);	n++;
	}
	return (XtCreateManagedWidget (name, labelWidgetClass, parent,args,n));
}




static void
SetLabel (w, label)
Widget w;
char *label;
/*
 * Set this label.
 */
{
	Arg args[2];

	XtSetArg (args[0], XtNlabel, label);
	XtSetValues (w, args, 1);
}



void
SetStatus (hk)
Housekeeping *hk;
/*
 * Set the status widget.
 */
{
	char string[200];
	Arg args[5];

	if (! SWMade)
		return;

	sprintf (string, "%d:%02d:%02d", hk->hour, hk->minute, hk->second);
	SetLabel (TimeLabel, string);

	sprintf (string, "%.1f", hk->azimuth/CORR_FACT);
	SetLabel (AzLabel, string);
	sprintf (string, "%.1f", hk->elevation/CORR_FACT);
	SetLabel (ElLabel, string);
	sprintf (string, "%.1f", hk->fixed/CORR_FACT);
	SetLabel (FixedLabel, string);

	sprintf (string, "%d", NBeam);
	SetLabel (BeamLabel, string);
	sprintf (string, "%d", NMissed);
	SetLabel (MissedLabel, string);
	uw_xevent ();
}
