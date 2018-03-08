static	char	rcsid[] = "$Id: tabdemo.c,v 1.1 2001-11-30 00:42:10 granger Exp $" ;

#include <stdio.h>

#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Core.h>
#include <X11/Object.h>
#include <X11/Shell.h>

#include <X11/Xaw/Box.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/AsciiText.h>

#include "Tabs.h"

	/* X11 stuff */

static	Display	*dpy ;
static	Atom	wm_protocols ;
static	Atom	wm_delete_window ;

	/* Xt stuff */

static	XtAppContext	app_ctx ;

static	Widget	Tabs ;
static	Widget	topLevel ;
static	Widget	mainPanel ;
static	Widget	demoPanel ;
static	Widget	report ;
static	Widget	testTab ;	/* tab to delete */

static	int		reportEnable = False ;	/* see setReport() */

static String fallback[] = {
	"*borderWidth:	0",
	"*TabsLbl.label: Index Tabs",
	"*childA.tabLabel: one",
	"*childA.label: Tabs child no. 1",
	"*childB.tabLabel: two",
	"*childB.label: Tabs child no. 2",
	"*childB.sensitive: False",
	"*childC.tabLabel: three",
	"*childC.label: Tabs child no. 3",
	"*childC.tabForeground: red",
	"*childD.tabLabel: four",
	"*childD.label: Tabs child no. 4",
	"*childD.tabForeground: red",
	"*childD.sensitive: False",
	"*childE.tabLabel: five",
	"*childE.label: Tabs child no. 5",
	"*childF.tabLabel: six",
	"*childF.tabLeftBitmap: /usr/X/include/X11/bitmaps/xlogo11",
	"*childG.tabLabel: seven",
	"*childH.tabLabel: eight",
	"Tabdemo.baseTranslations: <Message>WM_PROTOCOLS: WMProtocols()",

	NULL
};

static	void	TabsDemo() ;

static	void	TabsCB(Widget, XtPointer, XtPointer) ;
static	void	PopdownCB(Widget, XtPointer, XtPointer) ;
static	void	NewTabCB(Widget, XtPointer, XtPointer) ;
static	void	DelTabCB(Widget, XtPointer, XtPointer) ;

static	void	WMProtocols(Widget, XEvent *, String *, Cardinal *) ;


	/* define global actions table for the application.  This is done
	 * to catch messages.  This could also be done through the main
	 * event loop if we wrote one.  It would be nice if Xt had a way
	 * of regestering a callback proc for specific kinds of events.
	 */

XtActionsRec	tabdemo_actions[] = {
    {"WMProtocols", WMProtocols},
} ;

int
main(int argc, char **argv)
{
	topLevel = XtAppInitialize(&app_ctx, "Tabdemo", NULL,0,
		&argc,argv, fallback, NULL,0) ;
	dpy = XtDisplay(topLevel) ;

	XtAppAddActions(app_ctx, tabdemo_actions, XtNumber(tabdemo_actions));

	mainPanel = XtVaCreateManagedWidget("mainPanel",
		boxWidgetClass, topLevel,
		0) ;

	report = XtVaCreateManagedWidget("report", labelWidgetClass, mainPanel,
		XtNlabel, "",
		0) ;

#ifdef	COMMENT
	demoPanel = XtVaCreateManagedWidget("demoPanel",
		boxWidgetClass, mainPanel,
		0) ;
#endif	/* COMMENT */

	TabsDemo() ;

	XtRealizeWidget(topLevel) ;

	/* tell window system we're willing to handle window-delete messages */
	wm_protocols = XInternAtom(dpy, "WM_PROTOCOLS", False);
	wm_delete_window = XInternAtom(dpy, "WM_DELETE_WINDOW", False);
	(void) XSetWMProtocols(dpy, XtWindow(topLevel), &wm_delete_window,1);

	reportEnable = True ;		/* see setReport(), below */

	XtAppMainLoop(app_ctx) ;

	exit(0) ;
	/* NOTREACHED */
}



static	void
TabsDemo()
{
	Widget	nb ;
	Widget	a,b,c,d,e,f,g ;
static	String	lbls[] = {"tab1","tab2","tab3"} ;

	Tabs = XtVaCreateManagedWidget("TabsDemo", tabsWidgetClass, mainPanel,
		XtNlabels, lbls,
		0) ;

	a = XtCreateManagedWidget("childA", labelWidgetClass, Tabs, NULL, 0) ;
	b = XtCreateManagedWidget("childB", labelWidgetClass, Tabs, NULL, 0) ;
	c = XtCreateManagedWidget("childC", labelWidgetClass, Tabs, NULL, 0) ;
	d = XtCreateManagedWidget("childD", labelWidgetClass, Tabs, NULL, 0) ;
	e = XtCreateManagedWidget("childE", labelWidgetClass, Tabs, NULL, 0) ;

	f = XtCreateManagedWidget("childF", boxWidgetClass, Tabs, NULL, 0) ;
	g = XtCreateManagedWidget("childG", tabsWidgetClass, Tabs, NULL, 0) ;

	testTab = f ;

	(void) XtCreateManagedWidget("Composite Widget",
		labelWidgetClass, f, NULL, 0) ;
	(void) XtCreateManagedWidget("yes", commandWidgetClass, f, NULL,0) ;
	(void) XtCreateManagedWidget("no", commandWidgetClass, f, NULL,0) ;
	(void) XtCreateManagedWidget("maybe", commandWidgetClass, f, NULL,0) ;
	nb = XtCreateManagedWidget("newTab", commandWidgetClass, f, NULL,0) ;
	XtAddCallback(nb, XtNcallback, NewTabCB, NULL) ;
	nb = XtCreateManagedWidget("delTab", commandWidgetClass, f, NULL,0) ;
	XtAddCallback(nb, XtNcallback, DelTabCB, NULL) ;

#ifdef	COMMENT
	(void) XtVaCreateManagedWidget("User", asciiTextWidgetClass, g,
	    XtNtype, XawAsciiString,
	    XtNeditType, XawtextEdit,
	    NULL,0) ;
	(void) XtVaCreateManagedWidget("Login", asciiTextWidgetClass, g,
	    XtNtype, XawAsciiString,
	    XtNeditType, XawtextEdit,
	    NULL,0) ;
	(void) XtCreateManagedWidget("OK", commandWidgetClass, g, NULL,0) ;
#endif	/* COMMENT */

	(void) XtCreateManagedWidget("Tabs", labelWidgetClass, g, NULL, 0) ;
	(void) XtCreateManagedWidget("within", labelWidgetClass, g, NULL, 0) ;
	(void) XtCreateManagedWidget("tabs", labelWidgetClass, g, NULL, 0) ;

	XtAddCallback(Tabs, XtNcallback, TabsCB, NULL) ;
	XtAddCallback(Tabs, XtNpopdownCallback, PopdownCB, NULL) ;
}





	/* Gotcha: changing the label of the "report" widget will change
	 * its size, which we don't want.  Work around this by disabling
	 * changes to this item until after widgets are realized, at
	 * which time the report's parent widget will lock down the size.
	 */

static	void
setReport(String str)
{
	if( reportEnable )
	  XtVaSetValues(report, XtNlabel, str, 0) ;
}



/* ARGSUSED */
static	void
TabsCB(Widget idx, XtPointer client, XtPointer call_data)
{
	Widget	w = (Widget)call_data ;
static	char	line[128] ;

	sprintf(line, "Widget %s on top", XtName(w)) ;
	setReport(line) ;
}



/* ARGSUSED */
static	void
PopdownCB(Widget idx, XtPointer client, XtPointer call_data)
{
	Widget	w = (Widget)call_data ;

	printf("Widget %s no longer on top\n", XtName(w)) ;
}


/* ARGSUSED */
static	void
NewTabCB(Widget idx, XtPointer client, XtPointer call_data)
{
	Widget	a ;
	Cardinal count ;
	char	name[20], lbl[80] ;

	XtVaGetValues(Tabs, XtNnumChildren, &count, 0) ;
	sprintf(name, "child%d", count+1) ;
	sprintf(lbl, "This is child\n%d", count+1) ;

	a = XtVaCreateManagedWidget(name, labelWidgetClass, Tabs,
		XtNlabel, lbl, 0) ;
}


/* ARGSUSED */
static	void
DelTabCB(Widget w, XtPointer client, XtPointer call_data)
{
	XtDestroyWidget(testTab) ;
	XtSetSensitive(w, False) ;
}


/* ARGSUSED */
static	void
WMProtocols(w, ev, params, nparams)
	Widget	w ;
	XEvent	*ev ;
	String	*params ;
	Cardinal *nparams ;
{
	if( ev->type == ClientMessage  &&
	    ev->xclient.message_type == wm_protocols  &&
	    ev->xclient.data.l[0] == wm_delete_window )
	{
	  if( w == topLevel )
	    exit(0) ;
	}
}
