/*
 * An X11 Viewer of an Archiver model.
 */

# include <unistd.h>

# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Command.h>

# include <defs.h>
# include <message.h>
# include "Archiver.h"

# define HIDE_BUTTON 0

RCSID ("$Id: XArchiver.cc,v 1.4 2002-12-13 23:23:01 granger Exp $")

class XArchiverView : public ArchiverView
{
public:
	XArchiverView (ArchiverModel *model, char *display);

	enum { MAX_WAITBUTTONS = 5 };

	virtual void setStatus (int alarm, string message);

	virtual void disableFinish ();
	virtual void disableWrite ();
	virtual void enableFinish ();
	virtual void enableWrite ();
	virtual void enableAction ();
	virtual void disableAction ();
	virtual void enableWait ();
	virtual void disableWait ();

	virtual void setWriteStatus (string label);
	virtual void setActionStatus (string label);
	virtual void setBytes (string label);

	virtual void quit ()
	{
		// We can do this because we know we were created dynamically
		// by CreateXView.  Deleting ourself when we're finished
		// frees what CreateXView allocated.  Yeah, it's a kludge.
		delete this;
	}

	virtual void error (ErrorException &e)
	{
		// On a fatal X error, we cannot make any calls back into
		// Xlib, so we must forget about our X structures by
		// setting Top to 0 and leave them dangling
		if (connection >= 0)
		{
			::close (connection);
			msg_delete_fd (connection);
		}
		Top = 0;
		Appc = 0;
		Model->removeView ();
	}

	bool ok() { return OK; }

	virtual ~XArchiverView ()
	{
		close();
	}

	// The application context is shared among all view instances so
	// our xevent() callback handler has a handle to it.  We can't
	// (as far as I know) make xevent() a member function since it
	// needs C linkage.
	static XtAppContext Appc;

	//private:
	ArchiverModel *Model;

	void MakeWidgets ();
	void setWaitSensitivity (int sensitive);
	void Sync ();
	Widget CreateWaitButtons (String times, Widget parent, 
				  Widget left, Widget above);
	void close();

	Widget Top;
	Widget Form;
	Widget WStatus;
	Widget Bytes;
	Widget FinishButton;
	Widget Action;
	Widget WriteButton;
	Widget WaitButtons[MAX_WAITBUTTONS+1];

	Pixel RedPix;	 	/* Colors for the status widget. */
	Pixel WhitePix;
	bool OK;
	int connection;		// Our X connection fd
};



// message handler callback on our X connection fd
extern "C" int xevent ();

// X IO error handler -- fatal connection error which cannot return
extern "C" int xioerror (Display *display)
{
	msg_ELog (EF_PROBLEM, "X11 I/O error: connection lost to %s",
		  DisplayString (display));
	fprintf (stderr, "X11 I/O error: connection lost to %s\n",
		 DisplayString (display));
	throw ArchiverView::ErrorException();
}

// X error handler
extern "C" int xerror (Display *display, XErrorEvent *xe)
{
	/*
	 * This is not a fatal error handler, so we can report the
	 * error and try to keep going.
	 */
	char buf[256];
	XGetErrorText (display, xe->error_code, buf, sizeof(buf));
	msg_ELog (EF_PROBLEM, "X11 error: %s", buf);
	fprintf (stderr, "X11 error: %s\n", buf);
	return 0;
}


XtAppContext XArchiverView::Appc = 0;


#if HIDE_BUTTON
/*
 * Callback for the Hide button.  Remove ourselves as the view
 * for the model, which tells us to quit but leaves
 * the Archiver still running.
 */
/*ARGSUSED*/
extern "C" void
QuitView (Widget w, XtPointer call_data, XtPointer client_data)
{
	XArchiverView *view = (XArchiverView *)call_data;
	view->Model->removeView ();
}
#endif


ArchiverView *
CreateXView (ArchiverModel *model, char *display)
{
	XArchiverView *view = new XArchiverView (model, display);
	if (! view->ok())
	{
		delete view;
		view = 0;
	}
	return view;
}



XArchiverView::XArchiverView (ArchiverModel *model, char *display_name)
{
	Model = model;
	Top = 0;
	OK = false;
	connection = -1;

	Appc = XtCreateApplicationContext ();
	if (! Appc)
		return;
	XSetErrorHandler (xerror);
	XSetIOErrorHandler (xioerror);
/* 
 * Need to copy the command-line since we're not allowed to change it.
 */
	int argc = Model->argc;
	const char **argv = new const char *[argc+1];
	for (int i = 0; i <= argc; ++i)
		argv[i] = Model->argv[i];
/*
 * Display initialization.
 */
	Display *display;
	display = XtOpenDisplay (Appc, display_name, "Archiver", "Archiver",
				 Model->options, Model->num_options, 
				 &argc, (char **)argv);
	if (! display)
		return;
	Top = XtAppCreateShell (0/*name*/, "Archiver", 
				applicationShellWidgetClass,
				display, 0/*args*/, 0/*num_args*/);
	if (! Top)
	{
		XtCloseDisplay (display);
		return;
	}
/*
 * Finally, we can build our widget tree.
 */
	OK = true;
	MakeWidgets ();
/*
 * Now update with the model and realize everything.
 */
	Model->update (this);
	XtRealizeWidget (Top);
	Sync ();

	connection = XConnectionNumber (XtDisplay (Top));
	msg_add_fd (connection, (int (*)())xevent);
}



void
XArchiverView::close ()
{
	/*
	 * We don't destroy the application context, because normally
	 * there might be some events on it still in the message queue
	 * which need to be processed.
	 */
	if (Top)
	{
		Display *display = XtDisplay (Top);
		XtDestroyWidget (Top);
		XSync (display, False);
		xevent ();
		XtCloseDisplay (display);
		xevent ();
		// XtDestroyApplicationContext (Appc);
		if (connection >= 0)
			msg_delete_fd (connection);
	}
	// Appc = 0;
	Top = 0;
	OK = 0;
}



void
XArchiverView::MakeWidgets ()
/*
 * Put together our widget given that the Top widget shell
 * has already been set by our constructor.
 */
{
	Arg args[10];
	int n;
	Widget w, above, button;
	XColor screen, exact;
/*
 * The inevitable form.
 */
	Form = XtCreateManagedWidget ("form", formWidgetClass, Top, NULL, 0);
	above = NULL;
/*
 * Give our status.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Archiver status:");	n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	w = XtCreateManagedWidget ("stitle", labelWidgetClass, Form, args, n);
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, w);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	XtSetArg (args[n], XtNresize, True);            n++;
	XtSetArg (args[n], XtNresizable, True);		n++;
	WStatus = XtCreateManagedWidget ("status", labelWidgetClass, Form,
					 args, n);
/*
 * The line of control buttons.  All but the "take/release button" start
 * out insensitive, until we have a device to write to.
 * The sensitivity will be updated in the ActionButton() callback,
 * depending on whether the device is being opened or closed.
 */
	n = 0;
	above = w;
	button = 0;
#if HIDE_BUTTON
	XtSetArg (args[n], XtNlabel, "Hide");		n++;
	XtSetArg (args[n], XtNfromHoriz, 0);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	XtSetArg (args[n], XtNresize, True);            n++;
	XtSetArg (args[n], XtNresizable, True);		n++;
	button = XtCreateManagedWidget ("hide", commandWidgetClass, Form,
					args, n);
	XtAddCallback (button, XtNcallback, (XtCallbackProc)QuitView,
		       (XArchiverView *)this);
#endif

	n = 0;
	XtSetArg (args[n], XtNlabel, "Finish");		n++;
	if (button)
	{
	    XtSetArg (args[n], XtNfromHoriz, button);	n++;
	}
	XtSetArg (args[n], XtNfromVert, above);		n++;
	XtSetArg (args[n], XtNresize, True);            n++;
	XtSetArg (args[n], XtNresizable, True);		n++;
	button = XtCreateManagedWidget ("finish", commandWidgetClass, Form,
					args, n);
	XtAddCallback (button, XtNcallback, (XtCallbackProc) Finish, 0);
	FinishButton = button;
/*
 * A "take/release" button.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, button);	n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	XtSetArg (args[n], XtNresize, True);            n++;
	XtSetArg (args[n], XtNresizable, True);		n++;
	Action = XtCreateManagedWidget ("action", commandWidgetClass, Form,
					args, n);
	XtAddCallback (Action, XtNcallback, (XtCallbackProc) ActionButton, 0);
/*
 * The explicit request for "write now".  While suspended, this button
 * will read "Resume" rather than "Write Now"
 */
	n = 0;
	XtSetArg(args[n], XtNfromHoriz, Action);	n++;
	XtSetArg(args[n], XtNfromVert, above);		n++;
	XtSetArg (args[n], XtNresize, True);            n++;
	XtSetArg (args[n], XtNresizable, True);		n++;
	button = XtCreateManagedWidget ("write", commandWidgetClass,
					Form, args, n);
	XtAddCallback(button, XtNcallback, (XtCallbackProc) WriteNow, 0);
	WriteButton = button;

	above = CreateWaitButtons(WaitTimes, Form, button, above);
/*
 * Status info.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, above);		n++;
	XtSetArg (args[n], XtNresize, True);            n++;
	XtSetArg (args[n], XtNresizable, True);		n++;
	Bytes = XtCreateManagedWidget ("bytes", labelWidgetClass, Form,args,n);
/*
 * Also look up a couple of pixel colors.
 */
	XAllocNamedColor (XtDisplay (Top), DefaultColormap (XtDisplay (Top),0),
		"red", &exact, &screen);
	RedPix = screen.pixel;
	XAllocNamedColor (XtDisplay (Top), DefaultColormap (XtDisplay (Top),0),
		"white", &exact, &screen);
	WhitePix = screen.pixel;
}


/*
 * Create the wait buttons as specified in the string 'times'
 * The global WaitButtons array will be NULL terminated when done
 * Returns the widget which further widgets would come below,
 * i.e. either the first WaitButton, or 'above' if there is none
 */
Widget
XArchiverView::CreateWaitButtons (String times, Widget parent, 
				  Widget left, Widget above)
{
	Arg args[5];
	int n,i;
	int ntimes;
	char *scan = times;
	int delays[MAX_WAITBUTTONS];
	char buf[30];

	ntimes = 0;
	while (*scan && (ntimes < MAX_WAITBUTTONS) &&
			(sscanf(scan,"%i",&delays[ntimes]) == 1))
	{
		++ntimes;
		if ((scan = strchr(scan,',')) == NULL)
			break;
		++scan;		/* skip the comma */
	}

	for (i = 0; i < ntimes; ++i)
	{
		n = 0;
		sprintf(buf, "Wait %d min", delays[i]);
		XtSetArg(args[n], XtNlabel, buf);		n++;
		XtSetArg(args[n], XtNfromHoriz, left);		n++;
		XtSetArg(args[n], XtNfromVert, above);		n++;
		WaitButtons[i] = XtCreateManagedWidget ("suspend", 
					commandWidgetClass,
					parent, args, n);
		XtAddCallback(WaitButtons[i], XtNcallback, 
		      (XtCallbackProc) SuspendWrite, 
		      (XtPointer)(delays[i]*60));
		left = WaitButtons[i];
	}
	WaitButtons[ntimes] = NULL;
	if (ntimes == 0)
		return(above);
	else
		return(WaitButtons[0]);
}




#ifdef notdef
/*
 * Xt callback which tells the archiver to suspend writing.
 */
static void
SuspendWrite(w, call_data)
	Widget w;
	XtPointer call_data;
{
	int waitsecs = (int) call_data;
	archiver->suspendWrite (waitsecs);
}
#endif


void
XArchiverView::setActionStatus (string status)
{
	Arg args[2];

	XtSetArg (args[0], XtNlabel, status.c_str());
	XtSetValues (Action, args, 1);
}


void
XArchiverView::enableAction ()
{
	XtSetSensitive(Action, True);
	Sync ();
}


void
XArchiverView::disableAction ()
{
	XtSetSensitive(Action, False);
	Sync ();
}


void
XArchiverView::setWriteStatus (string status)
{
	XtVaSetValues(WriteButton, XtNlabel, status.c_str(), NULL);
}



void
XArchiverView::enableFinish ()
{
	XtSetSensitive(FinishButton, True);
}


void
XArchiverView::disableFinish ()
{
	XtSetSensitive(FinishButton, False);
}


void
XArchiverView::enableWrite ()
{
	XtSetSensitive(WriteButton, True);
}


void
XArchiverView::disableWrite ()
{
	XtSetSensitive(WriteButton, False);
}



void
XArchiverView::setStatus (int alarm, string message)
{
	Arg args[4];
	int n = 0;

	XtSetArg (args[n], XtNlabel, message.c_str());	n++;
	XtSetArg (args[n], XtNbackground, alarm ? RedPix : WhitePix); n++;
	XtSetValues (WStatus, args, n);
	Sync ();
}



void
XArchiverView::enableWait ()
{
	setWaitSensitivity (1);
}


void
XArchiverView::disableWait ()
{
	setWaitSensitivity (0);
}


void
XArchiverView::setWaitSensitivity (int sensitive)
{
	Widget *buttons = WaitButtons;

	while (*buttons)
		XtSetSensitive(*buttons++, sensitive);
}


void
XArchiverView::setBytes (string label)
{
	Arg args[2];
	XtSetArg (args[0], XtNlabel, label.c_str());
	XtSetValues (Bytes, args, 1);
	Sync ();
}



void
XArchiverView::Sync ()
/*
 * Synchronize the display.
 */
{
	XSync (XtDisplay (Top), False);
	xevent ();
}



int
xevent ()
/*
 * Deal with an Xt event.
 */
{
	XEvent event;
/*
 * Deal with events as long as they keep coming and the view is prepared 
 * to deal with them.
 */
 	while (XArchiverView::Appc && XtAppPending (XArchiverView::Appc))
	{
		XtAppNextEvent (XArchiverView::Appc, &event);
		XtDispatchEvent (&event);
	}
	return (0);
}



