/* -*- mode: C++; c-basic-offset: 8 -*-
 *
 * $Id: Archiver.h,v 1.3 2002-12-13 23:23:01 granger Exp $
 *
 * Define an interface for an Archiver process and its view
 * and controllers.  A controller works with the Archiver control
 * interface, while a view submits itself to the Archiver
 * to be updated whenever the Archiver's state changes.
 */

#include <string>

/*-- Xt callbacks --*/
extern "C" void Finish FP ((void));
extern "C" void ActionButton FP ((void));
extern "C" void WriteNow FP((void));
extern "C" void SuspendWrite FP((Widget w, XtPointer call_data));

extern String WaitTimes;

class ArchiverView
{
public:
	typedef std::string string;

	// General exception class for archiver views.
	class ErrorException { };

	virtual void setStatus (int alarm, string message) {}

	virtual void enableFinish () {}
	virtual void disableFinish () {}
	virtual void enableWrite () {}
	virtual void disableWrite () {}
	virtual void enableAction () {}
	virtual void disableAction () {}
	virtual void enableWait () {}
	virtual void disableWait () {}

	virtual void setWriteStatus (string label) {}
	virtual void setActionStatus (string label) {}
	virtual void setBytes (string label) {}

	virtual void quit () {}
	virtual void error (ErrorException &e) {}

	virtual ~ArchiverView() {}
};



/*
 * Our model holds the state which views reflect.
 * We inherit the view interface so the Archiver implementation
 * just call the view methods to change the state.  The Model
 * keeps the state available even while there aren't other
 * views currently needing the state.
 */
class ArchiverModel : public ArchiverView
{
public:	
	virtual void setStatus (int alarm, string message)
	{
		status = message;
		warning = (alarm != 0);
		view->setStatus (alarm, message);
	}

	virtual void enableFinish ()
	{
		finishEnabled = true; 
		view->enableFinish ();
	}

	virtual void disableFinish () 
	{
		finishEnabled = false; 
		view->disableFinish ();
	}

	virtual void enableWrite ()
	{
		writeEnabled = true;
		view->enableWrite ();
	}

	virtual void disableWrite () 
	{
		writeEnabled = false;
		view->disableWrite ();
	}

	virtual void enableAction ()
	{
		actionEnabled = true;
		view->enableAction ();
	}

	virtual void disableAction ()
	{
		actionEnabled = false;
		view->disableAction ();
	}

	virtual void enableWait ()
	{
		waitEnabled = true;
		view->enableWait ();
	}

	virtual void disableWait ()
	{
		waitEnabled = false;
		view->disableWait ();
	}

	virtual void setWriteStatus (string label)
	{
		writeStatus = label;
		view->setWriteStatus (label);
	}

	virtual void setActionStatus (string label)
	{
		actionStatus = label;
		view->setActionStatus (label);
	}

	virtual void setBytes (string label) 
	{
		byteStatus = label;
		view->setBytes (label);
	}

	virtual void quit ()
	{
		removeView ();
	}

	virtual void error (ErrorException &e)
	{
		view->error(e);
	}

	// Add a view which will be called on updates
	void addView (ArchiverView &new_view)
	{
		delete view;
		our_view = false;
		view = &new_view;
	}

	void removeView ()
	{
		view->quit();
		if (! our_view)
		{
			view = new ArchiverView;
			our_view = true;
		}
	}
	
	// Update an entire view with our current state
	void update (ArchiverView *view)
	{
		switch (finishEnabled)
		{
			case true: view->enableFinish (); break;
			case false: view->disableFinish (); break;
		}
		switch (writeEnabled)
		{
			case true: view->enableWrite (); break;
			case false: view->disableWrite (); break;
		}
		switch (actionEnabled)
		{
			case true: view->enableAction (); break;
			case false: view->disableAction (); break;
		}
		switch (waitEnabled)
		{
			case true: view->enableWait (); break;
			case false: view->disableWait (); break;
		}
		view->setStatus (warning, status);
		view->setWriteStatus (writeStatus);
		view->setActionStatus (actionStatus);
		view->setBytes (byteStatus);
	}


	ArchiverModel(XrmOptionDescRec *opts, int numopts,
		      int argc_, char **argv_) : 
		finishEnabled(false), writeEnabled(false),
		actionEnabled(true), waitEnabled(false),
		warning(false),
		options(opts), num_options(numopts)
	{
		/* 
		 * Need to copy the command-line in case it changes.
		 */
		argc = argc_;
		argv = new const char *[argc+1];
		for (int i = 0; i <= argc; ++i)
			argv[i] = argv_[i];

		// Use a default do-nothing view until we get a
		// real one.
		view = new ArchiverView;
		our_view = true;
	}

	virtual ~ArchiverModel()
	{
		delete[] argv;
		if (our_view)
			delete view;
	}

	// Action states
	bool finishEnabled;
	bool writeEnabled;
	bool actionEnabled;
	bool waitEnabled;

	// Other status indicators
	bool warning;		// true for a warning status message
	string status;
	string writeStatus;
	string actionStatus;
	string byteStatus;

	// Initialization context
	XrmOptionDescRec *options;
	int num_options;
	int argc;
	const char **argv;

private:
	ArchiverView *view;
	bool our_view;

};


/*
 * Rather than expose the whole X view implementation, we only need
 * to export one function for creating an X view.
 */
extern ArchiverView *CreateXView (ArchiverModel *model, char *display);

