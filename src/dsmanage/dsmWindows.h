//
// Definitions of window utilities.
//

//
// Hold our display info.
//
class dsDisplay
{
	XtAppContext dd_Appc;	// Application context.
public:
	Widget	dd_Top;		// Top level widget.
	dsDisplay (int *, char **);
	void run () { XtAppMainLoop (dd_Appc); }
//	~dsDisplay ();
};




//
// The base class of all our windows -- this thing just holds the info
// together.
//
class dsWindow
{
protected:
	Widget	dw_shell;		// The shell widget that holds it all
	Widget	dw_form;		// The form that holds everything.
public:
	dsWindow (char *, const dsDisplay &, int top=0); // Create with name
	~dsWindow ();
	virtual void popup ();			// Put on screen
	virtual void popdown ();		// Take off screen
};





//
// A subclass for popups -- adds title and a zap button.
//

class dsPopupWindow : public dsWindow
{
protected:
	Widget corner;		// Upper left widget -- for forms
public:
	dsPopupWindow (const dsDisplay &disp, char *title, int zapspace = 50);
	void SetTitle (const char *);
};






//
// The main window.
//
class dsMainWindow : private dsWindow
{
	Widget	spaceLabel;		// The free space label
public:
	dsMainWindow (const dsDisplay &);
};




//
// A useful little utility to add the "normal" restraints.
//
static inline void
AddConstraints (Arg *args, int *n)
//
// Constrain things so they don't get mangled.
//
{
	XtSetArg (args[*n], XtNleft, XtChainLeft);	(*n)++;
	XtSetArg (args[*n], XtNright, XtChainLeft);	(*n)++;
	XtSetArg (args[*n], XtNtop, XtChainTop);	(*n)++;
	XtSetArg (args[*n], XtNbottom, XtChainTop);	(*n)++;
}




static inline void
AddBotConstraints (Arg *args, int *n)
//
// Constrain things so they don't get mangled.
//
{
	XtSetArg (args[*n], XtNleft, XtChainLeft);	(*n)++;
	XtSetArg (args[*n], XtNright, XtChainLeft);	(*n)++;
	XtSetArg (args[*n], XtNtop, XtChainBottom);	(*n)++;
	XtSetArg (args[*n], XtNbottom, XtChainBottom);	(*n)++;
}




//
// A couple of globals.
//
extern dsDisplay *Disp;
extern dsMainWindow *Main;
