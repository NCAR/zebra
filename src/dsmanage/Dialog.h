//
// A simple dialog widget.
//
class dsDialog : public dsPopupWindow
{
	friend void DialogCb (Widget, XtPointer, XtPointer);
	void (*gocb) (char *);
	void (*cancelcb) ();
	Widget text, wcancel;
public:
	dsDialog (char * title, char *prompt, void (*goproc) (char *),
		void (*cancelproc) ());
	void popdown () { delete this; };
	void cancel ();
	void ok ();
};

