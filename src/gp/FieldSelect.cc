
#include <string>

# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/SimpleMenu.h>
# include <X11/Xaw/SmeBSB.h>
# include <X11/Shell.h>
# include <X11/Xaw/SmeLine.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Viewport.h>
# include <X11/Xaw/AsciiText.h>

extern "C" 
{
# include <ui.h>
# include <ui_date.h>
}

# include <defs.h>
# include <copyright.h>
# include <message.h>
# include <pd.h>
# include <ds_fields.h>
# include <DataStore.h>

extern "C" 
{

# include "GraphProc.h"
# include "FieldMenu.h"

}

RCSID ("$Id: FieldSelect.cc,v 2.2 2001-06-19 22:32:24 granger Exp $")


extern "C" void fs_QuitCallback (Widget w, XtPointer cdata, XtPointer);
extern "C" void fs_SelectCallback (Widget w, XtPointer cdata, XtPointer);
extern "C" void fs_SetFieldCallback (Widget w, XtPointer cdata, XtPointer);


class FieldSelector 
{
public:
    FieldSelector (char *spec = 0)
    {
	shell = 0;
	textField = 0;
	callbacks = 0;

	fm_GetContext (&fm_context, spec);
    }

    FieldSelector (fm_Context *fmc)
    {
	shell = 0;
	textField = 0;
	callbacks = 0;

	fm_context = *fmc;
    }

    void create ()
    {
        int     n;
        Arg     args[30];
        Widget  quit, title, info;
	Widget 	form, above, button;
	Widget  viewport, list;
	char 	label[256];
	fm_Context *fmc = &fm_context;
	FieldId *fields = fmc->fields;
	int nfield = fmc->nfield;
	/*
	 * Create the popup widget shell and the form widget within it that
	 * holds everything.
	 */
        n = 0;
        XtSetArg (args[n], XtNresize, True); 	n++;
        shell = XtCreatePopupShell ("FieldSelect",
				    topLevelShellWidgetClass, Top, args, n);

        n = 0;
        XtSetArg (args[n], XtNborderWidth, 3); n++;
        form = XtCreateManagedWidget ("fsForm", formWidgetClass, shell,
				      args, n);
	/*
	 * Put title/instructions at top
	 */
	sprintf (label, "%s", fmc->title);
        n = 0;
        XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
        XtSetArg (args[n], XtNfromVert, NULL);		n++;
        XtSetArg (args[n], XtNlabel, label);		n++;
        title = XtCreateManagedWidget ("title", labelWidgetClass, form,
				       args, n);
	/*
	 * Button to remove the popup
	 */
        n = 0;
	XtSetArg (args[n], XtNfromHoriz, title);	n++;
        XtSetArg (args[n], XtNfromVert, NULL);		n++;
        XtSetArg (args[n], XtNhorizDistance, 180);	n++;
        XtSetArg (args[n], XtNright, XtChainRight);	n++;
        XtSetArg (args[n], XtNlabel, "Dismiss");		n++;
        quit = XtCreateManagedWidget ("quit", commandWidgetClass, form,
				      args, n);
        XtAddCallback (quit, XtNcallback, fs_QuitCallback, (XtPointer) this);

	/*
	 * Platform and field info on next line.
	 */
	above = title;
	sprintf (label, "Platform: %s", fmc->platform);
        n = 0;
        XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
        XtSetArg (args[n], XtNfromVert, above);		n++;
        XtSetArg (args[n], XtNlabel, label);		n++;
	XtSetArg (args[n], XtNborderWidth, 0);		n++;
        title = XtCreateManagedWidget ("platform", labelWidgetClass, form,
				       args, n);
	/*
	 * Platform and field info on next line.
	 */
	sprintf (label, "Parameter: %s", fmc->fparam);
        n = 0;
        XtSetArg (args[n], XtNfromHoriz, title);	n++;
        XtSetArg (args[n], XtNfromVert, above);		n++;
        XtSetArg (args[n], XtNlabel, label);		n++;
	XtSetArg (args[n], XtNborderWidth, 0);		n++;
        XtCreateManagedWidget ("field", labelWidgetClass, form,
			       args, n);
	/*
	 * Text area for entering the field manually.
	 */
	above = title;
	strcpy (label, fmc->fcurrent);
        n = 0;
        XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
        XtSetArg (args[n], XtNfromVert, above);		n++;
	XtSetArg (args[n], XtNdisplayPosition, 0);	n++;
	XtSetArg (args[n], XtNborderWidth, 2);		n++;
	XtSetArg (args[n], XtNinsertPosition, 0);	n++;
	XtSetArg (args[n], XtNresize, XawtextResizeNever);	n++;
	XtSetArg (args[n], XtNwidth, 300);		n++;
	XtSetArg (args[n], XtNheight, 20);		n++;
	XtSetArg (args[n], XtNlength, 80);		n++;
	XtSetArg (args[n], XtNtype, XawAsciiString);	n++;
	XtSetArg (args[n], XtNuseStringInPlace, False);	n++;
	XtSetArg (args[n], XtNstring, label);		n++;
	XtSetArg (args[n], XtNleftMargin, 5);		n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit);	n++;
        textField = XtCreateManagedWidget ("textfield", asciiTextWidgetClass,
					   form, args, n);
	/*
	 * Button to set the field to the textField value.
	 */
        n = 0;
	XtSetArg (args[n], XtNfromHoriz, textField);	n++;
        XtSetArg (args[n], XtNfromVert, above);		n++;
        XtSetArg (args[n], XtNright, XtChainRight);	n++;
        XtSetArg (args[n], XtNlabel, "Set Field");	n++;
	button = XtCreateManagedWidget ("setfield", commandWidgetClass, form,
					args, n);
        XtAddCallback (button, XtNcallback, fs_SetFieldCallback, 
		       (XtPointer) this);
	above = textField;

	/*
	 * Finally create and add a field chooser.
	 */
	int nentry = fm_NumEntries(fmc);
	if (nentry > 0)
	    callbacks = new fm_Callback[nentry];
	Widget fc = fm_CreateFieldChooser (fmc, form, 
					   fs_SelectCallback, this, callbacks);
        n = 0;
        XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
        XtSetArg (args[n], XtNfromVert, above);		n++;
        XtSetValues (fc, args, n);

	XtInstallAllAccelerators (shell, shell);
	XtInstallAccelerators (textField, button);
    }


    ~FieldSelector ()
    {
	if (shell)
	{
	    XtPopdown (shell);
	    XtDestroyWidget (shell);
	}
	delete[] callbacks;
    }
    
    void popup ()
    {
	if (shell)
	{
	/*
	 * Place the shell, pop it up, and forget about it.
	 */
	    uw_PositionWidget (shell);
	    XtPopup (shell, XtGrabNone);	
	}
    }

    void selectField (char *field)
    {
	fm_StoreContext (&fm_context);
	fm_SelectField (&fm_context, field);
    }

    void selectFieldFromText ()
    {
	fm_SelectEntry (&fm_context, 0);
	if (textField)
	{
	    /* Query the text area for the value and use that to set the
	     * field */
	    String text;
	    Arg arg;

	    XtSetArg (arg, XtNstring, &text);
	    XtGetValues (textField, &arg, 1);
	    if (text[0])
	    {
		selectField (text);
	    }
	}
    }

    void setTextField (char *field)
    {
	if (textField)
	{
	    int     n;
	    Arg     args[5];

	    n = 0;
	    XtSetArg (args[n], XtNstring, field);	n++;
	    XtSetValues (textField, args, n);
	}
    }
    

private:

    Widget shell;
    Widget textField;
    struct fm_Callback *callbacks;
    fm_Context fm_context;
};


    
extern "C"
void
fs_QuitCallback (Widget w, XtPointer cdata, XtPointer)
/*
 * Remove (pop down) this field selector shell
 */
{
    FieldSelector *fs = (FieldSelector *) cdata;

    delete fs;
}


extern "C"
void
fs_SetFieldCallback (Widget w, XtPointer cdata, XtPointer)
{
    FieldSelector *fs = (FieldSelector *) cdata;

    fs->selectFieldFromText ();
}


extern "C"
void
fs_SelectCallback (Widget w, XtPointer cdata, XtPointer)
/*
 * A field was chosen.
 */
{
    struct fm_Callback *cb = (struct fm_Callback *) cdata;
    fm_Context *fmc = cb->fm_context;
    FieldSelector *fs = (FieldSelector *) cb->refdata;
    char *field = (cb->extra ? cb->extra : F_GetFullName (cb->fid));

    fs->setTextField (field);
    fm_SelectEntry (fmc, w);
    fs->selectField (field);
}


/*
 * Create and popup a field selector based on the given
 * command-line parameters.
 *
 * selectfield [field-menu-spec]
 */
extern "C" 
void
fs_Create (struct ui_command *cmds)
{
    char *spec = 0;
    if (cmds[1].uc_ctype != UTT_END)
    {
	spec = UPTR (cmds[1]);
    }
    FieldSelector *fs = new FieldSelector (spec);

    fs->create();
    fs->popup();
}


extern "C" 
void
fs_CreateWithContext (fm_Context *fmc)
{
    FieldSelector *fs = new FieldSelector (fmc);

    fs->create();
    fs->popup();
}



