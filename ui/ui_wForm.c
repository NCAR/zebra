# ifdef XSUPPORT
/* 
 * Implementation of forms.
 */
# include <X11/Intrinsic.h>
# include <X11/Xaw/Cardinals.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/MenuButton.h>
# include <X11/Xaw/AsciiText.h>

# include "ui.h"
# include "ui_param.h"
# include "ui_globals.h"
# include "ui_commands.h"
# include "ui_window.h"
# include "ui_error.h"
# include "ui_loadfile.h"

static char *Rcsid = "$Id: ui_wForm.c,v 1.11 2001-11-30 00:42:05 granger Exp $";

/*
 * malloc prototype
 */
void* malloc();


# define MAXENTRY 100
/*
 * So far, we know about these types of form entries:
 */
typedef enum
{
	FE_Label, FE_Blank, FE_Button, FE_Pulldown, FE_Newline
} EntryType;

/*
 * Entries in a form widget look like this:
 */
typedef struct _FormEntry
{
	char fe_Name[40];	/* The name of this entry		*/
	int fe_Flags;		/* Flags (see below)			*/
	EntryType fe_Type;	/* What kind of entry			*/
	unsigned long fe_Color;	/* Color of this entry (if specified)	*/
	unsigned int fe_Width;	/* Width (if specified)			*/
	char fe_Text[120];	/* Primary text				*/
	char fe_AuxText[120];	/* Other text				*/
	char fe_CName[32];	/* Color name (if specified)		*/
	char fe_FName[120];	/* Font name (if specified)		*/
	Widget fe_Widget;	/* The widget that goes with it.	*/
	FrameWidget *fe_Frame;	/* The frame around the whole damn thing */
} FormEntry;

/*
 * Form entry flags.
 */
# define FEF_COLOR	0x0001	/* The user specified a color		*/
# define FEF_WIDTH	0x0002	/* The user gave a width		*/
# define FEF_FONT	0x0004	/* They gave a font			*/
# define FEF_BITMAP	0x0008	/* "Text" is really a bitmap		*/
# define FEF_VARIABLE	0x0010	/* "Text" is really a var name		*/
# define FEF_CDONE	0x0020	/* We have looked up the color		*/
# define FEF_NEWLINE	0x0040	/* Start a new line here		*/

/*
 * The form widget.
 */
typedef struct _FormWidget
{
	int fw_type;			/* The type of this widget	*/
	struct gen_widget *fw_next;	/* The next widget in the chain */
	void (*fw_create)();	/* The routine to create this thing	*/
	void (*fw_popup) ();	/* Routine to be called on popups	*/
	void (*fw_destroy) ();	/* The destroy method			*/
	GenWidget *(*fw_clone) (); /* Make a new one			*/
	struct frame_widget *fw_frame;	/* The associated frame		*/
	Widget fw_w;		/* The actual X widget			*/
  /* -- End of gen_widget stuff */
	int	fw_NEntry;		/* How many form entries	*/
	FormEntry *fw_Entries;		/* The entries themselves	*/
	char	fw_PopupCmd[120];	/* Command to run on popup	*/
} UIFormWidget;

/*
 * What is the "label" resource for this widget?
 */
# define LblRes(fe) (((fe)->fe_Type == FE_Blank) ? XtNstring : XtNlabel)

/*
 * Translations for pulldown entries.
 */
static char *PDATrans =
	"<EnterWindow>:		highlight() \n\
	<LeaveWindow>:		reset() \n\
	!<Btn1Down>:		reset()UIFormPopup(%s,%s)";

/*
 * Blank translations.
 */
static char *BlTrans =
	"<Key>Return:		UIBlankRet(%s,%s) \n\
	<Key>Linefeed:		UIBlankRet(%s,%s)";



# ifdef __STDC__
	static void uw_FormCreate ();
	static GenWidget *uw_FormClone (UIFormWidget *, FrameWidget *);
	static int uw_InForm (UIFormWidget *, struct ui_command *);
	static void uw_CmdCB (Widget, XtPointer, XtPointer);
	int uw_GetFText (int, SValue *, int *, SValue *, int *);
	static Widget uw_FBlank (Arg *, int, char *, char *, Widget);
	static FormEntry * uw_FindEntry (char *, char *);
	static void uw_FDestroy (UIFormWidget *, int);
	static void uw_FormPopup (UIFormWidget *);
# else
	static void uw_FormCreate ();
	static GenWidget *uw_FormClone ();
	static int uw_InForm ();
	static void uw_CmdCB ();
	int uw_GetFText ();
	static Widget uw_FBlank ();
	static FormEntry * uw_FindEntry ();
	static void uw_FDestroy ();
	static void uw_FormPopup ();
# endif







struct gen_widget *
uw_FormDef (frame)
FrameWidget *frame;
{
	UIFormWidget *fw = (UIFormWidget *) malloc (sizeof (UIFormWidget));
	int check, store;
/*
 * Initialize.
 */
	fw->fw_type = WT_FORM;
	fw->fw_create = uw_FormCreate;
	fw->fw_popup = NULL;
	fw->fw_destroy = uw_FDestroy;
	fw->fw_clone = uw_FormClone;
	fw->fw_NEntry = 0;
	fw->fw_Entries = (FormEntry *) malloc (MAXENTRY*sizeof (FormEntry));
	fw->fw_w = NULL;
	fw->fw_PopupCmd[0] = '\0';
	fw->fw_frame = frame;
/*
 * Pull in all of the entries.
 */
	ERRORCATCH
		ui_subcommand ("ust$in_form", "Form>", uw_InForm, (long) fw);
	ON_ERROR
		free (fw->fw_Entries);
		free (fw);
		RESIGNAL;
	ENDCATCH
/*
 * Go through and collapse newline entries into the following entry.
 */
	for (check = store = 0; check < fw->fw_NEntry; check++)
	{
		if (fw->fw_Entries[check].fe_Type == FE_Newline)
			fw->fw_Entries[check + 1].fe_Flags |= FEF_NEWLINE;
		else
			fw->fw_Entries[store++] = fw->fw_Entries[check];
	}
	fw->fw_NEntry = store;
/*
 * Get rid of excess memory and return.
 */
	fw->fw_Entries = (FormEntry *) realloc (fw->fw_Entries,
				fw->fw_NEntry*sizeof (FormEntry));
	if (fw->fw_PopupCmd[0])
		fw->fw_popup = uw_FormPopup;
	return ((GenWidget *) fw);
}




static int
uw_InForm (fw, cmds)
UIFormWidget *fw;
struct ui_command *cmds;
/*
 * Deal with a form entry.
 */
{
	FormEntry *fe = fw->fw_Entries + fw->fw_NEntry++;
/*
 * Do initial stuff.
 */
	switch (UKEY (*cmds))
	{
	/*
	 * The various types of possible form entries.
	 */
	   case UIC_LABEL:
		fe->fe_Type = FE_Label;
		break;
	   case UIC_BUTTON:
	   	fe->fe_Type = FE_Button;
		strcpy (fe->fe_AuxText, "message 'No command defined'");
		break;
	   case UIC_PULLDOWN:
	   	fe->fe_Type = FE_Pulldown;
		break;
	   case UIC_BLANK:
	   	fe->fe_Type = FE_Blank;
		strcpy (fe->fe_AuxText, "<NoCommand>");
		break;
	/*
	 * Newlines are special.
	 */
	   case UIC_NEWLINE:
	   	fe->fe_Type = FE_Newline;
		return (TRUE);
	/*
	 * Now they can also give us a popup command.
	 */
	   case UIC_POPUP:
		fw->fw_NEntry--;
	   	strcpy (fw->fw_PopupCmd, UPTR (cmds[1]));
		return (TRUE);

	   case UIC_ENDFORM:
		fw->fw_NEntry--;
	   	return (FALSE);
	/*
	 * Overall control stuff.
	 */
	   case UIC_NOHEADER:
	   case UIC_LOCATION:
	   case UIC_SIZE:
	   case UIC_OVERRIDE:
		fw->fw_NEntry--;
	   	uw_DoFrameParam (fw->fw_frame, cmds);
		return (TRUE);
	}
/*
 * Grab the name and text.
 */
	fe->fe_Flags = 0;
	strcpy (fe->fe_Name, UPTR (cmds[1]));
	cmds += 2;
	if (cmds->uc_ctype == UTT_KW)
	{
		if (UKEY (*cmds) == UIC_VARIABLE)
			fe->fe_Flags |= FEF_VARIABLE;
		else
			fe->fe_Flags |= FEF_BITMAP;
		cmds++;
	}
	strcpy (fe->fe_Text, UPTR (*cmds));
/*
 * Now process optional parameters.
 */
	for (cmds++; cmds->uc_ctype != UTT_END; cmds += 2)
	{
		switch (UKEY (*cmds))
		{
		   case UIC_WIDTH:
		   	fe->fe_Flags |= FEF_WIDTH;
			fe->fe_Width = UINT (cmds[1]);
			break;
		   case UIC_FONT:
		   	fe->fe_Flags |= FEF_FONT;
			strcpy (fe->fe_FName, UPTR (cmds[1]));
			break;
		   case UIC_COLOR:
		   	fe->fe_Flags |= FEF_COLOR;
			strcpy (fe->fe_CName, UPTR (cmds[1]));
			break;
		   case UIC_COMMAND:
			if (fe->fe_Type != FE_Button && fe->fe_Type !=FE_Blank)
				ui_warning ("Command ignored");
			else
				strcpy (fe->fe_AuxText, UPTR (cmds[1]));
			break;
		   case UIC_MENU:
			if (fe->fe_Type != FE_Pulldown)
				ui_warning ("Menu ignored");
			else
				strcpy (fe->fe_AuxText, UPTR (cmds[1]));
			break;
		}
	}
	return (TRUE);
}





static void
uw_FormCreate (fw, parent)
UIFormWidget *fw;
Widget parent;
/*
 * Create this widget.
 */
{
	Arg args[20];
	int n, ent, type;
	Widget above = NULL, left = NULL, lbegin = NULL;
	char ctrans[512], *text;
	XtTranslations ttrans;
	SValue v;
	Pixmap pm;
	XFontStruct *fnt;
/*
 * Make the form that holds everything.
 */
	fw->fw_w = XtCreateManagedWidget ("UIForm", formWidgetClass, parent,
			NULL, 0);
/*
 * Now add all of the children to it.
 */
	for (ent = 0; ent < fw->fw_NEntry; ent++)
	{
		FormEntry *fe = fw->fw_Entries + ent;
	/*
	 * Deal with line breaks separately.
	 */
	 	if (fe->fe_Type == FE_Newline)
		{
			above = lbegin;
			lbegin = left = NULL;
			continue;
		}
		if (fe->fe_Flags & FEF_NEWLINE)
		{
			above = lbegin;
			lbegin = left = NULL;
		}
	/*
	 * Find out the contents of the label.
	 */
		text = fe->fe_Text;
		if (fe->fe_Flags & FEF_VARIABLE)
		{
			if (usy_g_symbol (Ui_variable_table, fe->fe_Text,
							&type, &v))
				text = v.us_v_ptr;
			else
				text = "Bad variable";
		}
	/*
	 * Tweak any parameters which are needed.
	 */
		n = 0;
		XtSetArg (args[n], LblRes (fe), text);			n++;
		if (fe->fe_Flags & FEF_WIDTH)
			{ XtSetArg (args[n], XtNwidth, fe->fe_Width); 	n++; }
		XtSetArg (args[n], XtNfromVert, above);			n++;
		XtSetArg (args[n], XtNfromHoriz, left);			n++;
	/*
	 * If there is a bitmap, try to pull it in.
	 */
		if (fe->fe_Flags & FEF_BITMAP &&
					(pm = uw_GetPixmap (fe->fe_Text)))
			{ XtSetArg (args[n], XtNbitmap, pm);		n++; }
	/*
	 * See if they want a font.
	 */
	 	if (fe->fe_Flags & FEF_FONT &&
					(fnt = uw_GetFont (fe->fe_FName)))
			{ XtSetArg (args[n], XtNfont, fnt);		n++; }
	/*
	 * If they wanted a color, get it for them.
	 */
	 	if (fe->fe_Flags & FEF_COLOR)
		{
			if (! (fe->fe_Flags & FEF_CDONE))
			{
				XColor exact, screen;
				Display *disp = XtDisplay (Top);
				XAllocNamedColor (disp,
					DefaultColormap (disp,
						DefaultScreen (disp)),
					fe->fe_CName, &screen, &exact);
				fe->fe_Color = screen.pixel;
				fe->fe_Flags |= FEF_CDONE;
			}
			XtSetArg (args[n], XtNforeground, fe->fe_Color); n++;
		}
	/*
	 * Create the widget.
	 */
	 	switch (fe->fe_Type)
		{
		/*
		 * For labels, we wire the border into nonexistence, but
		 * life is otherwise simple.
		 */
		   case FE_Label:
			XtSetArg (args[n], XtNborderWidth, 0);		n++;
		   	fe->fe_Widget = XtCreateManagedWidget (fe->fe_Name,
				labelWidgetClass, fw->fw_w, args, n);
			break;
		/*
		 * Buttons need our callback.
		 */
		   case FE_Button:
		   	fe->fe_Widget = XtCreateManagedWidget (fe->fe_Name,
				commandWidgetClass, fw->fw_w, args, n);
			XtAddCallback (fe->fe_Widget, XtNcallback, uw_CmdCB,
				(XtPointer) fe);
			break;
		/*
		 * Pulldowns need a special set of translations.
		 */
		   case FE_Pulldown:
			sprintf (ctrans, PDATrans, fe->fe_Frame->fw_name,
					fe->fe_AuxText);
			ttrans = XtParseTranslationTable (ctrans);
		   	XtSetArg (args[n], XtNtranslations, ttrans); n++;
			fe->fe_Widget = XtCreateManagedWidget (fe->fe_Name,
				menuButtonWidgetClass, fw->fw_w, args, n);
			uw_IWRealize (fe->fe_AuxText, Top);
			break;
		/*
		 * Blanks (text widgets) are complicated, so we farm it out.
		 */
		   case FE_Blank:
		   	fe->fe_Widget = uw_FBlank (args, n, fe->fe_Name,
					fe->fe_Frame->fw_name, fw->fw_w);
			break;
		}
	/*
	 * If this is the beginning of the line, remember it.
	 */
		left = fe->fe_Widget;
		if (! lbegin)
			lbegin = left;
	}
}




static void
uw_FDestroy (fw, realized)
UIFormWidget *fw;
bool realized;
/*
 * Zap this widget.
 */
{
	if (realized)
		XtDestroyWidget (fw->fw_w);
	free (fw->fw_Entries);
	free (fw);
}





static void
uw_FormPopup (fw)
UIFormWidget *fw;
/*
 * The popup callback.
 */
{
	if (fw->fw_PopupCmd[0])
	{
		SValue v;
		v.us_v_ptr = fw->fw_frame->fw_name;
		usy_s_symbol (Ui_variable_table, "ui$form", SYMT_STRING, &v);
		ui_perform (fw->fw_PopupCmd);
	}
}






static Widget
uw_FBlank (args, n, name, form, parent)
Arg *args;
int n;
char *name, *form;
Widget parent;
/*
 * Create a text widget as a form blank.
 */
{
	char ctrans[512];
	XtTranslations ttrans;
	Widget ret;
/*
 * Put together the translations.
 */
	sprintf (ctrans, BlTrans, form, name, form, name);
	ttrans = XtParseTranslationTable (ctrans);
/*
 * More args.
 */
	XtSetArg (args[n], XtNdisplayPosition, 0);		n++;
	XtSetArg (args[n], XtNinsertPosition, 0);		n++;
/*	XtSetArg (args[n], XtNheight, 20);			n++; */
	XtSetArg (args[n], XtNtype, XawAsciiString);		n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit);		n++;
/*
 * Create it.
 */
	ret = XtCreateManagedWidget (name, asciiTextWidgetClass, parent,
			args, n);
	XtOverrideTranslations (ret, ttrans);
	return (ret);
}





static void
uw_CmdCB (w, xfe, junk)
Widget w;
XtPointer junk, xfe;
/*
 * Somebody has poked a button.
 */
{
	FormEntry *fe = (FormEntry *) xfe;
	SValue v;
/*
 * Stash a symbol to let the callee know where we're coming from, then
 * perform the action.
 */
	v.us_v_ptr = fe->fe_Frame->fw_name;
	usy_s_symbol (Ui_variable_table, "ui$form", SYMT_STRING, &v);

	v.us_v_ptr = XtName (w);
	usy_s_symbol (Ui_variable_table, "ui$formentry", SYMT_STRING, &v);

	ui_perform (fe->fe_AuxText);
}




void
uw_FPopup (w, event, args, cardjunk)
Widget w;
XEvent *event;
String *args;
Cardinal *cardjunk;
/*
 * Popup a pulldown menu.
 */
{
	SValue v;
	char *menu = args[1];
/*
 * Set symbols so that the form and entry may be found.
 */
	v.us_v_ptr = args[0];
	usy_s_symbol (Ui_variable_table, "ui$form", SYMT_STRING, &v);

	v.us_v_ptr = XtName (w);
	usy_s_symbol (Ui_variable_table, "ui$formentry", SYMT_STRING, &v);
/*
 * Now make the menu appear.
 */
	XtCallActionProc (w, "PositionAndPopupRdssMenu", event, &menu, 1);
}




void
uw_FBRet (w, event, args, cardjunk)
Widget w;
XEvent *event;
String *args;
Cardinal *cardjunk;
/*
 * Deal with a return in a text window.
 */
{
	FormEntry *fe = uw_FindEntry (args[0], args[1]);
	SValue v;
/*
 * See if there is something they want to do.  If so, store our symbol
 * and go for it.
 */
	if (strcmp (fe->fe_AuxText, "<NoCommand>"))
	{
		v.us_v_ptr = args[0],
		usy_s_symbol (Ui_variable_table, "ui$form", SYMT_STRING, &v);

		v.us_v_ptr = XtName (w);
		usy_s_symbol (Ui_variable_table, "ui$formentry", SYMT_STRING, 
			      &v);

		ui_perform (fe->fe_AuxText);
	}
}






static GenWidget *
uw_FormClone (fw, frame)
UIFormWidget *fw;
FrameWidget *frame;
/*
 * Clone off an instance widget.
 */
{
	UIFormWidget *new = (UIFormWidget *) malloc (sizeof (UIFormWidget));
	int e;
/*
 * Copy the form widget stuff over, then create a new entry array and
 * copy it as well.
 */
	*new = *fw;
	new->fw_Entries = (FormEntry *)
			malloc (new->fw_NEntry*sizeof (FormEntry));
	memcpy (new->fw_Entries, fw->fw_Entries,
				new->fw_NEntry*sizeof (FormEntry));
/*
 * Go through the entries and remember the frame.
 */
	for (e = 0; e < new->fw_NEntry; e++)
		new->fw_Entries[e].fe_Frame = frame;
	return (GenWidget *) new;
}




static FormEntry *
uw_FindEntry (form, ent)
char *form, *ent;
/*
 * Find a form entry structure.
 */
{
	GenWidget *gw;
	UIFormWidget *fw;
	int i;
/*
 * Find this widget.
 */
	if ((gw = uw_g_widget (form)) == NULL)
		ui_error ("Unknown widget %s", form);
	if (! gw->gw_next || gw->gw_next->gw_type != WT_FORM)
		ui_error ("Widget %s not a form widget", form);
	fw = (UIFormWidget *) gw->gw_next;
/*
 * Now find the entry.
 */
	for (i = 0; i < fw->fw_NEntry; i++)
		if (! strcmp (ent, fw->fw_Entries[i].fe_Name))
			break;
	if (i >= fw->fw_NEntry)
		ui_error ("No entry %s in form %s", ent, form);
	return (fw->fw_Entries + i);
}




void
uw_FormText (form, ent, text)
char *form, *ent, *text;
/*
 * Tweak a form entry.
 */
{
	Arg args[2];
	FormEntry *fe = uw_FindEntry (form, ent);
/*
 * Do the change.
 */
	XtSetArg (args[0], LblRes (fe), text);
	XtSetValues (fe->fe_Widget, args, 1);
	uw_sync ();
}





void
uw_FormMenu (form, ent, menu)
char *form, *ent, *menu;
/*
 * Tweak the menu in a form entry.
 */
{
	Arg args[2];
	FormEntry *fe = uw_FindEntry (form, ent);
	XtTranslations ttrans;
	char ctrans[256];
/*
 * Make a new set of translations and store them into the widget.
 */
	uw_IWRealize (menu, Top);
	sprintf (ctrans, PDATrans, fe->fe_Frame->fw_name, menu);
	ttrans = XtParseTranslationTable (ctrans);
	XtOverrideTranslations (fe->fe_Widget, ttrans);
}





int
uw_GetFText (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
SValue *argv, *retv;
/*
 * Command line function to return the text from a form entry.
 */
{
	Arg args[2];
	FormEntry *fe = uw_FindEntry (argv[0].us_v_ptr, argv[1].us_v_ptr);
	char *value;
/*
 * Look it up and return it.
 */
	XtSetArg (args[0], LblRes (fe), &value);
	XtGetValues (fe->fe_Widget, args, 1);
	*rett = SYMT_STRING;
	retv->us_v_ptr = usy_string (value);
	return (TRUE);
}






void
uw_FSave (lun, fw)
int lun;
UIFormWidget *fw;
/*
 * Save out a form widget.
 */
{
/*
 * Save the form widget itself, then the entries.
 */
	bfput (lun, fw, sizeof (UIFormWidget));
	bfput (lun, fw->fw_Entries, fw->fw_NEntry*sizeof (FormEntry));
}




GenWidget *
uw_FLoad (lun)
int lun;
/*
 * Load this one in.
 */
{
	UIFormWidget *fw = NEW (UIFormWidget);
	int i;
/*
 * Pull in the two pieces.
 */
	bfget (lun, fw, sizeof (UIFormWidget));
	fw->fw_Entries = (FormEntry*) malloc(fw->fw_NEntry*sizeof (FormEntry));
	bfget (lun, fw->fw_Entries, fw->fw_NEntry*sizeof (FormEntry));
/*
 * Go through the entries and tweak the flags.
 */
	for (i = 0; i < fw->fw_NEntry; i++)
		fw->fw_Entries[i].fe_Flags &= ~FEF_CDONE;
/*
 * Tweak the routines and we are done.
 */
	fw->fw_create = uw_FormCreate;
	fw->fw_clone = uw_FormClone;
	fw->fw_destroy = uw_FDestroy;
	fw->fw_popup = fw->fw_PopupCmd[0] ? uw_FormPopup : NULL;
	return ((GenWidget *) fw);
}


# endif /* XSUPPORT */
