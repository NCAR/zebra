/*
 * Deal with user-originated events.
 */
static char *rcsid = "$Id: UserEvent.c,v 2.0 1991-07-18 23:00:21 corbet Exp $";

# include <X11/Intrinsic.h>
# include "../include/defs.h"
# include "../include/pd.h"
# include "../include/dm.h"
# include "../include/message.h"
# include "GraphProc.h"
# include "EventQueue.h"



/*
 * The structure which defines the response to a user event, such as a
 * mouse or keyboard event.
 */

# define MAX_EV_DATA	256
typedef struct ev_resp
{
	void	(*er_handler) ();	/* The function to handle this event */
	char	er_data[MAX_EV_DATA];	/* Data to pass to the function	*/
} EventResponse;


/*
 * Events are kept in a symbol table, stored by keystring or pointer 
 * button name.
 */
static stbl EventHandlers = 0;

/*
 * The procedure for dealing with motion events.  Normally, there is nothing
 * here, but some sorts of events can make motions interesting for a while.
 *
 * While motions are being processed, the usual actions for button events
 * may also be overridden.
 */
static void (*MotionHandler) () = 0;	/* Motion event handler		*/
static void (*ORBtnDown)() = 0;		/* Override button down handler	*/
static void (*ORBtnUp) () = 0;		/* Override button up handler	*/


/*
 * Forward definitions.
 */
# ifdef __STDC__
	static EventResponse *Ue_FindResponse (char *);
	static void Ue_EDMReport (XEvent *, char *);
	static void Ue_ELocal (XEvent *, char *);
	static void Ue_EMenu (XEvent *, char *);
# else
	static EventResponse *Ue_FindResponse ();
	static void Ue_EDMReport ();
	static void Ue_ELocal ();
	static void Ue_EMenu ();
# endif





Ue_Init ()
/*
 * Initialize.
 */
{
	EventHandlers = usy_c_stbl ("userevents");
}





Ue_NewBinding (dmsg)
struct dm_ebchange *dmsg;
/*
 * Deal with these event binding changes.
 */
{
	int i, type;
	EventResponse *resp;
	union usy_value v;
/*
 * Pass through the list of changes that DM has given us.
 */
	for (i = 0; i < dmsg->dmm_nbind; i++)
	{
		struct dm_evbind *bind = dmsg->dmm_bindings + i;
	/*
	 * Find and use the old response, if it exists.  Otherwise get a
	 * new one.
	 */
		if (! (resp = Ue_FindResponse (bind->dmm_code)))
			resp = ALLOC (EventResponse);
	/*
	 * Fill in the stuff.
	 */
		switch (bind->dmm_action)
		{
		   case AC_Report:
		   	resp->er_handler = Ue_EDMReport;
			strcpy (resp->er_data, bind->dmm_adata);
			break;

		   case AC_CommandText:
		   	resp->er_handler = Ue_ELocal;
			strcpy (resp->er_data, bind->dmm_adata);
			break;

		   case AC_PopupMenu:
			uw_IWRealize (bind->dmm_adata, Graphics);
			/* Ue_FixTransl (bind->dmm_adata, "<Btn3Down>"); */
		   	resp->er_handler = Ue_EMenu;
			strcpy (resp->er_data, bind->dmm_adata);
			break;

		   default:
		   	msg_log ("Unimplemented action: %d", bind->dmm_action);
			break;
		}
	/*
	 * Redefine the action.
	 */
		v.us_v_ptr = (char *) resp;
		usy_s_symbol (EventHandlers, bind->dmm_code, SYMT_POINTER, &v);
	}
}






Ue_FixTransl (menu, button)
char *menu, *button;
/*
 * Fix up the translations for this button to call this menu.
 */
{
	char trbuf[200];
	XtTranslations ttable;
/*
 * Generate the translation string.
 */
	sprintf (trbuf, "%s:	XawPositionSimpleMenu(%s)MenuPopup(%s)",
		button, menu, menu);
/*
 * Parse it, and add it to our graphics widget.
 */
	ttable = XtParseTranslationTable (trbuf);
	XtOverrideTranslations (Graphics, ttable);
/*
 * Now we return and throw away the ttable memory.  There doesn't seem to be
 * a way around it.
 */
}





static EventResponse *
Ue_FindResponse (code)
char *code;
/*
 * Find the response going with this event code.
 */
{
	union usy_value v;
	int type;

	if (! usy_g_symbol (EventHandlers, code, &type, &v))
		return (0);
	return ((EventResponse *) v.us_v_ptr);
}




/* ------------------------------------------------------------------ */
/*
 * The actual event handler routines.
 */


static void
Ue_EDMReport (event, data)
XEvent *event;
char *data;
/*
 * Simply report this event back to the display manager.
 */
{
	struct dm_event dme;

	dme.dmm_type = DM_EVENT;
	strcpy (dme.dmm_data, data);
	msg_send ("Displaymgr", MT_DISPLAYMGR, FALSE, &dme, sizeof (dme));
}





static void
Ue_ELocal (event, data)
XEvent *event;
char *data;
/*
 * Execute the "data" as a local command.
 */
{
	struct dm_event dme;

	ui_perform (data);
}




static void
Ue_EMenu (event, data)
XEvent *event;
char *data;
/*
 * Pop up this menu.
 */
{
/*	uw_IWPopup (data); */
	Widget uw_IWWidget ();
	Widget w = uw_IWWidget (data);

	XtCallActionProc (w, "XawPositionSimpleMenu", event, &data, 1);
	XtCallActionProc (w, "MenuPopup", event, &data, 1);
}






void
Ue_MotionEvent (w, event, params, nparam)
Widget w;
XEvent *event;
String *params;
Cardinal *nparam;
/*
 * Deal with motion events.
 */
{
	XMotionEvent *xme = (XMotionEvent *) event;
	if (MotionHandler)
		(*MotionHandler) (xme->x, xme->y);
}





void 
Ue_PointerEvent (w, event, params, nparam)
Widget w;
XEvent *event;
String *params;
Cardinal *nparam;
/*
 * Deal with a pointer event.
 */
{
	EventResponse *resp;
/*
 * Make sure params are right.
 */
	msg_ELog (EF_DEBUG, "Pointer event in widget 0x%x", w);
	if (*nparam != 1)
	{
		msg_log ("RESOURCE BUG: Ue_pointer_event with %d params",
			nparam);
		return;
	}
/*
 * If there is an override handler, use it.
 */
	if (ORBtnDown)
	{
		(*ORBtnDown) (event, params[0]);
		return;
	}
/*
 * Otherwise try to find our response.
 */
	if ((resp = Ue_FindResponse (params[0])) == 0)
	{
		msg_log ("Unknown event '%s'", params[0]);
		return;
	}
/*
 * OK, got it.  Call the action proc and be done with it.
 */
	(*resp->er_handler) (event, resp->er_data);
}




void Ue_el ()
{
	msg_log ("E/L event");
}



void 
Ue_KeyEvent (w, event, params, nparam)
Widget w;
XEvent *event;
String *params;
Cardinal *nparam;
/*
 * Deal with a keystroke event.
 */
{
	KeySym sym;
	XKeyEvent *key = (XKeyEvent *) event;
	char buf[30];
	EventResponse *resp;

	XLookupString (key, buf, 30, &sym, 0);
	if ((resp = Ue_FindResponse (XKeysymToString (sym))) == 0)
		return;
	(*resp->er_handler) (event, resp->er_data);
}




void
Ue_ButtonUp (w, event, params, nparam)
Widget w;
XEvent *event;
String *params;
int nparam;
/*
 * Deal with button events.  Our usual response is to prudently do nothing,
 * but that can be overridden.
 */
{
	if (ORBtnUp)
		(*ORBtnUp) (event);
}





void
Ue_Override (down, up, motion)
void (*down) (), (*up) (), (*motion) ();
/*
 * Temporarily take over handling of button events.
 */
{
	ORBtnDown = down;
	ORBtnUp = up;
	MotionHandler = motion;
}



void 
Ue_ResetOverride ()
/*
 * Get rid of a button override.
 */
{
	ORBtnDown = ORBtnUp = MotionHandler = 0;
}
