/*
 * Deal with user-originated events.
 */
static char *rcsid = "$Id: UserEvent.c,v 1.1 1990-06-04 17:20:56 corbet Exp $";

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
 * Forward definitions.
 */
# ifdef __STDC__
	static EventResponse *Ue_FindResponse (char *);
	static void Ue_EDMReport (XEvent *, char *);
# else
	static EventResponse *Ue_FindResponse ();
	static void Ue_EDMReport ();
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
	if (*nparam != 1)
	{
		msg_log ("RESOURCE BUG: Ue_pointer_event with %d params",
			nparam);
		return;
	}
/*
 * Try to find our response.
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
 * Deal with a buttonup event by efficiently doing nothing.  George Bush
 * would be proud.
 */
{ }
