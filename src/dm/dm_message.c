/*
 * Encapsulate our message interface for testing and debugging options.
 * When test mode is enabled, print the intended message to stdout before
 * sending the message.
 */

# include <string.h>
# include <stdio.h>

# include <ui.h>
# include <defs.h>
# include <message.h>

RCSID ("$Id: dm_message.c,v 2.2 1996-11-19 07:07:15 granger Exp $")

# include <dm.h>
# include "dm_vars.h"



void
dmsg_SendWindow (win, dmsg, len)
struct cf_window *win;
void *dmsg;
int len;
/*
 * Send a dm message to the named window.  We must first map the window
 * name to a process, from which we get the process' message handle.
 */
{
	if (TestMode)
	{
		dmsg_Show (win->cfw_process->p_name, dmsg, len, "sending to");
	}
	if (! win->cfw_process)
	{
		msg_ELog (EF_PROBLEM, "%s: window %s has no process",
			  "cannot send message", win->cfw_name);
	}
	else
	{
		msg_send (win->cfw_process->p_name, MT_DISPLAYMGR, FALSE, 
			  dmsg, len);
	}
}



void
dmsg_SendProcess (who, dmsg, len)
char *who;
void *dmsg;
int len;
{
	if (TestMode)
		dmsg_Show (who, dmsg, len, "sending to");
	msg_send (who, MT_DISPLAYMGR, FALSE, dmsg, len);
}



void
dmsg_Broadcast (group, dmsg, len)
char *group;
void *dmsg;
int len;
{
	if (TestMode)
		dmsg_Show (group, dmsg, len, "broadcasting to");
	msg_send (group, MT_DISPLAYMGR, TRUE, dmsg, len);
}



void
dmsg_SendTimer (msg, len)
void *msg;
int len;
{
	msg_send ("Timer", MT_TIMER, FALSE, msg, len);
}


void
dmsg_SendSound (msg, len)
void *msg;
int len;
{
	msg_send ("Sound", MT_SOUND, FALSE, msg, len);
}



void
dmsg_Show (who, msg, len, context)
char *who;
void *msg;
int len;
char *context;
{
	char buf[256];
	struct dm_msg *dmsg = (struct dm_msg *) msg;
	struct dm_reconfig *dmr = (struct dm_reconfig *) msg;
	struct dm_hello *dmh = (struct dm_hello *) msg;

	switch (dmsg->dmm_type)
	{
	   case DM_GEOMETRY:
		sprintf (buf, "DM_GEOMETRY: %dx%d+%d+%d",
			 dmsg->dmm_dx, dmsg->dmm_dy, dmsg->dmm_x, dmsg->dmm_y);
		break;
	   case DM_RECONFIG:
		sprintf (buf, "DM_RECONFIG: %dx%d+%d+%d, name %s",
			 dmr->dmr_dx, dmr->dmr_dy, dmr->dmr_x, dmr->dmr_y,
			 dmr->dmr_name);
		break;
	   case DM_SUSPEND:
		sprintf (buf, "DM_SUSPEND");
		break;
	   case DM_HELLO:
		sprintf (buf, "DM_HELLO: window %#0lx", dmh->dmm_win);
		break;
	   case DM_DIE:
		sprintf (buf, "DM_DIE");
		break;
	   case DM_DEFAULTS:
		sprintf (buf, "DM_DEFAULTS");
		break;
	   case DM_PDCHANGE:
		sprintf (buf, "DM_PDCHANGE");
		break;
	   case DM_PARCHANGE:
		sprintf (buf, "DM_PARCHANGE");
		break;
	   case DM_WBOUNDS:
		sprintf (buf, "DM_WBOUNDS");
		break;
	   case DM_HISTORY:
		sprintf (buf, "DM_HISTORY: ");
		TC_EncodeTime (&((struct dm_history *)dmsg)->dmm_time, 
			       TC_Full, buf+strlen(buf));
		break;
	   case DM_EVBIND:
		sprintf (buf, "DM_EVBIND");
		break;
	   default:
		sprintf (buf, "message type %d", dmsg->dmm_type);
		break;
	}
	ui_printf ("%s: %s %s [%s]\n", msg_myname(), context, who, buf);
}



