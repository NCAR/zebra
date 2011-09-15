/*
 * Library routines which display manager clients may find useful.
 *
 * The goal is that someday all interaction with the display manager will be
 * hidden within this interface, so that display manager clients can run
 * standalone by specifying a few command-line options and letting the
 * library routines transparently provide most of the services of the
 * display manager.
 */
#include <stdio.h>
#include <string.h>

#include <X11/Xlib.h>

#include <config.h>
#include "defs.h"
#include "message.h"
#include "dm.h"
#include "dm_ctable.h"
#include "setup.h"

RCSID ("$Id: dm_lib.c,v 2.4 1996-11-19 08:03:19 granger Exp $")

/*
 * Private prototypes
 */
static int dm_CTResponse FP ((struct message *msg, struct dm_ctable **ctr));

/*
 * Local variables
 */
char DisplayManager[ CFG_MSGNAME_LEN ];
char WindowName[ CFG_MSGNAME_LEN ];

static char MessageName[ CFG_MSGNAME_LEN ];
static char GroupName[ CFG_MSGNAME_LEN ];


void
dm_Setup (argc, argv, default_name)
int *argc;
char *argv[];
char *default_name;
/*
 * Parse the arg list for recognized options and remove them.
 * We don't support abbreviation here.
 */
{
	int n;
	int i = 1;

	WindowName[0] = 0;
	DisplayManager[0] = 0;
	GroupName[0] = 0;
	if (default_name)
		strcpy (MessageName, default_name);
	else
	{
		char *slash = strrchr (argv[0], '/');
		strcpy (MessageName, (slash && *slash) ? slash + 1 : argv[0]);
	}

	while (i < *argc)
	{
		if (! strcmp (argv[i], "-name"))
		{
			if (i+1 < *argc)
				strcpy (MessageName, argv[i+1]);
			else
			{
				printf ("-name: option needs argument\n");
				exit (2);
			}
		}
		else if (! strcmp (argv[i], "-dm"))
		{
			if (i+1 < *argc)
				strcpy (DisplayManager, argv[i + 1]);
			else
			{
				printf ("-dm: option needs argument\n");
				exit (2);
			}
		}
		else if (! strcmp (argv[i], "-window"))
		{
			if (i+1 < *argc)
				strcpy (WindowName, argv[i + 1]);
			else
			{
				printf ("-window: option needs argument\n");
				exit (2);
			}
		}
		else if (! strcmp (argv[i], "-join"))
		{
			if (i+1 < *argc)
				strcpy (GroupName, argv[i + 1]);
			else
			{
				printf ("-join: option needs argument\n");
				exit (2);
			}
		}
		else
		{
			++i;
			continue;
		}
		/* remove two options and leave i the same */
		*argc -= 2;
		for (n = i; n < *argc; ++n)
			argv[n] = argv[n+2];
	}
	if (!GroupName[0] && !DisplayManager[0])
		strcpy (GroupName, "Graphproc");
	if (! DisplayManager[0])
		strcpy (DisplayManager, DISPLAY_MANAGER);
	if (!GroupName[0])
		sprintf (GroupName, "group-%s", DisplayManager);
	if (!WindowName[0])
		strcpy (WindowName, MessageName);
}



char *
dm_ManagerName ()
/*
 * Return the connection name of our display manager
 */
{
	return (DisplayManager);
}



char *
dm_WindowName ()
/*
 * Return the name of the configuration window we are realizing
 */
{
	return (WindowName);
}



char *
dm_MessageName ()
/*
 * Return our connection name
 */
{
	return (MessageName);
}



char *
dm_GroupName ()
/*
 * Return our connection name
 */
{
	return (GroupName);
}
	


void
dm_Send (msg, len)
void *msg;
int len;
/*
 * Send this message to the display manager, without broadcasting
 */
{
	msg_send (DisplayManager, MT_DISPLAYMGR, FALSE, msg, len);
}



void
dm_Greet ()
/*
 * Say "hello" to the display manager so that we can get our
 * configuration and plot descriptions.
 */
{
	struct dm_hello dmh;
/*
 * Send the message.
 */
	dmh.dmm_type = DM_HELLO;
	dmh.dmm_win = 0;
	dm_Send (&dmh, sizeof (dmh));
}



void
dm_Reconfig (dmsg)
struct dm_reconfig *dmsg;
/*
 * Retrieve our window name from this reconfig message.
 */
{
	strcpy (WindowName, dmsg->dmr_name);
}



void
dm_SendWindowID (win)
long win;
/*
 * Send our window id to the display manager.
 */
{
	struct dm_hello dmh;
/*
 * Send the message.
 */
	dmh.dmm_type = DM_WINDOW;
	dmh.dmm_win = win;
	dm_Send (&dmh, sizeof (dmh));
}



struct dm_ctable *
dm_ColorTable (name)
char *name;
/*
 * Send off a request for this color table and return the result.
 * Return NULL if the request fails.  The returned pointer must be
 * freed by the application.
 */
{
	struct dm_ctable *repl;
	struct dm_ctr ctr;

	ctr.dmm_type = DM_R_CTABLE;
	strcpy (ctr.dmm_table, name);
	dm_Send (&ctr, sizeof (ctr));
/*
 * Now await the reply.
 */
	repl = NULL;
	msg_Search (MT_DISPLAYMGR, dm_CTResponse, &repl);
	return (repl);
}



static int
dm_CTResponse (msg, ctr)
struct message *msg;
struct dm_ctable **ctr;
/*
 * The color table search routine, called out of msg_Search.  This guy, on
 * success, returns a structure which must be freed after use.
 */
{
	struct dm_ctable *dmm = (struct dm_ctable *) msg->m_data;
/*
 * If this is not a color table response, blow it off.
 */
	if (dmm->dmm_type != DM_TABLE && dmm->dmm_type != DM_NOTABLE)
		return (MSG_ENQUEUE);
/*
 * If it's a negative response, we heave a sigh, and, head low, return the
 * bad news.
 */
	if (dmm->dmm_type == DM_NOTABLE)
	{
		*ctr = 0;
		return (MSG_DONE);
	}
/*
 * Got it.  Copy out the info and return.
 */
	/* *ctr = (dmm->dmm_type == DM_TABLE) ? dmm : 0; */
	*ctr = (struct dm_ctable *) malloc (msg->m_len);
	memcpy (*ctr, dmm, msg->m_len);
	return (MSG_DONE);
}



void
dm_Configure (display, win, x, y, width, height)
Display *display;
Window win;
int x, y;
int width, height;
/*
 * Configure this window with the given geometry.  Rely on the window
 * manager to send the ConfigureNotify to the client so that the 
 * window's children re-arrange themselves as necessary.
 */
{
	unsigned int mask;
	XWindowChanges changes;

	mask = CWX | CWY | CWWidth | CWHeight;
	changes.x = x;
	changes.y = y;
	changes.width = width;
	changes.height = height;
	XConfigureWindow (display, win, mask, &changes);
}



