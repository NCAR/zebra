/*
 * The new event logger.
 */

# include <X11/Intrinsic.h>
# include <X11/Xaw/Form.h>
# include <X11/Shell.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Command.h>
# include "defs.h"
# include <../include/message.h>


/*
 * Where the logging info lives.
 */
# define LBUFSIZE 10000
char Logbuf[LBUFSIZE];
char *Lp = Logbuf;


/*
 * Text info.
 */
static int Buflen = 0;
static char *Initmsg = "$Id: EventLogger.c,v 1.1 1990-06-14 15:41:19 corbet Exp $\n";

/*
 * Our widgets.
 */
Widget Top, Text, Shell, Form;
XtAppContext Appc;



static String Resources[] = 
{
	"	*input:		True",
	"	*Label*font:	-*-helvetica-bold-r-*-*-*-120-*-*-*-*-*-*",
	"	*Toggle*font:	-*-helvetica-medium-r-*-*-*-120-*-*-*-*-*-*",
	"	*Text*font:	-*-helvetica-medium-r-*-*-*-120-*-*-*-*-*-*",
	0
};


main (argc, argv)
int argc;
char **argv;
{
	Arg args[20];
	Widget w;
	int xevent (), msg_event ();
/*
 * Get set up with the toolkit.
 */
	Top = XtAppInitialize (&Appc, "EventLogger", NULL, 0, &argc, argv,
		Resources, NULL, 0);
/*
 * Create our shell.
 */
	XtSetArg (args[0], XtNinput, True);
	XtSetArg (args[1], XtNoverrideRedirect, False);
	Shell = XtCreatePopupShell ("EventLogger", topLevelShellWidgetClass,
		Top, args, 2);
/*
 * Put a form inside it.
 */
	Form = XtCreateManagedWidget ("form", formWidgetClass, Shell, args, 0);
/*
 * Add the clear button.
 */
	XtSetArg (args[0], XtNfromHoriz, NULL);
	XtSetArg (args[1], XtNfromVert, NULL);
	XtSetArg (args[2], XtNtop, XtChainTop);
	XtSetArg (args[3], XtNbottom, XtChainTop);
	XtSetArg (args[4], XtNleft, XtChainLeft);
	XtSetArg (args[5], XtNright, XtChainLeft);
	w = XtCreateManagedWidget ("Clear", commandWidgetClass, Form, args, 6);
/*
 * Now the big, hairy, text widget.
 */
	strcpy (Logbuf, "This is some exciting\nLog buffer stuff\n");
	XtSetArg (args[0], XtNresize, XawtextResizeNever);
	XtSetArg (args[1], XtNfromHoriz, NULL);
	XtSetArg (args[2], XtNfromVert, w);
	XtSetArg (args[3], XtNtype, XawAsciiString);
	XtSetArg (args[4], XtNeditType, XawtextAppend);
	XtSetArg (args[5], XtNscrollVertical, XawtextScrollAlways);
	XtSetArg (args[6], XtNtop, XtChainTop);
	XtSetArg (args[7], XtNstring, Initmsg);
	Text = XtCreateManagedWidget ("text", asciiTextWidgetClass, Form,
		args, 8);
	Buflen = strlen (Initmsg);
/*
 * Fire it up.
 */
	/* XtRealizeWidget (Shell); */
/*
 * Hook into the message system.
 */
	if (! msg_connect (msg_event, "Event logger"))
	{
		printf ("Unable to connect to message handler\n");
		exit (1);
	}
/*
 * Join the client event and event logger groups.
 */
	msg_join ("Client events");
	msg_join ("Event logger");
/*
 * Tell msglib about our X connection.
 */
	msg_add_fd (XConnectionNumber (XtDisplay (Shell)), xevent);
/*
 * Now we just wait.
 */
	XtPopup (Shell, XtGrabNone);
	sync ();
	xevent (0);
	msg_await ();
}






xevent (fd)
int fd;
/*
 * Deal with an Xt event.
 */
{
	XEvent event;
/*
 * Deal with events as long as they keep coming.
 */
 	while (XtAppPending (Appc))
	{
		XtAppNextEvent (Appc, &event);
		XtDispatchEvent (&event);
	}
	return (0);
}






msg_event (msg)
struct message *msg;
/*
 * Log a client event.
 */
{
	struct mh_client *client = (struct mh_client *) msg->m_data;
	char mb[200];
/*
 * Check for log messages.
 */
	if (msg->m_proto == MT_LOG)
	{
		sprintf (mb, "[%s]\t%s", msg->m_from, msg->m_data);
		do_log (mb);
		return;
	}
/*
 * Everything else is assumed to be a message handler event.
 */	
	switch (client->mh_type)
	{
	   case MH_CLIENT:
	   	switch (client->mh_evtype)
		{
		   case MH_CE_CONNECT:
		   	sprintf (mb,"[%s]\tConnect on %d", client->mh_client,
				msg->m_seq);
			break;
		   case MH_CE_DISCONNECT:
		   	sprintf (mb,"[%s]\tDisconnect on %d", client->mh_client,
				msg->m_seq);
			break;
		   case MH_CE_JOIN:
		   	sprintf (mb,"[%s]\tGroup %s joined on %d",
				client->mh_client, client->mh_group,
				msg->m_seq);
			break;
		}
		do_log (mb);
		break;
	   case MH_SHUTDOWN:
	   	printf ("MESSAGE SERVER SHUTDOWN!\n");
		break;
	   default:
	   	printf ("Unknown message type %d\n", client->mh_type);
	}
}





do_log (msg)
char *msg;
{
	int curpos = Lp - Logbuf;
	Arg args[10];
	XawTextBlock tb;
/*
 * Add the stuff to our buffer.
 */
	printf ("%s\n", msg);
# ifdef notdef
	strcpy (Lp, msg);
	Lp += strlen (Lp);
	*Lp++ = '\n';
	*Lp = '\0';
/*
 * Tell Xt about it.
 */
	XtSetArg (args[0], XtNlength, Lp - Logbuf);
	XtSetArg (args[1], XtNstring, Logbuf);
	XtSetArg (args[2], XtNinsertPosition, Lp - Logbuf - 1);
	XtSetValues (Text, args, 3);
	/* XawTextInvalidate (Text, curpos ? curpos - 1 : 0, Lp - Logbuf); */
	XawTextDisplay (Text);
	sync ();
# endif
	strcat (msg, "\n");	/* XXX */
	tb.firstPos = 0;
	tb.length = strlen (msg);
	tb.ptr = msg;
	tb.format = FMT8BIT;
	XawTextReplace (Text, Buflen, Buflen, &tb);
	XawTextDisplay (Text);
	sync ();
	Buflen += strlen (msg);
	XawTextSetInsertionPoint (Text, Buflen);
}




sync ()
/*
 * Synchronize with the window system.
 */
{
	XSync (XtDisplay (Top), False);
}
