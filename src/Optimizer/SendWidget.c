/*
 * Widget for handling direct communication with the radars
 */
/*		Copyright (C) 1987,88,89,90,91 by UCAR
 *	University Corporation for Atmospheric Research
 *		   All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced
 * or used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular purpose.
 * UCAR does not indemnify any infringement of copyright, patent, or trademark
 * through use or modification of this software.  UCAR does not provide 
 * maintenance or updates for its software.
 */

# include <stdio.h>
# include <stdlib.h>
# include <unistd.h>
# include <signal.h>
# include <errno.h>
# include <time.h>
# include <sys/time.h>
# include <setjmp.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Label.h>
# include <ui_error.h>
# include <config.h>
# include "globals.h"
# include "prototypes.h"
# include "radar.h"

RCSID("$Id: SendWidget.c,v 1.9 2000-04-24 19:08:36 burghart Exp $")

/*
 * Private prototypes
 */
static void sw_DialMode FP ((int));
static void sw_SendMode FP ((int));
static void sw_DialRadar FP ((Widget, XtPointer, XtPointer));
static void sw_SendScan FP ((Widget, XtPointer, XtPointer));
static void sw_Timeout FP ((int signal));
static void sw_KermitCheck FP ((int signal));
static int sw_MakeFile FP ((int, char *));
static void sw_KillKermit FP ((int));

/*
 * How long do we wait before we fail a file send? or a connection?
 */
# define SENDWAIT	60
# define CONNECTWAIT	90

/*
 * Process id's for each kermit and file pointers to and from each kermit
 */
int	Kermit[MAXRAD];
FILE	*ToKermit[MAXRAD], *FromKermit[MAXRAD];
# define KERMIT_EXEC "/zeb/src/Optimizer/nkermit/nkermit"

/*
 * The dial/send button and status widget for each radar
 */
Widget	WButton[MAXRAD], WStatus[MAXRAD];

/*
 * Last number used for a filename sent to each radar
 */
int	Last[MAXRAD];

/*
 * Environment buffer for setjmp use
 */
jmp_buf	Jmpbuf;

/*
 * Buffer for kermit I/O
 */
# define BUFLEN		256
char	Buf[BUFLEN];




Widget
sw_SWidget (parent)
Widget	parent;
/*
 * Create the "send" widget for delivering scan information directly to
 * the radars.
 */
{
	Widget	form, w, wstatus = NULL, above;
	int	n, r;
	Arg	args[10];
	char	wname[20];
/*
 * Create a form widget to hold everything
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 3); n++;
	XtSetArg (args[n], XtNwidth, 20); n++;
	XtSetArg (args[n], XtNheight, 1); n++;
	XtSetArg (args[n], XtNresizable, True); n++;
	XtSetArg (args[n], XtNresize, True); n++;
	form = XtCreateManagedWidget ("sendForm", formWidgetClass, parent,
		args, n);
/*
 * Put together a dial/send widget and status widget for each radar
 */
	above = NULL;
	
	for (r = 0; r < Nradars; r++)
	{
	/*
	 * No widgets for radars with no modem device/param file name
	 */
		if (! Rad[r].line_out)
		    continue;
	/*
	 * Dial/send widget
	 */
		sprintf (wname, "dialsend%d", r);

		n = 0;
		XtSetArg (args[n], XtNfromHoriz, NULL); n++;
		XtSetArg (args[n], XtNfromVert, above); n++;
		XtSetArg (args[n], XtNwidth, 100); n++;
		XtSetArg (args[n], XtNresizable, True); n++;
		XtSetArg (args[n], XtNresize, True); n++;
		w = WButton[r] = 
		    XtCreateManagedWidget (wname, commandWidgetClass, form, 
					   args, n);
	/*
	 * Status widget
	 */
		sprintf (wname, "status%d", r);

		n = 0;
		XtSetArg (args[n], XtNfromHoriz, w); n++;
		XtSetArg (args[n], XtNfromVert, above); n++;
		XtSetArg (args[n], XtNborderWidth, 0); n++;
		XtSetArg (args[n], XtNwidth, 100); n++;
		XtSetArg (args[n], XtNresizable, True); n++;
		XtSetArg (args[n], XtNresize, True); n++;
		wstatus = WStatus[r] = XtCreateManagedWidget (wname, 
			labelWidgetClass, form, args, n);

		above = w;
	/*
	 * If we have a phone number, start out in dial mode.  Otherwise,
	 * start in send mode.
	 */
		if (Rad[r].phone)
		  sw_DialMode (r);
		else
		  sw_SendMode (r);
	/*
	 * Initialize some stuff
	 */
		Kermit[r] = 0;
		ToKermit[r] = NULL;
		FromKermit[r] = NULL;
		Last[r] = 0;
	}
/*
 * Set a signal for a change in child status, so we know if a kermit goes
 * away later
 */
	signal (SIGCHLD, sw_KermitCheck);
/*
 * Done
 */
	return (form);
}




static void
sw_DialMode (r)
int	r;
/*
 * Go into "dial" mode for radar r
 */
{
	char	string[20];
	Arg	arg;
/*
 * Put "Dial <rad>" on the button widget and make it call the dial routine
 */	
	sprintf (string, "Dial %s", Rad[r].name);

	XtSetArg (arg, XtNlabel, string);
	XtSetValues (WButton[r], &arg, 1);
/*
 * Remove both possible callbacks
 */
	XtRemoveCallback (WButton[r], XtNcallback, sw_DialRadar, 
			  (XtPointer) r);
	XtRemoveCallback (WButton[r], XtNcallback, sw_SendScan, 
			  (XtPointer) r);
/*
 * Put in the dial callback
 */
	XtAddCallback (WButton[r], XtNcallback, sw_DialRadar, (XtPointer) r);
/*
 * Put "Not Connected" on the status widget
 */
	XtSetArg (arg, XtNlabel, "Not Connected");
	XtSetValues (WStatus[r], &arg, 1);
}




static void
sw_SendMode (r)
int	r;
/*
 * Go into "send" mode for radar r
 */
{
	char	string[40];
	Arg	arg;
/*
 * Put "Send to <rad>" on the button widget and make it call the send routine
 */	
	if (Rad[r].phone)
	    sprintf (string, "Send to %s", Rad[r].name);
	else
	    sprintf (string, "Write %s file", Rad[r].name);

	XtSetArg (arg, XtNlabel, string);
	XtSetValues (WButton[r], &arg, 1);
/*
 * Remove both possible callbacks
 */
	XtRemoveCallback (WButton[r], XtNcallback, sw_DialRadar, 
			  (XtPointer) r);
	XtRemoveCallback (WButton[r], XtNcallback, sw_SendScan, 
			  (XtPointer) r);
/*
 * Put in the send callback
 */
	XtAddCallback (WButton[r], XtNcallback, sw_SendScan, (XtPointer) r);
/*
 * Put either "Connected" or "OK" on the status widget
 */
	if (Rad[r].phone)
	  XtSetArg (arg, XtNlabel, "Connected");
	else
	  XtSetArg (arg, XtNlabel, "OK");

	XtSetValues (WStatus[r], &arg, 1);
}




static void
sw_DialRadar (w, val, junk)
Widget	w;
XtPointer	val, junk;
{
/*
 * Establish the modem connection with the selected radar
 */
	int	r = (int) val;
	int	in_sockets[2], out_sockets[2], status, done;
	char	speed[10];
/*
 * Open two pipes and create a fork for starting up a kermit
 */
	if (pipe (in_sockets) < 0 || pipe (out_sockets) < 0)
	{
		msg_ELog (EF_PROBLEM, "Error creating pipes to child");
		ui_bailout (NULL);
	}
/*
 * Fork an instantiation of kermit
 */
	if ((Kermit[r] = fork ()) == -1)
	{
		msg_ELog (EF_PROBLEM, "Unable to fork!");
		ui_bailout (NULL);
	}
	else if (Kermit[r] == 0)
	{
	/*
	 * Move the kermit's stdin, stdout, and stderr to use the pipes 
	 * we created above
	 */
		close (in_sockets[1]);
		close (out_sockets[0]);

		if (dup2 (in_sockets[0], 0) != 0 ||
		    dup2 (out_sockets[1], 1) != 1 ||
		    dup2 (out_sockets[1], 2) != 2)
		{
			msg_ELog (EF_PROBLEM, 
				"Error moving child's stdin, out, or err");
			_exit (1);
		}
	/*
	 * Run kermit from /tmp, where we want to create the files to send
	 */
		chdir ("/tmp");
	/*
	 * Exec kermit.  We don't dial if it's a radio modem, otherwise we do.
	 */
		sprintf (speed, "%d\0", Rad[r].baud);

		if (! strcmp (Rad[r].phone, "radio-modem"))
			status = execl (KERMIT_EXEC, "nkermit", "slb", 
					Rad[r].line_out, speed, NULL);
		else
			status = execl (KERMIT_EXEC, "nkermit", "slbn", 
					Rad[r].line_out, speed, Rad[r].phone, 
					NULL);

		msg_ELog (EF_PROBLEM, "Error %d executing nkermit!", status);
		_exit (1);
	}
/*
 * Keep the user informed
 */
	msg_ELog (EF_INFO, "Dialing %s using %s (%s)", Rad[r].name, 
		Rad[r].line_out, Rad[r].phone);
/*
 * Connect the pipes to ToKermit and FromKermit
 */
	close (in_sockets[0]);
	close (out_sockets[1]);

	ToKermit[r] = fdopen (in_sockets[1], "w");
	setlinebuf (ToKermit[r]);
	FromKermit[r] = fdopen (out_sockets[0], "r");

	if (! ToKermit[r] || ! FromKermit[r])
	{
		msg_ELog (EF_PROBLEM, "Error opening ToKermit or FromKermit");
		return;
	}
/*
 * Read output from kermit and make sure we get connected
 */
	signal (SIGALRM, sw_Timeout);
	alarm (CONNECTWAIT);

	done = FALSE;

	while (! done)
	{
		if (! setjmp (Jmpbuf))
			fgets (Buf, BUFLEN, FromKermit[r]);
		else
			sprintf (Buf, "%d s timeout", CONNECTWAIT);

		done = feof (FromKermit[r]) || (*Buf && 
			strncmp (Buf, "Warning, write access", 21) != 0);
	}

	alarm (0);

	if (strncmp (Buf, "Connected", 9) != 0)
	{
		if (Buf[strlen (Buf) - 1] == '\n')
			Buf[strlen (Buf) - 1] = '\0';

		msg_ELog (EF_PROBLEM, "%s connect failed: %s", Rad[r].name, 
			Buf);
		sw_KillKermit (r);
		return;
	}
/*
 * Success! Put the dial/send button into send mode
 */
	msg_ELog (EF_INFO, "Connected to %s", Rad[r].name);
	sw_SendMode (r);
}




static void
sw_SendScan (w, val, junk)
Widget	w;
XtPointer	val, junk;
/*
 * Send the current scan to the selected radar, or write the scan file
 */
{
    int	r = (int) val;
    time_t	itime;
    char	fn[20], label[20];
    Arg	arg;
/*
 * Make sure we have a valid scan option
 */
    if (Opt == NO_OPT)
    {
	msg_ELog (EF_INFO, "No scan to send!");
	return;
    }
/*
 * Modem option
 */
    if (Rad[r].phone)
    {
    /*
     * Increment the file number.  We count from 1 to 6, then start over
     */
	Last[r]++;
	if (Last[r] > 6)
	    Last[r] = 1;
    /*
     * Build the filename
     */
	sprintf (fn, "/tmp/%s%1d", Rad[r].name, Last[r]);
	msg_ELog (EF_INFO, "Sending file %s to %s", fn, Rad[r].name);
    /*
     * Create the file
     */
	sw_MakeFile (r, fn);
    /*
     * Tell kermit to send it
     */
	fprintf (ToKermit[r], "%s\n", fn);
    /*
     * Wait for a reply from kermit
     */
	signal (SIGALRM, sw_Timeout);
	alarm (SENDWAIT);

	if (! setjmp (Jmpbuf))
	    fgets (Buf, BUFLEN, FromKermit[r]);
	else
	    sprintf (Buf, "%d s timeout sending %s", SENDWAIT, fn);

	alarm (0);
    /*
     * Remove the file and check the result
     */
	if (strncmp (Buf, "Successfully sent", 17) != 0)
	{
	/*
	 * If we failed, kill this kermit and go back to dial mode
	 */
	    msg_ELog (EF_PROBLEM, "File send failed: %s", Buf);
	    Last[r]--;
	    sw_KillKermit (r);
	    return;
	}
    /*
     * Success! Update the status widget to show the time and file sent
     */
	msg_ELog (EF_INFO, "Successfully sent %s", fn);

	sprintf (label, "%s ", fn);
	time (&itime);
	strncat (label, ctime (&itime) + 11, 5);
	XtSetArg (arg, XtNlabel, label);
	XtSetValues (WStatus[r], &arg, 1);
    }
/*
 * File writing option
 */
    else
    {
    /*
     * Create the file
     */
	if (sw_MakeFile (r, Rad[r].line_out))
	{
	    msg_ELog (EF_INFO, "Successfully wrote scan file %s for %s\n", 
		      Rad[r].line_out, Rad[r].name);
	    XtVaSetValues (WStatus[r], XtNlabel, "OK", NULL);
	}
	else
	{
	    msg_ELog (EF_PROBLEM, "Write of scan file for %s failed\n", 
		      Rad[r].name);
	    XtVaSetValues (WStatus[r], XtNlabel, "FAILED", NULL);
	}
    }
}





void
sw_Timeout (int signal)
{
/* 
 * Sending a file has taken too long
 */
	longjmp (Jmpbuf, 1);
}




void
sw_KermitCheck (int signal)
/*
 * Handle a "change in child status" signal.  We only care if a child
 * died.
 */
{
	int	r;
/*
 * Make sure all the kermits we started are still around
 */
	for (r = 0; r < Nradars; r++)
	{
	/*
	 * If we never started a kermit or if we did and it's still 
	 * around, just return
	 */
		if (! Kermit[r] || kill (Kermit[r], 0) == 0)
			continue;
	/*
	 * If we get here, then this kermit died.  Use KillKermit to
	 * close the pipes and reinitialize.
	 */
		msg_ELog (EF_PROBLEM, "Connection to %s died!", Rad[r].name);
		sw_KillKermit (r);
	}
}




static int
sw_MakeFile (r, fname)
int	r;
char	*fname;
/*
 * Write scan parameter file 'fname' for radar r.  On exit, 'fullname'
 * contains the full pathname of the file created.
 */
{
    FILE *sfile;
    int	i;
    float left, right, bottom, top;
/*
 * Open the file
 */
    if (! (sfile = fopen (fname, "w")))
    {
	msg_ELog (EF_PROBLEM, "Error %d opening param file '%s'", errno, 
		  fname);
	return 0;
    }
/*
 * Generate the scan
 */
    ERRORCATCH
	GenScan (Rad + r, Slowtime[Opt+3], Hres + Opt * RES_STEP, 
		 Vres + Opt * RES_STEP, TRUE);
    ON_ERROR
	msg_ELog (EF_PROBLEM, "BUG: Scan should have worked!");
    ENDCATCH
/*
 * Find the scan limits
 */
    if (Rad[r].scantype == RHI)
    {
	bottom = MAX (Rad[r].el_bottom, Rad[r].min_elev);
	top = MIN (Rad[r].el_top, 
		   RAD_TO_DEG (atan (Vol_top / Rad[r].min_range)));

	left = Rad[r].anglist[0];
	right = Rad[r].anglist[Rad[r].nsweeps - 1];
    }
    else
    {
	bottom = Rad[r].anglist[0];
	top = Rad[r].anglist[Rad[r].nsweeps - 1];

	left = Rad[r].az_left;
	right = Rad[r].az_right;
    }
/*
 * Write the file
 */
    switch (Rad[r].scantype)
    {
      case PPI:
	fprintf (sfile, "PPI\n");
	break;
      case RHI:
	fprintf (sfile, "RHI\n");
	break;
      case SUR:
	fprintf (sfile, "SUR\n");
	break;
      default:
	msg_ELog (EF_PROBLEM, "Bad scan type %d in sw_MakeFile\n",
		  (int)Rad[r].scantype);
	return 0;
    }

    fprintf (sfile, "Sweeps: %d\n", Rad[r].nsweeps);
    fprintf (sfile, "Scan rate: %.2f deg/s\n", Rad[r].scanrate);
    fprintf (sfile, "Hits: %d\n", Rad[r].hits);
    fprintf (sfile, "PRF: %d Hz\n", Rad[r].prf);
    fprintf (sfile, "Left azimuth: %.1f\n", left);
    fprintf (sfile, "Right azimuth: %.1f\n", right);
    fprintf (sfile, "Bottom elevation: %.1f\n", bottom);
    fprintf (sfile, "Top elevation: %.1f\n", top);
    fprintf (sfile, "Angle list:");
    for (i = 0; i < Rad[r].nsweeps; i++)
    {
	if (! (i % 8))
	    fprintf (sfile, "\n");
	fprintf (sfile, "%.1f ", Rad[r].anglist[i]);
    }
    fprintf (sfile, "\n");

    fclose (sfile);
    return 1;
}




static void
sw_KillKermit (r)
int	r;
/*
 * Kill the kermit associated with radar r
 */
{
/*
 * Make sure there's a kermit to kill
 */
	if (! Kermit[r])
		return;
/*
 * Kill kermit
 */
	signal (SIGCHLD, SIG_IGN);
	kill (Kermit[r], SIGKILL);
	signal (SIGCHLD, sw_KermitCheck);
/*
 * Print out the stuff currently waiting in the FromKermit pipe
 */
	while (fgets (Buf, BUFLEN, FromKermit[r]) != NULL)
		msg_ELog (EF_PROBLEM, "-- %s", Buf);
/*
 * Close the pipes
 */
	fclose (ToKermit[r]);
	fclose (FromKermit[r]);
/*
 * Set the pipe and pid pointers to 0
 */
	ToKermit[r] = FromKermit[r] = NULL;
	Kermit[r] = 0;
/*
 * Reset the button to dial mode
 */
	sw_DialMode (r);
}
