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

static char *rcsid = "$Id: SendWidget.c,v 1.2 1991-09-26 16:49:38 gracio Exp $";

# include <stdio.h>
# include <signal.h>
# include <time.h>
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

/*
 * Private prototypes
 */
# ifdef __STDC__
	void sw_DialMode (int);
	void sw_SendMode (int);
	void sw_DialRadar (Widget, XtPointer, XtPointer);
	void sw_SendScan (Widget, XtPointer, XtPointer);
	void sw_Timeout (void);
	void sw_KermitCheck (void);
	void sw_MakeFile (int, char *, char *);
	void sw_KillKermit (int);
# else
	void sw_DialMode ();
	void sw_SendMode ();
	void sw_DialRadar ();
	void sw_SendScan ();
	void sw_Timeout ();
	void sw_KermitCheck ();
	void sw_MakeFile ();
	void sw_KillKermit ();
# endif

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
	Widget	form, w, wstatus = NULL;
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
 * Put together a dial/send widget and status widget for each radar with an
 * associated outgoing line
 */
	for (r = 0; r < Nradars; r++)
	{
		if (! Rad[r].line_out)
			continue;
	/*
	 * Dial/send widget
	 */
		sprintf (wname, "dialsend%d", r);

		n = 0;
		XtSetArg (args[n], XtNfromHoriz, wstatus); n++;
		XtSetArg (args[n], XtNfromVert, NULL); n++;
		XtSetArg (args[n], XtNwidth, 100); n++;
		XtSetArg (args[n], XtNresize, False); n++;
		w = WButton[r] = XtCreateManagedWidget (wname, 
			commandWidgetClass, form, args, n);
	/*
	 * Status widget
	 */
		sprintf (wname, "status%d", r);

		n = 0;
		XtSetArg (args[n], XtNfromHoriz, wstatus); n++;
		XtSetArg (args[n], XtNfromVert, w); n++;
		XtSetArg (args[n], XtNborderWidth, 0); n++;
		XtSetArg (args[n], XtNwidth, 100); n++;
		XtSetArg (args[n], XtNresize, False); n++;
		wstatus = WStatus[r] = XtCreateManagedWidget (wname, 
			labelWidgetClass, form, args, n);
	/*
	 * Start out in dial mode
	 */
		sw_DialMode (r);
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
	XtRemoveCallback (WButton[r], XtNcallback, sw_DialRadar, r);
	XtRemoveCallback (WButton[r], XtNcallback, sw_SendScan, r);
/*
 * Put in the dial callback
 */
	XtAddCallback (WButton[r], XtNcallback, sw_DialRadar, r);
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
	char	string[20];
	Arg	arg;
/*
 * Put "Send to <rad>" on the button widget and make it call the send routine
 */	
	sprintf (string, "Send to %s", Rad[r].name);

	XtSetArg (arg, XtNlabel, string);
	XtSetValues (WButton[r], &arg, 1);
/*
 * Remove both possible callbacks
 */
	XtRemoveCallback (WButton[r], XtNcallback, sw_DialRadar, r);
	XtRemoveCallback (WButton[r], XtNcallback, sw_SendScan, r);
/*
 * Put in the send callback
 */
	XtAddCallback (WButton[r], XtNcallback, sw_SendScan, r);
/*
 * Put "Connected" on the status widget
 */
	XtSetArg (arg, XtNlabel, "Connected");
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
	char	label[20], kermit_exec[200];
	char	speed[10];
	Arg	arg;
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

		sprintf (kermit_exec, "%s/Optimizer/nkermit/nkermit", FCCDIR);

		if (! strcmp (Rad[r].phone, "radio-modem"))
			status = execl (kermit_exec, "nkermit", "slb", 
				Rad[r].line_out, speed, NULL);
		else
			status = execl (kermit_exec, "nkermit", "slbn", 
				Rad[r].line_out, speed, Rad[r].phone, NULL);

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
 * Send the current scan to the selected radar
 */
{
	int	r = (int) val;
	int	itime;
	char	fn[20], fullname[40], label[20];
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
 * Increment the file number.  We count from 1 to 6, then start over
 */
	Last[r]++;
	if (Last[r] > 6)
		Last[r] = 1;
/*
 * Build the filename
 */
	sprintf (fn, "jon%1d", Last[r]);
	msg_ELog (EF_INFO, "Sending file %s to %s", fn, Rad[r].name);
/*
 * Create the file
 */
	sw_MakeFile (r, fn, fullname);
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
	remove (fullname);

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
	itime = time (0);
	strncat (label, ctime (&itime) + 11, 5);
	XtSetArg (arg, XtNlabel, label);
	XtSetValues (WStatus[r], &arg, 1);
}





void
sw_Timeout (void)
{
/* 
 * Sending a file has taken too long
 */
	longjmp (Jmpbuf, 1);
}




void
sw_KermitCheck (void)
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




static void
sw_MakeFile (r, fname, fullname)
int	r;
char	*fname, *fullname;
/*
 * Write scan parameter file 'fname' for radar r.  On exit, 'fullname'
 * contains the full pathname of the file created.
 */
{
	FILE	*sfile;
	int	i, state, nstates, first, sweeps;
	float	left, right, bottom, top;
/*
 * Open the file in /tmp
 */
	sprintf (fullname, "/tmp/%s", fname);
	if (! (sfile = fopen (fullname, "w")))
	{
		msg_ELog (EF_PROBLEM, "Cannot open %s", fullname);
		return;
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
 * Number of states (one state per 24 angles in the angle list)
 */
	nstates = (Rad[r].nsweeps + 23) / 24;
/*
 * Build the schedule portion
 */
	fprintf (sfile, "**********\n");
	fprintf (sfile, "%s\n", fname);	/* file name		*/
	fprintf (sfile, "21.00\n");	/* schedule = 21	*/

	for (i = 0; i < 5; i++)
	{
		if (i == 0)
			fprintf (sfile, "2000000.00\n");
		else if (i < nstates)
			fprintf (sfile, "0.00\n");
		else
			fprintf (sfile, "-1000000.00\n");
	}

	for (i = 0; i < 2; i++)
		fprintf (sfile, "-1000000.00\n");

	for (i = 0; i < 5; i++)
	{
		if (i < nstates)
			fprintf (sfile, "1000000.00\n");
		else
			fprintf (sfile, "-1000000.00\n");
	}

	for (i = 0; i < 5; i++)
	{
		if (i < nstates)
			fprintf (sfile, "1.00\n");
		else
			fprintf (sfile, "-1.00\n");
	}

	for (i = 0; i < 5; i++)
	{
		if (i < (nstates - 1))
			fprintf (sfile, "%.2f\n", (float) (i + 2));
		else if (i == (nstates - 1))
			fprintf (sfile, "0.00\n");
		else
			fprintf (sfile, "-2.00\n");
	}

	for (i = 0; i < 2; i++)
		fprintf (sfile, "-2.00\n");

	for (i = 0; i < 5; i++)
		fprintf (sfile, "0.00\n");

	for (i = 0; i < 6; i++)
	{
		if (i < nstates)
			fprintf (sfile, "%ss%d\n", fname, i + 1);
		else
			fprintf (sfile, ".none\n");
	}

	for (i = 0; i < 4; i++)
		fprintf (sfile, "\\\\\\\\\\\\\n");
/*
 * State and angle list file(s)
 */
	for (state = 0; state < nstates; state++)
	{
	/*
	 * State file
	 */
		fprintf (sfile, "**********\n");

		fprintf (sfile, "%ss%d\n", fname, state + 1); /* state name */

		if (Rad[r].scantype == PPI)		/* scan type */
			fprintf (sfile, "1.00\n");
		else if (Rad[r].scantype == RHI)
			fprintf (sfile, "2.00\n");
		else
			fprintf (sfile, "3.00\n");

		fprintf (sfile, "%.2f\n%.2f\n%.2f\n%.2f\n%.2f\n", left, right, 
			Rad[r].scanrate, bottom, top);

		fprintf (sfile, "0.00\n");	/* elevation step */
		fprintf (sfile, "%.2f\n", (float) Rad[r].ngates);
		fprintf (sfile, "%.2f\n", (float) Rad[r].hits);
		fprintf (sfile, "0.00\n");	/* R0 */
		fprintf (sfile, "%.2f\n", (float) Rad[r].gspacing);
		fprintf (sfile, "1.00\n");	/* recording on */
		fprintf (sfile, "0.00\n");	/* no half PRF */
		fprintf (sfile, "3.00\n");	/* data type */
		fprintf (sfile, "0.00\n");	/* no dual pol. processing */
		fprintf (sfile, "1024.00\n");	/* dual pol. switching */
		fprintf (sfile, "0.00\n");	/* azimuth step (not fixed) */
		fprintf (sfile, "%.2f\n", Rad[r].scanrate);
		fprintf (sfile, "0.00\n");	/* SUR start azimuth */
		fprintf (sfile, "0.00\n");	/* no full 360 on SUR */
		fprintf (sfile, "0.00\n");	/* COP baseline */
		fprintf (sfile, "%.2f\n", Rad[r].scantime);
		fprintf (sfile, "%.2f\n", (Rad[r].scantype == RHI) ? 
			Rad[r].res_vert : Rad[r].res_horiz);
		fprintf (sfile, "1.00\n");	/* Use angle list */
		fprintf (sfile, "%.2f\n", (float) Rad[r].prf);
		fprintf (sfile, "0.00\n");	/* filter */
		fprintf (sfile, "0.00\n");	/* use DC removal */

		for (i = 0; i < 3; i++)		/* three unused lines */
			fprintf (sfile, "0.00\n");

		fprintf (sfile, "%sa%d\n", fname, state + 1); /* angle list */
		for (i = 0; i < 9; i++)		/* 9 empty angle list names */
				fprintf (sfile, "\\\\\\\\\\\\\n");
	/*
	 * Angle list file
	 */
		first = 24 * state;
		sweeps = Rad[r].nsweeps - first;
		if (sweeps > 24)
			sweeps = 24;

		fprintf (sfile, "**********\n");
		fprintf (sfile, "%sa%d\n", fname, state + 1);

		fprintf (sfile, "11.00\n");		/* angle list */
		fprintf (sfile, "%.2f\n", (float)sweeps);/* length of list */
		fprintf (sfile, "%.2f\n",		/* az or elev? */
			(Rad[r].scantype == RHI) ? 2.0 : 1.0);

		for (i = 0; i < sweeps; i++)		/* angle list */
			fprintf (sfile, "%.2f\n", Rad[r].anglist[first+i]);

		for (i = sweeps; i < 24; i++)
			fprintf (sfile, "-1313.00\n");	/* unused angles */

		for (i = 0; i < 3; i++)
			fprintf (sfile, "0.00\n");	/* unused */

		for (i = 0; i < 10; i++)		/* unused */
			fprintf (sfile, "\\\\\\\\\\\\\n");
	}

	fprintf (sfile, "**********\n");

	fclose (sfile);
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
