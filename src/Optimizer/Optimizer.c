/*
 * Radar scan optimizer main driver
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

static char *rcsid = "$Id: Optimizer.c,v 1.7 1995-03-09 16:48:57 burghart Exp $";

# include <copyright.h>
# include <ctype.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <unistd.h>
# include <ui.h>
# include <ui_date.h>
# include <ui_error.h>
# include <config.h>
# include "globals.h"
# include "radar.h"
# include "keywords.h"
# include "prototypes.h"

/*
 * Declare global variables here
 */
Radar	Rad[MAXRAD];
int	Nradars = 0;
float	Hres, Vres;
float	Hsep_min, Vsep_min;
float	Vol_bot, Vol_top;
float	Vol_time;
int	Opt;
int	MinOpt, MaxOpt;
float	Slowtime[7];

XtAppContext	Appc;


/*
 * Pointers to routines to handle each command (stored by keyword number)
 */
static void	(*Cmd_routine[MAXKW+1])();

/*
 * Private prototypes
 */
# ifdef __STDC__
	static int	opt_Dispatch (int, struct ui_command *);
	static void	opt_CmdInit (void);
	static void	opt_DumpCmd (struct ui_command *);
	void	opt_LoadConfig (char *);
	int	opt_XEvent (int);
	void	opt_VolInfo (struct ui_command *);
	int	opt_Message (struct message *);
	static void	opt_ReportError (char *);
	static void	opt_Print (char *);
	void	opt_Usage (void);
# else
	static int	opt_Dispatch ();
	static void	opt_CmdInit ();
	static void	opt_DumpCmd ();
	int	opt_XEvent ();
	void	opt_LoadConfig ();
	void	opt_VolInfo ();
	int	opt_Message ();
	static void	opt_ReportError ();
	static void	opt_Print ();
	void	opt_Usage ();
# endif




main (argc, argv)
int	argc;
char	**argv;
{
	Widget	top;
	char	*name;
	int	n;
	Arg	args[5];
	char	configname[30];
/*
 * Set up an error catch until we loop for input
 */
	ERRORCATCH
	/*
	 * Initialize UI and our command table
	 */
		usy_init ();
		uw_init ();
		ui_errinit ();
# ifdef notdef
		ui_init ("../lib/Optimizer.lf", FALSE, TRUE);
		opt_CmdInit ();
# endif
		ui_setup ("Optimizer", &argc, argv, NULL);
		uw_ForceWindowMode (NULL, &top, &Appc);
	/*
	 * Make sure we have a configuration name
	 */
		if (argc > 1)
			strcpy (configname, argv[1]);
		else
			opt_Usage ();
	/*
	 * Get the user specified name (or use "Optimizer")
	 */
		if (argc > 2)
		{
			name = argv[2];

			n = 0;
			XtSetArg (args[n], "title", name); n++;
			XtSetValues (top, args, n);
		}
		else
			name = "Optimizer";
	/*
	 * Hook into the message system and the data store
	 */
		if (! msg_connect (opt_Message, name))
		{
			printf ("Unable to connect to message handler!\n");
			exit (1);
		}

		msg_DeathHandler (opt_Finish);
	/*
	 * Deal with UI error output so it goes to event logger
	 */
		ui_ErrorOutputRoutine (opt_ReportError);
		ui_OutputRoutine (opt_Print, opt_Print);
	/*
	 * Initialize the data store
	 */
		if (! ds_Initialize ())
		{
			printf ("Unable to connect to data store!\n");
			exit (1);
		}
	/*
	 * Load the configuration
	 */
		opt_LoadConfig (configname);
	/*
	 * Make the bitmaps and the widgets
	 */
		bm_BuildBitmaps (top);
		mw_MainWidget (top);
	/*
	 * Get the initial volume boundary
	 */
		bnd_InitBoundary ();
	/*
	 * Realize the top widget
	 */
		XtRealizeWidget (top);
	/*
	 * Get and list the first set of scan options
	 */
		ScanOptions ();
	ON_ERROR
		opt_Finish ();
	ENDCATCH
/*
 * Go
 */
	msg_add_fd (XConnectionNumber (XtDisplay (top)), opt_XEvent);
	while (TRUE)
	{
		ERRORCATCH
			while (TRUE)
				msg_await ();
		ENDCATCH
	}
/*
 * We're done
 */
	opt_Finish ();
}




# ifdef notdef
void
opt_CmdInit ()
/*
 * Initialize the array of command handling routines
 */
{
	Cmd_routine[KW_DISPLAY]		= so_Display;
	Cmd_routine[KW_VOLUME]		= opt_VolInfo;
}




int
opt_Dispatch (dummy, cmds)
int	dummy;
struct ui_command	*cmds;
/*
 * The command dispatcher
 */
{
	int	kwnum = UKEY (cmds[0]);
/*
 * Execute the command
 */
	if (Cmd_routine[kwnum])
	{
	/*
	 * We have a routine for this command; execute it, passing remaining
	 * commands (if any) as a parameter
	 */
		(*Cmd_routine[kwnum]) (cmds + 1);
	/*
	 * Regenerate our current options and we're done
	 */
		ScanOptions ();
		return (TRUE);
	}
	else
	{
	/*
	 * No routine, so dump the command
	 */
		for (;; cmds++)
		{
			opt_DumpCmd (cmds);
			if (cmds->uc_ctype == UTT_END)
				return (TRUE);
		}
	}
}




void
opt_DumpCmd (cmd)
struct ui_command *cmd;
/*
 * Dump a command structure
 */
{
	char	date_string[40];

	switch (cmd->uc_ctype)
	{
	   case UTT_END:
	   	ui_printf ("End of token list\n");
		return;
	   case UTT_VALUE:
	     switch (cmd->uc_vptype)
	     {
		case SYMT_FLOAT:
			ui_printf ("Value (float): %.4f\n", UFLOAT (cmd[0]));
			return;
		case SYMT_INT:
			ui_printf ("Value (int): %d\n", UINT (cmd[0]));
			return;
		case SYMT_STRING:
		   	ui_printf ("Value (string): '%s'\n", UPTR (cmd[0]));
			return;
		case SYMT_DATE:
			ud_format_date (date_string, &(UDATE (cmd[0])), 
				UDF_FULL);
			ui_printf ("Value (date): %s\n", date_string);
			return;
		case SYMT_BOOL:
			ui_printf ("Value (boolean): %s\n", 
				UBOOL (cmd[0]) ? "TRUE" : "FALSE");
			return;
		case SYMT_SYMBOL:
			ui_printf ("Value (symbol table)\n");
			return;
		case SYMT_POINTER:
			ui_printf ("Value (general pointer)\n");
			return;
		case SYMT_UNDEFINED:
			ui_printf ("Value (undefined symbol)\n");
			return;
		default:
			return;
	     }
	   case UTT_OTHER:
	   	ui_printf ("Type UTT_OTHER...\n");
		return;
	   case UTT_KW:
	   	ui_printf ("Keyword number %d\n", UKEY (cmd[0]));
		return;
	   default:
	   	ui_printf ("Something REALLY weird -- type = %d\n", 
			cmd->uc_ctype);
	}
}
# endif




void
opt_LoadConfig (cfg)
char	*cfg;
/*
 * Load the configuration file
 */
{
	int	status, i, baud;
	char	fname[200], string[30], line[30], phone[30];
	FILE	*cfile;
	Radar	r;
/*
 * Make sure we can find the config file
 */
	strcpy (fname, cfg);
	if (access (fname, R_OK) != 0)
	{
		sprintf (fname, "/zeb/src/Optimizer/%s", cfg);
		if (access (fname, R_OK) != 0)
		{
			msg_ELog (EF_PROBLEM, "Cannot open '%s' config file!",
				cfg);
			exit (1);
		}
	}
/*
 * Open the file and read everything out of it
 */
	cfile = fopen (fname, "r");
/*
 * Starting resolutions, minimum beam separations, volume height bounds,
 * and volume scan time
 */
	status = fscanf (cfile, "%f%f%f%f%f%f%s", &Hres, &Vres, &Hsep_min, 
		&Vsep_min, &Vol_bot, &Vol_top, string);
	if (status != 7)
	{
		msg_ELog (EF_PROBLEM, "Bad global line in the config file!\n");
		exit (1);
	}

	for (i = 0; i < strlen (string); i++)
		string[i] = tolower (string[i]);

	if (! strcmp (string, "asap"))
		Vol_time = TIME_ASAP;
	else
		sscanf (string, "%f", &Vol_time);
/*
 * Get the radar information
 */
	while (TRUE)
	{
	/*
	 * Radar name
	 */
		status = (int) fgets (r.name, RNAMELEN, cfile);
		if (status == NULL)
			break;

		if (strlen (r.name) == 1)
			continue;

		r.name[strlen (r.name) - 1] = '\0';
	/*
	 * Outgoing line, phone number, and baud rate for sending scan info
	 */
		fgets (string, sizeof (string), cfile);
		sscanf (string, "%s%s%d", line, phone, &baud);

		if (line[0] != '-')
		{
			r.line_out = (char *) 
				malloc ((1 + strlen (line)) * sizeof (char));
			strcpy (r.line_out, line);

			r.phone = (char *) 
				malloc ((1 + strlen (phone)) * sizeof (char));
			strcpy (r.phone, phone);

			r.baud = baud;
		}
		else
		{
			r.line_out = NULL;
			r.phone = NULL;
			r.baud = 0;
		}
	/*
	 * Lat and lon
	 */
		status = fscanf (cfile, "%f%f", &(r.lat), &(r.lon));
		if (status != 2)
			break;
	/*
	 * Max horizontal scan rate and acceleration
	 */
		status = fscanf (cfile, "%f%f", &(r.max_h_scanrate), 
			&(r.h_accel));
		if (status != 2)
			break;
	/*
	 * Max vertical scan rate and acceleration
	 */
		status = fscanf (cfile, "%f%f", &(r.max_v_scanrate),
			&(r.v_accel));
		if (status != 2)
			break;
	/*
	 * Min, max, and default hits
	 */
		status = fscanf (cfile, "%d%d%s", &(r.min_hits), 
			&(r.max_hits), string);
		if (status != 3)
			break;

		if (! strncmp (string, "var", 3))
			r.fix_hits = FALSE;
		else
		{
			r.fix_hits = TRUE;
			status = sscanf (string, "%d", &r.hits);
			if (status != 1)
			{
				msg_ELog (EF_PROBLEM, 
					"Bad hits '%s', using variable");
				r.fix_hits = FALSE;
			}
		}
	/*
	 * default PRF
	 */
		status = fscanf (cfile, "%d", &(r.prf));
		if (status != 1)
			break;
	/*
	 * Enabled/disabled
	 */
		status = fscanf (cfile, "%s", string);
		if (status != 1)
			break;

		for (i = 0; i < strlen (string); i++)
			string[i] = tolower (string[i]);

		r.enabled = (bool) strcmp (string, "disabled");
	/*
	 * Default scan type
	 */
		status = fscanf (cfile, "%s", string);
		if (status != 1)
			break;

		for (i = 0; i < strlen (string); i++)
			string[i] = tolower (string[i]);

		if (! strcmp (string, "ppi"))
			r.scantype = PPI;
		else if (! strcmp (string, "rhi"))
			r.scantype = RHI;
		else if (! strcmp (string, "sur"))
			r.scantype = SUR;
		else
		{
			msg_ELog (EF_PROBLEM, 
				"Unknown scan type '%s' for %s, using PPI",
				r.name, string);
			r.scantype = PPI;
		}
	/*
	 * Default minimum range and elevation
	 */
		status = fscanf (cfile, "%f%f", &(r.min_range), &(r.min_elev));
		if (status != 2)
			break;
	/*
	 * Constant/variable step
	 */
		status = fscanf (cfile, "%s", string);
		if (status != 1)
			break;

		r.fix_step = (bool) strcmp (string, "variable");
	/*
	 * Number of gates and gate spacing
	 */
		status = fscanf (cfile, "%d%d", &(r.ngates), &(r.gspacing));
		if (status != 2)
			break;
	/*
	 * Default to MatchBoth status
	 */
		r.status = MatchBoth;
	/*
	 * Make sure we don't get too many radars
	 */
		if (Nradars >= MAXRAD)
		{
			msg_ELog (EF_PROBLEM, 
				"Too many radars!  %d is the maximum.",
				MAXRAD);
			exit (1);
		}
	/*
	 * We have everything.  Save the radar and increment the count.
	 */
		Rad[Nradars++] = r;
		msg_ELog (EF_INFO, "Add radar %s", r.name);
	}
}




int
opt_XEvent (fd)
int	fd;
/*
 * Deal with an X event
 */
{
        XEvent event;
/*
 * Loop until we run out of events
 */
        while (XtAppPending (Appc))
        {
                XtAppNextEvent (Appc, &event);
                XtDispatchEvent (&event);
        }
        return (0);
}




void
opt_Finish ()
/*
 * Clean up and exit
 */
{
	ui_finish ();
	exit (0);
}




void
opt_VolInfo (cmds)
struct ui_command	*cmds;
/*
 * Set some volume parameter
 */
{
	int	cmd = UKEY (cmds[0]);

	switch (cmd)
	{
	    case KW_TOP:
		Vol_top = UFLOAT (cmds[1]);
		break;
	    case KW_BOTTOM:
		Vol_bot = UFLOAT (cmds[1]);
		break;
	    case KW_TIME:
		if (cmds[1].uc_ctype == UTT_KW)
			Vol_time = TIME_ASAP;
		else
			Vol_time = UFLOAT (cmds[1]);
		break;
	    default:
		ui_error ("BUG!  Unable to deal with this VOLUME command");
	}
}




int
opt_Message (msg)
struct message	*msg;
/*
 * Handle incoming messages
 */
{
	struct mh_template	*tm = (struct mh_template *) msg->m_data;
/*
 * Branch on the message type
 */
	msg_ELog (EF_INFO, "Got a message!");

	switch (msg->m_proto)
	{
	/*
	 * Message handler
	 */
	    case MT_MESSAGE:
		if (tm->mh_type == MH_SHUTDOWN)
			opt_Finish ();
		msg_ELog (EF_PROBLEM, "Unknown MESSAGE proto type: %d",
			tm->mh_type);
		break;
	/*
	 * Data store
	 */
	    case MT_DATASTORE:
		ds_DSMessage (msg);
		break;
	/*
	 * Unknown
	 */
	    default:
		msg_ELog (EF_PROBLEM, "Can't handle messages from %d!",
			msg->m_proto);
	}
	return (0);
}




static void
opt_ReportError (line)
char	*line;
/*
 * Report errors generated in UI.
 */
{
	msg_ELog (EF_PROBLEM, "UI ERROR: %s", line);
}




static void
opt_Print (line)
char	*line;
/*
 * Handle ui_printf'd stuff.
 */
{
	char	*nl, *start, *strchr ();
	char	tbuf[400];
/*
 * Break at NL's.
 */
	start = tbuf;

	strcpy (tbuf, line);
	while (nl = strchr (start, '\n'))
	{
		*nl = '\0';
		if (strlen (start) > 0)
			msg_ELog (EF_INFO, "%s", start);
		start = nl + 1;
	}

	if ((start - line) < strlen (line) && strlen (start) > 0)
		msg_ELog (EF_INFO, "%s", start);
}




void
opt_Usage ()
{
	printf ("Usage: Optimizer config [opt-name] [X options]");
	exit (1);
}
