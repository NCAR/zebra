/*
 * Radar scan optimizer main driver
 *
 * $Id: Optimizer.c,v 1.1 1991-06-16 17:02:25 burghart Exp $
 */
# include <X11/Intrinsic.h>
# include <unistd.h>
# include <ui.h>
# include <ui_date.h>
# include <ui_error.h>
# include <message.h>
# include "globals.h"
# include "radar.h"
# include "keywords.h"
# include "prototypes.h"

/*
 * Declare global variables here
 */
Radar	Rad[20];
int	Nradars = 0;
char	ConfigName[30];
float	Hres = 0.5, Vres = 0.5;
float	Hsep_min = 0.0, Vsep_min = 0.0;
float	Vol_bot = 0.0, Vol_top = 5.0;
float	Vol_time = TIME_ASAP;
bool	Msg = FALSE, Ds = FALSE;

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
	static void	opt_LoadConfig (void);
	static int	opt_XEvent (int);
	static void	opt_VolInfo (struct ui_command *);
	static int	opt_Message (struct message *);
	static void	opt_ReportError (char *);
	static void	opt_Print (char *);
# else
	static int	opt_Dispatch ();
	static void	opt_CmdInit ();
	static void	opt_DumpCmd ();
	static int	opt_XEvent ();
	static void	opt_LoadConfig ();
	static void	opt_VolInfo ();
	static int	opt_Message ();
	static void	opt_ReportError ();
	static void	opt_Print ();
# endif




main (argc, argv)
int	argc;
char	**argv;
{
	Widget		top;
/*
 * Lose the first command line argument (the name of the program) which
 * is there whether we want it or not
 */
	argv++;
	argc--;
/*
 * Use an error catch until we get to ui_get_command (which has its own)
 */
	ERRORCATCH
	/*
	 * Initialize UI and our command table
	 */
		ui_init ("../lib/Optimizer.lf", FALSE, TRUE);
		ui_setup ("Optimizer", &argc, argv, NULL);
		opt_CmdInit ();
	/*
	 * Hook into the message system and the data store if they're around
	 */
		Msg = msg_connect (opt_Message, "Optimizer");

		if (Msg)
		{
			msg_DeathHandler (opt_Finish);
		/*
		 * Deal with output so it goes to event logger
		 */
			ui_ErrorOutputRoutine (opt_ReportError);
			ui_OutputRoutine (opt_Print, opt_Print);
		/*
		 * Initialize the data store
		 */
			Ds = ds_Initialize ();
		}
	/*
	 * Get into window mode
	 */
		uw_ForceWindowMode (NULL, &top, &Appc);
	/*
	 * Make the bitmaps and the widgets
	 */
		bm_BuildBitmaps (top);
		mw_DefineMainWidget ();
	/*
	 * Make sure we have a configuration name
	 */
		if (argc)
			strcpy (ConfigName, argv[0]);
		else
			ui_string_prompt ("Enter configuration name", NULL, 
				ConfigName, NULL);
	/*
	 * Load the configuration
	 */
		opt_LoadConfig ();
	/*
	 * Get the initial volume boundary
	 */
		bnd_InitBoundary ();
	/*
	 * Pop up the main and radar widgets
	 */
		uw_popup ("Optimizer");
	/*
	 * Get and list the first set of scan options
	 */
		ScanOptions ();
	ON_ERROR
		opt_Finish ();
	ENDCATCH
/*
 * Get commands via UI if we don't have a widget, otherwise the interface is
 * interrupt driven and we just have to look for stuff from the message
 * handler 
 */
# ifdef notdef
	ui_get_command ("optimizer-initial", "->", opt_Dispatch, 0);
# endif
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




void
opt_LoadConfig ()
/*
 * Load the configuration file
 */
{
	int	status;
	char	fname[50], string[20];
	FILE	*cfile;
	Radar	r;
/*
 * Make sure we can find the config file
 */
	strcpy (fname, ConfigName);
	if (access (fname, R_OK) != 0)
	{
		sprintf (fname, "/fcc/optimizer/%s", ConfigName);
		if (access (fname, R_OK) != 0)
			ui_error ("Cannot open '%s' config file!", ConfigName);
	}
/*
 * Open the file and read everything out of it
 */
	cfile = fopen (fname, "r");

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
	 * Lat and lon
	 */
		status = fscanf (cfile, "%f %f", &(r.lat), &(r.lon));
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
	 * Min and max hits
	 */
		status = fscanf (cfile, "%d%d", &(r.min_hits), &(r.max_hits));
		if (status != 2)
			break;
	/*
	 * default PRF
	 */
		status = fscanf (cfile, "%d", &(r.prf));
		if (status != 1)
			break;
	/*
	 * Set up defaults
	 */
		r.enabled = TRUE;
		r.status = MatchBoth;
		r.scantype = PPI;
		r.fix_hits = FALSE;
		r.fix_step = FALSE;
		r.min_range = 5.0;
	/*
	 * We have everything.  Save the radar and increment the count.
	 */
		Rad[Nradars++] = r;
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




