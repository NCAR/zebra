/*
 * Deal with the titan dial box.
 */
static char *rcsid = "$Id: DialBox.c,v 1.2 1990-07-08 12:59:02 corbet Exp $";

# ifdef titan

# include <defs.h>
# include "dm_vars.h"
# include <stropts.h>
# include <fcntl.h>
# include <machine/gin.h>
# include <errno.h>


/*
 * Local data.
 */
static int Dlb_fd = 0;		/* The dialbox stream file descriptor	*/

/*
 * Functions.
 */
# ifdef __STDC__
static void dlb_Event (void);
static void dlb_SendDialEvent (int, int);
static void dlb_FindProc (struct dlb_action *);
# else
static void dlb_Event ();
static void dlb_SendDialEvent ();
static void dlb_FindProc ();
# endif

/*
 * The table holding our dialbox actions.
 */
# define NDIAL 8	/* Not too likely to change	*/
static struct dlb_action
{
	int	dlb_scale;	/* Scaling to apply to this dial	*/
	int	dlb_value;	/* Current dial value			*/
	void	(*dlb_func) ();	/* Function to deal with dial changes	*/
	char	dlb_proc[MAXNAME];	/* Target process		*/
	char	dlb_param[MAXNAME];	/* Param to modify		*/
	bool	dlb_bcast;	/* Do we broadcast this change?		*/
} Dial_table[NDIAL] =
{
	{ 0, 0,		0,	"",		"",		FALSE },
	{ 0, 0,		0,	"",		"",		FALSE },
	{ 0, 0,		0,	"",		"",		FALSE },
	{ 0, 0,		0,	"",		"",		FALSE },
	{ 0, 0,		0,	"",		"",		FALSE },
	{ 0, 0,		0,	"",		"",		FALSE },
	{ 0, 0,		0,	"",		"",		FALSE },
	{ 0, 0,		0,	"",		"",		FALSE }
};



/*
 * Access to GIN structures.
 */
# define DIAL(g)	((g).gin_data[0])
# define MOTION(g)	((g).gin_data[1])



dlb_Init ()
/*
 * Initialize the dial box.
 */
{
/*
 * Open and initialize the device.
 */
	if ((Dlb_fd = open ("/dev/dials", O_RDONLY)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Open error %d on /dev/dials", errno);
		return;
	}
        if (ioctl (Dlb_fd, I_POP, 0) < 0)
	{
		msg_ELog (EF_PROBLEM, "I_POP error %d on /dev/dials", errno);
		return;
	}
        if (ioctl (Dlb_fd, I_PUSH, "dlb") < 0)
	{
		msg_ELog (EF_PROBLEM, "I_PUSH error %d on /dev/dials", errno);
		return;
	}
/*
 * Now we gotta arrange to be called when things happen.
 */
	tty_watch (Dlb_fd, dlb_Event);
}




static void
dlb_Event ()
/*
 * Deal with a dialbox event.
 */
{
	struct gin g;
	struct dlb_action *dact;
/*
 * Get something from the dialbox.
 */
	if (read (Dlb_fd, &g, sizeof (g)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d on dialbox read", errno);
		tty_nowatch (Dlb_fd);
		close (Dlb_fd);
	}
/*
 * Dispatch it.
 */
	dact = Dial_table + DIAL (g);
	if (dact->dlb_func)
		(*dact->dlb_func) (DIAL (g), MOTION (g));
}





dlb_Define (cmds)
struct ui_command *cmds;
/*
 * Handle the DM DIAL command.
 */
{
	int dial = UINT (*cmds);
	struct dlb_action *dact;
/*
 * Sanity checking.
 */
	if (dial < 0 || dial > 7)
		ui_error ("Dial number %d out of range 0-7", dial);
/*
 * Reset some parameters.
 */
	dact = Dial_table + dial;
	dact->dlb_scale = 1;
	dact->dlb_value = 0;
/*
 * Fill in the dial entry.
 */
	cmds++;
	if (dact->dlb_bcast = (cmds->uc_ctype == UTT_KW))
		cmds++;
	strcpy (dact->dlb_proc, UPTR (*cmds)); cmds++;
	strcpy (dact->dlb_param, UPTR (*cmds)); cmds++;
	if (cmds->uc_ctype != UTT_END)
	{
		if (UINT (*cmds) < 1 || UINT (*cmds) > 100)
			ui_warning ("Preposterous scale %d ignored",
				UINT (*cmds));
		else
			dact->dlb_scale = UINT (*cmds);
	}
/*
 * Figure out which process is to handle this action.  Normally, it's
 * just dlb_SendDialEvent, but for local stuff we short some of that
 * out.
 */
	if (! strcmp (dact->dlb_proc, "Displaymgr") || 
	    ! strcmp (dact->dlb_proc, "displaymgr") ||
	    ! strcmp (dact->dlb_proc, "dm"))
		dlb_FindProc (dact);
	else
		dact->dlb_func = dlb_SendDialEvent;
}




static void
dlb_Print (dial, motion)
int dial, motion;
/*
 * Print out this dial motion.
 */
{
	msg_ELog (EF_INFO, "Dial %d moved %d", dial, motion);
}



static void
dlb_FindProc (dact)
struct dlb_action *dact;
/*
 * Find a local action for this dial.
 */
{
	if (! strcmp (dact->dlb_param, "histtime"))
		dact->dlb_func = tw_DialAdjust;
	else if (! strcmp (dact->dlb_param, "print"))
		dact->dlb_func = dlb_Print;
	else
	{
		dact->dlb_func = 0;
		ui_error ("Unknown DM parameter: %s", dact->dlb_param);
	}
}





static void 
dlb_SendDialEvent (dial, motion)
int dial, motion;
/*
 * Dispatch this dial event to the target process.
 */
{
	struct dlb_action *dact = Dial_table + dial;
/*
 * Figure the new dial value.
 */
	if ((motion * dact->dlb_value) < 0) /* Dir change */
		dact->dlb_value = motion;
	else
		dact->dlb_value += motion;
/*
 * See if it's time to send an event.
 */
	if (ABS (dact->dlb_value) >= dact->dlb_scale)
	{
		struct dm_dial dmd;
		dmd.dmm_type = DM_DIAL;
		dmd.dmm_motion = dact->dlb_value/dact->dlb_scale;
		strcpy (dmd.dmm_param, dact->dlb_param);
		msg_send (dact->dlb_proc, MT_DISPLAYMGR, dact->dlb_bcast,
			&dmd, sizeof (dmd));
		dact->dlb_value -= dmd.dmm_motion*dact->dlb_scale;
	}
}

# endif
