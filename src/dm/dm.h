/* $Id: dm.h,v 2.3 1992-02-07 21:03:48 corbet Exp $ */
/*
 * Display manager stuff.
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

# define DM_RECONFIG	1	/* Change screen configuration	*/
# define DM_SUSPEND	2	/* Disappear from screen	*/
# define DM_HELLO	3	/* New win announcing itself	*/
# define DM_DIE		4	/* Go away			*/
# define DM_EVENT	5	/* Window event report		*/
# define DM_EXCH	6	/* Unused			*/
# define DM_PDCHANGE	7	/* New plot description		*/
# define DM_DEFAULTS	8	/* New defaults table		*/
# define DM_HISTORY	9	/* History mode			*/
# define DM_REALTIME	10	/* Real time mode		*/
# define DM_EVBIND	11	/* Event binding change		*/
# define DM_PARCHANGE	12	/* Individual PD parameter change */
# define DM_WBOUNDS	13	/* Window bounds request/reply	*/

/*
 * Color table stuff.
 */
# define DM_R_CTABLE	11	/* Request a color table	*/
# define DM_R_REL_TABLE 12	/* Release a color table	*/
# define DM_NOTABLE	13	/* Failed color table request	*/
# define DM_TABLE	14	/* Color table information	*/

# define DM_DIAL	15	/* Dial event			*/

/*
 * The message structure sent out by the display manager.
 */
# define PDLEN 40
struct dm_msg
{
	int	dmm_type;		/* the type of this message	*/
	int	dmm_x, dmm_y;		/* New window location		*/
	int	dmm_dx, dmm_dy;		/* New window size		*/
};



/*
 * Hello message to be received from dm clients.
 */
struct dm_hello
{
	int dmm_type;
	Window dmm_win;
};

/*
 * PD change command.
 */
struct dm_pdchange
{
	int	dmm_type;		/* Message type = DM_PDCHANGE	*/
	int	dmm_pdlen;		/* RPD length			*/
	char	dmm_pdesc[1];		/* New pd			*/
};


/*
 * event message.
 */
# define MAXADATA 128	/* ??? */
struct dm_event
{
	int dmm_type;			/* = DM_EVENT		*/
	char dmm_data[MAXADATA];	/* The DM data		*/
};

/*
 * Requests for window bounds.
 */
struct dm_rq_wbounds
{
	int	dmm_type;		/* = DM_WBOUNDS		*/
	char	dmm_window[MAXADATA];	/* Window name		*/
};

struct dm_rp_wbounds
{
	int	dmm_type;		/* = DM_WBOUNDS		*/
	int	dmm_success;		/* Did it work?		*/
	char	dmm_pltype[40];		/* Plot type		*/
	float	dmm_x0, dmm_y0;		/* Origin		*/
	float	dmm_x1, dmm_y1;
	float	dmm_alt;		/* What the heck?	*/
};


/*
 * History event.
 */
struct dm_history
{
	int dmm_type;		/* == DM_HISTORY		*/
	ZebTime dmm_time;	/* The time of interest		*/
};


/*
 * Color table stuff.
 *
 * The format of a color table request.
 */
struct dm_ctr
{
	int dmm_type;		/* == DM_R_CTABLE or DM_R_RELTABLE */
	char dmm_table[PDLEN];	/* The name of the table of interest	*/
};

/*
 * A color table reply.
 */
# ifdef _XLIB_H_
struct dm_ctable
{
	int dmm_type;		/* == DM_TABLE or DM_NOTABLE		*/
	char dmm_table[PDLEN];	/* The name of the table again		*/
	int dmm_ncolor;		/* The length of the color table	*/
	XColor dmm_cols[1];	/* The array of color values		*/
};

# endif



/*
 * These are the action codes for the prefined event actions.
 */
typedef enum dm_ac
{
	AC_CommandText,		/* A command to be executed		*/
	AC_PopupMenu,		/* A menu to be popped up		*/
	AC_Report,		/* Simply report to DM			*/
	AC_Ignore		/* Do nothing at all			*/
} ActionCode;


/*
 * The structures describing an event binding
 */
# define CODENAMELEN	20	/* Length of an event code		*/
struct dm_evbind
{
	int	dmm_mods;	/* Modifiers				*/
	char	dmm_code[CODENAMELEN];	/* Key code or mouse button number */
	ActionCode	dmm_action;	/* Action code			*/
	char	dmm_adata[MAXADATA];	/* Data for action		*/
};


struct dm_ebchange
{
	int dmm_type;		/* == DM_EVBIND				*/
	int dmm_nbind;		/* Number of event bindings		*/
	struct dm_evbind dmm_bindings[1];	/* Actual bindings	*/
};


/*
 * For an individual parameter change.
 */
struct dm_parchange
{
	int dmm_type;		/* == DM_PARCHANGE			*/
	char dmm_comp[MAXADATA/2];/* The component containing the param	*/
	char dmm_param[MAXADATA/2]; /* The parameter to change		*/
	char dmm_value[MAXADATA]; /* The new value of that parameter	*/
};



/*
 * Dial events.
 */
struct dm_dial
{
	int dmm_type;		/* == DM_DIAL				*/
	int dmm_motion;		/* The amount of dial motion		*/
	char dmm_param[PDLEN];	/* The parameter to change		*/
};
