/*
 * DM variables.
 *
 * $Id: dm_vars.h,v 2.7 1993-11-15 21:07:00 corbet Exp $
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
# include <X11/Xlib.h>
# include <defs.h>
# include "dm.h"
# include <message.h>
# include <pd.h>

/*
 * Button map information.
 */
# define MAXBINDING 128		/* Biggest button map		*/
# define MAXNAME	40
typedef struct dm_bmap
{
	char	db_name[MAXNAME];		/* The name of this map	*/
	int	db_nentry;			/* Number of entries	*/
	struct dm_evbind db_bindings[MAXBINDING]; /* The actual info 	*/
} ButtonMap;

extern stbl Bmaps;
extern ButtonMap *Default_map;	/* The default button map	*/



/*
 * A configuration.
 */
# define MAXPROG	80
# define MAXARG		10
struct cf_window
{
	char	cfw_name[MAXNAME];	/* The name of this window	*/
	char	*cfw_args[MAXARG];	/* Arguments for exec		*/
	Window	cfw_win;		/* It's X window		*/
	int	cfw_x, cfw_y;		/* The location of the window	*/
	int	cfw_dx, cfw_dy;		/* The size of the window	*/
	char	cfw_prog[MAXPROG];	/* The program it is running	*/
	char	cfw_desc[MAXNAME];	/* The plot description	name	*/
	plot_description cfw_pd;	/* The actual PD		*/
	ButtonMap *cfw_bmap;		/* The button map for this win	*/
	struct cf_window *cfw_linksrc;	/* Source window for linked PD	*/
	short	cfw_linkpar;		/* PD link parameter		*/
	short	cfw_flags;		/* Various flags		*/
	short	cfw_ncroak;		/* How many times has it died?	*/
	bool	cfw_nongraph;		/* This is not a graphic window */
	bool	cfw_forcepd;		/* Always force new PD		*/
	bool	cfw_tmpforce;		/* Force PD once		*/
};

/*
 * The flag field values.
 */
# define CF_WIDGET	0x0001		/* This is a DM widget window	*/
# define CF_NONGRAPH	0x0002		/* Non-graphic window		*/
# define CF_FORCEPD	0x0004		/* Always force new PD 		*/

# define MAXWIN		40
struct config
{
	char c_name[MAXNAME];		/* The name of this configuration */
	int c_nwin;			/* Number of windows in this config */
	int c_nlink;			/* Number of link params	*/
	struct cf_window c_wins[MAXWIN];/* The actual windows		*/
};



/*
 * The symbol tables holding configurations and windows.
 */
extern stbl Configs;
extern stbl Windows;

/*
 * This table holds info on the windows which are currently mapped.
 */
extern stbl Current;
extern char Cur_config[MAXNAME];

/*
 * Configuration directories.
 */
extern char ConfigDir[200];
extern char ConfigPD[200];
extern char ConfigPath[512];
extern char CTablePath[512];	/* Color tables */

/*
 * The path for executable programs.
 */
# define ExecPathLen 512
extern char ExecPath[ExecPathLen];
/*
 * If we blast out too many new windows at once, things (i.e. listen queues)
 * get overwhelmed.  So we do an occasional sleep.
 */
extern int SleepFor, SleepAfter;

/*
 * The default window process.
 */
# define DEFPROG "gp"

/*
 * Stuff used for forcing history mode on things.
 */
extern int ForceHistory;	/* Do we force the issue? 	*/
extern int HistoryMode;		/* Are in global history mode	*/
extern ZebTime HistoryTime;	/* The time for said mode	*/

/*
 * X window system-oriented stuff.  Only compile this in if the routine
 * is prepared to deal with it.
 */
# ifdef _XLIB_H_
extern Display		*Dm_Display;		/* Our display		*/
# endif

/*
 * The amount of space to subtract from the height of graphics windows --
 * this is here to allow people to easily compensate for window manager
 * obnoxiousness without having to rewrite configurations.
 */
extern int TBSpace;


/*
 * Functions.
 */
struct cf_window *lookup_win FP ((char *, int));
void 	PickWin FP ((char *));
void 	SaveConfig FP ((char *));
struct config *LookupConfig FP ((char *));
int 	FindFile FP ((char *, char *, char *));
void	SetTimeMode FP ((char *, int, ZebTime *));
