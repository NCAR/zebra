/*
 * DM variables.
 *
 * $Id: dm_vars.h,v 2.0 1991-07-18 23:11:58 corbet Exp $
 */
# include <X11/Xlib.h>
# include "../include/defs.h"
# include "dm.h"
# include "../include/message.h"
# include "../include/pd.h"

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
 * The default window process.
 */
# define DEFPROG "graphproc"

/*
 * X window system-oriented stuff.  Only compile this in if the routine
 * is prepared to deal with it.
 */
# ifdef _XLIB_H_
extern Display		*Dm_Display;		/* Our display		*/
# endif



/*
 * Functions.
 */
# ifdef __STDC__
	struct cf_window *lookup_win (char *, int);
	void PickWin (char *);
# else
	struct cf_window *lookup_win ();
	void PickWin ();
# endif
