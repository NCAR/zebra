/*
 * DM variables.
 *
 * $Id: dm_vars.h,v 1.2 1990-08-30 16:35:11 corbet Exp $
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
struct cf_window
{
	char	cfw_name[MAXNAME];	/* The name of this window	*/
	Window	cfw_win;		/* It's X window		*/
	int	cfw_x, cfw_y;		/* The location of the window	*/
	int	cfw_dx, cfw_dy;		/* The size of the window	*/
	char	cfw_prog[MAXPROG];	/* The program it is running	*/
	char	cfw_desc[MAXNAME];	/* The plot description	name	*/
	plot_description cfw_pd;	/* The actual PD		*/
	ButtonMap *cfw_bmap;		/* The button map for this win	*/
	bool	cfw_nongraph;		/* This is not a graphic window */
	bool	cfw_forcepd;		/* Always force new PD		*/
};


# define MAXWIN		40
struct config
{
	char c_name[MAXNAME];		/* The name of this configuration */
	int c_nwin;			/* Number of windows in this config */
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
