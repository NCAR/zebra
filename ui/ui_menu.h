/* 12/87 jc */
/* $Id: ui_menu.h,v 1.2 1989-09-25 17:00:23 corbet Exp $ */
/*
 * Stuff associated with menus.
 */

# define MNAMELEN	40	/* Maximum menu name length	*/
# define MTITLEN	80	/* Maximum title length		*/
# define DISPLEN	200	/* Choice display length	*/
# define HELPLEN	52	/* Helpfile length		*/
# define MAXCHOICE	22	/* Maximum number of menu choices	*/

/*
 * This structure describes a menu.
 */
struct menu
{
	char	m_name[MNAMELEN];	/* The name of this menu	*/
	char	m_title[MTITLEN];	/* The displayed title 		*/
	struct mchoice *m_choices;	/* Array of choice structures	*/
	short	m_nchoice;		/* Number of choices		*/
	short	m_flags;		/* Menu flags			*/
	short	m_default;		/* Default menu choice		*/
};
/*
 * Flags.
 */
# define MF_INIT	0x0001		/* Read during initial startup */
# define MF_PERMDEF	0x0002		/* Default choice is permanent */
 

/*
 * Each individual choice in a menu has one of these structures.
 */
struct mchoice
{
	char mc_display[DISPLEN];	/* Displayed string, with params */
	char mc_helpfile[HELPLEN];	/* Helpfile			*/
	char mc_command[DISPLEN];	/* Command string		*/
	char mc_next[MNAMELEN];		/* Next menu to use		*/
	int mc_newdef;			/* New default on selection	*/
};
