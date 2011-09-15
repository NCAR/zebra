/*
 * DM variables.
 *
 * $Id: dm_vars.h,v 2.15 2000-06-12 23:49:45 granger Exp $
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

#ifndef __zeb_dm_vars_h__
#define __zeb_dm_vars_h__

#include "dm_process.h"		/* Need Process for config structure */

/*
 * File extension for saved display configurations.  The period should
 * be included.  To define no extension, use ""
 */
#define SAVED_EXT	".dc"

# define MAXNAME	40
# define MAXWIN		20	/* Maximum number of windows in a config */

/*
 * Button map information.
 */
# define MAXBINDING 128		/* Biggest button map		*/
typedef struct dm_bmap
{
	char	db_name[MAXNAME];		/* The name of this map	*/
	int	db_nentry;			/* Number of entries	*/
	struct dm_evbind db_bindings[MAXBINDING]; /* The actual info 	*/
} ButtonMap;

extern stbl Bmaps;
extern ButtonMap *Default_map;	/* The default button map	*/


/*
 * The display manager understands three classes of windows:
 * graphic, non-graphic, and widget.
 */
typedef enum { W_Unknown, W_Graphic, W_NonGraphic, W_Widget } WinClass;

/*
 * A window in a configuration is realized by a running process from a
 * particular process class.  All windows, regardless of class, have a
 * name (different from a process's message handle), id, size, and
 * location. 
 *
 * Graphic windows also need a plot description, button map, and
 * related stuff, which are stored in the cf_graphic structure.  Once
 * a graphic window has been allocated a pd, it owns it.  PDs 
 * inherited when forcepd is false are transferred from the original
 * owner to the new one.  The source window will need to get its own
 * when it is next displayed.
 */
struct cf_graphic
{
	ButtonMap *g_bmap;		/* The button map for this win	*/
	char	g_desc[MAXNAME];	/* The plot description	name	*/
	plot_description g_pd;		/* The actual PD		*/
	struct cf_window *g_linksrc;	/* Source window for linked PD	*/
	short	g_linkpar;		/* PD link parameter		*/
	zbool	g_forcepd;		/* Always force new PD		*/
	zbool	g_tmpforce;		/* Force PD once		*/
};

/*
 * Note the process class is stored as the name of the class
 * rather than a pointer to the class.  This allows a class definition
 * to change before a window's process is executed (late binding).
 */
struct cf_window
{
	char	cfw_name[MAXNAME];	/* The name of this window	*/
	WinClass cfw_class;		/* graphic, nongraphic, widget  */
	char	cfw_pcname[PROC_NAME_LEN];
			 /* Process class which can realize this window */
	int	cfw_x, cfw_y;		/* The location of the window	*/
	int	cfw_dx, cfw_dy;		/* The size of the window	*/
	short	cfw_ncroak;		/* No. of processes its killed  */
	zbool	cfw_force_exec;		/* One-time force a new process */
	struct cf_graphic *cfw_graphic; /* Graphic info if graphic      */
	Process *cfw_process;		/* Process assigned to this window */
};


/*
 * Some access convenience routines
 */
inline static int IsGraphic (cfw) struct cf_window *cfw;
{ 
	return (cfw->cfw_class == W_Graphic); 
}
inline static int IsNonGraphic (cfw) struct cf_window *cfw;
{ 
	return (cfw->cfw_class == W_NonGraphic); 
}
inline static int IsWidget (cfw) struct cf_window *cfw;
{ 
	return (cfw->cfw_class == W_Widget); 
}
inline static const char *WinClassName (cfw) struct cf_window *cfw;
{
	return ((IsGraphic (cfw)) ? "graphic" :
		(IsNonGraphic (cfw)) ? "nongraphic" :
		(IsWidget (cfw)) ? "widget" : "unknown");
}



/*
 * This structure contains a display configuration, which consists mostly
 * of a configuration name and the window configurations themselves.  The
 * list of windows is an array of pointers to windows.
 */
struct config
{
	char c_name[MAXNAME];		/* The name of this configuration */
	int c_nwin;			/* Number of windows in this config */
	int c_nlink;			/* Number of link params	*/
	struct cf_window *c_wins[MAXWIN];/* The window list		*/
};


/*
 * The idea is this:

 The user requests a configuration, which was either created with ui
 commands or gets created from a file.  When created, the config is just a
 list of windows: their geometries, plot descriptions, process classes, and
 so on.  No processes have been assigned to any of the windows.  When the
 config is displayed (becomes the current config), each window in the
 config is assigned a process of the appropriate class.  The process either
 already existed or it had to be executed.  When executed, a process is
 passed a unique message handle on the command line along with the message
 handle of this display manager (which presumably was uniquely determined).
 Messages sent to window names must be sent to the process assigned to that
 window.  No matter how many unique window names in whatever many configs,
 only as many processes of a particular class are executed as are needed
 simultaneously in a single config.

 */

/*
 * While in the currently displayed configuration, a window name appears
 * in the Current symbol table.  The name of the current config is in
 * Cur_config.  These are defined in dm_config.c.
 */
extern stbl Current;
extern char Cur_config[MAXNAME];

/*
 * The group name which clients should join 
 */
extern char GroupName[CFG_MSGNAME_LEN];

/*
 * Our global test mode
 */
extern int TestMode;

/*
 * X window system-oriented stuff.  Only compile this in if the routine
 * is prepared to deal with it.
 */
extern Display		*Dm_Display;		/* Our display		*/

/*
 * Public functions.
 */
void	ReleaseMemory FP ((void));
int 	PickWin FP ((char *));
void	SetTimeMode FP ((struct cf_window *, int, ZebTime *));
void	badwin FP((char *name));
void	def_bmap FP((char *name));
void	def_config FP((struct ui_command *cmds));
void	Prototype FP((struct ui_command *));

/*
 * Command-line functions.
 */
int	is_active FP ((int, SValue *, int *, SValue *, int *));
int	get_pd FP ((int, SValue *, int *, SValue *, int *));
int	pd_param FP ((int, SValue *, int *, SValue *, int *));
int	pd_defined FP ((int, SValue *, int *, SValue *, int *));
int 	pd_complist FP ((int, SValue *, int *, SValue *, int *));
int 	NthComponent FP ((int, SValue *, int *, SValue *, int *));
int 	nvalue FP ((int, SValue *, int *, SValue *, int *));

/* 
 * Color table functions
 */
void	dc_TableRequest FP((struct dm_ctr *ctr, char *proc));
void	dc_Define FP((char *name));
void	dc_Init FP((void));
void	dc_FreeTables FP((void));

/*
 * Help functions.
 */
extern void dm_Init FP ((void));
extern void dm_Help FP ((char *));
extern void dm_ExitHelp FP ((void));

/*
 * Plot description utilities
 */
plot_description find_pd FP((char *name));
void send_pd FP ((struct cf_window *win));
void	WritePD FP((char *name, char *filename));
void	StorePD FP((char *name, char *copyname, char *filename));
void	CopyPD FP((char *name, char *copyname));
void	CopyComp FP((char *pdname, char *comp, char *copyname));
void	MergeComp FP((char *pdname, char *comp, char *dname, char *dcomp));
void	def_pd FP((struct ui_command *cmds));
void	pdwrite FP((plot_description pd, char *filename));
void	pdload FP((char *file, char *name));
void	pddir FP ((char *dir));

/*
 * Configuration functions
 */
void dg_Init FP ((void));
void dg_SaveConfig FP ((char *source, char *name, int update));
void dg_Display FP ((struct ui_command *cmds));
void dg_List FP ((char *name));
void dg_SendPD FP ((struct cf_window *win));
struct cf_window *dg_CurrentWindow FP ((char *name));
struct cf_window *dg_AnyWindow FP ((char *name));
void dg_SyncWindow FP ((struct config *cfg, struct cf_window *newwin, 
			int force));
void dg_ConfigWindow FP ((struct cf_window *win));
void dg_PutNewWindow FP ((char *pcname, struct ui_command *cmds));
void dg_PutConfigAs FP ((char *name, char *template));
struct config *dg_LookupConfig FP ((char *name));
struct config *dg_CurrentConfig FP ((void));
struct config *dg_NewConfig FP ((char *name));
void dg_TableAdd FP ((struct config *cfg));
struct cf_window *dg_NewWindow FP ((struct config *, WinClass, char *name));
void dg_FreeConfigs FP ((void));
void dg_DeleteWindow FP ((struct config *cfg, struct cf_window *win));
void dg_ReleasePD FP ((struct cf_window *win));
struct config *dg_FindOwner FP ((struct cf_window *win));
int dg_Query FP ((char *who));

/*
 * Message utilities
 */
void dmsg_SendWindow FP ((struct cf_window *win, void *dmsg, int len));
void dmsg_SendProcess FP ((char *who, void *dmsg, int len));
void dmsg_Broadcast FP ((char *group, void *dmsg, int len));
void dmsg_SendTimer FP ((void *msg, int len));
void dmsg_SendSound FP ((void *msg, int len));
void dmsg_Show FP ((char *who, void *msg, int len, char *context));
 
/*
 * Time management routines 
 */
void dt_Init FP ((void));
void dt_SetTime FP ((UItime *when));
void dt_Realtime FP ((struct ui_command *cmds));
void dt_SetWindowNames FP ((void));
void dt_History FP ((struct ui_command *cmds));
void dt_SendTime FP ((struct cf_window *win));



#endif /* !__zeb_dm_vars_h__ */
