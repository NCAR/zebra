/* $Id: ui_window.h,v 1.2 1990-03-06 11:07:23 corbet Exp $ */
/*
 * Definitions for windowing code.
 */

/*
 * Widget types.
 */
# define WT_LIST	0
# define WT_CMENU	1
# define WT_FRAME	2
# define WT_STACK	3
# define WT_MENUBAR	4


/*
 * An entry in a mapping table, which is used to map selector variable
 * values onto highlighted entries.
 */
struct map_table
{
	int mt_type;		/* Domain value type		*/
	union usy_value mt_v;	/* The domain value		*/
	int mt_target;		/* The target value.		*/
};
# define MAXMAP	40	/* Maximum entries in a map table	*/

/*
 * Temporary structure used when parsing map tables.
 */
struct mtemp
{
	int *mtm_nmap;			/* Pointer to # of entries	*/
	struct map_table *mtm_t;	/* The actual table		*/
};


/*
 * Window-system related stuff.
 */
Widget Top;		/* Our (unrealized) top widget		*/
XtAppContext Appc;	/* The application context		*/
XFontStruct *Labelfont;	/* The font for labels.			*/
Cursor Zapcursor;	/* The cursor for zap buttons		*/

/*
 * This pixmap holds the little mark used in pulldown menus.
 */
Pixmap Mb_mark;
char Mb_mark_file[200];
# define DEFAULT_MARK_FILE "/usr/include/X11/bitmaps/star"



/*
 * We have two symbol tables.
 */
stbl Widget_table;	/* Where the actual widgets live	*/
stbl Widget_vars;	/* Variables used by commands and such  */



# define MAXTITLE 40
# define MAXCB	256
/*
 * The "generic" widget, which contains the fields that every widget 
 * structure must have.
 */
struct gen_widget
{
	int gw_type;		/* The type of this widget		*/
	struct gen_widget *gw_next;	/* The next widget in the chain */
	void (*gw_create)();	/* The routine to create this thing	*/
	void (*gw_popup) ();	/* Routine to be called on popups	*/
	void (*gw_destroy) ();	/* The destroy method			*/
	struct frame_widget *gw_frame;	/* The associated frame		*/
	Widget gw_w;		/* The actual X widget			*/
};

/*
 * The "frame" which is the outermost structure for all ui widgets.
 */
struct frame_widget
{
	int	fw_type;	/* = WT_FRAME				*/
	struct gen_widget *fw_next;	/* Next widget in the chain	*/
	void (*fw_create)();	/* The routine to create this thing	*/
	void (*fw_popup) ();	/* Popup routine			*/
	void (*fw_destroy) ();	/* The destroy method			*/
	struct frame_widget *fw_frame;	/* The associated frame		*/
	Widget fw_w;		/* The actual popup widget structure	*/
	/* -- end of gen_widget stuff */
	char fw_name[MAXTITLE];	/* The name of this widget		*/
	char fw_title[MAXTITLE];/* The title of this widget		*/
	int fw_flags;		/* Status flags				*/
	int fw_nchild;		/* Number of children widgets		*/
	int fw_x, fw_y;		/* Override position			*/
	int fw_width, fw_height;/* Override size			*/
	Widget fw_vp;		/* The internal vpaned widget		*/
	Widget fw_form;		/* The internal form widget		*/
	Widget fw_bottom;	/* Last widget on the stack.		*/
};

/*
 * Frame widget flags.
 */
# define WF_INTERNAL	0x0001	/* Internally-defined widget		*/
# define WF_CREATED	0x0002	/* The X widget has been created	*/
# define WF_POPPED	0x0004	/* This widget is actually displayed	*/
# define WF_INIT	0x0008	/* Was this widget created during init? */
# define WF_OVERRIDE	0x0010	/* This is an OVERRIDE widget		*/
# define WF_NODEC	0x0020	/* Do not decorate this widget		*/

/*
 * Internal functions.
 */
# ifdef __STDC__
int uw_in_map (struct mtemp *mt, struct ui_command *cmds);
char *uw_LoadString (int lun);
struct map_table *uw_LoadMap (int lun, int nmap);
char ** uw_nt_to_array (char *strings);
struct frame_widget *uw_make_frame (char *name, char *title);

# else

int uw_in_map ();
char *uw_LoadString ();
struct map_table *uw_LoadMap ();
char ** uw_nt_to_array ();
struct frame_widget *uw_make_frame ();
# endif
