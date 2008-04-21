/* $Id: ui_window.h,v 1.10 1995-05-20 06:48:03 granger Exp $ */
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
# define WT_APPL	5	/* Application-defined widget	*/
# define WT_INTPOPUP	6	/* Internal popup menu		*/
# define WT_MENUBUTTON	7	/* A popup menu button (+ menu)	*/
# define WT_FORM	8	/* Form fillin			*/

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
extern Widget Top;		/* Our (unrealized) top widget		*/
extern XtAppContext Appc;	/* The application context		*/
extern XFontStruct *Labelfont;	/* The font for labels.			*/
extern Cursor Zapcursor;	/* The cursor for zap buttons		*/

/*
 * This pixmap holds the little mark used in pulldown menus.
 */
extern Pixmap Mb_mark;
extern char Mb_mark_file[200];

/*
 * We have two symbol tables.
 */
extern stbl Widget_table;	/* Where the actual widgets live	*/
extern stbl Widget_vars;	/* Variables used by commands and such  */



# define MAXTITLE 40
# define MAXCB	256
/*
 * The "generic" widget, which contains the fields that every widget 
 * structure must have.
 */
typedef struct gen_widget
{
	int gw_type;		/* The type of this widget		*/
	struct gen_widget *gw_next;	/* The next widget in the chain */
	void (*gw_create)();	/* The routine to create this thing	*/
	void (*gw_popup) ();	/* Routine to be called on popups	*/
	void (*gw_destroy) ();	/* The destroy method			*/
	struct gen_widget *(*gw_clone) (); /* Clone method 		*/
	struct frame_widget *gw_frame;	/* The associated frame		*/
	Widget gw_w;		/* The actual X widget			*/
} GenWidget;

/*
 * The "frame" which is the outermost structure for all ui widgets.
 */
typedef struct frame_widget
{
	int	fw_type;	/* = WT_FRAME				*/
	struct gen_widget *fw_next;	/* Next widget in the chain	*/
	void (*fw_create)();	/* The routine to create this thing	*/
	void (*fw_popup) ();	/* Popup routine			*/
	void (*fw_destroy) ();	/* The destroy method			*/
	GenWidget *(*fw_clone) ();/* Clone method (prototypes)		*/
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
	struct frame_widget *fw_inst;	/* Instantiations		*/
	int fw_ninst;		/* Number of instantiations.		*/
} FrameWidget;

/*
 * Frame widget flags.
 */
# define WF_INTERNAL	0x0001	/* Internally-defined widget		*/
# define WF_CREATED	0x0002	/* The X widget has been created	*/
# define WF_POPPED	0x0004	/* This widget is actually displayed	*/
# define WF_INIT	0x0008	/* Was this widget created during init? */
# define WF_OVERRIDE	0x0010	/* This is an OVERRIDE widget		*/
# define WF_NOFRAME	0x0020	/* Do not add a frame to this widget	*/
# define WF_NOHEADER	0x0040	/* No title header or zap button	*/
# define WF_PROTOTYPE	0x0080	/* This is a prototype widget		*/
# define WF_INSTANCE	0x0100	/* Instantiation of a prototype		*/

/*
 * The default value for geometry is to let the window manager deal with it.
 */
# ifdef __STDC__
static const int NotSpecified = -1;
# else
# define NotSpecified -1
# endif
/*
 * Internal functions.
 */
int uw_in_map (struct mtemp *mt, struct ui_command *cmds);
char *uw_LoadString (int lun);
struct map_table *uw_LoadMap (int lun, int nmap);
char ** uw_nt_to_array (char *strings);
struct frame_widget *uw_make_frame (char *name, char *title);
void uw_DoFrameParam (FrameWidget *, struct ui_command *);
GenWidget *uw_g_widget (char *);
Pixmap uw_GetPixmap (char *);
XFontStruct *uw_GetFont (char *);
void uw_NoHeader (char *name);
void uw_sync ();
