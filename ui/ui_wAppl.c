/*
 * Hooks for application-defined widgets.
 */
# ifdef XSUPPORT

# include <X11/Intrinsic.h>
# include "ui.h"
# include "ui_window.h"

static char *rcsid = "$Id: ui_wAppl.c,v 1.7 1998-02-26 21:18:56 burghart Exp $";

/*
 * The format of an application widget.
 */
struct appl_widget
{
	int aw_type;		/* The type of this widget		*/
	struct gen_widget *aw_next;	/* The next widget in the chain */
	void (*aw_create)();	/* The routine to create this thing	*/
	void (*aw_popup) ();	/* Routine to be called on popups	*/
	void (*aw_destroy) ();	/* The destroy method			*/
	GenWidget *(*aw_clone) (); /* (unused)				*/
	struct frame_widget *aw_frame;	/* The associated frame		*/
	Widget aw_w;		/* The actual X widget			*/
	/* -- End of gen_widget stuff */
	Widget (*aw_acreate) ();/* The application create function	*/
	void (*aw_adestroy) ();	/* Application destroy method		*/
	char *aw_tag;		/* Application tag			*/
};


static void uw_ApplCreate (), uw_ApplDestroy ();

void
uw_def_widget (name, title, create, destroy, tag)
char *name, *title;
Widget (*create) ();
void (*destroy) ();
char *tag;
/*
 * Define an application-specific widget.
 */
{
	struct appl_widget *new = NEW (struct appl_widget);
	struct frame_widget *frame;
/*
 * Get the frame to hold this thing. 
 */
	frame = uw_make_frame (name, title);
	frame->fw_flags &= ~WF_INTERNAL;
/*
 * Fill in the appl widget.
 */
	new->aw_type = WT_APPL;
	new->aw_next = 0;
	new->aw_create = uw_ApplCreate;
	new->aw_destroy = uw_ApplDestroy;
	new->aw_popup = 0;
	new->aw_acreate = create;
	new->aw_destroy = destroy;
	new->aw_tag = tag;
/*
 * Define everything.
 */
	uw_add_child (frame, (struct gen_widget *) new);
	uw_wdef (frame);
}



void
uw_NoHeader (name)
char *name;
/*
 * Set the WF_NOHEADER flag for this application widget.
 * Widget must have already been defined, and flag change
 * is not effective unless the widget has not yet been created.
 */
{
	FrameWidget *fw = (FrameWidget *) uw_g_widget (name);
/*
 * Sanity checks.
 */
	if (! fw)
		ui_error ("NoHeader call on nonexistent widget '%s'", name);
	if (fw->fw_type != WT_FRAME)
		ui_error ("NoHeader call on nonframe widget '%s'", name);
/*
 * Flag change
 */
	fw->fw_flags |= WF_NOHEADER;
}	



static void
uw_ApplCreate (aw, parent)
struct appl_widget *aw;
Widget parent;
/*
 * Invoke the application create procedure.
 */
{
	aw->aw_w = (*aw->aw_acreate) (aw->aw_tag, parent, Appc);
}




static void uw_ApplDestroy (aw, created)
struct appl_widget *aw;
bool created;
/*
 * Invoke the application destroy routine.
 */
{
	if (aw->aw_adestroy)
		(*aw->aw_adestroy) (aw->aw_w, aw->aw_tag, created);
}


# else

uw_def_widget (name, title, create, destroy, tag)
{ }


# endif
