/*
 * Hooks for application-defined widgets.
 */
# ifdef XSUPPORT

# include <X11/Intrinsic.h>
# include "ui.h"
# include "ui_window.h"


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
	struct frame_widget *aw_frame;	/* The associated frame		*/
	Widget aw_w;		/* The actual X widget			*/
	/* -- End of gen_widget stuff */
	Widget (*aw_acreate) ();/* The application create function	*/
	void aw_adestroy ();	/* Application destroy method		*/
	char *aw_tag;		/* Application tag			*/
};


static void uw_ApplCreate (), uw_ApplDestroy ();


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
/*
 * Define everything.
 */
	uw_add_child (frame, (struct gen_widget *) new);
	uw_wdef (frame);
}




static void
uw_ApplCreate (aw, parent)
struct appl_widget *aw;
Widget parent;
/*
 * Invoke the application create procedure.
 */
{
	aw->aw_w = (*aw->aw_acreate) (aw->aw_tag, parent);
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
