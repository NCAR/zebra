# include <stdio.h>
# include <X11/IntrinsicP.h>
# include <X11/StringDefs.h>
# include "GraphicsWP.h"

# ifdef use_XB
#	include <X11/XB.h>
# endif

void	Realize (), Destroy (), Redraw (), Resize (), ChangeManaged ();
Boolean	SetValues ();
XtGeometryResult	GeometryManager ();

static XtResource resources[] =
{
	{XtNbackground, XtCBackground, XtRPixel, sizeof (Pixel),
		XtOffset (GraphicsWidget, core.background_pixel), 
		XtRString, "black"},
	{XtNframeCount, XtCFrameCount, XtRInt, sizeof (int),
		XtOffset (GraphicsWidget, graphics.frame_count), 
		XtRString, "1"},
};

GraphicsClassRec graphicsClassRec =
{
/* CorePart */
	{
	/* superclass		*/	(WidgetClass) &compositeClassRec,
	/* class_name		*/	"Graphics",
	/* widget_size		*/	sizeof(GraphicsRec),
	/* class_initialize	*/	NULL,
	/* class_part_initialize*/	NULL,
	/* class_inited		*/	FALSE,
	/* initialize		*/	NULL,
	/* initialize_hook	*/	NULL,
	/* realize		*/	Realize,
	/* actions		*/	NULL,
	/* num_actions		*/	0,
	/* resources		*/	resources,
	/* num_resources	*/	XtNumber (resources),
	/* xrm_class		*/	NULLQUARK,
	/* compress_motion	*/	FALSE,
	/* compress_exposure	*/	TRUE,
	/* compress_enterleave	*/	FALSE,
	/* visible_interest	*/	FALSE,
	/* destroy		*/	Destroy,
	/* resize		*/	Resize,
	/* expose		*/	Redraw, 
	/* set_values		*/	SetValues,
	/* set_values_hook	*/	NULL,	
	/* set_values_almost	*/	XtInheritSetValuesAlmost,  
	/* get_values_hook	*/	NULL,			
	/* accept_focus		*/	NULL,
	/* version		*/	XtVersion,
	/* callback_offsets	*/	NULL,
	/* tm_table		*/	NULL,
	/* query_geometry	*/	XtInheritQueryGeometry,
	/* display_accelerator	*/	XtInheritDisplayAccelerator,
	/* extension		*/	NULL
	},
/* Composite part */
	{
	/* geometry_handler     */	GeometryManager,
	/* change_managed       */	ChangeManaged,
	/* insert_child		*/	XtInheritInsertChild,
	/* delete_child		*/	XtInheritDeleteChild,
	/* extension		*/	NULL
	},
/* Graphics part */
	{
	/* empty		*/	0
	}
};

WidgetClass graphicsWidgetClass = (WidgetClass)&graphicsClassRec;



void
Realize (w, value_mask, attributes)
GraphicsWidget	w;
Mask		*value_mask;
XSetWindowAttributes	*attributes;
{
	GC		gc;
	XGCValues	gcvals;
	XVisualInfo	vinfo;
	int		i, status, depth = 8;
/*
 * Get an eight deep pseudocolor visual if possible, otherwise go 
 * with monochrome
 */
	status = XMatchVisualInfo (XtDisplay (w), 
		XScreenNumberOfScreen (w->core.screen), depth, PseudoColor, 
		&vinfo);

	if (status == 0)
	{
		depth = 1;
		status = XMatchVisualInfo (XtDisplay (w), 
			XScreenNumberOfScreen (w->core.screen), depth, 
			StaticGray, &vinfo);
	}

	if (status == 0)
		XtError ("No visual matches for realizing a GraphicsWidget");

	w->core.depth = depth;
/*
 * Make the window and get its attributes
 */
# ifdef use_XB
	if (! strncmp ("Ardent", ServerVendor (XtDisplay (w)), 6))
	{
		w->graphics.ardent_server = True;
		w->core.window = XBCreateWindow (XtDisplay (w), 
			(w->core.parent ? 
			w->core.parent->core.window : w->core.screen->root),
			w->core.x, w->core.y, w->core.width, w->core.height, 
			w->core.border_width, w->core.depth, InputOutput, 
			vinfo.visual, 2, True, *value_mask, attributes);
	}
	else
	{
		w->graphics.ardent_server = False;
		XtCreateWindow (w, (unsigned int) InputOutput,
			vinfo.visual, *value_mask, attributes);
	}
# else
	XtCreateWindow (w, (unsigned int) InputOutput, vinfo.visual, 
		*value_mask, attributes);
# endif /* use_XB */
/*
 * Allocate the pixmaps for the frames and clear them out
 */
	w->graphics.frames = (Pixmap *) 
		XtMalloc (w->graphics.frame_count * sizeof (Pixmap));

	gcvals.function = GXcopy;
	gcvals.foreground = w->core.background_pixel;
	gc = XtGetGC (w, GCFunction | GCForeground, &gcvals);

	for (i = 0; i < w->graphics.frame_count; i++)
	{
		w->graphics.frames[i] = XCreatePixmap (XtDisplay (w), 
			XtWindow (w), w->core.width, w->core.height, 
			w->core.depth);

		XFillRectangle (XtDisplay (w), w->graphics.frames[i], gc, 
			0, 0, w->core.width, w->core.height);
	}
/*
 * Initialize the draw and display frame numbers
 */
	w->graphics.draw_frame = 0;
	w->graphics.display_frame = 0;

# ifdef use_XB
/*
 * Initialize XB stuff if we're using it
 */
	if (w->graphics.ardent_server)
	{
		w->graphics.display_buffer = 0;
		w->graphics.frame_in_buffer[0] = 0;
		w->graphics.frame_in_buffer[1] = 0;
	}
# endif
}




void
Destroy (w)
GraphicsWidget	w;
{
	int	i;

	for (i = 0; i < w->graphics.frame_count; i++)
		XFreePixmap (XtDisplay (w), w->graphics.frames[i]);

	XtFree ((char *) w->composite.children);
	XtFree ((char *) w->graphics.frames);
}



void
Redraw (w, event, region)
GraphicsWidget	w;
XEvent		*event;
Region		region;
{
	GC		gc;
	XGCValues	gcvals;
	GraphicsPart	gp;

	gp = w->graphics;

# ifdef use_XB
/*
 * Make sure the current XB draw buffer is the same as the display buffer
 */
	if (w->graphics.ardent_server)
		XBSetDrawBuffer (XtDisplay (w), XtWindow (w), 
			gp.display_buffer, 0);
# endif
/*
 * Do a CopyArea to copy the current frame into the window
 */
	gc = XtGetGC (w, 0, &gcvals);

	XCopyArea (XtDisplay (w), gp.frames[gp.display_frame], 
		XtWindow (w), gc, 0, 0, w->core.width, w->core.height, 0, 0);
}




void
Resize (w)
GraphicsWidget	w;
{
	GC		gc;
	XGCValues	gcvals;
	int		i;
	int		have_frames = (w->graphics.frames != NULL);
/*
 * If we don't have frames yet, just return 
 */
	if (! have_frames)
		return;
/*
 * Free the old pixmaps, get new pixmaps and clear them out
 */
	gcvals.function = GXcopy;
	gcvals.foreground = w->core.background_pixel;
	gc = XtGetGC (w, GCFunction | GCForeground, &gcvals);

	for (i = 0; i < w->graphics.frame_count; i++)
	{
		XFreePixmap (XtDisplay (w), w->graphics.frames[i]);

		w->graphics.frames[i] = XCreatePixmap (XtDisplay (w), 
			XtWindow (w), w->core.width, w->core.height, 
			w->core.depth);

		XFillRectangle (XtDisplay (w), w->graphics.frames[i], gc, 
			0, 0, w->core.width, w->core.height);
	}

	XFillRectangle (XtDisplay (w), w->core.window, gc, 0, 0, 
		w->core.width, w->core.height);
/*
 * Redraw the frames, if possible
 */
	if (w->graphics.plot_routine)
		(w->graphics.plot_routine)(w->graphics.plot_data);
}




Boolean
SetValues (current, request, new)
GraphicsWidget	current, request, new;
/*
 * Deal with a call from XtSetValues ()
 */
{
	int	i;
	int	oldcount = current->graphics.frame_count;
	int	newcount = new->graphics.frame_count;
/*
 * Return now if the frame count didn't change
 */
	if (oldcount == newcount)
		return (False);
/*
 * Release excess pixmaps (if any)
 */
	for (i = newcount; i < oldcount; i++)
		XFreePixmap (XtDisplay (new), new->graphics.frames[i]);
/*
 * Reallocate the space for the pixmap array and create pixmaps
 * if necessary 
 */
	new->graphics.frames = (Pixmap *) XtRealloc (new->graphics.frames, 
		new->graphics.frame_count * sizeof (Pixmap));

	for (i = oldcount; i < newcount; i++)
		new->graphics.frames[i] = XCreatePixmap (XtDisplay (new),
			XtWindow (new), new->core.width, new->core.height,
			new->core.depth);
# ifdef notdef
/*
 * Clear out all of the pixmaps
 */
	GWClearFrame (new, ClearAll);
# endif

	return (False);
}




XtGeometryResult
GeometryManager (w, request, reply)
Widget	w;
XtWidgetGeometry	*request, *reply;
/*
 * The geometry manager.  We're easy -- we'll allow anything.  If they request
 * something that puts them off the window, that's their problem....
 */
{
/*
 * Stash the new values into the widget.
 */
	if (request->request_mode & CWWidth)
		w->core.width = request->width;
	if (request->request_mode & CWHeight)
		w->core.height = request->height;
	if (request->request_mode & CWX)
		w->core.x = request->x;
	if (request->request_mode & CWY)
		w->core.y = request->y;
	if (request->request_mode & CWBorderWidth)
		w->core.border_width = request->border_width;
/*
 * Return our approval.
 */
	return (XtGeometryYes);
}




void
ChangeManaged (w)
Widget	w;
{
	return;
}




/*
 * Convenience routines
 */

Pixmap
GWFrame (w)
GraphicsWidget	w;
/*
 * Return the pixmap for the current draw frame
 */
{
	return (w->graphics.frames[w->graphics.draw_frame]);
}




int
GWWidth (w)
GraphicsWidget	w;
{
	return (w->core.width);
}




int
GWHeight (w)
GraphicsWidget	w;
{
	return (w->core.height);
}




int
GWDepth (w)
GraphicsWidget	w;
{
	return ((int) w->core.depth);
}




void
GWPlotRoutine (w, routine, plot_data)
GraphicsWidget	w;
void	(*routine)();
caddr_t	plot_data;
/*
 * Set the plot routine and data for widget w
 */
{
	w->graphics.plot_routine = routine;
	w->graphics.plot_data = plot_data;
}




void
GWResize (w, width, height)
GraphicsWidget	w;
int	width, height;
{
	if (XtMakeResizeRequest (w, width, height, NULL, NULL) == 
		XtGeometryYes)
		Resize (w);
}




void
GWClearFrame (w, frame)
GraphicsWidget	w;
int		frame;
/*
 * Clear the selected frame of widget w
 * (frame == ClearAll clears all frames)
 */
{
	GC		gc;
	XGCValues	gcvals;
	int		i;

	gcvals.function = GXcopy;
	gcvals.foreground = w->core.background_pixel;
	gc = XtGetGC (w, GCFunction | GCForeground, &gcvals);

	if (frame == ClearAll)
	{
		for (i = 0; i < w->graphics.frame_count; i++)
			XFillRectangle (XtDisplay (w), w->graphics.frames[i], 
				gc, 0, 0, w->core.width, w->core.height);
	}
	else
	{
		XFillRectangle (XtDisplay (w), w->graphics.frames[frame],
			gc, 0, 0, w->core.width, w->core.height);
	}
}




void
GWDrawInFrame (w, frame)
GraphicsWidget	w;
unsigned int	frame;
/*
 * Set the current draw frame of 'w' to 'frame'
 */
{
	if (frame < 0 || frame >= w->graphics.frame_count)
		XtError ("Invalid frame number in GWDrawFrame");

	w->graphics.draw_frame = frame;
}



void
GWDisplayFrame (w, frame)
GraphicsWidget	w;
unsigned int	frame;
/*
 * Make 'frame' the currently displayed frame for widget 'w'
 */
{
	GC		gc;
	XGCValues	gcvals;
	unsigned int	use_buffer;
/*
 * Sanity check
 */
	if (frame < 0 || frame >= w->graphics.frame_count)
		XtError ("Invalid frame number in GWDisplayFrame");
/*
 * OK, this is a legal frame to display
 */
	w->graphics.display_frame = frame;

# ifdef use_XB
/*
 * Use the XB buffer not being displayed as the draw buffer
 */
	if (w->graphics.ardent_server)
	{
		use_buffer = w->graphics.display_buffer ^ 0x1;
		XBSetDrawBuffer (XtDisplay (w), XtWindow (w), use_buffer, 0);
	}
# endif

/*
 * Do a CopyArea of the frame into the window
 */
	gc = XtGetGC (w, 0, &gcvals);

	XCopyArea (XtDisplay (w), w->graphics.frames[frame], XtWindow (w), gc, 
		0, 0, w->core.width, w->core.height, 0, 0);

# ifdef use_XB
/*
 * Now display the XB buffer we just wrote into
 */
	if (w->graphics.ardent_server)
	{
		XBSetDisplayBuffer (XtDisplay (w), XtWindow (w), use_buffer);
		w->graphics.display_buffer = use_buffer;
		w->graphics.frame_in_buffer[use_buffer] = frame;
	}
# endif
}
