/*
 * The graphics widget; a composite widget which has a user-specified number
 * of pixmap "frames" associated with it.  Zero frames means just write 
 * everything directly to the window.
 */
static char *rcsid = "$Id: GraphicsW.c,v 2.8 1992-12-18 05:29:57 granger Exp $";
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
# include <stdio.h>
# include <errno.h>
# include <X11/IntrinsicP.h>
# include <X11/StringDefs.h>
# include "config.h"		/* to establish dependency on SHM def */
# include "defs.h"
# include "message.h"
# include "pd.h"
# include "GraphProc.h"
# include "GraphicsWP.h"

# ifdef SHM
# include <sys/ipc.h>
# include <sys/shm.h>
# include <X11/extensions/XShm.h>
Pixmap gw_GetShmPixmap();
# endif

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
	{XtNresizeCallback, XtCResizeCallback, XtRCallback, 
		sizeof (XtCallbackList), 
		XtOffset (GraphicsWidget, graphics.resize_callback), 
		XtRPointer, NULL},
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
XtValueMask		*value_mask;
XSetWindowAttributes	*attributes;
{
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
	XtCreateWindow ((Widget) w, (unsigned int) InputOutput, vinfo.visual, 
		*value_mask, attributes);
# endif /* use_XB */

/*
 * Get the GC that travels with the widget
 */
	w->graphics.gc = XCreateGC (XtDisplay (w), XtWindow (w), 0, NULL);

/*
 * Allocate the pixmaps for the frames and clear them out
 */
	if (w->graphics.frame_count > 0)
	{
		w->graphics.frames = (Pixmap *) 
			XtMalloc (w->graphics.frame_count * sizeof (Pixmap));
# ifdef SHM
		if(GWShmPossible(w))
		{
			w->graphics.frameaddr = (char **) 
				XtMalloc (w->graphics.frame_count * 
				sizeof (char *));
			w->graphics.shminfo = (XShmSegmentInfo *) 
				XtMalloc (w->graphics.frame_count * 
				sizeof (XShmSegmentInfo));
			w->graphics.image = (XImage **) 
				XtMalloc (w->graphics.frame_count * 
				sizeof (XImage *));
		}
# endif
	}
	else w->graphics.frames = NULL;
	
	XSetForeground (XtDisplay (w), w->graphics.gc, 
		w->core.background_pixel);
	for (i = 0; i < w->graphics.frame_count; i++)
	{
# ifdef SHM
		if(GWShmPossible(w))
			w->graphics.frames[i] = gw_GetShmPixmap(w,
				w->core.width, w->core.height, 
				w->core.depth, i);
		else
# endif
			w->graphics.frames[i] = XCreatePixmap (XtDisplay (w), 
				XtWindow (w), w->core.width, w->core.height, 
				w->core.depth);
		
		XFillRectangle (XtDisplay (w), w->graphics.frames[i], 
			w->graphics.gc, 0, 0, w->core.width, w->core.height);
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
# ifdef SHM
		if(GWShmPossible(w))
			GWZapShmPixmap(w, i);
		else
# endif
			XFreePixmap (XtDisplay (w), w->graphics.frames[i]);

	XtFree ((char *) w->composite.children);
	if (w->graphics.frames)
		XtFree ((char *) w->graphics.frames);
# ifdef SHM
	if(GWShmPossible(w))
	{
		if (w->graphics.frameaddr)
			XtFree ((char *) w->graphics.frameaddr);
		if (w->graphics.shminfo)
			XtFree ((char *) w->graphics.shminfo);
		if (w->graphics.image)
			XtFree ((char *) w->graphics.image);
	}
# endif
}


void
Redraw (w, event, region)
GraphicsWidget	w;
XEvent		*event;
Region		region;
{
/*
 * If we don't have any frames, we have nothing from which to redraw
 */
	if (w->graphics.frame_count < 1)
		return;
/*
 *  If the index of the display_frame is no good then return.
 */
	if(w->graphics.display_frame < 0 || 
		w->graphics.display_frame >= w->graphics.frame_count)
	{
		msg_ELog (EF_PROBLEM, "Can't Redraw window.");
		msg_ELog (EF_DEBUG, "Invalid frame number (%d) in Redraw.",
			w->graphics.display_frame);
		return;
	}
# ifdef use_XB
/*
 * Make sure the current XB draw buffer is the same as the display buffer
 */
	if (w->graphics.ardent_server)
		XBSetDrawBuffer (XtDisplay (w), XtWindow (w), 
			w->graphics.display_buffer, 0);
# endif

/*
 * Do a CopyArea to copy the current frame into the window
 */
	XCopyArea (XtDisplay (w),w->graphics.frames[w->graphics.display_frame], 
		XtWindow (w), w->graphics.gc, 0, 0, w->core.width, 
		w->core.height, 0, 0);
}




void
Resize (w)
GraphicsWidget	w;
{
	int		i;
	int		have_frames = (w->graphics.frames != NULL);
	
/*
 * If we don't have frames, just return 
 * 6/91 (!) jc -- if we don't have a window, this stuff doesn't make a
 * 	 	  whole hell of a lot of sense.  How it worked until now
 *		  I will never know.
 */
	if (! have_frames || ! w->core.window)
		return;

/*
 * Free the old pixmaps, get new pixmaps and clear them out
 */
	XSetForeground (XtDisplay (w), w->graphics.gc, 
		w->core.background_pixel);

	for (i = 0; i < w->graphics.frame_count; i++)
	{
# ifdef SHM
		if(GWShmPossible(w))
		{
			GWZapShmPixmap(w, i);
			w->graphics.frames[i] = gw_GetShmPixmap(w,
				w->core.width, w->core.height, 
				w->core.depth, i);
		}
		else
# endif
		{
			XFreePixmap (XtDisplay (w), w->graphics.frames[i]);
			w->graphics.frames[i] = XCreatePixmap (XtDisplay (w), 
				XtWindow (w), w->core.width, w->core.height, 
				w->core.depth);
		}
		XFillRectangle (XtDisplay (w), w->graphics.frames[i], 
			w->graphics.gc, 0, 0, w->core.width, w->core.height);
	}

	XFillRectangle (XtDisplay (w), w->core.window, w->graphics.gc, 0, 0, 
		w->core.width, w->core.height);
/*
 * If there is a resize callback routine, then call it.
 */
	XtCallCallbackList ((Widget) w, w->graphics.resize_callback, NULL);
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
	int	newcount = request->graphics.frame_count;
	Pixel	oldbg = current->core.background_pixel;
	Pixel	newbg = request->core.background_pixel;
	
/*
 * Return now if the frame count and background color didn't change
 */
	if (oldcount == newcount && oldbg == newbg)
		return (False);

/*
 * Deal with background color change if necessary
 */
	if (oldbg != newbg)
		GWClearFrame (new, ClearAll);

/*
 * Deal with frame count change if necessary
 */
	if (oldcount != newcount)
	{
	/*
	 * Release excess pixmaps (if any) 
	 */
		for (i = newcount; i < oldcount; i++)
# ifdef SHM
			if(GWShmPossible(new))
				GWZapShmPixmap(new, i);
			else
# endif
				XFreePixmap (XtDisplay (new), 
					new->graphics.frames[i]);
	/*
	 * Reallocate the space for the pixmap array 
	 * and create pixmaps if necessary 
	 */
		new->graphics.frames = (Pixmap *)
			XtRealloc ((char *) new->graphics.frames, 
			newcount * sizeof (Pixmap));
# ifdef SHM
		if(GWShmPossible(new))
		{
			new->graphics.frameaddr = (char **) XtRealloc (
				(char *) new->graphics.frameaddr, 
				newcount * sizeof (char *));
			new->graphics.shminfo = (XShmSegmentInfo *) XtRealloc (
				(char *) new->graphics.shminfo, 
				newcount * sizeof (XShmSegmentInfo));
			new->graphics.image = (XImage **) XtRealloc (
				(char *) new->graphics.image, 
				newcount * sizeof (XImage *));
		}
# endif
		for (i = oldcount; i < newcount; i++)
		{
# ifdef SHM
			if(GWShmPossible(new))
				new->graphics.frames[i] = gw_GetShmPixmap(new,
					new->core.width, new->core.height, 
					new->core.depth, i);
			else
# endif
				new->graphics.frames[i] = 
					XCreatePixmap (XtDisplay (new), 
					XtWindow (new),
					new->core.width, new->core.height, 
					new->core.depth);
			
			XFillRectangle (XtDisplay (new), 
				new->graphics.frames[i], new->graphics.gc, 
				0, 0, new->core.width, new->core.height);
		}
	}
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
	if (w->graphics.frame_count > 0)
		return (w->graphics.frames[w->graphics.draw_frame]);
	else
		return ((Pixmap) w->core.window);
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
GWResize (w, width, height)
GraphicsWidget	w;
int	width, height;
{
	if (XtMakeResizeRequest ((Widget) w, (Dimension) width,
		(Dimension) height, 
		(Dimension *) NULL, (Dimension *) NULL) == XtGeometryYes)
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
	int		i;

	XSetForeground (XtDisplay (w), w->graphics.gc, 
		w->core.background_pixel);

/*
 * If we have no frames, clear the window
 */
	if (w->graphics.frame_count < 1)
		XFillRectangle (XtDisplay (w), w->core.window, w->graphics.gc, 
			0, 0, w->core.width, w->core.height);

/*
 * Clear all frames?
 */
	else if (frame == ClearAll)
	{
		for (i = 0; i < w->graphics.frame_count; i++)
			XFillRectangle (XtDisplay (w), w->graphics.frames[i], 
				w->graphics.gc, 0, 0, w->core.width, 
				w->core.height);
	}

/*
 * Just clear the one frame
 */
	else
		XFillRectangle (XtDisplay (w), w->graphics.frames[frame],
			w->graphics.gc, 0, 0, w->core.width, w->core.height);
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
	{
		msg_ELog (EF_DEBUG, "Invalid frame number %d", frame);
		XtError ("Invalid frame number in GWDrawInFrame");
	}
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
	unsigned int	use_buffer;

/*
 * Sanity check
 */
	if (frame < 0 || frame >= w->graphics.frame_count)
	{
		msg_ELog (EF_DEBUG, "Invalid frame number %d", frame);
		XtError ("Invalid frame number in GWDisplayFrame");
	}
/*
 * If we don't have any frames, just return now
 */
	if (w->graphics.frame_count < 1)
		return;

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
	XCopyArea (XtDisplay (w), w->graphics.frames[frame], XtWindow (w), 
		w->graphics.gc, 0, 0, w->core.width, w->core.height, 0, 0);

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


GC
GWGetGC(w)
GraphicsWidget w;
{
 	return(w->graphics.gc);
}


Pixmap
GWGetFrame(w, p)
GraphicsWidget w;
int p;
{
 	return(w->graphics.frames[p]);
}


/*
 *  Shared Memory Routines
 */

# ifdef SHM

int 
GWGetBPL(w, p)
GraphicsWidget w;
int p;
{
 	return(w->graphics.image[p]->bytes_per_line);
}

char *
GWGetFrameAddr(w, p)
GraphicsWidget w;
int p;
{
 	return(w->graphics.frameaddr[p]);
}


int
GWShmPossible(w)
GraphicsWidget w;
/*
 *  Return true if we can do shared memory.
 */
{
	static int known = FALSE, possible;
	int maj, min, sp;
	
	if(known)
		return(possible);
	known = TRUE;
	possible = XShmQueryVersion(XtDisplay(w), &maj, &min, &sp);
	possible = possible && sp;
	msg_ELog(EF_DEBUG, "Shared memory: %s", possible ? "True" : "False");
	return(possible);
}


Pixmap  
gw_GetShmPixmap(w, width, height, depth, index)
GraphicsWidget w;
int width, height, depth, index;
/*
 *  Return a shared memory Pixap.
 */
{
	Display *disp = XtDisplay(w);
	Pixmap pixmap;
	int bpl;
	struct shmid_ds buf;
/*
 *  Create the shared memory image.
 */
	w->graphics.image[index] = XShmCreateImage(disp, 0, depth, ZPixmap, 0, 
		w->graphics.shminfo + index, width, height);
	bpl = w->graphics.image[index]->bytes_per_line;
	msg_ELog(EF_DEBUG, "SHM width %d bytes/line %d", width, bpl);

/*
 *  Create the shared memory segment
 */
	w->graphics.shminfo[index].shmid=shmget(IPC_PRIVATE, bpl * height, 
		IPC_CREAT|0777);
	if(w->graphics.shminfo[index].shmid < 0)
		msg_ELog(EF_EMERGENCY, "SHM get failure (%d)!", errno);
	w->graphics.shminfo[index].shmaddr = (char *) 
		shmat(w->graphics.shminfo[index].shmid, 0, 0);
	if(w->graphics.shminfo[index].shmaddr == ((char *) -1))
		msg_ELog(EF_EMERGENCY, "SHM attach failure (%d)!",errno);
	w->graphics.shminfo[index].readOnly = False;
	msg_ELog(EF_DEBUG,"shminfo shmid %d shmaddr 0x%X readOnly %d shmseg %d",
		w->graphics.shminfo[index].shmid,
		w->graphics.shminfo[index].shmaddr,
		w->graphics.shminfo[index].readOnly,
		w->graphics.shminfo[index].shmseg);
	
/*
 *  Hook everything together and create the shared memory pixmap.
 */
	w->graphics.frameaddr[index] =  w->graphics.shminfo[index].shmaddr;
	w->graphics.image[index]->data =  w->graphics.shminfo[index].shmaddr;
	XShmAttach(disp, w->graphics.shminfo + index);
	pixmap = XShmCreatePixmap(disp, XtWindow(w),
	       w->graphics.shminfo[index].shmaddr,w->graphics.shminfo+index,
		width, height, depth);
	XSync(disp, False);
	if(shmctl(w->graphics.shminfo[index].shmid, IPC_RMID, 0) < 0)
		msg_ELog(EF_PROBLEM, "SHM remove failure (%d)!", errno);
/*
 *  Return the pixmap id.
 */
	return(pixmap);		
}


void
GWZapShmPixmap(w, index)
GraphicsWidget w;
int index;
/*
 *  If there is a shared memory Pixmap get rid of it.
 */
{
	Display *disp = XtDisplay(w);
	
	XShmDetach(disp, w->graphics.shminfo + index);
	XFreePixmap(disp, w->graphics.frames[index]);
	XtFree((char *) w->graphics.image[index]);
	if(shmdt(w->graphics.shminfo[index].shmaddr) < 0)
		msg_ELog(EF_PROBLEM, "SHM detach failure (%d)!", errno);

}


# endif



