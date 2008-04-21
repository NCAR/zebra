/*
 * The graphics widget; a composite widget which has a user-specified number
 * of pixmap "frames" associated with it.  Zero frames means just write 
 * everything directly to the window.
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
/*
 * Some joker who put together the "contrib" X11 on HP required that we do
 * the following.  Illegal my ass.
 */
# ifdef hpux
# define XLIB_ILLEGAL_ACCESS	/* grumble */
# endif


# include <stdio.h>
# include <errno.h>
# include <X11/IntrinsicP.h>
# include <X11/StringDefs.h>

# include <config.h>
# include "defs.h"
# include "message.h"
# include "pd.h"
# include "GraphicsWP.h"

RCSID("$Id: GraphicsW.c,v 2.28 1999-03-11 17:00:29 burghart Exp $")

/*
 * The SHM definition just tells us that we can link with the shared
 * memory functions; it does not mean use of the X shared memory
 * extension will always be possible, it just means we can
 * attempt it.  We try to account for shared memory failures when possible
 * by resorting to the slower, non-shared approach.  Without SHM defined,
 * the shared memory routines either automatically disable shared memory
 * pixmaps, or do nothing.  An application can include code to use
 * shared memory pixmaps when available, but they will not be available
 * without SHM defined.
 */

# ifdef SHM
# include <sys/ipc.h>
# include <sys/shm.h>
# include <X11/extensions/XShm.h>
static Pixmap gw_GetShmPixmap();
static void gw_SetShmPossible();
static int gw_CatchXShmError (Display *d, XErrorEvent *ev);

static int HadShmError;	/* for XShm testing */
# endif

static void gw_GetVisualAndColormap (/* Graphics w, Visual **visual, 
				     int *depth, Colormap *colormap */);
static void gw_CreateFrame(/* Graphics w, int frame */);
static void gw_DestroyFrame(/* Graphics w, int frame */);



static void Initialize(), Realize(), Destroy(), Redraw(), Resize();
static void ChangeManaged();
static Boolean SetValues ();
static XtGeometryResult GeometryManager ();

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
	/* initialize		*/	Initialize,
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



static void
Initialize (request, w)
GraphicsWidget request;
GraphicsWidget w;
{
/*
 * Police the public members and initialize our private members
 */
	if ((int)request->graphics.frame_count < 0)
		w->graphics.frame_count = 0;
	w->graphics.gc = None;
	w->graphics.frames = NULL;
	w->graphics.shm_possible = False;
	w->graphics.frameaddr = (char **) NULL;
	w->graphics.frame_shared = (Boolean *) NULL;
# ifdef SHM
	w->graphics.shminfo = (XShmSegmentInfo *) NULL;
# endif
	w->graphics.image = (XImage **) NULL;
/*
 * Initialize the draw and display frame numbers
 */
	w->graphics.draw_frame = 0;
	w->graphics.display_frame = 0;
}




static void
Realize (w, value_mask, attributes)
GraphicsWidget	w;
XtValueMask		*value_mask;
XSetWindowAttributes	*attributes;
{
	int		i, depth;
	Colormap	colormap;
	Visual		*visual;
/*
 * Get an appropriate visual, with its depth and an associated colormap.
 */
	gw_GetVisualAndColormap (w, &visual, &depth, &colormap);
	w->core.depth = depth;
	attributes->colormap = colormap;
/*
 * Make the window and get its attributes
 */
	XtCreateWindow ((Widget) w, (unsigned int) InputOutput, visual, 
			*value_mask, attributes);
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
		gw_SetShmPossible (w);
		if(GWShmPossible(w))
		{
			w->graphics.frameaddr = (char **) 
				XtMalloc (w->graphics.frame_count * 
					  sizeof (char *));
			w->graphics.frame_shared = (Boolean *) 
				XtMalloc (w->graphics.frame_count * 
					  sizeof (Boolean));
			w->graphics.shminfo = (XShmSegmentInfo *) 
				XtMalloc (w->graphics.frame_count * 
					  sizeof (XShmSegmentInfo));
			w->graphics.image = (XImage **) 
				XtMalloc (w->graphics.frame_count * 
					  sizeof (XImage *));
		}
# endif
	}
	else 
		w->graphics.frames = NULL;
	
	XSetForeground (XtDisplay (w), w->graphics.gc, 
			w->core.background_pixel);
	for (i = 0; i < w->graphics.frame_count; i++)
	{
		gw_CreateFrame (w, i);
		XFillRectangle (XtDisplay (w), w->graphics.frames[i], 
				w->graphics.gc, 0, 0, w->core.width, 
				w->core.height);
	}
/*
 * Initialize the draw and display frame numbers
 */
	w->graphics.draw_frame = 0;
	w->graphics.display_frame = 0;
}




static void
gw_GetVisualAndColormap (w, visual, depth, colormap)
GraphicsWidget	w;
Visual		**visual;
int		*depth;
Colormap	*colormap;
{
/*
 * Try to use the default visual if possible (so we can also use the
 * default colormap).  We'll take it if it's either an 8-bit pseudocolor
 * visual or a 1-bit (monochrome) pseudocolor visual.
 */
	*visual = DefaultVisualOfScreen (w->core.screen);
	*depth = DefaultDepthOfScreen (w->core.screen);
	*colormap = DefaultColormapOfScreen (w->core.screen);
	msg_ELog (EF_DEBUG, "Got visual 0x%x with depth %d", *visual, *depth);

# ifdef OLD_NASTY_STUFF
	int		nmatches;
	XVisualInfo	template, *matches;
	if ((*visual)->class == PseudoColor && (*depth == 8 || *depth == 1))
	{
		*colormap = DefaultColormapOfScreen (w->core.screen);
		msg_ELog (EF_DEBUG, 
			  "Graphics widget using default (%d-bit) visual",
			  *depth);

		return;
	}
/*
 * OK, we don't like the default visual.  See if we can get an 8-bit 
 * pseudocolor visual.
 */
	template.screen = XScreenNumberOfScreen (w->core.screen);
        template.depth = 8;
	template.class = PseudoColor;

	matches = XGetVisualInfo (XtDisplay (w), VisualScreenMask | 
				  VisualDepthMask | VisualClassMask, &template,
				  &nmatches);
/*
 * If that failed, try for any 1-bit visual.
 */
	if (! matches)
	{
		template.depth = 1;
		matches = XGetVisualInfo (XtDisplay (w), VisualScreenMask | 
					  VisualDepthMask, &template, 
					  &nmatches);
		if (! matches)
			XtError ("No good visuals for a GraphicsWidget");
	}
/*
 * We have one or matches, so just take the first one.
 */
	*visual = matches[0].visual;
	*depth = matches[0].depth;
	XFree ((void *)matches);
/*
 * Now we need to get an appropriate colormap.  Since I know of no way to
 * get information about existing virtual colormaps, we can't find a good
 * one that already exists.  We just have to create our own...
 */
	*colormap = XCreateColormap (XtDisplay (w), 
				     w->core.parent->core.window, *visual,
				     AllocNone);
	msg_ELog (EF_DEBUG, "Graphics widget using non-default %d-bit visual", 
		  *depth);
	msg_ELog (EF_DEBUG, "and creating its own colormap. :-(");
# endif
}



static void
Destroy (w)
GraphicsWidget	w;
{
	int	i;

	for (i = 0; i < w->graphics.frame_count; i++)
		gw_DestroyFrame (w, i);

	if (w->composite.children)
		XtFree ((char *) w->composite.children);
	w->composite.children = NULL;
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
		if (w->graphics.frame_shared)
			XtFree ((char *) w->graphics.frame_shared);
	}
# endif
}




static void
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
	if((int)w->graphics.display_frame < 0 || 
		w->graphics.display_frame >= w->graphics.frame_count)
	{
		msg_ELog (EF_PROBLEM, "Can't Redraw window.");
		msg_ELog (EF_DEBUG, "Invalid frame number (%d) in Redraw.",
			w->graphics.display_frame);
		return;
	}
/*
 * Do a CopyArea to copy the current frame into the window
 */
	XCopyArea (XtDisplay (w),
		   w->graphics.frames[w->graphics.display_frame], 
		   XtWindow (w), w->graphics.gc, 0, 0, w->core.width, 
		   w->core.height, 0, 0);
}




static void
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
 * Free the old pixmaps, get new pixmaps using the new window sizes,
 * and clear them out
 */
	XSetForeground (XtDisplay (w), w->graphics.gc, 
		w->core.background_pixel);

	for (i = 0; i < w->graphics.frame_count; i++)
	{
		gw_DestroyFrame (w, i);
	/*
	 * This time use our new core width, height, and depth
	 */
		gw_CreateFrame (w, i);
	/*
	 * Clear the new frame
	 */
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




static Boolean
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
			gw_DestroyFrame (new, i);
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
			new->graphics.frame_shared = (Boolean *) XtRealloc (
				(char *) new->graphics.frame_shared,
				newcount * sizeof (Boolean));
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
			gw_CreateFrame (new, i);
			XFillRectangle (XtDisplay (new), 
				new->graphics.frames[i], new->graphics.gc, 
				0, 0, new->core.width, new->core.height);
		}
	}
	return (False);
}



static XtGeometryResult
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



static void
ChangeManaged (w)
Widget	w;
{
	return;
}


static void
gw_CreateFrame (w, i)
GraphicsWidget w;
int i;
{
# ifdef SHM
	w->graphics.frames[i] = None;
	if(GWShmPossible(w))
	{
		w->graphics.frame_shared[i] = False;
		w->graphics.frames[i] = gw_GetShmPixmap(w,
			w->core.width, w->core.height, 
			w->core.depth, i);
		if (w->graphics.frames[i] != None)
			w->graphics.frame_shared[i] = True;
	}
/*
 * If shared memory not available, go the conventional route
 */
	if (w->graphics.frames[i] == None)
# endif /* SHM */
		w->graphics.frames[i] = XCreatePixmap (XtDisplay (w), 
			XtWindow (w), w->core.width, w->core.height, 
			w->core.depth);
}



static void
gw_DestroyFrame (w, i)
GraphicsWidget w;
int i;
/*
 * Destroy a frame according to whether its shared or not
 */
{
	if (GWFrameShared (w, i))
		GWZapShmPixmap (w, i);
	else if (w->graphics.frames[i] != None)
		XFreePixmap (XtDisplay (w), w->graphics.frames[i]);
	w->graphics.frames[i] = None;
}



/* 
 * Private shared memory routines
 */
# ifdef SHM		/* not used if SHM not defined */
static void
gw_SetShmPossible(w)
GraphicsWidget w;
/*
 *  Set private member according to whether we think shared memory is possible
 */
{
#   define HOSTLEN 50
    int maj, min, sp, possible;
    XShmSegmentInfo seg;
    Display *dpy = XtDisplay(w);
    int (*oldhandler)(Display*, XErrorEvent*);
/*
 * Make sure the server has the extension and that it supports shared
 * pixmaps
 */
    possible = XShmQueryExtension (dpy) && 
	XShmQueryVersion (dpy, &maj, &min, &sp) && sp;

    msg_ELog (EF_DEBUG, "XShmExt: %s supported by display %s",
	      possible ? "IS" : "NOT", DisplayString (dpy));

    if (!possible)
    {
	w->graphics.shm_possible = False;
	return;
    }
/*
 * Create a temporary shared memory segment
 */
    if (((seg.shmid = shmget (IPC_PRIVATE, 128, IPC_CREAT | 0777)) < 0) ||
	((seg.shmaddr = (char *) shmat (seg.shmid, 0, 0)) == (char*) -1))
    {
	msg_ELog (EF_PROBLEM, "gw: shmget or shmatt error.  SHM disabled.");
	w->graphics.shm_possible = False;
	if (seg.shmid >= 0)
	    shmctl (seg.shmid, IPC_RMID, 0);
	return;
    }
/*
 * See if we can successfully attach X to the segment.  We set
 * up our own X error handler momentarily to catch a failure of
 * XShmAttach() and we force an XSync() to make sure the failure 
 * happens now, if it happens at all.
 */
    HadShmError = 0;

    oldhandler = XSetErrorHandler (gw_CatchXShmError);

    XShmAttach(dpy, &seg);
    XSync (dpy, 0);

    XSetErrorHandler (oldhandler);
/*
 * If no error from XShmAttach(), we can use X shared memory stuff.  
 * Otherwise we can't.
 */
    w->graphics.shm_possible = ! HadShmError;
    
    if (HadShmError)
	msg_ELog (EF_INFO, "gw: XShmAttach failed.  XShm disabled.");
    else
	XShmDetach(dpy, &seg);
/*
 * Get rid of our temporary shared memory segment
 */
    shmctl (seg.shmid, IPC_RMID, 0);
}


static int
gw_CatchXShmError (Display *d, XErrorEvent *ev)
/*
 * X error handler that we use briefly to watch for an error attaching
 * the server to shared memory.  Just flag an error if we get called...
 */
{
    msg_ELog (EF_DEBUG, "gw_CatchXShmError: %d/%d/%d", ev->error_code,
	      ev->request_code, ev->minor_code);
    HadShmError = 1;
    return (1);
}



static Pixmap  
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
	Boolean failure = False;
/*
 *  Create the shared memory image.
 */
	w->graphics.shminfo[index].shmseg = (ShmSeg) 0;
	w->graphics.image[index] = XShmCreateImage(disp, 0, depth, ZPixmap, 0, 
		w->graphics.shminfo + index, width, height);
	bpl = w->graphics.image[index]->bytes_per_line;
	msg_ELog(EF_DEBUG, "gw attempting SHM: width %d, bytes/line %d",
		 width, bpl);
/*
 *  Create the shared memory segment
 */
	w->graphics.shminfo[index].shmid = shmget(IPC_PRIVATE, bpl * height, 
						  IPC_CREAT|0777);
	if (w->graphics.shminfo[index].shmid < 0)
	{
		failure = True;
		msg_ELog(EF_PROBLEM, "gw frame %d: SHM get failure (%d)!", 
			 index, errno);
	}
	if (!failure)
		w->graphics.shminfo[index].shmaddr = (char *) 
			shmat(w->graphics.shminfo[index].shmid, 0, 0);
	if (!failure && (w->graphics.shminfo[index].shmaddr == ((char *) -1)))
	{
		failure = True;
		msg_ELog(EF_PROBLEM, "gw frame %d: SHM attach failure (%d)!", 
			 index, errno);
	}
	if (failure)
	{
		XtFree((char *) w->graphics.image[index]);
		return (None);
	}
	w->graphics.shminfo[index].readOnly = False;
	msg_ELog(EF_DEBUG,"%s shmid %d shmaddr 0x%X readOnly %d shmseg %0x%lx",
		 "XShmSegmentInfo:",
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
	return (pixmap);		
}
# endif /* SHM */




/*
 * ======================================================================
 * 			   Convenience Routines
 * ----------------------------------------------------------------------
 * The shared memory routines are always defined, but they have no effect
 * unless shared memory support was compiled into the graphics widget
 * by defining SHM.  Applications using a graphics widget without shared
 * memory support can just think that shared memory was not available.
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



int
GWBDepth (w)
GraphicsWidget w;
/*
 * Return the "byte depth" of the display.  There is an assumption here
 * that 24-bit color is done in 32-bit words.  It's probably a good
 * assumption.  In any case, the raster code can't cope if it's otherwise.
 */
{
	int bd = (int) w->core.depth/8;
	return (bd == 3 ? 4 : bd);
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
	if ((int)frame < 0 || frame >= w->graphics.frame_count)
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
/*
 * Sanity check
 */
	if ((int)frame < 0 || frame >= w->graphics.frame_count)
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
/*
 * Do a CopyArea of the frame into the window
 */
	XCopyArea (XtDisplay (w), w->graphics.frames[frame], XtWindow (w), 
		w->graphics.gc, 0, 0, w->core.width, w->core.height, 0, 0);
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
 * Shared Memory Convenience Routines
 */

int 
GWGetBPL(w, p)
GraphicsWidget w;
int p;
{
	if (w->graphics.image)
		return (w->graphics.image[p]->bytes_per_line);
	else
		return (0);
}



/*
 * What byte order does our memory frame need?
 */
int
GWGetByteOrder (w, p)
GraphicsWidget w;
int p;
{
	if (w->graphics.image)
		return (w->graphics.image[p]->byte_order);
	else
		return (0); /* hopeless at this point */
}
			     



char *
GWGetFrameAddr(w, p)
GraphicsWidget w;
int p;
/*
 * Returns NULL if shared memory access is not possible for this frame.
 */
{
	if (GWFrameShared (w, p))
		return (w->graphics.frameaddr[p]);
	else
		return (NULL);
}



int
GWShmPossible(w)
GraphicsWidget w;
/*
 * If the widget has not been realized, it is not possible to know
 * whether the shared memory extension is available.  In which case,
 * this function returns False.
 */
{
	return (w->graphics.shm_possible);
}



int
GWFrameShared(w, p)
GraphicsWidget w;
int p;
/*
 * Returns True iff this frame of the Graphics widget is shared and can
 * be accessed directly, via the address returned by GWGetFrameAddr().
 */
{
	if (w->graphics.frame_shared)
		return (w->graphics.frame_shared[p]);
	else
		return (False);
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
	
# ifdef SHM
	if (GWFrameShared (w, index))
	{
		XShmDetach(disp, w->graphics.shminfo + index);
		XFreePixmap(disp, w->graphics.frames[index]);
		XtFree((char *) w->graphics.image[index]);
		if(shmdt(w->graphics.shminfo[index].shmaddr) < 0)
			msg_ELog(EF_PROBLEM,"SHM detach failure (%d)!",errno);
		w->graphics.frame_shared[index] = False;
		w->graphics.frames[index] = None;
	}
# else
	/* do nothing */

# endif /* SHM */
}

