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
# ifndef _GraphicsWP_h
# define _GraphicsWP_h

# include "GraphicsW.h"
# include <X11/CompositeP.h>

# ifdef use_Xd
#	include <X11/Xdirect.h>
# endif

# ifdef SHM 
# 	include <X11/extensions/XShm.h>
# endif

typedef struct {
	int empty;
} GraphicsClassPart;

typedef struct _GraphicsClassRec {
	CoreClassPart		core_class;
	CompositeClassPart	composite_class;
	GraphicsClassPart	graphics_class;
} GraphicsClassRec;

extern GraphicsClassRec graphicsClassRec;

typedef struct {
    /* resources */
	unsigned int	frame_count;	/* How many frames? */
	XtCallbackList	resize_callback;/* Routine to call on resize */
    /* private state */
	GC		gc;		/* Graphics context */
	Pixmap		*frames;	/* Pixmaps for the frames */
	Boolean		shm_possible;	/* True if shared memory possible */
	Boolean		*frame_shared;	/* Remember if pixmap shared or not */
	char		**frameaddr;	/* Pointer to pixmap data if SHM */
# ifdef  SHM
	XShmSegmentInfo *shminfo;	/* Pointer to shared memory segments */
# endif
	XImage		**image;	/* Used to create shm pixmap  */
	unsigned int	draw_frame;	/* Frame to draw */
	unsigned int	display_frame;	/* Frame to display */
# ifdef use_XB
	Boolean		ardent_server;	/* Are we using an Ardent server? */
	unsigned int	display_buffer;	/* Current Ardent XB display buffer */
	unsigned int	frame_in_buffer[2]; /* Which frame is in each buffer */
# endif
} GraphicsPart;

typedef struct _GraphicsRec {
	CorePart	core;
	CompositePart	composite;
	GraphicsPart	graphics;
} GraphicsRec;

# endif /* _GraphicsWP_h */
