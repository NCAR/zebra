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
    /* private state */
	GC		gc;		/* Graphics context */
	Pixmap		*frames;	/* Pixmaps for the frames */
# ifdef  SHM
	char		**frameaddr;	/* Pointer to pixmap data if SHM */
	XShmSegmentInfo *shminfo;	/* Pointer to shared memory segments */
	XImage		**image;	/* Used to create shm pixmap  */
# endif
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
