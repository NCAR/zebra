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
# ifndef _GraphicsW_h
# define _GraphicsW_h

/*
 * Graphics widget (Composite widget subclass)
 */

/*
 * Resources:
 * 
 * Name		     Class		RepType		Default Value
 * ----		     -----		-------		-------------
 * background	     Background		Pixel		XtDefaultBackground
 * border	     BorderColor	Pixel		XtDefaultForeground
 * borderWidth	     BorderWidth	Dimension	1
 * destroyCallback   Callback		Pointer		NULL
 * frameCount	     FrameCount		Int		1
 * height	     Height		Dimension	0
 * mappedWhenManaged MappedWhenManaged	Boolean		True
 * sensitive	     Sensitive		Boolean		True
 * width	     Width		Dimension	0
 * x		     Position		Position	0
 * y		     Position		Position	0
 * resizeCallback    Callback           CallbackList    NULL
 */

# define XtNframeCount	"frameCount"
# define XtCFrameCount	"FrameCount"
# define XtNresizeCallback "resizeCallback"
# define XtCResizeCallback "ResizeCallback"

/*
 * Convenience routines
 */
extern Pixmap	GWFrame (/* GraphicsWidget */);
extern int	GWWidth (/* GraphicsWidget */);
extern int	GWHeight (/* GraphicsWidget */);
extern int	GWDepth (/* GraphicsWidget */);
extern int	GWBDepth (/* GraphicsWidget */);
extern void	GWPlotRoutine (/* GraphicsWidget, void (*)(), caddr_t */);
extern void	GWText (/* GraphicsWidget, String, int, int, float, 
			float, Pixel, int, int */);
extern void	GWClearFrame (/* GraphicsWidget, int */);
extern void	GWDrawInFrame (/* GraphicsWidget, int */);
extern void	GWDisplayFrame (/* GraphicsWidget, int */);
extern GC	GWGetGC(/* GraphicsWidget */);
extern Pixmap	GWGetFrame(/* GraphicsWidget, int */);
extern char 	*GWGetFrameAddr(/* GraphicsWidget, int frame */);
extern int	GWFrameShared(/* GraphicsWidget, int frame */);
extern int	GWShmPossible(/* GraphicsWidget */);
extern int	GWGetBPL(/* GraphicsWidget, int frame */);
extern void	GWZapShmPixmap(/* GraphicsWidget, int frame */);

/*
 * class and instance data types
 */
typedef struct _GraphicsClassRec*	GraphicsWidgetClass;
typedef struct _GraphicsRec*	GraphicsWidget;

/*
 * class constant
 */
extern WidgetClass graphicsWidgetClass;

/*
 * Other definitions
 */
# define ClearAll	-1	/* Clear all frames of the widget */

# endif /* _GraphicsW_h */
