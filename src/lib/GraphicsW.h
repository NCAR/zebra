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
 */

# define XtNframeCount	"frameCount"
# define XtCFrameCount	"FrameCount"

/*
 * Convenience routines
 */
extern Pixmap	GWFrame (/* GraphicsWidget */);
extern int	GWWidth (/* GraphicsWidget */);
extern int	GWHeight (/* GraphicsWidget */);
extern int	GWDepth (/* GraphicsWidget */);
extern void	GWPlotRoutine (/* GraphicsWidget, void (*)(), caddr_t */);
extern void	GWText (/* GraphicsWidget, String, int, int, float, 
			float, Pixel, int, int */);
extern void	GWClearFrame (/* GraphicsWidget, int */);
extern void	GWDrawInFrame (/* GraphicsWidget, int */);
extern void	GWDisplayFrame (/* GraphicsWidget, int */);

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
