/*
 * Contour.h -- Common definitions for Contour.c and FillContour.c
 */

/*
 * The array we're contouring, its x and y dimensions
 * and a macro to reference it by vertex
 */
static float	*Z;
static int	Nx, Ny;
static float	Badflag;
static int	Use_flag = FALSE;
# define ZVAL(i,j)	Z[(i)*Ny + (j)]

/*
 * Color stuff
 */
static int	Color_center = 1, Ncolor = 1;
static XColor	*Colors, Color_outrange;

/*
 * Clipping rectangle
 */
static XRectangle	Clip;

/*
 * Graphics context
 */
static GC	Gcontext;

/*
 * The widget and drawable we're using (the drawable should belong to the
 * widget, i.e., either its window or an associated pixmap)
 */
static Widget		W;
static Drawable		D;

/*
 * Arrays for pixel coordinates in x and y plus
 * x and y spacing.
 */
static float	*Xpos, *Ypos;
static float	Xinc, Yinc;

/*
 * General definitions
 */
# ifndef TRUE
#	define TRUE	1
#	define FALSE	0
# endif

# define ABS(x)	((x) < 0 ? -(x) : (x))	/* Cheap absolute value */

