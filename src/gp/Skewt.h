/*
 * Useful stuff from skewt that we want access to elsewhere.
 */

/*
 * Line style
 */
typedef enum {L_solid, L_dashed, L_dotted} LineStyle;

/*
 * Color indices.
 */
# define C_BLACK	0
# define C_WHITE	1
# define C_BG1		2
# define C_BG2		3
# define C_BG3		4
# define C_BG4		5
# define C_DATA(i)	(6 + (i))

/*
 * Routines.
 */
void	sk_InitPlotLimits ();
void	sk_Polyline FP ((float *, float*, int, LineStyle, int, XColor)); 
void	sk_DrawText FP ((char *, double, double, double, XColor, double,
		int, int)); 
void	sk_Clip FP ((double, double, double, double)); 
