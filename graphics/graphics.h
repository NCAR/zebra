/* 5/87 jc */
/* $Id: graphics.h,v 1.5 2002-07-11 23:14:34 burghart Exp $ */
/*
 * Include file for external graphics users.
 */

/*
 * This is the type of value (ws = workstation) returned by g_open ().
 */
typedef char *ws;
typedef char *overlay;
typedef char *image_file;

/*
 * Polyline types.	(These are essentially the GKS-standard line types).
 */
# define GPLT_SOLID	0	/* Solid, boring line __________________*/
# define GPLT_DASH	1	/* Basic dashed line _ _ _ _ _ _ _ _ _ _*/
# define GPLT_DOT	2	/* Dotted line . . . . . . . . . . . . .*/
# define GPLT_DASH_DOT	3	/* Both _ . _ . _ . _ . _ . _ . _ . _ . */
# define GPLT_NTYPES	4	/* The number of line types we have	*/

/*
 * Text positioning.
 */
# define GT_LEFT	0	/* Left justified	*/
# define GT_CENTER	1	/* Centered text	*/
# define GT_RIGHT	2	/* Right justified	*/
# define GT_TOP		2	/* Top justified	*/
# define GT_BOTTOM	0	/* Bottom justified	*/
# define GT_BASELINE	3	/* Baseline justified	*/

/*
 * Predefined fonts.
 */
# define GTF_STROKE	0	/* Nice stroke font	*/
# define GTF_MINSTROKE	1	/* Minimal stroke font	*/
# define GTF_MINPIXEL	2	/* Minimal pixel font	*/
# define GTF_DEV	3	/* Built in fonts	*/

/*
 * Character font types.
 */
# define GFT_PIXEL	0	/* Pixel-mapped font	*/
# define GFT_STROKE	1	/* vector stroke font	*/
# define GFT_DEV	2	/* Hardware font	*/

/*
 * Inquiry types.
 */
# define GIW_NCOLOR	0	/* Number of colors			*/
# define GIW_XRES	1	/* Device X resolution			*/
# define GIW_YRES	2	/* Device Y resolution			*/
# define GIW_PIXEL	3	/* Pixel operations are *possible*	*/
# define GIW_PREC	4	/* Pixel operations are *recommended*	*/
# define GIW_VECTOR	5	/* Device performs vector operations	*/
				/* (such ops are *always* possible)	*/
# define GIW_SEGMENT	6	/* Device performs segmented operations	*/
# define GIW_HARDCOPY	7	/* This is a hardcopy device		*/
# define GIW_NBUTTON	8	/* The number of buttons for picks	*/

/*
 * Overlay requests.
 */
# define GIO_WX0	0	/* World coordinates -- x0		*/
# define GIO_WY0	1	/* ...and so on ...			*/
# define GIO_WX1	2
# define GIO_WY1	3
# define GIO_XRES	4	/* Overlay X resolution			*/
# define GIO_YRES	5	/* Overlay Y resolution			*/

/*
 * Flags for G_open.
 */
# define GF_NOCLEAR	0x0001	/* Do not clear the screen.		*/

/*
 * Info for describing the organization of pixel-map data.
 */
# define GP_BYTE	0	/* Byte-length data		*/
# define GP_SHORT	1	/* Short-length data		*/

# define GPO_RASTOR	0	/* Data is organized by rastor lines */

/*
 * Pixel fill modes.
 */
# define GPM_OVERWRITE	0	/* Overwrite the entire area	*/
# define GPM_OVERLAY	1	/* Overlay the area 		*/

 
/*
 * The list of error codes.
 */
# define GE_OK		 0	/* Everything is just fine (normal return) */
# define GE_BAD_TYPE	 1	/* Bad device type		*/
# define GE_BAD_DEVICE	 2	/* Bad device name		*/
# define GE_BAD_ITEM	 3	/* Bad item code		*/
# define GE_NCOLOR	 4	/* Bad number of colors		*/
# define GE_BAD_COLOR	 5	/* Out of range color value	*/
# define GE_BAD_COORDS	 6	/* A bad set of world coord values */
# define GE_BAD_FILE	 7	/* Un-openable file		*/
# define GE_DEVICE_UNABLE 8	/* Device can't handle this	*/
# define GE_OFFSCREEN	 9	/* Point is offscreen		*/
# define GE_UNK_FILL_MODE 10	/* Unknown fill mode		*/
# define GE_NOT_IFILE	11	/* Not an image file		*/
# define GE_BAD_IMAGE	12	/* Bad image number		*/
/*
 * One can get at these messages this way:
 */
extern char *G_messages[];

/*
 * Function prototypes.
 */
int G_clear (overlay ov);
int G_clip_window (overlay ov, double x0, double y0, double x1, double y1);
int G_close (ws sta);
int G_get_colors (ws sta, int ncolor, int *base);
overlay G_new_overlay (ws stn, int priority);
int G_open (char *device, char *type, ws *workstation, int flags);
ws G_ov_to_ws (char *cov);
int G_pixel_fill (overlay ov, char *data, int x, int y, int xs, int ys,
	int size, int mode);
int G_polyline (overlay ov, int ltype, int color, int npt, float *x, float *y);
int G_put_target (overlay cov, double x, double y);
int G_redraw (ws sta);
int G_set_color_map (ws sta, int base, int ncolor, float *red, float *green,
	float *blue);
int G_set_coords (overlay ov, double x0, double y0, double x1, double y1);
int G_target (overlay ov, float *x, float *y);
int G_tex_font (char *font);
int G_text (overlay ov, int color, int font, double height, int hjust,
	int vjust, double x, double y, char *text);
int G_tx_box (overlay ov, int font, double height, int hjust, int vjust,
	double x, double y, char *text, float *x0, float *y0, float *x1,
	float *y1);
int G_untarget (overlay ov);
void G_update (ws sta);
int G_viewport (overlay ov, double x0, double y0, double x1, double y1);
int G_visible (overlay ov, int state);
int G_w_inquire (ws sta, int item, int *value);
int G_wc_to_px (overlay ov, double wx, double wy, int *px, int *py);
int G_wr_box (overlay ov, int font, double height, int hjust, int vjust, 
	double x, double y, double rot, char *text, float *x0, float *y0,
	float *x1, float *y1);
int G_write (overlay ov, int color, int font, double height, int hjust,
	int vjust, double x, double y, double rot, char *text);
int G_ws_clear (ws sta);
int G_zap_overlay (overlay ov);
int gt_font_type (int font);
