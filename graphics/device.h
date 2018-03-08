/* $Id: device.h,v 1.9 1993-11-03 00:35:36 cook Exp $ */
/* 5/87 jc */
/*
 * This file contains the definition of the device structure.
 */
# define MAXALT		12	/* max number of alt device names */

struct device
{
	char	*gd_type;		/* Device type string		*/
	int	gd_nalt;		/* # of alt names		*/
	char	*gd_alttype[MAXALT];	/* Alternate type names.	*/
	int	gd_flags;		/* Device flags -- see below	*/
	int	gd_ncolor;		/* Number of colors		*/
	int	gd_xres;		/* X resolution			*/
	int	gd_yres;		/* Y resolution			*/
	float	gd_aspect;		/* Pixel aspect ratio		*/
	int	gd_xb;			/* X block size (see pixel)	*/
	int	gd_yb;			/* Y block size			*/
	int	gd_nbutton;		/* Number of target buttons	*/
	int	gd_background;		/* Background color for pixmaps	*/
/*
 * Actual device functions
 */
	int	(*gd_open) ();		/* The open device routine	*/
	int	(*gd_close) ();		/* Close down the device.	*/
	int 	(*gd_flush) ();		/* Flush output			*/
	int	(*gd_flush_nr) ();	/* Flush with no screen renew	*/
	int	(*gd_set_color) ();	/* Set color lookup table	*/
	int	(*gd_polyline) ();	/* Draw a polyline		*/
	int	(*gd_pixel) ();		/* Pixel fill operation		*/
	int	(*gd_qtext) ();		/* Query text			*/
	int	(*gd_tsize) ();		/* Text size			*/
	int	(*gd_text) ();		/* Draw text			*/
	int	(*gd_set_hcw) ();	/* Set hardware clip window	*/
	int	(*gd_clear) ();		/* Clear entire screen		*/
	int	(*gd_pfill) ();		/* Polygon fill			*/
	int	(*gd_s_init) ();	/* Initialize hardware segment	*/
	int	(*gd_s_clear) ();	/* Clear hardware segment	*/
	int	(*gd_s_select) ();	/* Select hardware segment	*/
	int	(*gd_s_end) ();		/* Finish segment update	*/
	int	(*gd_s_attr) ();	/* Set segment attributes	*/
	int	(*gd_target) ();	/* Read target			*/
	int	(*gd_put_target) ();	/* Put target			*/
	int	(*gd_untarget) ();	/* Remove target		*/
	int	(*gd_casn) ();		/* Color assignment		*/
	int	(*gd_check) ();		/* Exposure checking (Xwin)	*/
	int	(*gd_viewport) ();	/* Viewport (zoom/origin)	*/
	int	(*gd_readscreen) ();	/* Read back screen data	*/
	int	(*gd_pick) ();		/* Pick a screen point		*/
	int	(*gd_coff) ();		/* Return color offset		*/
	int	(*gd_print) ();		/* Hardware print screen	*/
};



/*
 * Device flags.
 */
# define GDF_PIXEL	0x0001		/* Does it do pixel-map ops	*/
# define GDF_AVOID_PIX	0x0002		/* Should we avoid them when poss.?*/
# define GDF_SEGMENT	0x0004		/* This device has hardware segments */
# define GDF_HCW	0x0008		/* Device has hardware clip window */
# define GDF_VECTOR	0x0010		/* Device does line drawing	*/
# define GDF_TOP	0x0020		/* Origin is at top of screen	*/
# define GDF_DEV_COLOR	0x0040		/* Device does own color assignment */
# define GDF_ATARGET	0X0080		/* "Active" target		*/
# define GDF_VP		0x0100		/* Device has adjustable viewport */
# define GDF_HARDCOPY	0x0200		/* Hardcopy device		*/
# define GDF_MONO	0x0400		/* Monochrome device		*/
# define GDF_TEXT	0x0800		/* Hardware text supported	*/

struct device *gd_get_dev ();
