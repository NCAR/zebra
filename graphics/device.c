/* 5/87 jc */
/* $Id: device.c,v 1.4 1989-08-07 14:19:48 corbet Exp $ */
/*
 * Handle device specifics.
 */
# include <ctype.h>
# include "graphics.h"
# include "param.h"
# include "device.h"

/*
 * Extern definitions for the device table.
 */
# ifdef DEV_XWIN
extern int gx_open (), gx_clear (), gx_close (), gx_flush (), gx_color ();
extern int gx_pl (), gx_casn (), gx_pixel (), gx_target (), gx_check ();
# endif
# ifdef DEV_X11
extern int x11_open (), x11_clear (), x11_close (), x11_flush (), x11_noop ();
extern int x11_poly (), x11_pick (), x11_target (), x11_casn (), x11_color ();
extern int x11_pixel (), x11_put_target ();
# endif
# ifdef DEV_XTITAN
extern int xt_open (), xt_clear (), xt_close (), xt_flush (), xt_noop ();
extern int xt_poly (), xt_pick (), xt_target (), xt_color ();
extern int xt_pixel (), xt_put_target ();
# endif
# ifdef DEV_RAMTEK
extern int rm_open (), rm_clear (), rm_flush (), rm_poly (), rm_color_map ();
extern int rm_close (), rm_hcw (), rm_pixel (), rm_target (), rm_vp ();
extern int rm_put_target ();
# endif
# ifdef DEV_4107
extern int tek_open (), tek_clear (), tek_flush (), tek_cmap (), tek_poly ();
extern int tek_s_init (), tek_s_clear (), tek_s_select (), tek_s_end ();
extern int tek_s_attr (), tek_close (), tek_pixel (), tek_vp ();
extern int tek_target (), tek_put_target (), tek_flush_nr ();
# endif
# ifdef DEV_COMTAL
extern int zb_open (), zb_close (), zb_pixel (), zb_color (), zb_clear ();
extern int zb_flush (), zb_target (), zb_put_target ();
# endif

# ifdef DEV_LN03
extern int ln_open (), ln_close (), ln_clear (), ln_poly (), ln_cmap ();
extern int ln_flush ();
# endif

# ifdef DEV_SUNVIEW
extern int sv_open (), sv_close (), sv_flush (), sv_clear (), sv_color ();
extern int sv_poly (), sv_pixel (), sv_readscreen (), sv_target ();
extern int sv_pick (), sv_vp ();
# endif

# ifdef DEV_PIXRECT
extern int pix_open (), pix_close (), pix_flush (), pix_clear (), pix_color ();
extern int pix_poly (), pix_pixel ();
# endif

# ifdef DEV_NULL
extern int nl_open (), nl_ok ();
# endif

/*
 * The actual table.
 */
struct device D_tab[] =
{
# ifdef DEV_NULL
/*
 * A Null device.
 */
	{
		"null",
		3, { "nl", "bitbucket", "blackhole", "defensebudget" },
		GDF_VECTOR | GDF_PIXEL,
		150,
		512, 512,		/* Our resolution for now */
		1.0,			/* Square pixels (X assumption) */
		100, 100,
		0,			/* no buttons, for now		*/
		0,			/* Background color		*/
		nl_open,		/* The open routine		*/
		nl_ok,			/* close			*/
		nl_ok,			/* Flush			*/
		___,			/* (no) flush w/o screen renew	*/
		nl_ok,			/* Set color table		*/
		nl_ok,			/* Draw polyline		*/
		nl_ok,			/* Pixel fill			*/
		___,			/* Text				*/
		___,			/* Set clip window		*/
		nl_ok,			/* Clear screen			*/
		___,			/* Polygon fill			*/
		___,			/* (no) Segment init		*/
		___,			/* (no) Segment clear		*/
		___,			/* (no) Segment select		*/
		___,			/* (no) Segment end		*/
		___,			/* (no) Segment attributes	*/
		___,			/* Read target			*/
		___,			/* (no) Put target		*/
		___,			/* Color assignment		*/
		___,			/* Exposure checking		*/
		___,			/* (no) viewport adjustment	*/
		___,			/* (no) readscreen		*/
		___,			/* (no) pick			*/
	},
# endif
# ifdef DEV_XWIN
/*
 * X window system. (10.4)
 */
	{
		"x",
		1, { "xwindow" },
		GDF_VECTOR | GDF_DEV_COLOR | GDF_PIXEL | GDF_TOP,
		150,
		700, 700,		/* Our resolution for now */
		1.0,			/* Square pixels (X assumption) */
		100, 100,
		0,			/* no buttons, for now		*/
		0,			/* Background color		*/
		gx_open,		/* The open routine		*/
		gx_close,		/* close			*/
		gx_flush,		/* Flush			*/
		___,			/* (no) flush w/o screen renew	*/
		gx_color,		/* Set color table		*/
		gx_pl,			/* Draw polyline		*/
		gx_pixel,		/* Pixel fill			*/
		___,			/* Text				*/
		___,			/* Set clip window		*/
		gx_clear,		/* Clear screen			*/
		___,			/* Polygon fill			*/
		___,			/* (no) Segment init		*/
		___,			/* (no) Segment clear		*/
		___,			/* (no) Segment select		*/
		___,			/* (no) Segment end		*/
		___,			/* (no) Segment attributes	*/
		gx_target,		/* Read target			*/
		___,			/* (no) Put target		*/
		gx_casn,		/* Color assignment		*/
		gx_check,		/* Exposure checking		*/
		___,			/* (no) viewport adjustment	*/
		___,			/* (no) readscreen		*/
		___,			/* (no) pick			*/
	},
# endif

# ifdef DEV_PIXRECT
	{
		"pixrect",
		1, { "pr" },
		GDF_VECTOR | GDF_PIXEL | GDF_TOP,
		128,			/* 128 colors, for now	*/
		1152, 900,		/* Our resolution for now */
		1.0,			/* Square pixels (X assumption) */
		100, 100,
		0,			/* no buttons, for now		*/
		0,			/* Background color		*/
		pix_open,		/* The open routine		*/
		pix_close,		/* close			*/
		pix_flush,		/* Flush			*/
		___,			/* (no) flush w/o screen renew	*/
		pix_color,		/* Set color table		*/
		pix_poly,		/* Draw polyline		*/
		pix_pixel,		/* Pixel fill			*/
		___,			/* Text				*/
		___,			/* Set clip window		*/
		pix_clear,		/* Clear screen			*/
		___,			/* Polygon fill			*/
		___,			/* (no) Segment init		*/
		___,			/* (no) Segment clear		*/
		___,			/* (no) Segment select		*/
		___,			/* (no) Segment end		*/
		___,			/* (no) Segment attributes	*/
		___,		/* Read target			*/
		___,			/* (no) Put target		*/
		___,		/* Color assignment		*/
		___,		/* Exposure checking		*/
		___,		/* (no)	viewport adjustment	*/
		___,		/* readscreen			*/
		___,			/* (no) pick			*/
	},
# endif


/*
 * X11.3.  The open routine may well make changes to this structure,
 * depending on what we actually get.
 */
# ifdef DEV_X11
	{
		"X11",
		3, { "X11-700", "X700", "X500" },
		GDF_VECTOR | GDF_TOP | GDF_DEV_COLOR,
		256,			/* x11_open will modify...	*/
		500, 500,		/* Our resolution for now */
		1.0,			/* Square pixels (X assumption) */
		100, 100,
		3,			/* 3 buttons			*/
		1,			/* Background color		*/
		x11_open,		/* The open routine		*/
		x11_close,		/* close			*/
		x11_flush,		/* Flush			*/
		___,			/* (no) flush w/o screen renew	*/
		x11_color,		/* Set color table		*/
		x11_poly,		/* Draw polyline		*/
		x11_pixel,		/* Pixel fill			*/
		___,			/* Text				*/
		___,			/* Set clip window		*/
		x11_clear,		/* Clear screen			*/
		___,			/* Polygon fill			*/
		___,			/* (no) Segment init		*/
		___,			/* (no) Segment clear		*/
		___,			/* (no) Segment select		*/
		___,			/* (no) Segment end		*/
		___,			/* (no) Segment attributes	*/
		x11_target,		/* Read target			*/
		x11_put_target,		/* (no) Put target		*/
		x11_casn,		/* Color assignment		*/
		___,		/* Exposure checking		*/
		___,		/* (no)	viewport adjustment	*/
		___,		/* readscreen			*/
		x11_pick,			/* (no) pick		*/
	},
# endif

/*
 * X11.3, with Ardent titan extensions.  The open routine may well make
 * changes to this structure, depending on what we actually get.
 */
# ifdef DEV_XTITAN
	{
		"titan",
		3, { "titan-700", "titan700", "titan500" },
		GDF_VECTOR | GDF_TOP,
		256,			/* x11_open will modify...	*/
		500, 500,		/* Our resolution for now */
		1.0,			/* Square pixels (X assumption) */
		100, 100,
		3,			/* 3 buttons			*/
		0,			/* Background color		*/
		xt_open,		/* The open routine		*/
		xt_close,		/* close			*/
		xt_flush,		/* Flush			*/
		___,			/* (no) flush w/o screen renew	*/
		xt_color,		/* Set color table		*/
		xt_poly,		/* Draw polyline		*/
		xt_pixel,		/* Pixel fill			*/
		___,			/* Text				*/
		___,			/* Set clip window		*/
		xt_clear,		/* Clear screen			*/
		___,			/* Polygon fill			*/
		___,			/* (no) Segment init		*/
		___,			/* (no) Segment clear		*/
		___,			/* (no) Segment select		*/
		___,			/* (no) Segment end		*/
		___,			/* (no) Segment attributes	*/
		xt_target,		/* Read target			*/
		xt_put_target,		/* (no) Put target		*/
		___,			/* Color assignment		*/
		___,		/* Exposure checking		*/
		___,		/* (no)	viewport adjustment	*/
		___,		/* readscreen			*/
		xt_pick,			/* (no) pick		*/
	},
# endif

# ifdef DEV_SUNVIEW
/*
 * Sunview window system.
 */
	{
		"sun500",
		1, { "sunview500" },
		GDF_VECTOR | GDF_PIXEL | GDF_TOP | GDF_VP,
		128,			/* 128 colors, for now	*/
		500, 500,		/* Our resolution for now */
		1.0,			/* Square pixels (X assumption) */
		100, 100,
		3,			/* no buttons, for now		*/
		0,			/* Background color		*/
		sv_open,		/* The open routine		*/
		sv_close,		/* close			*/
		sv_flush,		/* Flush			*/
		___,			/* (no) flush w/o screen renew	*/
		sv_color,		/* Set color table		*/
		sv_poly,		/* Draw polyline		*/
		sv_pixel,		/* Pixel fill			*/
		___,			/* Text				*/
		___,			/* Set clip window		*/
		sv_clear,		/* Clear screen			*/
		___,			/* Polygon fill			*/
		___,			/* (no) Segment init		*/
		___,			/* (no) Segment clear		*/
		___,			/* (no) Segment select		*/
		___,			/* (no) Segment end		*/
		___,			/* (no) Segment attributes	*/
		sv_target,		/* Read target			*/
		___,			/* (no) Put target		*/
		___,		/* Color assignment		*/
		___,		/* Exposure checking		*/
		sv_vp,		/* (no)	viewport adjustment	*/
		sv_readscreen,		/* readscreen			*/
		sv_pick,		/* pick				*/
	},

	{
		"sun256",
		0, { 0 },
		GDF_VECTOR | GDF_PIXEL | GDF_TOP | GDF_VP,
		256,			/* 256 colors, for now	*/
		500, 500,		/* Our resolution for now */
		1.0,			/* Square pixels (X assumption) */
		100, 100,
		3,			/* 3 buttons			*/
		0,			/* Background color		*/
		sv_open,		/* The open routine		*/
		sv_close,		/* close			*/
		sv_flush,		/* Flush			*/
		___,			/* (no) flush w/o screen renew	*/
		sv_color,		/* Set color table		*/
		sv_poly,		/* Draw polyline		*/
		sv_pixel,		/* Pixel fill			*/
		___,			/* Text				*/
		___,			/* Set clip window		*/
		sv_clear,		/* Clear screen			*/
		___,			/* Polygon fill			*/
		___,			/* (no) Segment init		*/
		___,			/* (no) Segment clear		*/
		___,			/* (no) Segment select		*/
		___,			/* (no) Segment end		*/
		___,			/* (no) Segment attributes	*/
		sv_target,		/* Read target			*/
		___,			/* (no) Put target		*/
		___,		/* Color assignment		*/
		___,		/* Exposure checking		*/
		sv_vp,		/* (no)	viewport adjustment	*/
		sv_readscreen,		/* readscreen			*/
		sv_pick,		/* pick				*/
	},

	{
		"sun700",
		2, { "sunview", "sun" },
		GDF_VECTOR | GDF_PIXEL | GDF_TOP | GDF_VP,
		128,			/* 128 colors, for now	*/
		700, 700,		/* Our resolution for now */
		1.0,			/* Square pixels (X assumption) */
		100, 100,
		3,			/* no buttons, for now		*/
		0,			/* Background color		*/
		sv_open,		/* The open routine		*/
		sv_close,		/* close			*/
		sv_flush,		/* Flush			*/
		___,			/* (no) flush w/o screen renew	*/
		sv_color,		/* Set color table		*/
		sv_poly,		/* Draw polyline		*/
		sv_pixel,		/* Pixel fill			*/
		___,			/* Text				*/
		___,			/* Set clip window		*/
		sv_clear,		/* Clear screen			*/
		___,			/* Polygon fill			*/
		___,			/* (no) Segment init		*/
		___,			/* (no) Segment clear		*/
		___,			/* (no) Segment select		*/
		___,			/* (no) Segment end		*/
		___,			/* (no) Segment attributes	*/
		sv_target,		/* Read target			*/
		___,			/* (no) Put target		*/
		___,		/* Color assignment		*/
		___,		/* Exposure checking		*/
		sv_vp,		/* (no)	viewport adjustment	*/
		sv_readscreen,		/* readscreen			*/
		sv_pick,		/* pick				*/
	},

# endif /* DEV_SUNVIEW */


# ifdef DEV_RAMTEK
/*
 * Ramtek 9460.
 */
 	{
		"rm9460",		/* Device type rm9460		*/
		0, { ___ },
		GDF_PIXEL | GDF_HCW | GDF_VECTOR | GDF_VP,
		256,			/* 256 colors			*/
		1280, 1024,		/* Our screen resolution	*/
		1.0,			/* Assume square aspect for now */
		80, 64,			/* 160 X 128 pixel blocks	*/
		0,			/* no buttons, for now		*/
		0,			/* Background color		*/
		rm_open,		/* The open routine		*/
		rm_close,		/* close			*/
		rm_flush,		/* Flush			*/
		___,			/* (no) flush w/o screen renew	*/
		rm_color_map,		/* Set color table		*/
		rm_poly,		/* Draw polyline		*/
		rm_pixel,		/* Pixel fill			*/
		___,			/* Text				*/
		rm_hcw,			/* Set clip window		*/
		rm_clear,		/* Clear screen			*/
		___,			/* Polygon fill			*/
		___,			/* (no) Segment init		*/
		___,			/* (no) Segment clear		*/
		___,			/* (no) Segment select		*/
		___,			/* (no) Segment end		*/
		___,			/* (no) Segment attributes	*/
		rm_target,		/* Read target			*/
		rm_put_target,		/* Put target			*/
		___,			/* (no) color assignment	*/
		___,			/* (no) exposure checking	*/
		rm_vp,			/* Viewport adjustment		*/
		___,			/* (no) readscreen		*/
		___,			/* (no) pick			*/
	},
/*
 * The Ramtek 9460 in "image" (quarter-screen) mode.
 */
 	{
		"rmimg1",		/* Device type rmimgN		*/
		3, { "rmimg2", "rmimg3", "rmimg4" },
		GDF_PIXEL | GDF_HCW | GDF_VECTOR | GDF_VP,
		256,			/* 256 colors			*/
		640, 512,		/* Our screen resolution	*/
		1.0,			/* Assume square aspect for now */
		80, 64,			/* 160 X 128 pixel blocks	*/
		0,			/* no buttons, for now		*/
		0,			/* Background color		*/
		rm_open,		/* The open routine		*/
		rm_close,		/* close			*/
		rm_flush,		/* Flush			*/
		___,			/* (no) flush w/o screen renew	*/
		rm_color_map,		/* Set color table		*/
		rm_poly,		/* Draw polyline		*/
		rm_pixel,		/* Pixel fill			*/
		___,			/* Text				*/
		rm_hcw,			/* Set clip window		*/
		rm_clear,		/* Clear screen			*/
		___,			/* Polygon fill			*/
		___,			/* (no) Segment init		*/
		___,			/* (no) Segment clear		*/
		___,			/* (no) Segment select		*/
		___,			/* (no) Segment end		*/
		___,			/* (no) Segment attributes	*/
		rm_target,		/* Read target			*/
		rm_put_target,		/* Put target			*/
		___,			/* (no) color assignment	*/
		___,			/* (no) exposure checking	*/
		rm_vp,			/* Viewport adjustment		*/
		___,			/* (no) readscreen		*/
		___,			/* (no) pick			*/
	},
/*
 * The Ramtek 9460 in "image" (quarter-screen) mode.  This variant uses
 * raster-line organized pixel maps, which will run more efficiently with
 * some applications.
 */
 	{
		"rrimg1",		/* Device type rmimgN		*/
		3, { "rrimg2", "rrimg3", "rrimg4" },
		GDF_PIXEL | GDF_HCW | GDF_VECTOR | GDF_VP,
		256,			/* 256 colors			*/
		640, 512,		/* Our screen resolution	*/
		1.0,			/* Assume square aspect for now */
		640, 16,		/* Rastor-oriented blocks	*/
		0,			/* no buttons, for now		*/
		0,			/* Background color		*/
		rm_open,		/* The open routine		*/
		rm_close,		/* close			*/
		rm_flush,		/* Flush			*/
		___,			/* (no) flush w/o screen renew	*/
		rm_color_map,		/* Set color table		*/
		rm_poly,		/* Draw polyline		*/
		rm_pixel,		/* Pixel fill			*/
		___,			/* Text				*/
		rm_hcw,			/* Set clip window		*/
		rm_clear,		/* Clear screen			*/
		___,			/* Polygon fill			*/
		___,			/* (no) Segment init		*/
		___,			/* (no) Segment clear		*/
		___,			/* (no) Segment select		*/
		___,			/* (no) Segment end		*/
		___,			/* (no) Segment attributes	*/
		rm_target,		/* Read target			*/
		rm_put_target,		/* Put target			*/
		___,			/* (no) color assignment	*/
		___,			/* (no) exposure checking	*/
		rm_vp,			/* Viewport adjustment		*/
		___,			/* (no) readscreen		*/
		___,			/* (no) pick			*/
	},
# endif

# ifdef DEV_4107
/*
 * Tektronix 4107/4208 terminal.
 */
 	{
		"4107",			/* Device type 			*/
		1, { "tek4107" },
		GDF_SEGMENT | GDF_VECTOR | GDF_VP,
		16,			/* 16 colors			*/
		640, 480,		/* Our screen resolution	*/
		1.0,			/* Assume square aspect for now */
		0, 0,			/* Blocks irrelevant		*/
		0,			/* no buttons, for now		*/
		0,			/* Background color		*/
		tek_open,		/* The open routine		*/
		tek_close,		/* Close			*/
		tek_flush,		/* Flush			*/
		tek_flush_nr,		/* Flush with no screen renew	*/
		tek_cmap,		/* Set color table		*/
		tek_poly,		/* Draw polyline		*/
		___,			/* Pixel fill			*/
		___,			/* Text				*/
		___,			/* Set clip window		*/
		tek_clear,		/* Clear screen			*/
		___,			/* Polygon fill			*/
		tek_s_init,		/* Segment init			*/
		tek_s_clear,		/* Segment clear		*/
		tek_s_select,		/* Segment select		*/
		tek_s_end,		/* Segment end			*/
		tek_s_attr,		/* Segment attributes		*/
		tek_target,		/* Target read			*/
		tek_put_target,		/* Put target			*/
		___,			/* (no) color assignment	*/
		___,			/* (no) exposure checking	*/
		tek_vp,			/* (no) viewport adjustment	*/
		___,			/* (no) readscreen		*/
		___,			/* (no) pick			*/
	},
/*
 * This is a special variation on the 4107 that uses the pixel operations
 * in rastor memory.  Slow, but possible.  The down side to this mode
 * is that the 4107's segmentation ability does not work with pixel ops,
 * and thus is unusable here.
 */
 	{
		"4107p",		/* Device type 			*/
		1, { "4107pxl" },
		GDF_PIXEL | GDF_VECTOR | GDF_VP,
		16,			/* 16 colors			*/
		640, 480,		/* Our screen resolution	*/
		1.0,			/* Assume square aspect for now */
		32, 24,			/* Smallish block size		*/
		0,			/* no buttons, for now		*/
		0,			/* Background color		*/
		tek_open,		/* The open routine		*/
		tek_close,		/* Close			*/
		tek_flush,		/* Flush			*/
		tek_flush_nr,		/* Flush w/o screen renew	*/
		tek_cmap,		/* Set color table		*/
		tek_poly,		/* Draw polyline		*/
		tek_pixel,		/* Pixel fill			*/
		___,			/* Text				*/
		___,			/* Set clip window		*/
		tek_clear,		/* Clear screen			*/
		___,			/* Polygon fill			*/
		___,			/* (no) Segment init		*/
		___,			/* (no) Segment clear		*/
		___,			/* (no) Segment select		*/
		___,			/* (no) Segment end		*/
		___,			/* (no) Segment attributes	*/
		tek_target,		/* Target read			*/
		tek_put_target,	/* Put target			*/
		___,			/* (no) color assignment	*/
		___,			/* (no) exposure checking	*/
		tek_vp,			/* (no) viewport adjustment	*/
		___,			/* (no) readscreen		*/
		___,			/* (no) pick			*/
	},
# endif

# ifdef DEV_COMTAL
/*
 * Comtal vision 1/20.
 */
 	{
		"comtal",		/* Device type comtal		*/
		4, { "comtal1", "comtal2", "comtal3", "comtal4" },
		GDF_PIXEL | GDF_TOP,
		256,			/* 256 colors			*/
		512, 512,		/* Our screen resolution	*/
		1.0,			/* Assume square aspect for now */
		32, 512,		/* Funny strips - avoid win problem */
		0,			/* no buttons, for now		*/
		0,			/* Background color		*/
		zb_open,		/* The open routine		*/
		zb_close,		/* Close			*/
		zb_flush,		/* Flush			*/
		___,			/* (no) flush w/o screen renew	*/
		zb_color,		/* Set color table		*/
		___,			/* Draw polyline		*/
		zb_pixel,		/* Pixel fill			*/
		___,			/* Text				*/
		___,			/* Set clip window		*/
		zb_clear,		/* Clear screen			*/
		___,			/* Polygon fill			*/
		___,			/* Segment init			*/
		___,			/* Segment clear		*/
		___,			/* Segment select		*/
		___,			/* Segment end			*/
		___,			/* Segment attributes		*/
		zb_target,		/* Target read (not yet)	*/
		zb_put_target,		/* Put target (not yet)		*/
		___,			/* (no) color assignment	*/
		___,			/* (no) exposure checking	*/
		___,			/* (no) viewport adjustment	*/
		___,			/* (no) readscreen		*/
		___,			/* (no) pick			*/
	},
# endif

# ifdef DEV_LN03
/*
 * The LN03+ laser printer.
 */
 	{
		"ln03",
		0, { ___ },
		GDF_VECTOR | GDF_HARDCOPY | GDF_MONO,
		2,			/* 2 colors			*/
		4096, 3072,		/* Our screen resolution	*/
		1.0,			/* Assume square aspect for now */
		0, 0,			/* Blocks irrelevant		*/
		0,			/* no buttons, for now		*/
		0,			/* Background color		*/
		ln_open,		/* The open routine		*/
		ln_close,		/* close			*/
		ln_flush,		/* Flush			*/
		___,			/* (no) flush w/o screen renew	*/
		ln_cmap,		/* Set color table		*/
		ln_poly,		/* Draw polyline		*/
		___,			/* Pixel fill			*/
		___,			/* Text				*/
		___,			/* Set clip window		*/
		ln_clear,		/* Clear screen			*/
		___,			/* Polygon fill			*/
		___,			/* (no) Segment init		*/
		___,			/* (no) Segment clear		*/
		___,			/* (no) Segment select		*/
		___,			/* (no) Segment end		*/
		___,			/* (no) Segment attributes	*/
		___,			/* Read target			*/
		___,			/* Put target			*/
		___,			/* (no) color assignment	*/
		___,			/* (no) exposure checking	*/
		___,			/* Viewport adjustment		*/
		___,			/* (no) readscreen		*/
		___,			/* (no) pick			*/
	},
# endif
};
# define N_DEVICE (sizeof (D_tab) / sizeof (struct device))




struct device *
gd_get_dev (type)
char *type;
/*
 * Look up a device by type.
 * Entry:
 *	if the type is known then
 *		A pointer to the appropriate device structure is returned.
 *	else
 *		NULL is returned.
 */
{
	int dev, alt, gd_strcmp ();
	
	for (dev = 0; dev < N_DEVICE; dev++)
	{
		if (! gd_strcmp (type, D_tab[dev].gd_type))
			return (D_tab + dev);
		for (alt = 0; alt < D_tab[dev].gd_nalt; alt++)
			if (! gd_strcmp (type, D_tab[dev].gd_alttype[alt]))
				return (D_tab + dev);
	}
	return ((struct device *) NULL);
}




int
gd_strcmp (str1, str2)
char	*str1, *str2;
/*
 * Perform a case-independent strcmp
 */
{
	int	i = 0;
	char	char1, char2;

	while (TRUE)
	{
		char1 = str1[i];
		char2 = str2[i];
		char1 += (char1 >= 'A' && char1 <= 'Z') ? 'a' - 'A' : (char) 0;
		char2 += (char2 >= 'A' && char2 <= 'Z') ? 'a' - 'A' : (char) 0;

		if (char1 != char2)
			return ((int) str2[i] - (int) str1[i]);	

		if (char1 == (char) 0)
			return (0);

		i++;
	}

}
