/* 5/87 jc */
static char *rcsid = "$Id: device.c,v 1.23 2002-07-11 23:15:37 burghart Exp $";
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
extern int x11_open (), x11_clear (), x11_close (), x11_flush (), x11_noop ();
extern int x11_poly (), x11_pick (), x11_target (), x11_casn (), x11_color ();
extern int x11_pixel (), x11_put_target (), x11_untarget (), x11_clip ();
extern int x11_vp (), x11_event (), x11_coff (), x11_readscreen ();
extern int x11_qtext (), x11_text (), x11_tsize ();

extern int ps_open (), ps_close (), ps_flush (), ps_poly (), ps_hcw ();
extern int ps_cmap (), ps_clear (), ps_vp (), ps_print (), ps_qtext ();
extern int ps_tsize (), ps_text ();

extern int psc_open (), psc_close (), psc_flush (), psc_poly (), psc_clip ();
extern int psc_cmap (), psc_clear (), psc_vp (), psc_print (), psc_qtext ();
extern int psc_tsize (), psc_text (), psc_pixel (), psc_viewport ();

extern int nl_open (), nl_ok ();

/*
 * The actual table.
 */
struct device D_tab[] =
{
/*
 * A Null device.
 */
	{
		"null",
		3, { "nl", "bitbucket", "blackhole", "defensebudget" },
		GDF_VECTOR | GDF_PIXEL,
		150,
		500, 500,		/* Our resolution for now */
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
		___,			/* (no) query text		*/
		___,			/* (no) Text size		*/
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
		___,			/* (no) Remove target		*/
		___,			/* Color assignment		*/
		___,			/* Exposure checking		*/
		___,			/* (no) viewport adjustment	*/
		___,			/* (no) readscreen		*/
		___,			/* (no) pick			*/
		___,			/* (no) color offset		*/
		___,			/* Hardware print screen routine*/
	},

/*
 * X11.3.  The open routine may well make changes to this structure,
 * depending on what we actually get.
 */
	{
		"X11",
		4, { "X11-huge", "X700", "X500", "Sun-screen" },
		GDF_VECTOR | GDF_TOP | GDF_DEV_COLOR |GDF_HCW|GDF_VP|GDF_TEXT,
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
		x11_qtext,		/* query text			*/
		x11_tsize,		/* Text size			*/
		x11_text,		/* Text				*/
		x11_clip,		/* Set clip window		*/
		x11_clear,		/* Clear screen			*/
		___,			/* Polygon fill			*/
		___,			/* (no) Segment init		*/
		___,			/* (no) Segment clear		*/
		___,			/* (no) Segment select		*/
		___,			/* (no) Segment end		*/
		___,			/* (no) Segment attributes	*/
		x11_target,		/* Read target			*/
		x11_put_target,		/* Put target			*/
		x11_untarget,		/* Remove target		*/
		x11_casn,		/* Color assignment		*/
		x11_event,		/* Exposure checking		*/
		x11_vp,			/* viewport adjustment	*/
		x11_readscreen,		/* readscreen			*/
		x11_pick,		/* pick		*/
		x11_coff,		/* (no) color offset		*/
		___,			/* Hardware print screen routine*/
	},

/*
 * PostScript printer:  1, 2 and 4 window modes
 */
 	{
		"ps",
		3, { "ps1", "ps2", "ps4"},
		GDF_VECTOR | GDF_HARDCOPY | GDF_MONO | GDF_HCW|GDF_VP|GDF_TEXT,
		2,			/* 2 colors			*/
		2250, 3000,		/* Our screen resolution	*/
		1.0,			/* Assume square aspect for now */
		0, 0,			/* Blocks irrelevant		*/
		0,			/* no buttons, for now		*/
		0,			/* Background color		*/
		ps_open,		/* The open routine		*/
		ps_close,		/* close			*/
		ps_flush,		/* Flush			*/
		___,			/* (no) flush w/o screen renew	*/
		ps_cmap,		/* Set color table		*/
		ps_poly,		/* Draw polyline		*/
		___,			/* Pixel fill			*/
		ps_qtext,		/* query text			*/
		ps_tsize,		/* Text size			*/
		ps_text,		/* Text				*/
		ps_hcw,			/* Set clip window		*/
		ps_clear,		/* Clear screen			*/
		___,			/* Polygon fill			*/
		___,			/* (no) Segment init		*/
		___,			/* (no) Segment clear		*/
		___,			/* (no) Segment select		*/
		___,			/* (no) Segment end		*/
		___,			/* (no) Segment attributes	*/
		___,			/* Read target			*/
		___,			/* Put target			*/
		___,			/* (no) Remove target		*/
		___,			/* (no) color assignment	*/
		___,			/* (no) exposure checking	*/
		ps_vp,			/* Viewport adjustment		*/
		___,			/* (no) readscreen		*/
		___,			/* (no) pick			*/
		___,			/* (no) color offset		*/
		ps_print,		/* Hardware print screen routine*/
	},

/*
 * PostScript COLOR printer:  1, 2 and 4 window color, grayscale,
 * monochrome, and Level 1 modes,
 */
 	{
		"psc",
		12, { "psc1", "psc2", "psc4", "psg1", "psg2", "psg4",
		     "psm1", "psm2", "psm4", "psc1L1", "psc2L1", "psc4L1"},
		GDF_VECTOR | GDF_HARDCOPY | GDF_HCW | GDF_VP | GDF_TEXT |
			GDF_PIXEL | GDF_TOP,
		256,			/* 256 colors			*/
		0, 0,			/* resolution set at open	*/
		1.0,			/* square pixels		*/
	        64, 64,			/* pixel block size		*/
		0,			/* no buttons, for now		*/
		0,			/* Background color		*/
		psc_open,		/* The open routine		*/
		psc_close,		/* close			*/
		psc_flush,		/* Flush			*/
		___,			/* (no) flush w/o screen renew	*/
		psc_cmap,		/* Set color table		*/
		psc_poly,		/* Draw polyline		*/
		psc_pixel,		/* Pixel fill			*/
		psc_qtext,		/* query text			*/
		psc_tsize,		/* Text size			*/
		psc_text,		/* Text				*/
		psc_clip,		/* Set clip window		*/
		psc_clear,		/* Clear screen			*/
		___,			/* Polygon fill			*/
		___,			/* (no) Segment init		*/
		___,			/* (no) Segment clear		*/
		___,			/* (no) Segment select		*/
		___,			/* (no) Segment end		*/
		___,			/* (no) Segment attributes	*/
		___,			/* Read target			*/
		___,			/* Put target			*/
		___,			/* (no) Remove target		*/
		___,			/* (no) color assignment	*/
		___,			/* (no) exposure checking	*/
		psc_viewport,		/* Viewport adjustment		*/
		___,			/* (no) readscreen		*/
		___,			/* (no) pick			*/
		___,			/* (no) color offset		*/
		psc_print,		/* Hardware print screen routine*/
	},
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
 * If this function has run time problems,
 * you may need to bump up MAXALT in device.h.
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
