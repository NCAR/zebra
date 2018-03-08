/*
 * Test module.
 */
# include <stdlib.h>
# include "param.h"
# include "graphics.h"
# include "pixel.h"

# define ALPHABET "ABCDEFGHIJKLMNOPQRSTUVWXYZ !\"#$%&'()*+,-./ 0123456789 :;<=>?@[\\]^_` abcdefghijklmnopqrstuvwxyz"

char Cbuf[16*1024];


main ()
{
	char c, *cp, *pair, fname[100], fstr[100];
	ws sta;
	overlay ov, ov1;
	float x[100], y[100], step, tx, ty;
	float red[256] =   { 0.0, .99, 0.0, 0.0, .99, 0.0, .99, 0.4, .99 };
	float green[256] = { 0.0, 0.0, .99, 0.0, .99, .99, 0.0, 0.4, .99 };
	float blue[256] =  { 0.0, 0.0, 0.0, .99, 0.0, .99, .99, 0.4, .99 };
	int color = 1, pat = 0, nc, np = 0, font = 1, i, status, font1;
	int cbase, italic, sserif, roman, xres, yres;
	pixel_map pm;

	for (i = 9; i < 256; i++)
		red[i] = green[i] = blue[i] = ((float) i)/256.0;

	if (! getenv ("DEVTYPE"))
	{
		printf ("Assign DEVTYPE, man!\n");
		exit (1);
	}
/*
 * Get the display set up.
 */
	if ((status = G_open ("devname", getenv ("DEVTYPE"), &sta, 0)) != 
	    GE_OK)
	{
		printf ("Workstation open failure: %s\n", G_messages[status]);
		exit (1);
	}
	ov = G_new_overlay (sta, 100);
	G_get_color (sta, 13, &cbase);
	G_set_color_map (sta, cbase, 9, red, green, blue);
/*
 * Set our coordinates to get actual pixel addresses.
 */
	G_w_inquire (sta, GIW_XRES, &xres);
	G_w_inquire (sta, GIW_YRES, &yres);
	ui_printf ("A %d by %d display...\n", xres, yres);
	G_set_coords (ov, 0.0, 0.0, xres - 1, yres - 1);
/*
 * Put in the color bar.
 */
	memset (Cbuf, 1, 10000);
	G_pixel_fill (ov, Cbuf, 100, 300, 100, 100, 0, GPM_OVERWRITE);
	G_pixel_fill (ov, Cbuf, 300, 100, 100, 100, 0, GPM_OVERWRITE);
	memset (Cbuf, 2, 10000);
	G_pixel_fill (ov, Cbuf, 200, 300, 100, 100, 0, GPM_OVERWRITE);
	G_pixel_fill (ov, Cbuf, 100, 100, 100, 100, 0, GPM_OVERWRITE);
	memset (Cbuf, 3, 10000);
	G_pixel_fill (ov, Cbuf, 300, 300, 100, 100, 0, GPM_OVERWRITE);
	G_pixel_fill (ov, Cbuf, 200, 100, 100, 100, 0, GPM_OVERWRITE);
	memset (Cbuf, 4, 10000);
	G_pixel_fill (ov, Cbuf, 100, 200, 100, 100, 0, GPM_OVERWRITE);
	memset (Cbuf, 5, 10000);
	G_pixel_fill (ov, Cbuf, 200, 200, 100, 100, 0, GPM_OVERWRITE);
	memset (Cbuf, 6, 10000);
	G_pixel_fill (ov, Cbuf, 300, 200, 100, 100, 0, GPM_OVERWRITE);
/*
 * Some text.
 */
 	font = G_tex_font ("tex$pxldir:cmr6.1500pxl");
	G_text (ov, 7, font, 0, GT_RIGHT, GT_TOP, 100.0, 99.0, "(0, 0)");
	G_text (ov, 7, font, 0, GT_CENTER, GT_TOP, 200.0, 99.0, "(100, 0)");
	G_text (ov, 7, font, 0, GT_CENTER, GT_TOP, 300.0, 99.0, "(200, 0)");
	G_text (ov, 7, font, 0, GT_CENTER, GT_TOP, 400.0, 99.0, "(300, 0)");
	G_text (ov, 7, font, 0, GT_RIGHT, GT_CENTER, 99.0, 200.0, "(0, 100)");
	G_text (ov, 7, font, 0, GT_RIGHT, GT_CENTER, 99.0, 300.0, "(0, 200)");
	G_text (ov, 7, font, 0, GT_RIGHT, GT_CENTER, 99.0, 400.0, "(0, 300)");
	G_text (ov, 8, font, 0, GT_CENTER, GT_BOTTOM, 250.0, 403.0, 
			"Aspect ratio test");

	G_update (sta);
	G_close (sta);
}
