/*
 * Test module.
 */
# include "param.h"
# include "graphics.h"
# include "pixel.h"

# define ALPHABET "ABCDEFGHIJKLMNOPQRSTUVWXYZ !\"#$%&'()*+,-./ 0123456789 :;<=>?@[\\]^_` abcdefghijklmnopqrstuvwxyz"

unsigned char Cbuf[32*1024];

static float red[256] =   { 0.0, .99, 0.0, 0.0, .99, 0.0, .99, 0.6, .99 };
static float green[256] = { 0.0, 0.0, .99, 0.0, .99, .99, 0.0, 0.6, .99 };
static float blue[256] =  { 0.0, 0.0, 0.0, .99, 0.0, .99, .99, 0.6, .99 };

main ()
{
	unsigned char c, *cp, *pair, fname[100], fstr[100];
	ws sta;
	overlay ov, ov1;
	float x[100], y[100], step, tx, ty;
	int color = 1, pat = 0, nc, np = 0, font = 1, i, status, font1;
	int italic, sserif, roman, xres, yres, cbase, pass = 0;
	pixel_map pm;
/*
 * Get the display set up.
 */
top:
	if ((status = G_open ("TT", "4107p", &sta, 0)) != GE_OK)
	{
		printf ("Workstation open failure: %s\n", G_messages[status]);
		exit (1);
	}
	ov = G_new_overlay (sta, 100);
	G_get_color (sta, 13, &cbase);
	printf ("Color base is %d\n", cbase);
	G_set_color_map (sta, cbase, 9, red, green, blue);
	
	lib$init_timer ();
/*
 * Axes.
 */
 	x[0] = 0.0; x[1] = 1.0;
	y[0] = y[1] = 0.5;
	G_polyline (ov, GPLT_SOLID, 7 + cbase, 2, x, y);
	G_polyline (ov, GPLT_SOLID, 7 + cbase, 2, y, x);
	G_text (ov, 1 + cbase, GTF_MINPIXEL, 0.03, GT_LEFT, GT_BOTTOM, 0.5, 0.5,
		"Text Example");
/*
 * Color block.
 */
	cp = Cbuf;
	for (i = 0; i < 9; i++)
		for (color = 0; color < 128; color++)
		{
			*cp++ = i + cbase;
			*cp++ = i + cbase;
			*cp++ = i + cbase;
			*cp++ = i + cbase;
		}
	G_pixel_fill (ov, Cbuf, 10, 10, 128, 36, 0, 0);

	G_update (sta);

	getchar ();
	if (pass++ == 0)
		goto top;
	G_close (sta);
	lib$show_timer ();
}
