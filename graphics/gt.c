/*
 * Test module.
 */
# include "param.h"
# include "graphics.h"
# include "pixel.h"

# define ALPHABET "ABCDEFGHIJKLMNOPQRSTUVWXYZ !\"#$%&'()*+,-./ 0123456789 :;<=>?@[\\]^_` abcdefghijklmnopqrstuvwxyz"

unsigned char Cbuf[32*1024];

static float red[256] =   { 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.6, 1.0 };
static float green[256] = { 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 0.6, 1.0 };
static float blue[256] =  { 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.6, 1.0 };


main ()
{
	unsigned char c, *cp, *pair, fname[100], fstr[100];
	ws sta;
	overlay ov, ov1;
	float x[100], y[100], step, tx, ty;
	int color = 1, pat = 0, nc, np = 0, font = 1, i, status, font1;
	int italic, sserif, roman, xres, yres, cbase, pass = 0, pix, ncolor;
	int hcopy, button;
	pixel_map pm;
	char label[100], *getenv ();
	char *devname = getenv ("DEVNAME") ? getenv ("DEVNAME") : "screen";
/*
 * Get the display set up.
 */
	if ((status = G_open (devname, getenv ("DEVTYPE"), &sta,0)) != GE_OK)
	{
		printf ("Workstation open failure: %s\n", G_messages[status]);
		exit (1);
	}
	ov = G_new_overlay (sta, 100);
	G_get_color (sta, 13, &cbase);
	G_set_color_map (sta, cbase, 9, red, green, blue);
/*
 * Axes.
 */
 	x[0] = 0.0; x[1] = 1.0;
	y[0] = y[1] = 0.5;
	G_polyline (ov, GPLT_SOLID, 7 + cbase, 2, x, y);
	G_polyline (ov, GPLT_SOLID, 7 + cbase, 2, y, x);
/*
 * Label the page.
 */
	G_text (ov, 1 + cbase, GTF_STROKE, 0.04, GT_LEFT, GT_TOP,
		0., 0.99, "RDSS graphics test page");
	sprintf (label, "Device name '%s', type '%s'", getenv ("DEVNAME"),
		getenv ("DEVTYPE"));
	G_text (ov, 1 + cbase, GTF_STROKE, 0.02, GT_LEFT, GT_TOP, 0.03,
		0.94, label);
/*
 * Try out inquire.
 */
 	G_w_inquire (sta, GIW_NCOLOR, &ncolor);
 	G_w_inquire (sta, GIW_PIXEL, &pix);
 	G_w_inquire (sta, GIW_XRES, &xres);
 	G_w_inquire (sta, GIW_YRES, &yres);
 	G_w_inquire (sta, GIW_HARDCOPY, &hcopy);
	sprintf (label, "Resolution %d by %d, %spixel capable, %d colors",
		xres, yres, pix ? "" : "NOT ", ncolor);
	G_text (ov, 1 + cbase, GTF_STROKE, 0.02, GT_LEFT, GT_TOP, 0.03,
		0.92, label);
	G_text (ov, 1 + cbase, GTF_STROKE, 0.02, GT_LEFT, GT_TOP, 0.03,
		0.90, hcopy ? "This is a hardcopy device" :
			      "This is NOT a hardcopy device");
/*
 * Line patterns.
 */
 	G_text (ov, 1 + cbase, GTF_STROKE, 0.03, GT_LEFT, GT_BOTTOM, 0.5,
		0.915, "Line Patterns");
 	line_pat (ov, cbase, "Solid", GPLT_SOLID, .53, .90, 0.2, .02);
 	line_pat (ov, cbase, "Dashed", GPLT_DASH, .53, .875, 0.2, .02);
 	line_pat (ov, cbase, "Dotted", GPLT_DOT, .53, .85, 0.2, .02);
 	line_pat (ov, cbase, "Dash-dot", GPLT_DASH_DOT, .53, .825, 0.2, .02);
/*
 * Colors
 */
	if (ncolor >= 8)
	{
		G_text (ov, 1 + cbase, GTF_STROKE, 0.03, GT_LEFT, GT_BOTTOM,
			0.0, 0.75, "Colors:");
		G_text (ov, 1 + cbase, GTF_STROKE, 0.02, GT_LEFT, GT_BOTTOM,
			0.03, 0.725, "Red");
		G_text (ov, 2 + cbase, GTF_STROKE, 0.02, GT_LEFT, GT_BOTTOM,
			0.03, 0.7, "Green");
		G_text (ov, 3 + cbase, GTF_STROKE, 0.02, GT_LEFT, GT_BOTTOM,
			0.03, 0.675, "Blue");
		G_text (ov, 4 + cbase, GTF_STROKE, 0.02, GT_LEFT, GT_BOTTOM,
			0.03, 0.65, "Yellow");
		G_text (ov, 5 + cbase, GTF_STROKE, 0.02, GT_LEFT, GT_BOTTOM,
			0.03, 0.625, "Cyan");
		G_text (ov, 6 + cbase, GTF_STROKE, 0.02, GT_LEFT, GT_BOTTOM,
			0.03, 0.6, "Magenta");
		G_text (ov, 7 + cbase, GTF_STROKE, 0.02, GT_LEFT, GT_BOTTOM,
			0.03, 0.575, "Gray");
		G_text (ov, 8 + cbase, GTF_STROKE, 0.02, GT_LEFT, GT_BOTTOM,
			0.03, 0.55, "White");
	}
/*
 * Text justification.
 */
 	G_text (ov, 1 + cbase, GTF_STROKE, 0.03, GT_LEFT, GT_BOTTOM, 0.5,
		0.46, "Text justification");
	x[0] = x[1] = 0.6;
	y[0] = 0.40;
	y[1] = 0.30;
	G_polyline (ov, GPLT_SOLID, 1 + cbase, 2, x, y);
	G_text (ov, 2 + cbase, GTF_STROKE, 0.02, GT_RIGHT, GT_BOTTOM,
		0.6, 0.37, "Right");
	G_text (ov, 2 + cbase, GTF_STROKE, 0.02, GT_CENTER, GT_BOTTOM,
		0.6, 0.34, "Center");
	G_text (ov, 2 + cbase, GTF_STROKE, 0.02, GT_LEFT, GT_BOTTOM,
		0.6, 0.31, "Left");
/*
 * Vertical text positioning is a little trickier.
 */
	x[0] = 0.7;
	x[1] = 0.95;
	y[0] = y[1] = 0.35;
	G_polyline (ov, GPLT_SOLID, 1 + cbase, 2, x, y);
	G_text (ov, 2 + cbase, GTF_STROKE, 0.02, GT_LEFT, GT_BOTTOM,
		0.72, 0.35, "Bottom");
	G_tx_box (ov, GTF_STROKE, 0.02, GT_LEFT, GT_BOTTOM, 0.72, 0.35,
		"Bottom", x, y, x + 1, y + 1);
	G_text (ov, 2 + cbase, GTF_STROKE, 0.02, GT_LEFT, GT_CENTER,
		x[1] + 0.02, 0.35, "Center");
	G_tx_box (ov, GTF_STROKE, 0.02, GT_LEFT, GT_CENTER, x[1] + 0.02, 0.35,
		"Center", x, y, x + 1, y + 1);
	G_text (ov, 2 + cbase, GTF_STROKE, 0.02, GT_LEFT, GT_TOP,
		x[1] + 0.02, 0.35, "Top");
/*
 * Fonts.
 */
 	G_text (ov, 1 + cbase, GTF_STROKE, 0.03, GT_LEFT, GT_BOTTOM, 0.0,
		0.45, "Text fonts");
	y[0] = 0.44;
	do_font (ov, GTF_STROKE, "High quality stroke", y, 0.02, 2 + cbase);
	do_font (ov, GTF_MINSTROKE, "LOW QUALITY STROKE", y, 0.02, 2 + cbase);
	if (pix)
	{
		do_font (ov, GTF_MINPIXEL, "Minimal pixel font", y, 0.02,
			2+cbase);
# ifdef notdef
		font = G_tex_font ("sserif-120.pxl");
		do_font (ov, font, "TeX sans serif", y, 0.02, 2+cbase);
		font = G_tex_font ("csc-120.pxl");
		do_font (ov, font, "TeX Caps/small caps", y, 0.02, 2+cbase);
		font = G_tex_font ("tw-120.pxl");
		do_font (ov, font, "TeX typewriter", y, 0.02, 2+cbase);
# endif
	}
/*
 * Color block.
 */
	G_update (sta);
	if (pix)
	{
		cp = Cbuf;
		for (i = 0; i < 9; i++)
			for (color = 0; color < 128; color++)
			{
				*cp++ = i + cbase;
				*cp++ = i + cbase;
				*cp++ = i + cbase;
				*cp++ = i + cbase;
			}
		G_pixel_fill (ov, Cbuf, 100, 100, 128, 36, 0, 0);
		printf ("Hit <return> for color block (pmap) test: ");
		getchar ();
		G_update (sta);
	}
	do
	{
		G_pick (ov, &button, x, y);
		printf ("Target is at %.2f %.2f, button %d\n", x[0], y[0],
			button);
		if (button == 1)
			G_viewport (ov, 0.0, 0.0, 1.0, 1.0);
		else
			G_viewport (ov, x[0] < .5 ? 0.0 : 0.5,
				y[0] < .5 ? 0.0 : 0.5, 0.5, 0.5);
	} while (button < 2);
	G_close (sta);
}




line_pat (ov, cbase, ctype, ltype, x, y, length, hgt)
overlay ov;
char *ctype;
int ltype, cbase;
float x, y, length, hgt;
/*
 * Put out an example line type.
 */
{
	int retcode;
	float xp[2], yp[2], x0, y0, x1, y1;
/*
 * output our label.
 */
	retcode = G_text (ov, 2 + cbase, GTF_STROKE, hgt, GT_LEFT, GT_CENTER,
		x, y, ctype);
	G_tx_box (ov, GTF_STROKE, hgt, GT_LEFT, GT_CENTER, x, y, ctype,
		&x0, &y0, &x1, &y1);
/*
 * Now put out the line.
 */
	xp[0] = x1 + 0.01;
	xp[1] = xp[0] + length;
	yp[0] = yp[1] = y;
	G_polyline (ov, ltype, 3 + cbase, 2, xp, yp);
}





do_font (ov, font, name, y, height, color)
overlay ov;
int font;
char *name;
float *y, height;
int color;
/*
 * Demonstrate a font, returning a position for the next one.
 */
{
	float x0, y0, x1, y1;
/*
 * Put out the font name.
 */
 	G_text (ov, color, font, height, GT_LEFT, GT_TOP, 0.03, *y, name);
	G_tx_box (ov, font, height, GT_LEFT, GT_TOP, 0.03, *y, name,
		&x0, &y0, &x1, &y1);
	*y = y0;
}
