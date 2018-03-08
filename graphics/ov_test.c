/*
 * Test module.
 */
# include "graphics.h"

main ()
{
	char c, *cp, *pair;
	ws sta;
	overlay ov, ov1;
	int cbase;
	float x[100], y[100], step;
	float red[9] =   { 0.0, .99, 0.0, 0.0, .99, 0.0, .99, 0.5, .99 };
	float green[9] = { 0.0, 0.0, .99, 0.0, .99, .99, 0.0, 0.5, .99 };
	float blue[9] =  { 0.0, 0.0, 0.0, .99, 0.0, .99, .99, 0.5, .99 };
	int color = 1, pat = 0, nc, np = 0, font = 1, i;
/*
 * Get the display set up.
 */
/*	G_open ("rma0", "rm9460", &sta); */
	G_open ("screen", "x700", &sta, 0);
	ov = G_new_overlay (sta, 100);
	ov1 = G_new_overlay (sta, 200);
	G_set_coords (ov, -640.0, -512.0, 640.0, 512.0);
	G_set_coords (ov1, -640.0, -512.0, 640.0, 512.0);
	G_get_color (sta, 13, &cbase);
	G_set_color_map (sta, cbase, 9, red, green, blue);
/*
 * Draw the origin.
 */
 	x[0] = -500.0;
	y[0] = y[1] = 0.0;
	x[1] = 500.0;
	G_polyline (ov, GPLT_SOLID, 7, 2, x, y);
	G_polyline (ov, GPLT_SOLID, 7, 2, y, x);
/*
 * Stuff.
 */
	x[0] = y[0] = 0.0;
	y[1] = 511.0;
	for (i = -600; i < 600; i+= 20)
	{
		x[1] = (float) i;
		G_polyline (ov1, GPLT_SOLID, color, 2, x, y);
		color = (color + 1) % 7;
	}
/*
 * Put some text into each overlay.
 */
	G_text (ov, 1, 1, 0.5, GT_LEFT, GT_BOTTOM, 0.0, -10.0, "OVERLAY 1");
	G_text (ov1, 2, 1, 0.5, GT_LEFT, GT_BOTTOM, 0.0, -10.0, "OVERLAY 2");
	G_update (sta);
/*
 * Now play with visibility.
 */
	getchar ();
 	G_visible (ov, 0);
	G_update (sta);
	getchar ();
 	G_visible (ov, 1);
 	G_visible (ov1, 0);
	G_update (sta);
	getchar ();
 	G_visible (ov1, 1);
	G_update (sta);
	getchar ();
	G_close (sta);
}
