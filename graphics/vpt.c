# include <stdlib.h>
# include "graphics.h"



main ()
{
	int x0, y0, x1, y1, status, xres, yres;
	ws wsta;
	overlay ov;
/*
 * Open up and initialize.
 */
 	if ((status = G_open ("DEVNAME", getenv ("DEVTYPE"), &wsta,
			GF_NOCLEAR)) != GE_OK)
	{
		printf ("Workstation open failure: %s\n", G_messages[status]);
		exit (1);
	}
	ov = G_new_overlay (wsta, 10);
/*
 * Get the dimensions of this device, and set the world coords to match.
 */
 	G_w_inquire (wsta, GIW_XRES, &xres);
	G_w_inquire (wsta, GIW_YRES, &yres);
	G_set_coords (ov, 0, 0, xres - 1, yres - 1);
/*
 * Now we just loop and tweak viewports.
 */
 	for (;;)
	{
		printf ("Coords: (x0, y0, x1, y1) ");
		scanf ("%d%d%d%d", &x0, &y0, &x1, &y1);
		G_viewport (ov, (float) x0, (float) y0, (float) x1, (float) y1);
		G_update (wsta);
	}
}
