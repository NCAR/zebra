/*
 * Display stuff.
 */
/*		Copyright (C) 1987,88,89,90,91 by UCAR
 *	University Corporation for Atmospheric Research
 *		   All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced
 * or used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular purpose.
 * UCAR does not indemnify any infringement of copyright, patent, or trademark
 * through use or modification of this software.  UCAR does not provide 
 * maintenance or updates for its software.
 */

# include <fcntl.h>
# include <defs.h>
# include "HouseKeeping.h"
# include "radar_ingest.h"
# include "display.h"



GC Gcontext;




void
OpenDisplay (argc, argv)
int *argc;
char **argv;
/*
 * Open up the display.
 */
{
	Arg args[5];
	int n;
/*
 * Open things up.
 */
	Top = XtAppInitialize (&Actx, "radar_ingest", NULL, 0, argc, argv,
		NULL, NULL, 0);
/*
 * Make a graphics widget to go inside.
 */
	n = 0;
	XtSetArg (args[n], XtNwidth, XRes);		n++;
	XtSetArg (args[n], XtNheight, YRes);		n++;
	XtSetArg (args[n], XtNframeCount, 2);		n++;
	Graphics = XtCreateManagedWidget ("graphics", graphicsWidgetClass,
		Top, args, n);
/*
 * Put it on the screen.
 */
	XtRealizeWidget (Top);
	DoXEvents ();
	Gcontext = XCreateGC (XtDisplay (Top), GWFrame (Graphics), 0, NULL);
	XSetForeground (XtDisplay (Graphics), Gcontext, 4);
}






DoXEvents ()
/*
 * Plow through any X Events.
 */
{
	XEvent event;

	XSync (XtDisplay (Top), False);
 	while (XtAppPending (Actx))
	{
		XtAppNextEvent (Actx, &event);
		XtDispatchEvent (&event);
	}
}




UpdateDisplay ()
{
	static int frame = 0;
# ifdef notdef
	XDrawArc (XtDisplay (Graphics), GWFrame (Graphics), Gcontext,
		16, 16, 768, 768, 0, 360*64);
	XDrawArc (XtDisplay (Graphics), GWFrame (Graphics), Gcontext,
		208, 208, 384, 384, 0, 360*64);
	XDrawArc (XtDisplay (Graphics), GWFrame (Graphics), Gcontext,
		304, 304, 192, 192, 0, 360*64);
# endif
	frame = frame ? 0 : 1;
	GWDisplayFrame (Graphics, frame);
	DoXEvents ();
	/* getchar (); */
	DoXEvents ();
}



FixColors (table)
char *table;
/*
 * Load this color table.
 */
{
	unsigned short red[257], green[257], blue[257];
	int fd, color;
	unsigned long  pixels[128];
	XColor xc[128];
/*
 * Open up the file.
 */
	if ((fd = open (table, O_RDONLY)) < 0)
	{
		perror (table);
		return;
	}
/*
 * Pull in the colors.
 */
 	read (fd, green, 514);
 	read (fd, red, 514);
 	read (fd, blue, 514);
	close (fd);
# ifdef notdef
/*
 * Swap them all around to something useful. 
 */
	for (color = 0; color < 256; color++)
	{
		char *rcp = (char *) (red + color);
		char *bcp = (char *) (blue + color);
		char *gcp = (char *) (green + color), c;
		
		c = rcp[0]; rcp[0] = rcp[1]; rcp[1] = c;
		c = gcp[0]; gcp[0] = gcp[1]; gcp[1] = c;
		c = bcp[0]; bcp[0] = bcp[1]; bcp[1] = c;
	}
# endif
/*
 * Allocate some colors.
 */
	XAllocColorCells (XtDisplay (Top), DefaultColormap (XtDisplay (Top),0),
		False, NULL, 0, pixels, 128);
/*
 * Go through and store all of the colors.
 */
	for (color = 0; color < 128; color++)
	{
		xc[color].pixel = pixels[color];
		xc[color].red = red[color*2];
		xc[color].blue = blue[color*2];
		xc[color].green = green[color*2];
		xc[color].flags = DoRed | DoGreen | DoBlue;
	}
	XStoreColors (XtDisplay (Top), DefaultColormap (XtDisplay (Top), 0),
		xc, 128);
/*
 * Make the color map used in rasterization.
 */
	for (color = 0; color < 256; color++)
		CMap[color] = pixels[color/2];
}





/*
 * Major kludgery to get around temporary graphicsw problems.
 */

int PlotTime;
fc_LookupFrame ()
{
	return (0);
}
