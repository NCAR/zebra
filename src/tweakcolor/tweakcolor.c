/*
 * Attempt to allocate all color cells and set them to a bright
 * color, so that xoring stuff is more visible.
 */
static char *rcsid="$Id: tweakcolor.c,v 1.1 1992-01-28 17:41:16 corbet Exp $";

# include <X11/Xlib.h>



main (argc, argv)
int argc; 
char **argv;
{
	char *cname = (argc > 1) ? argv[1] : "red";
	XColor exact, screen;
	Display *disp;
	Colormap cmap;
	int ncolor;
	unsigned long pix;
/*
 * Connect to the display.
 */
	if ((disp = XOpenDisplay (NULL)) == NULL)
	{
		printf ("Unable to open display\n");
		exit (1);
	}
/*
 * Look up our color.
 */
	cmap = DefaultColormap (disp, DefaultScreen (disp));
	if (! XLookupColor (disp, cmap, cname, &exact, &screen))
	{
		printf ("Color '%s' unknown\n", cname);
		XLookupColor (disp, cmap, "red", &exact, &screen);
	}
/*
 * Now we just allocate colors as long as we get away with it.
 */
	for (ncolor = 0; ; ncolor++)
	{
		if (! XAllocColorCells (disp, cmap, False, NULL, 0, &pix, 1))
			break;
		screen.pixel = pix;
		XStoreColor (disp, cmap, &screen);
	}
	printf ("%d colors tweaked\n", ncolor);
	XCloseDisplay (disp);
	exit (0);
}
