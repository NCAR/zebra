/*
 * Attempt to allocate all color cells and set them to a bright
 * color, so that xoring stuff is more visible.
 */
static char *rcsid="$Id: tweakcolor.c,v 1.2 1992-06-04 20:07:52 burghart Exp $";

# include <X11/Xlib.h>



main (argc, argv)
int argc; 
char **argv;
{
	char *cname = (argc > 1) ? argv[1] : "yellow";
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
 * Get the default colormap
 */
	cmap = DefaultColormap (disp, DefaultScreen (disp));
/*
 * Now we just allocate colors as long as we get away with it.
 */
	for (ncolor = 0; ; ncolor++)
	{
		if (! XAllocColorCells (disp, cmap, False, NULL, 0, &pix, 1))
			break;

		XStoreNamedColor (disp, cmap, cname, pix, 
			DoRed | DoGreen | DoBlue);
	}
	printf ("%d colors tweaked\n", ncolor);
	XCloseDisplay (disp);
	exit (0);
}
