/*
 * Attempt to allocate all color cells and set them to a bright
 * color, so that xoring stuff is more visible.
 */
static char *rcsid="$Id: tweakcolor.c,v 1.4 1994-01-12 17:02:34 burghart Exp $";

# include <X11/Intrinsic.h>

main (argc, argv)
int argc; 
char **argv;
{
	int	ncolor = 0;
	char	*cname = (argc > 1) ? argv[1] : "yellow";
	Widget	top;
	Pixel	pix;
        XtAppContext    appc;
	Colormap	cmap;
/*
 * Connect to the display.
 */
	top = XtAppInitialize (&appc, "tweakcolor", NULL, 0, &argc, argv, NULL,
			       NULL, 0);
/*
 * Get the default colormap
 */
	cmap = DefaultColormapOfScreen (XtScreen (top));
/*
 * Now we just allocate colors as long as we get away with it.
 */
	while (XAllocColorCells (XtDisplay (top), cmap, False, NULL, 0, 
				 &pix, 1))
	{
		XStoreNamedColor (XtDisplay (top), cmap, cname, pix, 
			DoRed | DoGreen | DoBlue);
		ncolor++;
	}

	printf ("%d colors tweaked\n", ncolor);

	exit (0);
}
