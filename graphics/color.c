/*
 * Color management.
 */
# include <stdio.h>
# include "color.h"
# include "graphics.h"
# include "device.h"
# include "overlay.h"
# include "workstation.h"

/*
 * Table of all colors for which we can do color name to RGB translation
 */
static struct ctranstbl
{
	char	*name;
	double	red, green, blue;
	struct ctranstbl	*next;
} *Cname_to_rgb = (struct ctranstbl *) 0;

/*
 * The default color database file, if not passed in by the compiler.
 */
# ifndef COLORDB
# define COLORDB "/usr/lib/X11/rgb.txt"
# endif


char *getvm ();

void
gc_init (wsta, ncolor)
struct workstation *wsta;
int ncolor;
/*
 * Initialize color accounting for this station.
 */
{
	wsta->ws_color = (struct colors *) getvm (sizeof (struct colors));
	wsta->ws_color->c_base = 0;
	wsta->ws_color->c_nc = ncolor;
	wsta->ws_color->c_next = 
               (struct colors *) getvm (sizeof (struct colors));
        wsta->ws_color->c_next = (struct colors *) 0;
}





int
gc_assign (wstn, ncolor, base)
struct workstation *wstn;
int ncolor, *base;
/*
 * Attempt to assign this many colors.
 */
{
	struct colors *cp, *lp = 0;

	for (cp = wstn->ws_color; cp; cp = cp->c_next)
	{
		if (cp->c_nc >= ncolor)
		{
			*base = cp->c_base;
			if ((cp->c_nc -= ncolor) <= 0)
			{
				if (! lp)
					wstn->ws_color = cp->c_next;
				else
					lp->c_next = cp->c_next;
				relvm (cp);
			}
			else
				cp->c_base += ncolor;
			return (GE_OK);
		}
		lp = cp;
	}
	return (GE_NCOLOR);
}





void
gc_close (wstn)
struct workstation *wstn;
/*
 * Close out this set of color entries.
 */
{
	struct colors *cp;
	
	while (wstn->ws_color)
	{
		cp = wstn->ws_color->c_next;
		relvm (wstn->ws_color);
		wstn->ws_color = cp;
	}
}




gc_name_to_rgb (cname, r, g, b)
char	*cname;
double	*r, *g, *b;
/*
 * Translate the named color to RGB values
 */
{
	struct ctranstbl	*clr;
	int	status;
/*
 * Build the translation table if necessary
 */
	if (!Cname_to_rgb)
		if ((status = gc_transtable ()) != GE_OK)
			return (status);
/*
 * Look up this color in the table of all colors
 */
	clr = Cname_to_rgb;

	while (clr)
		if (! strcmp (clr->name, cname))
			break;
		else
			clr = clr->next;

	if (clr)
	{
		*r = clr->red;
		*g = clr->green;
		*b = clr->blue;
		return (GE_OK);
	}
	else
		return (GE_BAD_COLOR);
}




gc_transtable ()
/*
 * Build the color name to rbg translation structure from the X11
 * rgb.txt file
 */
{
	struct ctranstbl	*clr, *prev;
	int	r, g, b;
	char	string[128], colorname[50];
	FILE	*infile;
	char *getenv();
/*
 * Get the name of the translation file
 */
	if (getenv ("COLORNAMES") != 0)
		strcpy (string, getenv ("COLORNAMES"));
	else
		strcpy (string, COLORDB);
/*
 * Open the file
 */
	if ((infile = fopen (string, "r")) == NULL)
		return (GE_BAD_FILE);
/*
 * Loop and read everything
 */
	clr = (struct ctranstbl *) 0;

	while (fgets (string, sizeof (string), infile))
	{
	/*
	 * Skip comments (lines with '!' as the first character) and lines
	 * that don't parse properly.
	 */
		if ((string[0] == '!') ||
		    (sscanf (string, "%d%d%d %[^\n]", &r, &g, &b, 
			     colorname) != 4))
			continue;
	/*
	 * Allocate the next table entry
	 */
		prev = clr;
		clr = (struct ctranstbl *) 
			calloc (1, sizeof (struct ctranstbl));
	/*
	 * Set the next pointer (or set the head of the list)
	 */
		if (prev)
			prev->next = clr;
		else
			Cname_to_rgb = clr;
	/*
	 * Assign the color values
	 */
		clr->red = (double) r / 255.0;
		clr->green = (double) g / 255.0;
		clr->blue = (double) b / 255.0;
	/*
	 * Save the name
	 */
		clr->name = (char *) malloc (1 + strlen (colorname));
		strcpy (clr->name, colorname);
	}
/*
 * Close the file
 */
	fclose (infile);
	return (GE_OK);
}
