/*
 * Color management.
 */
# include "color.h"
# include "graphics.h"
# include "device.h"
# include "overlay.h"
# include "workstation.h"




char *getvm ();


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
	wsta->ws_color->c_next = (struct colors *) 0;
}









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
