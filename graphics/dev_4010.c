/* 4010 device driver */
static char *rcsid = "$Id: dev_4010.c,v 1.2 1998-02-27 16:00:32 burghart Exp $";

# include "param.h"

# ifdef DEV_4010

# include "graphics.h"
# include "tty.h"
# include "device.h"
# ifdef VMS
# include "lib_include:lib_proto.h"
# endif


/*
 * Defines.
 */
# define XRES 1024
# define YRES 780

/*
 * Unpacking info
 */
# define LOW5BITS	0x1F
# define EXTRA_YBITS	0xC
# define EXTRA_XBITS	0x3

/*
 * Character codes of interest.
 */
# define US	31	/* Leave graphic mode	*/
# define GS	29	/* Enter graphic mode	*/
# define TRANS "\30"		/* Transparent mode string	*/
# define VECT  "\35"		/* Vector mode string GS */
# define ALPHA "\35\37"		/* Alpha mode string GS US */
# define CLEAR "\35\33\14"	/* Clear screen string GS ESC FF   */
				/* (leaves terminal in alpha mode) */

/*
 * The structure type of our "device tag"
 */
typedef enum { t4010, t4014, vt100 } dtype;

struct tek_tag
{
	dtype  tek_type;		/* What kind of dev we have	*/
	struct tty_tag	*tek_tty;	/* The terminal buffer info	*/
	int	tek_hires;		/* High resolution device?	*/
};


/*
 * Map graphics line types onto Tek types.
 */
static int Tek_ltypes[] =
{
	0,	/* GPLT_SOLID	*/
	4,	/* GPLT_DASH	*/
	1,	/* GPLT_DOT	*/
	2	/* GPLT_DASH_DOT*/
};

/*
 * Forwards
 */
void t10_endplot();



int
t10_open (device, type, tag, dev)
char *device, *type, **tag;
struct device *dev;
/*
 * Open up a TEK 4107 terminal.
 */
{
	int status;
	struct tek_tag *t = (struct tek_tag *) getvm (sizeof (struct tek_tag));
/*
 * Get a channel to the device.
 */
 	if ((status = gtty_open (device, &t->tek_tty)) != GE_OK)
		return (status);
/*
 * Figure out what our device type is.
 */
	if (! strcmp (type, "vt100"))
		t->tek_type = vt100;
	else if (! strcmp (type, "4014"))
		t->tek_type = t4014;
	else
		t->tek_type = t4010;
/*
 * If this is a vt100, clear things out.
 */
	if (t->tek_type == vt100)
	{
		gtty_out (t->tek_tty, ALPHA);
		gtty_out (t->tek_tty, "\033[0");
	}
/*
 * Set the dev resolution.
 */
	if (t->tek_type == t4014)
		t->tek_hires = 1;
	else
	{
		t->tek_hires = 0;
		dev->gd_xres = 1024;
		dev->gd_yres = 780;
	}
/*
 * Return the tag.
 */
	*tag = (char *) t;
	return (GE_OK);
}





void
t10_clear (tag)
struct tek_tag *tag;
/*
 * Clear the screen.
 */
{
	gtty_out (tag->tek_tty, "\35\33\14\30");
/*
 * Try to get the cursor out of the way.
 */
	if (tag->tek_type == vt100)
		gtty_out (tag->tek_tty, "\033[24;1H");
	else
	{
		gtty_out (tag->tek_tty, VECT);
		tek_xy (0, 0, tag->tek_tty, tag->tek_hires);
		gtty_out (tag->tek_tty, TRANS);
	}
}



void
t10_flush (ctag)
char *ctag;
/*
 * Perform a device flush.
 */
{
	struct tek_tag *tag = (struct tek_tag *) ctag;

	t10_endplot (tag, TRUE);
	gtty_flush (tag->tek_tty);
}






int
t10_cmap (ctag, base, ncolor, r, g, b)
char *ctag;
int base, ncolor;
float *r, *g, *b;
/*
 * Load up a color map.
 */
{
	return (GE_OK);
}




void
t10_poly (ctag, color, ltype, npt, data)
char *ctag;
int color, ltype, npt, *data;
/*
 * Draw a polyline with the given number of points.
 */
{
	struct tek_tag *tag = (struct tek_tag *) ctag;
	int point;
/*
 * Go into polyline mode.
 */
	gtty_flush (tag->tek_tty);
 	gtty_out (tag->tek_tty, "\035");	/* GS (ENTER VECTOR MODE) */
/*
 * Send out the coords for each point.
 */
	for (point = 0; point < npt; point++)
	{
		tek_xy (data[0], data[1], tag->tek_tty, tag->tek_hires);
		data += 2;
	}
/*
 * Back into normal mode.
 */
	gtty_out (tag->tek_tty, "\037");	/* US */
}




void
t10_endplot (tag, renew)
struct tek_tag *tag;
int	renew;
/*
 * We're at the end of a plot, so let's get out of graphics mode and
 * into normal terminal mode.  Do a full screen renew if requested
 */
{
/*
 * Try to get the cursor out of the way.
 */
	if (tag->tek_type == vt100)
	{
		gtty_out (tag->tek_tty, TRANS);
		gtty_out (tag->tek_tty, "\0330");
		gtty_out (tag->tek_tty, "\033[24;1H");
	}
	else
	{
		gtty_out (tag->tek_tty, VECT);
		tek_xy (20, 40, tag->tek_tty, tag->tek_hires);
		gtty_out (tag->tek_tty, TRANS);
	}
}


void
t10_close (ctag)
char *ctag;
/*
 * Close down this device.
 */
{
	struct tek_tag *tag = (struct tek_tag *) ctag;
/*
 * For vt100's, go back into the VT mode.
 */
	if (tag->tek_type == vt100)
		gtty_out (tag->tek_tty, "\0330");
	gtty_flush (tag->tek_tty);
/*
 * Close down our channel, and release storage.
 */
	gtty_close (tag->tek_tty);
	relvm (tag);
}


# endif /* DEV_4107 */
