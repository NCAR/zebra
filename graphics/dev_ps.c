/*	1/90 pw		*/
/*
 * Device driver for PostScript
 */
# include "param.h"

# ifdef DEV_PS

# include "graphics.h"
# include "device.h"
# include <stdio.h>

static char *rcsid = "$Id: dev_ps.c,v 1.1 1990-04-05 10:58:15 corbet Exp $";
/*
 * The tag structure
 */
# define DBUFLEN 1024
struct ps_tag
{
# ifdef UNIX
	FILE	*pt_file;	/* File pointer to the device	*/
	int	pt_pipe;	/* Piped into lpr?		*/
# endif
# ifdef VMS
	int	pt_channel;	/* Channel to printer		*/
	int	pt_ef;		/* Event flag			*/
	char	pt_queue[80];	/* Queue name			*/
# endif
	char	pt_buf[DBUFLEN];/* Device output buffer		*/
	char	*pt_bufp;	/* Current position in ps_buf	*/
	int	pt_winset;	/* Are we set in a window?	*/
	int	pt_mode;	/* Portrait or landscape?	*/
	int	pt_nwin;	/* Number of windows per page	*/
	int	pt_win;		/* Current window		*/
	int	pt_winfilled;	/* Are any windows filled?	*/
	int	pt_cx0, pt_cx1, /* User's clip window		*/
		pt_cy0, pt_cy1;
};

/*
 * Defines
 */
# define PS_PORTRAIT	0
# define PS_LANDSCAPE	1
/*
 * Res[PS_PORTRAIT,PS_LANDSCAPE][#windows-1][x,y]
 */
static int Res[2][4][2] =
{
	{ /* PS_PORTRAIT */
		{ 2250, 3000 },
		{ 2250, 1500 },
		{ 0, 0},
		{ 1125, 1500 },
	},
	{ /* PS_LANDSCAPE */
		{ 3000, 2250 },
		{ 1500, 2250 },
		{ 0, 0 },
		{ 1500, 1125 },
	},
};

/*
 * Line type info.
 */
static char *Ps_ltype[] =
{
	"[]",		/* GPLT_SOLID	*/
	"[16 16]",	/* GPLT_DASH	*/
	"[2 8]",	/* GPLT_DOT	*/
	"[16 8 2 8]",	/* GPLT_DASH_DOT */
};



ps_open (device, type, tag, dev)
char *device, *type, **tag;
struct device *dev;
{
	struct ps_tag *ptp = (struct ps_tag *) malloc (sizeof (struct ps_tag));
	char command[80];

# ifdef UNIX
	if (strchr (device, '/'))
	/*
	 * Open up a file name 'device'
	 */
	{
		ptp->pt_pipe = FALSE;
		if (!(ptp->pt_file = fopen (device, "w")))
			ui_error ("Unable to open file '%s'", device);
	}
	else
	/*
	 * Open up a pipe to lpr -P'device'.
	 */
	{
		ptp->pt_pipe = TRUE;
		strcpy (command, "lpr -P");
		strcat (command, device);
		if (!(ptp->pt_file = popen (command, "w")))
			ui_error ("Unable to open pipe '%s'", command);
	}
# endif
/*
 * See how the user wants the page formatted (device type name is psN)
 */
	ptp->pt_nwin = type[2] ? type[2] - '0' : 1;
	ptp->pt_mode = ptp->pt_nwin == 2 ? PS_PORTRAIT : PS_LANDSCAPE;
/*
 * Set our device resolution
 */
	dev->gd_xres = Res[ptp->pt_mode][ptp->pt_nwin-1][0];
	dev->gd_yres = Res[ptp->pt_mode][ptp->pt_nwin-1][1];
/*
 * Reset the buffer to the beginning
 */
	ptp->pt_bufp = ptp->pt_buf;
/*
 * Initialize the printer
 */
	ps_out_s (ptp, "%!\n");
	ps_init (ptp);
/*
 * PostScript defines
 */
	ps_def_out (ptp);
/*
 * Return the info
 */
	*tag = (char *) ptp;
	return (GE_OK);
}





ps_close (ctag)
char *ctag;
/*
 * Finish up and close the temporary file
 */
{
	struct ps_tag *ptp = (struct ps_tag *) ctag;

	if (ptp->pt_winfilled)
		ps_out_s (ptp, "showpage\n");
	ps_buf_out (ptp);
	if (ptp->pt_pipe)
		pclose (ptp->pt_file);
	else
		fclose (ptp->pt_file);
}





ps_flush (ctag)
char *ctag;
/*
 * Finish up a plot, then flush out the command buffer.
 */
{
	struct ps_tag *ptp = (struct ps_tag *) ctag;
/*
 * Make sure we've done something
 */
	if (!ptp->pt_winset)
		return;
/*
 * Restore the graphics state
 */
	ps_out_s (ptp, "grestore\n");
	ptp->pt_winset = FALSE;
/*
 * Do a page eject if we have filled all the windows
 */
	if (ptp->pt_win == ptp->pt_nwin)
	{
		ps_out_s (ptp, "showpage\n");
		ps_init (ptp);
	}
	else
	{
		ptp->pt_winfilled = TRUE;
		ptp->pt_win++;
	}
/*
 * Write out the buffer
 */
	ps_buf_out (ptp);
}





ps_cmap (ctag, base, ncolor, r, g, b)
char *ctag;
int base, ncolor;
float *r, *g, *b;
/*
 * Although PostScript can do color, this is a no-op routine for now...
 */
{}





ps_poly (ctag, color, ltype, npt, data)
char *ctag;
int color, ltype, npt, *data;
/*
 * Draw a polyline with the given number of points.
 */
{
	struct ps_tag *ptp = (struct ps_tag *) ctag;
	int	points = 0;
	char	command[80];

	if (!ptp->pt_winset)
		ps_set_win (ptp);
/*
 * Do a setdash ('sd') and a newpath ('n')
 */
	sprintf (command, "%s 0 sd n\n", Ps_ltype[ltype]);
	ps_out_s (ptp, command);
/*
 * position to the first point ('m' = moveto)
 */
	sprintf (command, "%d %d m\n", data[0], data[1]);
	ps_out_s (ptp, command);
	data += 2;
/*
 * Loop through all points ('z' = lineto)
 */
	while (points++ < npt - 1)
	{
		sprintf (command, "%d %d z\n", data[0], data[1]);
		ps_out_s (ptp, command);
		data += 2;
	}
/*
 * Connect the dots ('s' = stroke)
 */
	ps_out_s (ptp, "s\n");
}





ps_hcw (ctag, x0, y0, x1, y1)
char *ctag;
int x0, y0, x1, y1;
/*
 * Hardware clipping routine
 */
{
	struct ps_tag *ptp = (struct ps_tag *) ctag;
	char command[80];
/*
 * Make a path around the clipping rectangle, then send 'clip'
 *	'i' = initclip, 'n' = newpath, 'm' = moveto, 'z' = lineto
 *	'c' = closepath, 'cp' = clip
 */
	sprintf (command, "i n %d %d m %d %d z %d %d z %d %d z c cp\n", x0, y0,
			x0, y1, x1, y1, x1, y0);
	ps_out_s (ptp, command);
	ptp->pt_cx0 = x0;
	ptp->pt_cy0 = y0;
	ptp->pt_cx1 = x1;
	ptp->pt_cy1 = y1;
}





ps_clear (ctag)
char *ctag;
/*
 * Don't do anything, for now.
 */
{}





ps_vp (ctag, x0, y0, x1, y1)
char *ctag;
int x0, y0, x1, y1;
{
	struct ps_tag *ptp = (struct ps_tag *) ctag;
}





ps_print (ctag)
char *ctag;
{
	struct ps_tag *ptp = (struct ps_tag *) ctag;

	if (!ptp->pt_winfilled)
		return;
	ps_out_s (ptp, "showpage\n");
	ps_init (ptp);
	ps_buf_out (ptp);
}




ps_init (ptp)
struct ps_tag *ptp;
/*
 * Initialize a page
 */
{
/*
 * Move axes so we have 1/2" margins and set scale for 300 pixels/inch
 */
	ps_out_s (ptp, "0.24 0.24 scale 150 150 translate 1 setlinewidth\n");
	if (ptp->pt_mode == PS_LANDSCAPE)
		ps_out_s (ptp, "90 rotate\n0 -2250 translate\n");
	ptp->pt_winset = FALSE;
	ptp->pt_winfilled = FALSE;
	ptp->pt_win = 1;
}




ps_def_out (ptp)
struct ps_tag *ptp;
/*
 * An attempt to minimize output file size
 */
{
	ps_out_s (ptp, "/sd {setdash} def\n");
	ps_out_s (ptp, "/n {newpath} def\n");
	ps_out_s (ptp, "/m {moveto} def\n");
	ps_out_s (ptp, "/z {lineto} def\n");
	ps_out_s (ptp, "/s {stroke} def\n");
	ps_out_s (ptp, "/t {translate} def\n");
	ps_out_s (ptp, "/i {initclip} def\n");
	ps_out_s (ptp, "/c {closepath} def\n");
	ps_out_s (ptp, "/cp {clip} def\n");
}





ps_set_win (ptp)
struct ps_tag *ptp;
/*
 * Set up to plot into a window
 */
{
	char	command[80];
/*
 * Save our current state (origin in lower left corner, 72 dpi)
 */
	ps_out_s (ptp, "gsave\n");
	ptp->pt_winset = TRUE;
/*
 * Translate coordinate axis
 */
	switch (ptp->pt_nwin)
	{
	/*
 	 * 1 window
	 *	do nothing since we're already positioned
	 */
		case 1:
			break;
	/*
 	 * 2 windows
	 * 	PS_PORTRAIT		PS_LANDSCAPE
	 *	-----------		------------------
	 *	|    1    |		|        |       |
	 *	|         |		|   1    |   2   |
	 *	-----------		|        |       |
	 *	|    2    |		------------------
	 *	|         |
	 *	-----------
	 */
		case 2:
			if (ptp->pt_mode == PS_PORTRAIT)
			    sprintf (command, "0 %d t\n",
				ptp->pt_win == 1 ? Res[PS_PORTRAIT][1][1] : 0);
			else
			    sprintf (command, "%d 0 t\n",
				ptp->pt_win == 2 ? Res[PS_LANDSCAPE][1][0] :0);
			ps_out_s (ptp, command);
			break;
	/*
	 * 4 windows
	 *	Use our resolutions to compute offsets
	 *
	 *	---------------
	 *	|  1   |   2  |
	 *      ---------------
	 *      |  3   |   4  |
         *      ---------------
	 */
		case 4:
			sprintf (command, "%d %d t\n",
			  ((ptp->pt_win == 2) || (ptp->pt_win == 4) ?
			  Res[ptp->pt_mode][3][0] : 0),
			  ((ptp->pt_win < 3) ? Res[ptp->pt_mode][3][1] : 0));
			ps_out_s (ptp, command);
			break;
	}
/*
 * Reset our clipping window
 */
	ps_hcw ((char *) ptp, ptp->pt_cx0, ptp->pt_cy0, ptp->pt_cx1,
			ptp->pt_cy1);
}	





ps_out_s (ptp, str)
struct ps_tag *ptp;
char *str;
/*
 * Put a C string in the output buffer
 */
{
	ps_chk_buf (ptp);
	while (*str)
		*ptp->pt_bufp++ = *str++;
}





ps_chk_buf (ptp)
struct ps_tag *ptp;
/*
 * Check the output buffer to see if we need to do a flush
 */
{
	if (ptp->pt_bufp - ptp->pt_buf > DBUFLEN - 100)
		ps_buf_out (ptp);
}





ps_buf_out (ptp)
struct ps_tag *ptp;
/*
 * Write out the command buffer
 */
{
# ifdef UNIX
	fwrite (ptp->pt_buf, 1, ptp->pt_bufp - ptp->pt_buf, ptp->pt_file);
# endif
/*
 * Reset the buffer.
 */
	ptp->pt_bufp = ptp->pt_buf;
}

# endif /* DEV_PS */
