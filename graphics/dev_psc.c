/*	1/90 pw (mono),  7/20/93 GFC (color)	*/
/*
 * Device driver for Color PostScript
 */
# include "param.h"

# ifdef DEV_PSC

# include "graphics.h"
# include "device.h"
# include <stdio.h>

static char *rcsid = "$Id: dev_psc.c,v 1.2 1993-07-27 19:52:41 cook Exp $";
/*
 * The tag structure
 */
# define DBUFLEN 1024
# define MAXCOLS 256
struct psc_tag
{
# ifdef unix
	FILE	*pt_file;	/* File pointer to the device	*/
	int	pt_pipe;	/* Piped into lpr?		*/
	char	*pt_devname;	/* printer name 		*/
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
	int	pt_pixsize;	/* Current font pixel size	*/
	int	base;		/* min color value 		*/
	int	ncolor;		/* max color value 		*/
	int	cmode;		/* 0=mono, 1=gray, 2=color	*/
	int	black;		/* color position for black	*/
	float	r[MAXCOLS];	/* red colortable array		*/
	float	g[MAXCOLS];	/* green colortable array	*/
	float	b[MAXCOLS];	/* blue colortable array	*/
};

/*
 * Defines
 */
# define PSC_PORTRAIT	0
# define PSC_LANDSCAPE	1
/*
 * Res[PSC_PORTRAIT,PSC_LANDSCAPE][#windows-1][x,y]
 */
static int Res[2][4][2] =
{
	{ /* PSC_PORTRAIT */
		{ 2250, 3000 },
		{ 2250, 1500 },
		{ 0, 0},
		{ 1125, 1500 },
	},
	{ /* PSC_LANDSCAPE */
		{ 3000, 2250 },
		{ 1500, 2250 },
		{ 0, 0 },
		{ 1500, 1125 },
	},
};

/*
 * Line type info.
 */
static char *Psc_ltype[] =
{
	"[]",		/* GPLT_SOLID	*/
	"[16 16]",	/* GPLT_DASH	*/
	"[2 8]",	/* GPLT_DOT	*/
	"[16 8 2 8]",	/* GPLT_DASH_DOT */
};


/*
 * Kludge aspect ratio for courier text.
 */
static float Faspect = 0.6;
# define DESC 4




psc_open (device, type, tag, dev)
char *device, *type, **tag;
struct device *dev;
{
	struct psc_tag *ptp = (struct psc_tag *)malloc(sizeof(struct psc_tag));
	char command[80];
	char *getenv ();
	double atof();

# ifdef unix
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
	 * Open up a pipe to lpr -P'device'.  SystemV uses lp -d'. 
	 */
	{
		ptp->pt_pipe = TRUE;
#ifdef SYSV
                sprintf (command, "lp -d%s", device);
#else
		sprintf (command, "lpr -P%s", device);
#endif
		if (!(ptp->pt_file = popen (command, "w")))
			ui_error ("Unable to open pipe '%s'", command);
	}
	/*
	 * Save the device name
	 */
	ptp->pt_devname = (char *) malloc ((1 + strlen (device)) *
		sizeof (char));
	strcpy (ptp->pt_devname, device);
# endif
/*
 * The third char is m for mono, g for gray or c for color: psm1, psg1, psc1.
 */
	if (type[2] == 'm') ptp->cmode = 0;
	else if (type[2] == 'g') ptp->cmode = 1;
	else ptp->cmode = 2;
/*
 * Pick the format style (1,2,4) or null from the device type name: pscN.
 */
	ptp->pt_nwin = type[3] ? type[3] - '0' : 1;
	ptp->pt_mode = ptp->pt_nwin == 2 ? PSC_PORTRAIT : PSC_LANDSCAPE;
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
	psc_out_s (ptp, "%!PS-Adobe-2.0\n");
	psc_init (ptp);
/*
 * PostScript defines
 */
	psc_def_out (ptp);
/*
 * Return the info
 */
	ptp->pt_pixsize = -1;
	if (getenv ("PSASPECT"))
	{
		Faspect = atof (getenv ("PSASPECT"));
		printf ("FP aspect ratio: %.2f", Faspect);
	}
	*tag = (char *) ptp;
	return (GE_OK);
}





psc_close (ctag)
char *ctag;
/*
 * Finish up and close the temporary file
 */
{
	struct psc_tag *ptp = (struct psc_tag *) ctag;

	if (ptp->pt_winfilled)
		psc_out_s (ptp, "showpage\n");

	psc_out_s (ptp, "grestoreall\n");
	psc_buf_out (ptp);

	if (ptp->pt_pipe)
	{
		pclose (ptp->pt_file);
		free (ptp->pt_devname);
	}
	else
		fclose (ptp->pt_file);

	free (ptp);
}





psc_flush (ctag)
char *ctag;
/*
 * Finish up a plot, then flush out the command buffer.
 */
{
	struct psc_tag *ptp = (struct psc_tag *) ctag;
/*
 * Make sure we've done something
 */
	if (!ptp->pt_winset)
		return;
/*
 * Restore the graphics state
 */
	psc_out_s (ptp, "grestore\n");
	ptp->pt_winset = FALSE;
	ptp->pt_pixsize = -1;
/*
 * Do a page eject if we have filled all the windows
 */
	if (ptp->pt_win == ptp->pt_nwin)
		psc_finish_page (ptp);
	else
	{
		ptp->pt_winfilled = TRUE;
		ptp->pt_win++;
	}
/*
 * Write out the buffer
 */
	psc_buf_out (ptp);
}





psc_cmap (ctag, base, ncolor, r, g, b)
char *ctag;
int base, ncolor;
float *r, *g, *b;
/*
 * Set up the color table here.
 */
{
	struct psc_tag *ptp = (struct psc_tag *) ctag;
	char	command[80];
	int i, gr;

/*
 * copy base and n color for checking
 */
	ptp->base = base;
	ptp->ncolor = ncolor;

/*
 * make a local copy of the colors so the table can be output
 * at the beginning of every page.
 */
	for (i=base; i<=ncolor; i++)
	{
		ptp->r[i] = r[i];
		ptp->g[i] = g[i];
		ptp->b[i] = b[i];
	}
/*
 * don't set a color table if in monochrome mode.
 */
	if (ptp->cmode == 0)
		return (GE_OK);

/*
 * Output the color table in PostScript form.
 */
	psc_ctable_out (ptp);

/*
 * Locate the color positions the first ocurrences of black.
 * Save e black index for dealing with out of bound colors.
 */
	ptp->black = 0;

	for (i=base; i<=ncolor; i++)
	{
		if ((r[i]+g[i]+b[i]) == 0.0)
		{
			ptp->black = i;
			break;
		}
	}

	return (GE_OK);
}





psc_ctable_out (ptp)
struct psc_tag *ptp;
/*
 * Output the nifty PostScript Level 2 color table here.
 */
{
	char	command[80];
	int i, gr;

/*
 * Output the PostScript color table command.
 */
	sprintf (command, "[/Indexed /DeviceRGB %d\n<\n", ptp->ncolor);
	psc_out_s (ptp, command);

/*
 * Build the PostScript color or gray table
 */
	if (ptp->cmode == 2)	/* color mode, all colors are as specified */
	{
	    for (i=ptp->base; i<=ptp->ncolor; i++)
	    {
		/* Turn invisible white to black */
		if ((ptp->r[i]+ptp->g[i]+ptp->b[i]) <= 2.99)
			sprintf (command, "%02x%02x%02x\n",
				(unsigned char) (ptp->r[i] * 255.0),
				(unsigned char) (ptp->g[i] * 255.0),
				(unsigned char) (ptp->b[i] * 255.0));
		else
			sprintf (command, "000000\n");
		psc_out_s (ptp, command);
	    }
	}
	else if (ptp->cmode == 1) /* fixed linear grayscale mode */
	{
	    for (i=ptp->base; i<=ptp->ncolor; i++)
	    {
		gr = i * (int)((255.0 / (float)ptp->ncolor));

		sprintf (command, "%02x%02x%02x\n", gr, gr, gr);

		psc_out_s (ptp, command);
	    }
	}

	psc_out_s (ptp, ">\n] setcolorspace\n");
}





psc_poly (ctag, color, ltype, npt, data)
char *ctag;
int color, ltype, npt, *data;
/*
 * Draw a polyline with the given number of points.
 */
{
	struct psc_tag *ptp = (struct psc_tag *) ctag;
	int	point = 0;
	char	command[80];

	if (!ptp->pt_winset)
		psc_set_win (ptp);
/*
 * Check color value sanity and then set the color using # setcolor.
 * Default to black if it is out of bounds.
 */
	if (ptp->cmode)		/* only for color or gray */
	{
	    if ((color >= ptp->base) && (color <= ptp->ncolor))
		sprintf (command, "%d sc\n", color);
	    else
		sprintf (command, "%d sc\n", ptp->black);

	    psc_out_s (ptp, command);
	}
/*
 * Do a setdash ('sd') and newpath ('n')
 */
	sprintf (command, "%s 0 sd n\n", Psc_ltype[ltype]);
	psc_out_s (ptp, command);
/*
 * position to the first point ('m' = moveto)
 */
	sprintf (command, "%d %d m\n", data[0], data[1]);
	psc_out_s (ptp, command);
	data += 2;
/*
 * Draw the polyline (break it into chunks of 100 points
 */
	for (point = 1; point < npt; point++)
	{
	/*
	 * Add a segment ('z' = lineto)
	 */
		sprintf (command, "%d %d z\n", data[0], data[1]);
		psc_out_s (ptp, command);
	/*
	 * Draw the line every 100 points
	 */
		if ((point % 100) == 0)
		{
		/*
		 * Stroke ('s') and newpath ('n')
		 */
			psc_out_s (ptp, "s n\n");
		/*
		 * Do a moveto ('m') to start off where we finished
		 */
			sprintf (command, "%d %d m\n", data[0], data[1]);
			psc_out_s (ptp, command);
		}
	/*
	 * Move through the data
	 */
		data += 2;	
	}
/*
 * Connect the dots ('s' = stroke)
 */
	psc_out_s (ptp, "s\n");
}





psc_hcw (ctag, x0, y0, x1, y1)
char *ctag;
int x0, y0, x1, y1;
/*
 * Hardware clipping routine
 */
{
	char command[80];
/*
 * Make a path around the clipping rectangle, then send 'clip'
 *	'i' = initclip, 'n' = newpath, 'm' = moveto, 'z' = lineto
 *	'c' = closepath, 'cp' = clip
 */
	sprintf (command, "i n %d %d m %d %d z %d %d z %d %d z c cp\n", x0, y0,
			x0, y1, x1, y1, x1, y0);
	psc_out_s (ctag, command);

	((struct psc_tag *)ctag)->pt_cx0 = x0;
	((struct psc_tag *)ctag)->pt_cy0 = y0;
	((struct psc_tag *)ctag)->pt_cx1 = x1;
	((struct psc_tag *)ctag)->pt_cy1 = y1;
}





psc_clear (ctag)
char *ctag;
/*
 * Don't do anything, for now.
 */
{
}





psc_vp (ctag, x0, y0, x1, y1)
char *ctag;
int x0, y0, x1, y1;
{
}





psc_print (ctag)
char *ctag;
{
	struct psc_tag *ptp = (struct psc_tag *) ctag;

	if (!ptp->pt_winfilled)
		return;

	psc_finish_page (ptp);
}




psc_finish_page (ptp)
struct psc_tag	*ptp;
{
	char	command[80];
	static int	pagecount = 0;
/*
 * Finish this page
 */
	psc_out_s (ptp, "showpage\n");
	psc_buf_out (ptp);
/*
 * Close and reopen the pipe to eject a page.
 */
	if (ptp->pt_pipe)
	{
		pclose (ptp->pt_file);
#ifdef SYSV
                sprintf (command, "lp -d%s", ptp->pt_devname);
#else
		sprintf (command, "lpr -P%s", ptp->pt_devname);
#endif
		if (!(ptp->pt_file = popen (command, "w")))
			ui_error ("Unable to open pipe '%s'", command);
	/*
	 * Initialize and make appropriate definitions
	 */
		psc_out_s (ptp, "%!PS-Adobe-2.0\n");
		psc_def_out (ptp);
	}
	else
	{
		fclose (ptp->pt_file);
#if 0
		if (!(ptp->pt_file = fopen (ptp->pt_devname, "w")))
			ui_error ("Unable to open file '%s'", ptp->pt_devname);
#endif
	}
/*
 * Initialize for the next page
 */
	psc_init (ptp);

/*
 * Output a PS color table unless in monochrome mode.
 */
	if (ptp->cmode)
		psc_ctable_out (ptp);
}





psc_init (ptp)
struct psc_tag *ptp;
/*
 * Initialize a page
 */
{
/*
 * Move axes so we have 1/2" margins and set scale for 300 pixels/inch
 */
	psc_out_s (ptp, "0.24 0.24 scale\n");
	psc_out_s (ptp, "150 150 translate\n");
	psc_out_s (ptp, "1 setlinewidth\n");
	if (ptp->pt_mode == PSC_LANDSCAPE)
	{
		psc_out_s (ptp, "90 rotate\n");
		psc_out_s (ptp, "0 -2250 translate\n");
	}
	ptp->pt_winset = FALSE;
	ptp->pt_winfilled = FALSE;
	ptp->pt_win = 1;
}




psc_def_out (ptp)
struct psc_tag *ptp;
/*
 * An attempt to minimize output file size
 */
{
	psc_out_s (ptp, "/c {closepath} def\n");
	psc_out_s (ptp, "/cp {clip} def\n");
	psc_out_s (ptp, "/i {initclip} def\n");
	psc_out_s (ptp, "/m {moveto} def\n");
	psc_out_s (ptp, "/n {newpath} def\n");
	psc_out_s (ptp, "/s {stroke} def\n");
	psc_out_s (ptp, "/sd {setdash} def\n");
	psc_out_s (ptp, "/sc {setcolor} def\n");
	psc_out_s (ptp, "/sh {show} def\n");
	psc_out_s (ptp, "/t {translate} def\n");
	psc_out_s (ptp, "/z {lineto} def\n");
}





psc_set_win (ptp)
struct psc_tag *ptp;
/*
 * Set up to plot into a window
 */
{
	char	command[80];
/*
 * Save our current state (origin in lower left corner, 72 dpi)
 */
	psc_out_s (ptp, "gsave\n");
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
	 * 	PSC_PORTRAIT		PSC_LANDSCAPE
	 *	-----------		------------------
	 *	|    1    |		|        |       |
	 *	|         |		|   1    |   2   |
	 *	-----------		|        |       |
	 *	|    2    |		------------------
	 *	|         |
	 *	-----------
	 */
		case 2:
			if (ptp->pt_mode == PSC_PORTRAIT)
			    sprintf (command, "0 %d t\n",
				ptp->pt_win == 1 ? Res[PSC_PORTRAIT][1][1] : 0);
			else
			    sprintf (command, "%d 0 t\n",
				ptp->pt_win == 2 ? Res[PSC_LANDSCAPE][1][0] :0);
			psc_out_s (ptp, command);
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
			psc_out_s (ptp, command);
		break;
	}
/*
 * Reset our clipping window
 */
	psc_hcw ((char *)ptp, ptp->pt_cx0, ptp->pt_cy0, ptp->pt_cx1,
			ptp->pt_cy1);
}	





psc_out_s (ptp, str)
struct psc_tag *ptp;
char *str;
/*
 * Put a C string in the output buffer
 */
{
	int len = strlen (str);

	psc_chk_buf (ptp, len);
	memcpy (ptp->pt_bufp, str, len);
	ptp->pt_bufp += len;
}





psc_chk_buf (ptp, len)
struct psc_tag *ptp;
int len;
/*
 * Check the output buffer to see if we need to do a flush
 */
{
	if ((int)(ptp->pt_bufp - ptp->pt_buf) > (DBUFLEN - len))
		psc_buf_out (ptp);
}





psc_buf_out (ptp)
struct psc_tag *ptp;
/*
 * Write out the command buffer
 */
{
# ifdef unix
	fwrite (ptp->pt_buf, 1, ptp->pt_bufp - ptp->pt_buf, ptp->pt_file);
# endif
/*
 * Reset the buffer.
 */
	ptp->pt_bufp = ptp->pt_buf;
}




/*
 * Dev text routines.
 */

int
psc_qtext (ctag, pixsize, rot)
char *ctag;
int pixsize;
float rot;
{
	return (rot == 0.0);	/* Lazy!	*/
}


psc_tsize (ctag, pixsize, rot, text, width, height, desc)
char *ctag, *text;
int pixsize, *width, *height, *desc;
float rot;
/*
 * The text size routine.
 */
{
	*height = pixsize;	/* you want it, you get it	*/
	*width = (int) (((float) pixsize*strlen (text)) * Faspect);
	*desc = pixsize/DESC;	/* ??? */
}



psc_text (ctag, x, y, color, pixsize, rot, text)
char *ctag, *text;
int x, y, color, pixsize;
float rot;
/*
 * The hardware text routine.
 */
{
	struct psc_tag *tag = (struct psc_tag *) ctag;
	char cbuf[200], *cp;
	int len = strlen (text), i;
/*
 * Initialize if necessary.
 */
	if (! tag->pt_winset)
		psc_set_win (tag);
/*
 * Check color value sanity and then set the color using # setcolor.
 * Default to color 0 (black) if it is out of bounds.
 */
	if (tag->cmode)		/* only for color or gray */
	{
	    if ((color >= tag->base) && (color <= tag->ncolor))
		sprintf (cbuf, "%d sc\n", color);
	    else
		sprintf (cbuf, "%d sc\n", tag->black);

	    psc_out_s (tag, cbuf);
	}
/*
 * If we don't have the desired font loaded, do it now.  This could be smarter.
 */
	if (pixsize != tag->pt_pixsize)
	{
		/* changed from Courier to Helvetica cuz it looks better */
		psc_out_s (tag, "/Helvetica findfont\n");

		sprintf (cbuf, "%d scalefont setfont\n", pixsize);
		psc_out_s (tag, cbuf);
		tag->pt_pixsize = pixsize;
	}
/*
 * Move to the desired location.
 */
	sprintf (cbuf, "%d %d m ", x, y + pixsize/DESC);
	psc_out_s (tag, cbuf);
/*
 * Now move the string over, taking care to escape certain characters.
 */
	cbuf[0] = '(';
	cp = cbuf + 1;
	for (i = 0; i < len; i++)
	{
		if (text[i] == '(' || text[i] == ')' || text[i] == '\\') 
			*cp++ = '\\';
		*cp++ = text[i];
	}
	*cp++ = ')';
	*cp = '\0';
	psc_out_s (tag, cbuf);
	psc_out_s (tag, " sh\n");
}
			

# endif /* DEV_PSC */
