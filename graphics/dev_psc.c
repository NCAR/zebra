/*	1/90 pw (mono),  7/20/93 GFC (color)	*/
/*
 * Device driver for Color PostScript
 */
# include "param.h"

# include "graphics.h"
# include "device.h"
# include <stdio.h>

static char *rcsid = "$Id: dev_psc.c,v 1.8 2002-07-11 23:14:34 burghart Exp $";
/*
 * The tag structure
 */
# define DBUFLEN 1024
# define MAXCOLS 256
struct psc_tag
{
	FILE	*pt_file;	/* File pointer to the device	*/
	int	pt_pipe;	/* Piped into lpr?		*/
	char	*pt_devname;	/* printer name 		*/
	int	pt_channel;	/* Channel to printer		*/
	int	pt_ef;		/* Event flag			*/
	char	pt_queue[80];	/* Queue name			*/
	char	pt_buf[DBUFLEN];/* Device output buffer		*/
	char	*pt_bufp;	/* Current position in ps_buf	*/
	int	pt_winset;	/* Window info written yet?	*/
	int	pt_mode;	/* Portrait or landscape?	*/
	int	pt_nwin;	/* Number of windows per page	*/
	int	pt_win;		/* Current window		*/
	int	pt_winfilled;	/* Are any windows filled?	*/
	int	pt_width;	/* x resolution of window	*/
	int	pt_height;	/* y resolution of window	*/
	int	pt_cx0, pt_cx1; /* User's clip window		*/
	int	pt_cy0, pt_cy1;
	int	pt_pixsize;	/* Current font pixel size	*/
	int	base;		/* min color value 		*/
	int	ncolor;		/* max color value 		*/
	int	cmode;		/* 0=mono, 1=gray, 2=color	*/
	int	black;		/* color position for black	*/
	int	pslevel;	/* 1=level 1 PS, 2=level 2 PS	*/
	float	r[MAXCOLS];	/* red colortable array		*/
	float	g[MAXCOLS];	/* green colortable array	*/
	float	b[MAXCOLS];	/* blue colortable array	*/
	int	pt_vpx0, pt_vpx1;/* user's viewport		*/
	int	pt_vpy0, pt_vpy1;
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
		{ 768, 1024 },
		{ 768, 512 },
		{ 0, 0},
		{ 384, 512 },
	},
	{ /* PSC_LANDSCAPE */
		{ 1024, 768 },
		{ 512, 768 },
		{ 0, 0 },
		{ 512, 384 },
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
 * Page number
 */
int	PageNum = 1;

/*
 * Scratch string into which we can write commands
 */
char	Command[128];

/*
 * Kludge aspect ratio for courier text.
 */
static float Faspect = 0.6;
# define DESC 4

/*
 * Forwards
 */
void psc_ctable_out(), psc_finish_page(), psc_init(), psc_def_out();
void psc_set_win(), psc_DoViewport(), psc_DoClip(), psc_out_s(), psc_out();
void psc_chk_buf(), psc_buf_out();




int
psc_open (device, type, tag, dev)
char *device, *type, **tag;
struct device *dev;
{
	struct psc_tag *ptp = (struct psc_tag *)malloc(sizeof(struct psc_tag));
	char *getenv ();
	double atof();

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
#if (defined(__SVR4) || defined(__SYSV))
                sprintf (Command, "lp -d%s", device);
#else
		sprintf (Command, "lpr -P%s", device);
#endif
		if (!(ptp->pt_file = popen (Command, "w")))
			ui_error ("Unable to open pipe '%s'", Command);
	}
	/*
	 * Save the device name
	 */
	ptp->pt_devname = (char *) malloc ((1 + strlen (device)) *
		sizeof (char));
	strcpy (ptp->pt_devname, device);
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
 * The fifth char is L for PS Level 1: psc1L1, psc2L1, psc4L1
 */
	if (type[4] == 'L') 
		ptp->pslevel = 1;
	else 
		ptp->pslevel = 2;
/*
 * Set our device resolution
 */
	dev->gd_xres = ptp->pt_width = Res[ptp->pt_mode][ptp->pt_nwin-1][0];
	dev->gd_yres = ptp->pt_height = Res[ptp->pt_mode][ptp->pt_nwin-1][1];
/*
 * User clip coordinates (start with the whole window)
 */
	ptp->pt_cx0 = 0;
	ptp->pt_cx1 = ptp->pt_width;

	ptp->pt_cy0 = 0;
	ptp->pt_cy1 = ptp->pt_height;
/*
 * Initial viewport (whole window)
 */
	ptp->pt_vpx0 = 0;
	ptp->pt_vpx1 = ptp->pt_width;

	ptp->pt_vpy0 = 0;
	ptp->pt_vpy1 = ptp->pt_height;
/*
 * Reset the buffer to the beginning
 */
	ptp->pt_bufp = ptp->pt_buf;
/*
 * Header
 */
	if (ptp->pslevel == 1) 
		psc_out_s (ptp, "%!PS-Adobe\n");
	else 
		psc_out_s (ptp, "%!PS-Adobe-2.0\n");
/*
 * PostScript defines and page initialization
 */
	psc_def_out (ptp);
	psc_init (ptp);
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




void
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




void
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
	sprintf (Command, "grestore  %% Finish window %d of %d window page\n",
		 ptp->pt_win, ptp->pt_nwin);
	psc_out_s (ptp, Command);

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
		psc_set_win (ptp);
	}
/*
 * Write out the buffer
 */
	psc_buf_out (ptp);
}




int
psc_cmap (ctag, base, ncolor, r, g, b)
char *ctag;
int base, ncolor;
float *r, *g, *b;
/*
 * Set up the color table here.
 */
{
	struct psc_tag *ptp = (struct psc_tag *) ctag;
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
 * Put out the color table
 */
	if (ptp->pt_winset)
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




void
psc_ctable_out (ptp)
struct psc_tag *ptp;
/*
 * Output the nifty PostScript Level 2 or
 * crusty old PostScript Level 1 color table here.
 */
{
	int i, gr;

/*
 * Output the start of the PostScript color table command.
 */
	if (ptp->pslevel == 1)
		sprintf (Command, "/TABLE %d array def\n", ptp->ncolor);
	else
		sprintf (Command, "[/Indexed /DeviceRGB %d\n<\n", ptp->ncolor);

	psc_out_s (ptp, Command);

/*
 * Build the PostScript color or gray table
 */
	if (ptp->cmode == 2)	/* color mode, all colors are as specified */
	{
	    for (i=ptp->base; i<=ptp->ncolor; i++)
	    {
		if (ptp->pslevel == 1)	/* level 1 PS */
		{
		    /* Turn invisible white to black */
		    if ((ptp->r[i]+ptp->g[i]+ptp->b[i]) <= 2.99)
			sprintf (Command, "16#%02x%02x%02x\n",
				(unsigned char) (ptp->r[i] * 255.0),
				(unsigned char) (ptp->g[i] * 255.0),
				(unsigned char) (ptp->b[i] * 255.0));
		    else
			sprintf (Command, "16#000000\n");
		}
		else	/* level 2 PS */
		{
		    /* Turn invisible white to black */
		    if ((ptp->r[i]+ptp->g[i]+ptp->b[i]) <= 2.99)
			sprintf (Command, "%02x%02x%02x\n",
				(unsigned char) (ptp->r[i] * 255.0),
				(unsigned char) (ptp->g[i] * 255.0),
				(unsigned char) (ptp->b[i] * 255.0));
		    else
			sprintf (Command, "000000\n");
		}
		psc_out_s (ptp, Command);
	    }
	}
	else if (ptp->cmode == 1) /* fixed linear grayscale mode, Lvl 2 only */
	{
	    for (i=ptp->base; i<=ptp->ncolor; i++)
	    {
		gr = i * (int)((255.0 / (float)ptp->ncolor));

		sprintf (Command, "%02x%02x%02x\n", gr, gr, gr);

		psc_out_s (ptp, Command);
	    }
	}

	if (ptp->pslevel == 1)
		psc_out_s (ptp, "TABLE astore pop\n");
	else
		psc_out_s (ptp, ">\n] setcolorspace\n");
}




void
psc_pixel (ctag, x, y, xs, ys, data, size, org)
char *ctag;
unsigned char *data;
int x, y, xs, ys, size, org;
/*
 * The pixel fill routine.
 */
{
	struct psc_tag	*ptp = (struct psc_tag *) ctag;
	int	i;
	unsigned char	*dp;
/*
 * Initialize if necessary
 */
	if (! ptp->pt_winset)
		psc_set_win (ptp);
/*
 * Start with scaling and translation.
 */
	sprintf (Command, "gsave %d %d t %d %d scale  %% begin pixel image\n", 
		 x, y, xs, ys);
	psc_out_s (ptp, Command);
/*
 * Different handling for level 1 and level 2 PostScript
 */
	if (ptp->pslevel == 1)
	{
		unsigned char	rgb[3];
	/*
	 * Level 1: Use "colorimage" and RGB data.  This makes for a big 
	 * file...
	 */
		sprintf (Command, "/pixdata %d string def\n", xs);
		psc_out_s (ptp, Command);
		sprintf (Command, "%d %d 8 [%d 0 0 %d 0 %d] ", xs, ys, xs, -ys,
			 ys); 
		psc_out_s (ptp, Command);

		psc_out_s (ptp, "{currentfile pixdata readstring pop}\n");
		psc_out_s (ptp, "false 3 colorimage\n");
	/*
	 * Data
	 */
		for (dp = (unsigned char *) data; dp < data + xs * ys; dp++)
		{
			rgb[0] = (unsigned char) (ptp->r[*dp] * 255.0);
			rgb[1] = (unsigned char) (ptp->g[*dp] * 255.0);
			rgb[2] = (unsigned char) (ptp->b[*dp] * 255.0);
			psc_out (ptp, rgb, 3);
		}
	}
	else
	{
	/*
	 * Level 2: We use "image" with a dictionary so that it uses our
	 * color map.
	 */
		psc_out_s (ptp, "<<\n");

		sprintf (Command, "/Width %d\n/Height %d\n", xs, ys);
		psc_out_s (ptp, Command);
	
		sprintf (Command, "/ImageMatrix [%d 0 0 %d 0 %d]\n", xs, -ys,
			 ys);
		psc_out_s (ptp, Command);

		psc_out_s (ptp, "/ImageType 1\n");
		psc_out_s (ptp, "/BitsPerComponent 8\n");
		psc_out_s (ptp, "/Decode [0 255]\n");
		psc_out_s (ptp, 
			"/DataSource currentfile\n");
		psc_out_s (ptp, ">> image\n");
	/*
	 * Data
	 */
		psc_out (ptp, data, xs * ys);
	}

	psc_out_s (ptp, "\n");
/*
 * Done with this scaling and translation
 */
	psc_out_s (ptp, "grestore  % end pixel image\n");
}





void
psc_poly (ctag, color, ltype, npt, data)
char *ctag;
int color, ltype, npt, *data;
/*
 * Draw a polyline with the given number of points.
 */
{
	struct psc_tag *ptp = (struct psc_tag *) ctag;
	int	point = 0;

        if (!ptp->pt_winset)
                psc_set_win (ptp);
/*
 * Check color value sanity and then set the color using # setcolor.
 * Default to black if it is out of bounds.
 */
	if (ptp->cmode)		/* only for color or gray */
	{
	    if ((color >= ptp->base) && (color <= ptp->ncolor))
		sprintf (Command, "%d sc\n", color);
	    else
		sprintf (Command, "%d sc\n", ptp->black);

	    psc_out_s (ptp, Command);
	}
/*
 * Do a setdash ('sd') and newpath ('n')
 */
	sprintf (Command, "%s 0 sd n\n", Psc_ltype[ltype]);
	psc_out_s (ptp, Command);
/*
 * position to the first point ('m' = moveto)
 */
	sprintf (Command, "%d %d m\n", data[0], data[1]);
	psc_out_s (ptp, Command);
	data += 2;
/*
 * Draw the polyline (break it into chunks of 100 points
 */
	for (point = 1; point < npt; point++)
	{
	/*
	 * Add a segment ('z' = lineto)
	 */
		sprintf (Command, "%d %d z\n", data[0], data[1]);
		psc_out_s (ptp, Command);
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
			sprintf (Command, "%d %d m\n", data[0], data[1]);
			psc_out_s (ptp, Command);
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




void
psc_clip (ctag, x0, y0, x1, y1)
char *ctag;
int x0, y0, x1, y1;
/*
 * User requested clipping rectangle
 */
{
	struct psc_tag *ptp = (struct psc_tag *) ctag;
/*
 * Save the limits of the user's clip rectangle, then establish it.
 */
	ptp->pt_cx0 = x0;
	ptp->pt_cy0 = y0;
	ptp->pt_cx1 = x1;
	ptp->pt_cy1 = y1;


	if (ptp->pt_winset)
		psc_DoClip (ptp);
}




void
psc_clear (ctag)
char *ctag;
/*
 * Don't do anything, for now.
 */
{
}




void
psc_viewport (ctag, x0, y0, x1, y1)
char *ctag;
int x0, y0, x1, y1;
{
	struct psc_tag	*ptp = (struct psc_tag *) ctag;
	int	old_x0, old_x1, old_y0, old_y1;

	old_x0 = ptp->pt_vpx0;
	old_x1 = ptp->pt_vpx1;
	old_y0 = ptp->pt_vpy0;
	old_y1 = ptp->pt_vpy1;
/*
 * Scale and translate to undo old viewport
 */
	sprintf (Command, "%.6f %.6f scale %d %d t  %% Undo old viewport\n", 
		 (float)(old_x1 - old_x0) / ptp->pt_width, 
		 (float)(old_y1 - old_y0) / ptp->pt_height, old_x0, old_y0);
	psc_out_s (ptp, Command);
/*
 * Set limits for new viewport, and establish it
 */
	ptp->pt_vpx0 = x0;
	ptp->pt_vpx1 = x1;
	ptp->pt_vpy0 = y0;
	ptp->pt_vpy1 = y1;

	if (ptp->pt_winset)
		psc_DoViewport (ptp);
}




void
psc_print (ctag)
char *ctag;
{
	struct psc_tag *ptp = (struct psc_tag *) ctag;

	if (!ptp->pt_winfilled)
		return;

	psc_finish_page (ptp);
}



void
psc_finish_page (ptp)
struct psc_tag	*ptp;
{
/*
 * Finish this page
 */
	psc_out_s (ptp, "showpage\n");
	psc_buf_out (ptp);
	PageNum++;
/*
 * Close and reopen the pipe to eject a page.
 */
	if (ptp->pt_pipe)
	{
		pclose (ptp->pt_file);
#if (defined(__SVR4) || defined(__SYSV))
                sprintf (Command, "lp -d%s", ptp->pt_devname);
#else
		sprintf (Command, "lpr -P%s", ptp->pt_devname);
#endif
		if (!(ptp->pt_file = popen (Command, "w")))
			ui_error ("Unable to open pipe '%s'", Command);
	/*
	 * Initialize and make appropriate definitions
	 */
		psc_out_s (ptp, "%!PS-Adobe-2.0\n");
		psc_def_out (ptp);
	}
/*
 * Set up for the first window of the page
 */
	ptp->pt_winfilled = FALSE;
	ptp->pt_win = 1;

	psc_init (ptp);
}




void
psc_init (ptp)
struct psc_tag *ptp;
/*
 * Initialize a page
 */
{
	sprintf (Command, "%%Page: %d\n", PageNum);
	psc_out_s (ptp, Command);
/*
 * Set scale for 100 pixels/inch and translate to center our 7.680" by
 * 10.240" graphics area.
 */
	psc_out_s (ptp, "0.72 0.72 scale\n");
	psc_out_s (ptp, "1 setlinewidth\n");

	psc_out_s (ptp, "41 38 translate\n");

	if (ptp->pt_mode == PSC_LANDSCAPE)
	{
		psc_out_s (ptp, "90 rotate\n");
		psc_out_s (ptp, "0 -768 translate\n");
	}
/*
 * Set up for the first window of the page
 */
	ptp->pt_winset = FALSE;
	ptp->pt_winfilled = FALSE;
	ptp->pt_win = 1;
}



void
psc_def_out (ptp)
struct psc_tag *ptp;
/*
 * Minimize output file size by redefining frequently used keywords.
 */
{
	psc_out_s (ptp, "/m {moveto} def\n");
	psc_out_s (ptp, "/n {newpath} def\n");
	psc_out_s (ptp, "/s {stroke} def\n");
	psc_out_s (ptp, "/sd {setdash} def\n");
	psc_out_s (ptp, "/sh {show} def\n");
	psc_out_s (ptp, "/t {translate} def\n");
	psc_out_s (ptp, "/z {lineto} def\n");

	if (ptp->pslevel == 1)
	{
		/* Build PS level 1 definition for sc */
		psc_out_s (ptp, "/sc {/INDEX exch 1 sub def\n");

		/* get the whole rgb word */
		psc_out_s (ptp, "/RGB TABLE INDEX get def\n"); 

		/* extract red component */
		psc_out_s (ptp,"/RR RGB -16 bitshift 16#ff and 255 div def\n");

		/* extract green component */
		psc_out_s (ptp, "/GG RGB -8 bitshift 16#ff and 255 div def\n");

		/* extract blue component */
		psc_out_s (ptp, "/BB RGB 16#ff and 255 div def\n");

		psc_out_s (ptp, "RR GG BB setrgbcolor} def\n");
	}
	else
		psc_out_s (ptp, "/sc {setcolor} def\n");
}




void
psc_set_win (ptp)
struct psc_tag *ptp;
/*
 * Set up to plot into a window
 */
{
	int	width, height, x0, y0;
/*
 * Save our current state
 */
	sprintf (Command, "gsave  %% Begin window %d of %d window page\n",
		 ptp->pt_win, ptp->pt_nwin);
	psc_out_s (ptp, Command);

	ptp->pt_winset = TRUE;
/*
 * Color table
 */
	if (ptp->cmode)
		psc_ctable_out (ptp);
/*
 * Translate coordinate axis
 */
	width = Res[ptp->pt_mode][ptp->pt_nwin - 1][0];
	height = Res[ptp->pt_mode][ptp->pt_nwin - 1][1];
	x0 = y0 = 0;

	switch (ptp->pt_nwin)
	{
	/*
 	 * 1 window
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
		if (ptp->pt_mode == PSC_PORTRAIT && ptp->pt_win == 1)
			y0 = height;
		else if (ptp->pt_mode == PSC_LANDSCAPE && ptp->pt_win == 2)
			x0 = width;
		break;
	/*
	 * 4 windows
	 *
	 *	---------------
	 *	|  1   |   2  |
	 *      ---------------
	 *      |  3   |   4  |
         *      ---------------
	 */
	    case 4:
		if (ptp->pt_win == 2 || ptp->pt_win == 4)
			x0 = width;

		if (ptp->pt_win == 1 || ptp->pt_win == 2)
			y0 = height;
		break;
	}

		
	sprintf (Command, "%d %d t\n", x0, y0);
	psc_out_s (ptp, Command);
/*
 * Establish viewport (which also sets clipping)
 */
	psc_DoViewport (ptp);
}	



void
psc_DoViewport (ptp)
struct psc_tag	*ptp;
/*
 * Add commands to establish viewport
 */
{
	int	x0, x1, y0, y1;

	x0 = ptp->pt_vpx0;
	x1 = ptp->pt_vpx1;
	y0 = ptp->pt_vpy0;
	y1 = ptp->pt_vpy1;
/*
 * Scale and translate for viewport
 */
	sprintf (Command, "%.6f %.6f scale %d %d t %% Establish viewport\n", 
		 (float)ptp->pt_width  / (x1 - x0), 
		 (float)ptp->pt_height / (y1 - y0), -x0, -y0);
	psc_out_s (ptp, Command);
/*
 * Redo clipping
 */
	psc_DoClip (ptp);
}



void
psc_DoClip (ptp)
struct psc_tag	*ptp;
/*
 * Add commands to establish clipping
 */
{
/*
 * Start by clipping around edges of the viewport
 *
 * Make a path around the clipping rectangle, then 'clip'
 *	'n' = newpath, 'm' = moveto, 'z' = lineto, 'c' = closepath, 'cp' = clip
 */
	psc_out_s (ptp, "initclip  % Clip for viewport, then user's clip\n");
	sprintf (Command, 
		 "n %d %d m %d %d z %d %d z %d %d z closepath clip n\n",
		 ptp->pt_vpx0, ptp->pt_vpy0, ptp->pt_vpx0, ptp->pt_vpy1, 
		 ptp->pt_vpx1, ptp->pt_vpy1, ptp->pt_vpx1, ptp->pt_vpy0);
	psc_out_s (ptp, Command);
/*
 * Finally, user's clipping
 */
	sprintf (Command, 
		 "n %d %d m %d %d z %d %d z %d %d z closepath clip n\n",
		 ptp->pt_cx0, ptp->pt_cy0, ptp->pt_cx0, ptp->pt_cy1, 
		 ptp->pt_cx1, ptp->pt_cy1, ptp->pt_cx1, ptp->pt_cy0);
	psc_out_s (ptp, Command);
}





void
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




void
psc_out (ptp, data, len)
struct psc_tag	*ptp;
char	*data;
int	len;
/*
 * Add 'len' bytes to our output buffer
 */
{
	char	*dp;
	int	nleft = len, nout;

	for (dp = data; dp < data + len; dp += DBUFLEN / 2)
	{
		nout = (nleft < DBUFLEN / 2) ? nleft : DBUFLEN / 2;

		psc_chk_buf (ptp, nout);
		memcpy (ptp->pt_bufp, dp, nout);
		ptp->pt_bufp += nout;
		nleft -= nout;
	}
}




void
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




void
psc_buf_out (ptp)
struct psc_tag *ptp;
/*
 * Write out the command buffer
 */
{
	fwrite (ptp->pt_buf, 1, ptp->pt_bufp - ptp->pt_buf, ptp->pt_file);
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

void
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


void
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
