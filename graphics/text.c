/* 5/87 jc */
/*
 * Routines for performing text operations.
 */
# include <math.h>
# include "param.h"
# include "graphics.h"
# include "workstation.h"
# include "device.h"
# include "oplist.h"
# include "overlay.h"
# include "pixel.h"

static char *rcsid = "$Id: text.c,v 1.11 2002-07-11 23:09:45 burghart Exp $";

# ifdef WORDS_BIGENDIAN
#	define SWAP(v) (v)
# else
#	define SWAP(v) swap4 (v)
# endif

# define DEG_TO_RAD(x)	((x) * 0.017453292)

/*
 * A font directory entry.  (For TeX fonts)
 */
struct font_dir
{
	short	fd_pheight;	/* Character pixel height	*/
	short	fd_pwidth;	/* Character pixel width	*/
	short	fd_ref_y;	/* Reference point Y coordinate */
	short	fd_ref_x;	/* Reference point X coordinate */
	int	fd_rastor;	/* Offset to raster info	*/
	int	fd_tfm_width;	/* The TFM width of a character */
};

extern short *Gt_sf_0[128], *Gt_sf_1[128];
extern struct font_dir Pf0_dir[];
extern int Pf0_rasters[];

/*
 * The description of a text font.
 */
# define MAXFONT	20
static struct font
{
	int	f_type;		/* Font type -- see below		*/
	short	**f_data;	/* Stroke table (vector font)		*/
	int	*f_rast;	/* Rastor information			*/
	struct font_dir	*f_fdir;	/* Font directory		*/
} F_table[MAXFONT] = 
{
/*
 * The old GKS stroke fonts.
 */
	{	GFT_STROKE,	Gt_sf_0,	___,	___	},
	{	GFT_STROKE,	Gt_sf_1,	___,	___	},
	{	GFT_PIXEL,	___,	Pf0_rasters,   Pf0_dir  },
	{	GFT_DEV,	___,		___,	___	},
};

static int N_font = 4;

/*
 * Forwards
 */
void gt_pixel(), gt_px_map(), gt_pix_dim(), gt_stroke();




void
gt_get_start (x, y, font, scale, hjust, vjust, rot, aspect, text, 
	sx, sy, ex, ey, dev, tag)
int x, y, font, hjust, vjust, *sx, *sy, *ex, *ey;
float scale, rot, aspect;
char *text, *tag;
struct device *dev;
/*
 * Figure out the start point for a line of text.
 * Entry:
 *	X, Y	is the user-given start point, in device coordinates.
 *	FONT	is the ID of the font to use.
 *	SCALE	is the scaling of the text.
 *	HJUST	is the horizontal justification.
 *	VJUST	is the vertical justification.
 *	ROT	is the rotation in degrees counterclockwise from horizontal.
 *	ASPECT	is the pixel aspect ratio of the device being used.
 *	TEXT	is the actual text string.
 * Exit:
 *	SX, SY	is the real start point, with justification, scaling and
 *		rotation taken into account.
 *	EX, EY	is the ending point
 */
{
	int	cheight, desc, width;
	short	*cp;
	float	xoffset, yoffset, fsx, fsy;
	float	cos_rot = (float) cos (DEG_TO_RAD (rot));
	float	sin_rot = (float) sin (DEG_TO_RAD (rot));
/*
 * Get our character size info.
 */
 	if (F_table[font].f_type == GFT_PIXEL)
	{
		scale = 1.0;	/* Pixel fonts aren't scalable */
		gt_pix_dim (font, text, &width, &cheight, &desc);
	}
	else if (F_table[font].f_type == GFT_DEV)
	{
		(*dev->gd_tsize) (tag, (int) scale, rot, text, &width,
				&cheight, &desc);
		scale = 1;
	}
	else
	{
	/*
	 * Get the character height.  This particular measure, which includes 
	 * descenders, may be excessive, but we'll try it.
	 */
		cp = F_table[font].f_data[0];
	 	cheight = cp[0] - cp[3];
		desc = cp[2] - cp[3];	/* Descender size */
	   	width = gt_line_width (text, font);
	}
/*
 * Find the y offset for the given justification (ignoring rotation for now)
 */
 	switch (vjust)
	{
	  case GT_BOTTOM:
		yoffset = 0.0;
		break;
	  case GT_BASELINE:
		yoffset = scale * (float) desc + 0.5;
		break;
	  case GT_TOP:
		yoffset = scale * (float)(cheight - 1);
		break;
	  case GT_CENTER:
		yoffset = scale * (float)((cheight / 2) - 1);
		break;
	}
/*
 * Find the x offset for the given justification (ignoring rotation for now)
 */
 	switch (hjust)
	{
	   case GT_LEFT:
		xoffset = 0.0;
		break;
	   case GT_RIGHT:
		xoffset = scale * (float) width;
		break;
	   case GT_CENTER:
		xoffset = scale * (float) width / 2.0;
		break;
	}
/*
 * Find the starting points, putting in the necessary rotation
 */
	fsx = x - (cos_rot * xoffset - sin_rot * yoffset);
	fsy = y - (sin_rot * aspect * xoffset + cos_rot * yoffset);

	*sx = (int)(fsx + 0.5);
	*sy = (int)(fsy + 0.5);
  
/*
 * Find the ending points
 */
	*ex = (int)(fsx + (cos_rot * scale * width - 
		sin_rot * scale * (cheight - 1)) + 0.5);
	*ey = (int)(fsy + (sin_rot * aspect * scale * width + 
		cos_rot * scale * (cheight - 1)) + 0.5);
}



int
gt_line_width (text, font)
char *text;
int font;
/*
 * Return the pixel width of an unscaled line of text.
 */
{
	int len = 0;
	short *cp;

	while (*text)
	{
		cp = F_table[font].f_data[*text++];
		len += cp[3] - cp[2];
	}
	return (len);
}



void
gt_do_text (wstn, ov, color, x, y, font, scale, rot, text, dev)
struct workstation *wstn;
struct overlay *ov;
int color, x, y, font, scale, dev;
char *text;
float	rot;
/*
 * Actually perform a text operation.
 */
{
	switch (F_table[font].f_type)
	{
	   case GFT_STROKE:
		gt_stroke (wstn, ov, color, x, y, font, scale, rot, text, dev);
		break;
	   case GFT_DEV:
	   	if (! dev)
			c_panic ("DEV graphics call not to device");
	   	(*wstn->ws_dev->gd_text) (wstn->ws_tag, x, y, color,
			scale/100, rot, text);
		break;
	   case GFT_PIXEL:
		gt_pixel (wstn, ov, color, x, y, font, text, dev);
		break;
	}
}




void
gt_pixel (wstn, ov, color, x, y, font, text, dev)
struct workstation *wstn;
struct overlay *ov;
int color, x, y, font, dev;
char *text;
/*
 * Put out pixel text.
 */
{
	int xd, yd, baseline, px, row, col;
	char *data, c;
	int *rastp;
/*
 * Get the dimensions of this string in pixel space.  Since some devices
 * are unhappy about odd dimensions, we will just round both dimensions
 * up to even numbers.
 */
	gt_pix_dim (font, text, &xd, &yd, &baseline);
	xd = (xd + 1) & ~0x1;
	yd = (yd + 1) & ~0x1;
/*
 * Check against the clip window.  If this text is entirely without the
 * window, we just drop the operation on the floor.
 */
 	if ((x + xd) <= ov->ov_cx0 || x > ov->ov_cx1 ||
	    (y + yd) <= ov->ov_cy0 || y > ov->ov_cy1)
		return;
/*
 * Allocate enough memory to hold the pixel info.
 */
 	data = getvm (xd * yd);
	memset (data, wstn->ws_dev->gd_background, xd * yd);
/*
 * Now it's time to actually bitmap the data.
 */
	px = 0;
	for (c = *text++; c; c = *text++)
	{
		struct font_dir *dir = F_table[font].f_fdir + c;
	/*
	 * Handle blanks separately, since they are not actually part of
	 * the font file.  Instead, just advance by the width of the
	 * character "x".
	 */
	 	if (c == ' ')
		{
			px += F_table[font].f_fdir['x'].fd_pwidth;
			continue;
		}
	/*
	 * Everything else is a little harder.  Find the rastor info, then
	 * pass through each rastor line.
	 */
		rastp = F_table[font].f_rast + dir->fd_rastor;
		for (row = 0; row < dir->fd_pheight; row++)
		{
			int memline = baseline + dir->fd_ref_y - row;
			gt_px_map (data + memline*xd + px, rastp,
				dir->fd_pwidth, color,
				wstn->ws_dev->gd_background);
			rastp += (dir->fd_pwidth + 31)/32;
		}
	/*
	 * Advance our X position, taking into account a negative reference
	 * point.
	 */
		px += dir->fd_pwidth;
		if (dir->fd_ref_x < 0)
			px -= dir->fd_ref_x;
	}
/*
 * If the text does not lie entirely with the clip window, trim it down
 * to something that does fit.
 */
	if ((ov->ov_ws->ws_dev->gd_flags & GDF_HCW) == 0 && (x < ov->ov_cx0 ||
			y < ov->ov_cy0 || (x + xd) > ov->ov_cx1 ||
			(y + yd) > ov->ov_cy1))
	{
		int nx0, ny0, nx1, ny1;
		char *new = gp_carve_window (data, x, y, x + xd - 1,
			y + yd - 1, ov->ov_cx0, ov->ov_cy0, ov->ov_cx1,
			ov->ov_cy1, &nx0, &ny0, &nx1, &ny1);
		relvm (data);
		data = new;
		x = nx0; y = ny0;
		xd = nx1 - nx0 + 1;  yd = ny1 - ny0 + 1;
	}
/*
 * Finally, ship the data out to the device.
 */
	if (dev)
	{
		if (wstn->ws_dev->gd_flags & GDF_TOP)	/* XXX */
			gp_invert (data, xd, yd);
		(*wstn->ws_dev->gd_pixel) (wstn->ws_tag, x, y, xd, yd, data,
				GP_BYTE, GPO_RASTOR);
	}
	else if (ov->ov_flags & OVF_PIXMAP)
		gp_insert (data, GP_BYTE, xd, yd, ov->ov_pmap, x, y);
	else
		c_panic ("I can't hack oplist inserts yet");
	relvm (data);
}



void
gt_px_map (dest, rastp, width, color, bg)
char *dest;
int *rastp, width, color, bg;
/*
 * Pixel-map one line of rastor data.
 */
{
	int bit, nword = (width + 31)/32;
# ifdef WORDS_BIGENDIAN
	static char stbl[32] = { 7, 6, 5, 4, 3, 2, 1, 0, 15, 14, 13, 12, 11,
		10, 9, 8, 23, 22, 21, 20, 19, 18, 17, 16, 31, 30, 29, 28,
		27, 26, 25, 24 };
# endif

	for (; nword > 0; nword--)
	{
		int rdata = SWAP (*rastp++), ndo = (width > 32) ? 32 : width;
		for (bit = 0; bit < ndo; bit++)
# ifdef WORDS_BIGENDIAN
			*dest++ = (rdata & (1 << stbl[bit])) ? color : bg;
# else
			*dest++ = (rdata & (1 << (31 - bit))) ? color : bg;
# endif
		width -= 32;
	}
}




void
gt_pix_dim (font, text, xd, yd, baseline)
int font;
char *text;
int *xd, *yd, *baseline;
/*
 * Return the dimensions of this text array.
 */
{
	int height = 0, below = 0, width = 0;
	struct font_dir *fp = F_table[font].f_fdir;

	while (*text)
	{
		char c = *text++;
		int b;
	/*
	 * If this is a blank, give it the width of an "x".
	 */
	 	if (c == ' ')
		{
			width += fp['x'].fd_pwidth;
			continue;
		}
	/*
	 * Increment the width by the width of this character.  If the
	 * X reference point is negative (the usual case), widen the
	 * string by that much more.
	 */
		width += fp[c].fd_pwidth;
		if (fp[c].fd_ref_x < 0)
			width -= fp[c].fd_ref_x;
	/*
	 * Figure out the height above and below the baseline.
	 */
	 	if (fp[c].fd_ref_y + 1 > height)
			height = fp[c].fd_ref_y + 1;
		b = fp[c].fd_pheight - fp[c].fd_ref_y - 1;
		if (b > below)
			below = b;
# ifdef notdef
	printf ("%c: cd %d x %d, ref (%d,%d), w: %d, h: %d, b: %d\n", c,
		fp[c].fd_pwidth, fp[c].fd_pheight, fp[c].fd_ref_x,
		fp[c].fd_ref_y, width, height, below);
# endif
	}
	*xd = width;
	*yd = height + below;
	*baseline = below;
}




void
gt_stroke (wstn, ov, color, x, y, font, scale, rot, text, dev)
struct workstation *wstn;
struct overlay *ov;
int color, x, y, font, scale, dev;
char *text;
float rot;
/*
 * Handle the actual generation of stroke text.
 */
{
	int	pdata[200], *pdp, cbase;
	short	*cp;
	float	xpos = (float) x, ypos = (float) y, del_x, del_y;
	float	cos_rot = (float) cos (DEG_TO_RAD (rot));
	float	sin_rot = (float) sin (DEG_TO_RAD (rot));
	float	aspect = ov->ov_ws->ws_dev->gd_aspect;
/*
 * Get the character height.  This particular measure, which includes 
 * descenders, may be excessive, but we'll try it.
 */
	cp = F_table[font].f_data[0];
 	cbase = cp[3];
/*
 * Now step through each character.
 */
 	while (*text)
	{
		int npoint = 0, npair;
		short *point;
	/*
	 * Locate the vector info.
	 */
	 	cp = F_table[font].f_data[*text++];
		point = cp + 4;
	/*
	 * Special case for blanks -- don't draw any lines.
	 */
		if (text[-1] == ' ')
		{
		 	xpos += cos_rot * ((cp[3] - cp[2])*scale)/100.0;
			ypos += sin_rot * ((cp[3] - cp[2])*scale)/100.0;
			continue;
		}
	/*
	 * Now step through and draw the lines.
	 */
		pdp = pdata;
	 	for (npair = 0; npair < cp[0]; npair++)
		{
		/*
		 * Check for the end of a group of lines, and put out our
		 * info if this is such an end.
		 */
			if (*point == -128)
			{
				if (! dev)
				{
					if (ov->ov_flags & OVF_PIXMAP)
						gp_pl (ov, color, GPLT_SOLID,
							npoint, pdata);
					else
						gc_pl_clip(ov,color,GPLT_SOLID,
							npoint, pdata, FALSE);
				}
				else /* if (wstn->ws_dev->gd_flags & GDF_HCW)*/
					(*wstn->ws_dev->gd_polyline)
						(wstn->ws_tag, color,
						GPLT_SOLID, npoint, pdata);
				npoint = 0;
				pdp = pdata;
				point += 2;
			}
		/*
		 * Otherwise throw in another set of points.
		 */
		 	else
			{
			/*
			 * Delta position (unrotated)
			 */
				del_x = (*point++ - cp[2]) * scale / 100.0;
				del_y = (*point++ - cbase) * scale / 100.0;
			/*
			 * New point, with rotation
			 */
				*pdp++ = (int) (xpos + del_x * cos_rot -
					del_y * sin_rot);
				*pdp++ = (int) (ypos + del_x * sin_rot + 
					del_y * cos_rot);

				npoint++;
			}
		}
	/*
	 * Advance the position to the next character slot.
	 */
	 	xpos += cos_rot * ((cp[3] - cp[2])*scale)/100.0;
	 	ypos += sin_rot * aspect * ((cp[3] - cp[2])*scale)/100.0;
	}
}
		

int
gt_f_height (font)
int font;
/*
 * Return the nominal height, in pixels, of this font.
 */
{
	short *cp;

	if (F_table[font].f_type == GFT_STROKE)
	{
		cp = F_table[font].f_data[0];
	 	return (cp[0] - cp[3]);
	}
	else if (F_table[font].f_type == GFT_DEV)
		return (1);
	else
		return (F_table[font].f_fdir['('].fd_pheight);
}




int
gt_load_font (file)
char *file;
/*
 * Load up a TeX font, returning a font descriptor.
 */
{
	int fd, len, *ip, nread = 0, fontno, *ifont, i;
	char *data, *dp;
/*
 * Try to open the file.
 */
 	if ((fd = open (file, 0)) < 0)
		return (GE_BAD_FILE);
/*
 * Figure out how big it is, allocate memory, and read it in.
 */
	len = lseek (fd, (long) 0, 2);
	dp = data = getvm (len);
	lseek (fd, 0, 0);
	while ((nread += read (fd, dp, len)) < len)
		;
/*
 * Allocate a font table entry.
 */
 	fontno = N_font++;
	F_table[fontno].f_type = GFT_PIXEL;
/*
 * Search back for the font ID, and then the actual font directory.
 */
	len = len/4 - 1;
	for (ip = (int *) data; ip[len] == 0; len--)
		;
	F_table[fontno].f_fdir = (struct font_dir *) (ip + SWAP (ip[len-1]));
/*
 * Now go through and swap all of the data in the font directory, since
 * we will be referring to it often.
 */
	ifont = (int *) F_table[fontno].f_fdir;
	for (i = 0; i < 512; i++)
		ifont[i] = SWAP (ifont[i]);
# ifdef notdef
	for (i = 'a'; i < 'm'; i++)
	{
		struct font_dir *fp = F_table[fontno].f_fdir + i;
		printf ("%c:\th: %d, w: %d, ref (%d,%d), rast %d\n", i,
			fp->fd_pheight, fp->fd_pwidth, fp->fd_ref_x,
			fp->fd_ref_y, fp->fd_rastor);
	}
# endif
/*
 * Store the rest of the info away.
 */
 	F_table[fontno].f_rast = ip;
/*
 * Close the file and return our info.
 */
 	close (fd);
	return (fontno);
}





int
gt_font_type (font)
int font;
/*
 * Return the integer type of this font.
 */
{
	return (F_table[font].f_type);
}
