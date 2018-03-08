/* 7/87 jc */
/*
 * This file contains definitions pertaining to pixel-mapped operations.
 */

/*
 * Pixel maps are stored in a slightly nonstandard way.  While storage
 * as straight rastor info may seem appealing, it turns out to be better
 * to keep this data in rectangular chunks.  This scheme helps to cut
 * back on page faulting, and also helps in partial updates.
 *
 * The actual block size is device dependant, and is found in the actual
 * device structure.
 *
 * Data within a block is stored as rastor lines.  Since some displays
 * like to put the origin on top, and others on the bottom, we are preprepared
 * to stack rastors from the top down, or the reverse.
 */

/*
 * This is the structure describing a pixel map.
 */
struct pixmap
{
	unsigned char *pm_data;		/* The actual pixel data	*/
	int pm_org;			/* The data organization	*/
	int pm_xb, pm_yb;		/* The X and Y block dimensions */
	int pm_xres, pm_yres;		/* The full size		*/
	int pm_bsize;			/* Size of a block		*/
	int pm_nxb;			/* Number of X blocks		*/
	char *pm_mod;			/* Block modified flag vector	*/
	int pm_flags;			/* Flag word.			*/
	int *pm_xmap, *pm_ymap;		/* X and Y address maps		*/
	int pm_bg;			/* Background color		*/
};
typedef struct pixmap *pixel_map;

/*
 * Flags.
 */
# define PF_TOP		0x0001		/* Organized from top down	*/
# define PF_ENTIRE	0x0002		/* Entire map modified		*/

/*
 * Pixel storing operations.
 */

# define STORE_PXL(pm,x,y,value) \
	{ 	register int __idx__ = (pm)->pm_xmap[x] + (pm)->pm_ymap[y]; \
		(pm)->pm_data[__idx__] = (value); \
		(pm)->pm_mod[__idx__/(pm)->pm_bsize] = TRUE; \
	}

# define GET_PXL(pm,x,y,value) \
	value = (pm)->pm_data[(pm)->pm_xmap[x] + (pm)->pm_ymap[y]];


/*
 * Routines.
 */
struct device;
struct overlay;

struct pixmap *gp_make_pmap (struct device *dev);
void gp_pl (struct overlay *ov, int color, int ltype, int npt, int *ndata);
char *gp_carve_window (char *old, int x0, int y0, int x1, int y1, int cx0,
	int cy0, int cx1, int cy1, int *nx0, int *ny0, int *nx1, int *ny1);
struct pixmap *G_get_pmap (overlay ov);
