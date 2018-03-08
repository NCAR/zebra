/* 1/85 jc */
/*
 * Interpretation info for .pxl files.
 */

/*
 * A font directory entry.
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

/*
 * Params of interest.
 */
# define FONT_ID	1001
