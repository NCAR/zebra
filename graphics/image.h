/*
 * Image file stuff.
 */


/*
 * This is the image file header, which lives in the first block
 * of the file.
 */
struct if_hdr
{
	int	ih_version;	/* File format version			*/
	int	ih_nimage;	/* Number of images in this file	*/
	int	ih_toc;		/* Offset to the table of contents	*/
	int	ih_toc_size;	/* Number of TOC entries allocated	*/
	int	ih_ffb;		/* First free block of space.		*/
};

/*
 * This is the current version number.
 */
# define IF_VERSION	0x01020304

/*
 * An entry in the table of contents looks like this:
 */
# define DESCSIZE	120
struct if_toc
{
	int	it_x, it_y;	/* The size of this image		*/
	int	it_flags;	/* Flags -- see below			*/
	int	it_offset;	/* Offset in the file			*/
	char	it_desc[DESCSIZE]; /* Image description			*/
};

/*
 * Flag bits.
 */
# define IFF_TOP	0x0001	/* This is a top-origin image		*/

/*
 * Other stuff.
 */
# define BLOCKSIZE	512
# define INITTOC	20	/* Initial number of TOC entries	*/
