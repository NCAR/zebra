/*
 * The structures defining a boundary file.
 */
/* $Id: BoundaryFile.h,v 1.2 1991-02-26 19:00:39 corbet Exp $ */

# define BH_MAGIC	0x910221
# define MAXBFPLAT	60
/*
 * The boundary file header looks like this:
 */
struct BFHeader
{
	int	bh_Magic;		/* Magic ID == BH_MAGIC		*/
	char	bh_Platform[MAXBFPLAT];	/* The platform stored here	*/
	int	bh_MaxBoundary;		/* Max samples/platform		*/
	time	bh_Begin;		/* Earliest time in file	*/
	time	bh_End;			/* Latest time in file		*/
	int	bh_NBoundary;		/* How many boundaries now?	*/
};

/*
 * Immediately following the header is the boundary table, describing
 * each individual boundary in the file.
 */
struct BFBTable
{
	int	bt_NPoint;		/* How many points 		*/
	time	bt_Time;		/* When is this boundary?	*/
	int	bt_Offset;		/* Where first point is in file */
};

/*
 * Points themselves are just a series of Location structures, located
 * at bt_Offset within the file.
 */
