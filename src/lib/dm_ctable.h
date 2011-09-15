#ifndef _dm_ctable_h_
#define _dm_ctable_h_

/*
 * A color table reply.
 */
struct dm_ctable
{
	int dmm_type;		/* == DM_TABLE or DM_NOTABLE		*/
	char dmm_table[PDLEN];	/* The name of the table again		*/
	int dmm_ncolor;		/* The length of the color table	*/
	XColor dmm_cols[1];	/* The array of color values		*/
};

struct dm_ctable *dm_ColorTable FP ((char *name));

#endif /* _dm_ctable_h_ */
