/*
 * The first shot at an image format.  What we do here is implement enough 
 * to take over from the old data object format, but no more.  Future nifty
 * stuff could include:
 *
 *	- Three dimensional image data
 *	- Compressed images
 */
/*		Copyright (C) 1987,88,89,90,91,92 by UCAR
 *	University Corporation for Atmospheric Research
 *		   All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced
 * or used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular purpose.
 * UCAR does not indemnify any infringement of copyright, patent, or trademark
 * through use or modification of this software.  UCAR does not provide 
 * maintenance or updates for its software.
 */


# include <defs.h>
# include <message.h>
# include "DataStore.h"
# include "ds_fields.h"
# include "DataChunk.h"
# include "DataChunkP.h"
MAKE_RCSID ("$Id: dc_Image.c,v 1.5 1994-01-03 07:18:03 granger Exp $")

# define SUPERCLASS DCC_RGrid
# define CLASSDEPTH 4

/*
 * Our class-specific AuxData structure types.
 */
# define ST_SCALING	1



/*
 * Local routines.
 */
static DataChunk *dc_ImgCreate FP((DataClass));

RawDCClass ImageMethods =
{
	"Image",
	SUPERCLASS,		/* Superclass			*/
	CLASSDEPTH,		/* Depth, Raw = 0		*/
	dc_ImgCreate,
	InheritMethod,		/* No special destroy		*/
	0,			/* Add??			*/
	0,		/* Dump				*/
};





static DataChunk *
dc_ImgCreate (class)
DataClass class;
/*
 * Create a chunk of this class.
 */
{
	DataChunk *dc;
/*
 * The usual.  Make a superclass chunk and tweak it to look like us.  We don't
 * add any info here, because we don't know it yet.
 */
	dc = DC_ClassCreate (SUPERCLASS);
	dc->dc_Class = class;
	return (dc);
}






void
dc_ImgSetup (dc, nfld, fields, scale)
DataChunk *dc;
int nfld;
FieldId *fields;
ScaleInfo *scale;
/*
 * Initialize this RGrid data chunk.
 * Entry:
 *	DC	is a new data chunk which is a subclass of DCC_IRGrid.
 *	NFLD	is the number of fields to be stored in this DC.
 *	FIELDS	is the list of those fields.
 *	SCALE	is the scaling information for each field.  Note that all
 *		samples in this chunk must use the same scale.
 * Exit:
 *	The data chunk has been set up.
 */
{
	ScaleInfo *sc;
	int i;
/*
 * Checking time.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Image, "ImgSetup"))
		return;
/*
 * Do the RGrid setup.  Set all of our types to unsigned char so that the
 * sample size can be correctly calculated by superclasses, and prevent further
 * changes of the element type.
 */
	dc_RGSetup (dc, nfld, fields);
	for (i = 0; i < nfld; ++i)
		dc_SetType (dc, fields[i], DCT_UnsignedChar);
	dc_BlockChanges (dc);
/*
 * Tack on our scalinfo structure.
 */
	sc = (ScaleInfo *) malloc (nfld * sizeof (ScaleInfo));
	memcpy (sc, scale, nfld * sizeof (ScaleInfo));
	dc_AddADE (dc, sc, DCC_Image, ST_SCALING, nfld*sizeof (ScaleInfo),
			TRUE);
}





void
dc_ImgAddImage (dc, sample, field, origin, rg, t, data, len)
DataChunk *dc;
int sample, len;
FieldId field;
Location *origin;
RGrid *rg;
ZebTime *t;
unsigned char *data;
/*
 * Add this field to the DC.
 * Entry:
 *	DC	is the data chunk to be modified.
 *	SAMPLE	is the sample to be added.
 *	FIELD	is the field ID for this grid.
 *	ORIGIN	is the origin of the grid.
 *	RG	is the spacing information
 *	T	is the time of this data
 *	DATA	is the actual data
 *	LEN	is the length of the grid data, IN BYTES.  If it is specified
 *		as zero, the length will be calculated from the dimensions.
 * Exit:
 *	The grid has been added.
 */
{
/*
 * Checking time.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Image, "AddImage"))
		return;
/*
 * Add the info.
 */
	if (len == 0)
		len = rg->rg_nX*rg->rg_nY;
	dc_RGAddGrid (dc, sample, field, origin, rg, t, (void *) data, len);
}





unsigned char *
dc_ImgGetImage (dc, sample, field, origin, rg, len, scale)
DataChunk *dc;
int sample, *len;
FieldId field;
Location *origin;
RGrid *rg;
ScaleInfo *scale;
/*
 * Retrieve an image from this DC.
 */
{
	void *gret;
	ScaleInfo *sc;
	int findex, nfield;
	FieldId *flds;
/*
 * Checking time.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Image, "GetImage"))
		return (NULL);
/*
 * Find the data itself.
 */
	if (! (gret = dc_RGGetGrid (dc, sample, field, origin, rg, len)))
		return (NULL);
	if (! scale)
		return ((unsigned char *) gret);
/*
 * Find the scale info.
 */
	if (! (sc = (ScaleInfo *) dc_FindADE (dc, DCC_Image, ST_SCALING,NULL)))
	{
		msg_ELog (EF_PROBLEM, "Somebody swiped the scale info");
		return ((unsigned char *) gret);
	}
/*
 * Find the index of this field.
 */
	flds = dc_GetFields (dc, &nfield);
	for (findex = 0; findex < nfield; findex++)
		if (flds[findex] == field)
			break;
	if (findex >= nfield)
		msg_ELog (EF_PROBLEM, "Strange...field %s vanished",
				F_GetName (field));
	else
		*scale = sc[findex];
	return ((unsigned char *) gret);
}
