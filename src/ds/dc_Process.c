/*
 * Detail-handling and generic datachunk processing.
 */
/*		Copyright (C) 1987-1996 by UCAR
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

# include <sys/types.h>
# include <string.h>

# include <defs.h>
# include <message.h>
# include "DataStore.h"
# include "DataChunkP.h"
# include <zl_regex.h>

RCSID ("$Id: dc_Process.c,v 3.1 1996-11-19 09:17:14 granger Exp $")


static int NDefaultDetail = 0;
static dsDetail *DefaultDetails = NULL;


/*
 * As a rule, these process routines take a datachunk and some arguments
 * and return an integer: zero if successful and nonzero otherwise.
 */

/*
 * Some candidates for datachunk detail handling and post-processing:
 *
 * bad value conversion (one to another, many to one)
 * units conversion (one field or all compatible fields)
 * type conversion (all fields to one type, one type to another)
 */

static int dc_ApplyBadval FP ((DataChunk *dc, float target));


void
ds_SetDefaultDetails (details, ndetail)
dsDetail *details;
int ndetail;
/*
 * 'details' must point to valid memory (or be NULL) for as long as the
 * defaults will be used.
 */
{
	NDefaultDetail = ndetail;
	DefaultDetails = details;
}


/*
 * For file-format-independent detail handling:
 */


int 
dc_ProcessDetails (dc, details, ndetail)
DataChunk *dc;
dsDetail *details;
int ndetail;
/*
 * Handle all recognized details which we can process here at the
 * datachunk level.
 */
{
	float bv;

	if (ds_GetFloatDetail (DD_FETCH_BADVAL, details, ndetail, &bv))
	{
		dc_ApplyBadval (dc, bv);
	}
	else if (ds_GetFloatDetail (DD_FETCH_BADVAL, DefaultDetails, 
			       NDefaultDetail, &bv))
	{
		dc_ApplyBadval (dc, bv);
	}
	return (0);
}



static int
dc_ApplyBadval (dc, target)
DataChunk *dc;
float target;
/*
 * For each field in the datachunk, test it's bad value against the
 * requested value, and if not equal convert all of that field's values
 * to the requested value.  For now we only support floating point fields.
 */
{
	int nfield;
	int i;
	FieldId *fields;
	int nsample = dc_GetNSample (dc);

	if (! dc_IsSubClass (dc->dc_ClassP, DCP_MetData))
		return (0);
	fields = dc_GetFields (dc, &nfield);
	for (i = 0; i < nfield; ++i)
	{
		float badval;
		float *data;
		int len;
		int j, k;

		if (dc_Type (dc, fields[i]) != DCT_Float)
			continue;
		if ((badval = dc_GetFloatBadval (dc, fields[i])) == target)
			continue;
		for (j = 0; j < nsample; ++j)
		{
			data = (float *)dc_GetMData (dc, j, fields[i], &len);
			for (k = 0; k < len/sizeof(float); ++k)
			{
				if (data[k] == badval)
					data[k] = target;
			}
		}
		dc_SetFieldBadval (dc, fields[i], (void *)&target);
	}
	/* Make note of the new global bad value in the datachunk */
	dc_SetBadval (dc, target);
	return (0);
}



#ifdef notyet
/*
 * Figure out what bad value flag they want, and make sure it is stored
 * in the DC.
 */
#ifdef READ_BADVALUE_ATT
	if (dc->dc_Class != DCC_Location)
	{
		badval = ds_GetDetail ("badval", details, ndetail, &v) ?
				v.us_v_float : 99999.9;
		dc_SetBadval (dc, badval);
	}
#endif /* READ_BADVALUE_ATT */
#ifdef APPLY_BADVALUE
		else if (dc_Type (dc, fids[field]) == DCT_Float)
			dnc_ApplyBadval (tag, vfield, dc, fids[field],
					 badval, temp, nsamp);
#endif /* APPLY_BADVALUE */


#ifdef APPLY_BADVALUE
/*
 * The attributes we check for to find a bad value.  The
 * 'MissingValue' string is for RAF ADS netCDF files.
 */

static char *BadValueAtts[] = { 
	VATT_MISSING, "MissingValue", "bad_value_flag", "bad_value", NULL
};

static void
dnc_ApplyBadval (tag, vfield, dc, fid, badval, data, ndata)
NCTag *tag;
int vfield;
DataChunk *dc;
FieldId fid;
float badval, *data;
int ndata;
/*
 * Turn the bad value flag stored with the data, if any, into the user-
 * supplied one.  If no bad value flag, no conversion is done.
 */
{
	float ncbadval;
	nc_type atype;
	char buf[256];
	char **flag;
	int len;
	int i;

	/*
	 * If the att is not a float already, try to convert it to a string
	 * first (from whatever type it is) and then convert to a float.
	 */
	for (flag = BadValueAtts; *flag != NULL; ++flag)
	{
		if (ncattinq (tag->nc_id, vfield, *flag, &atype, &len) >= 0)
			break;
	}
	if (*flag == NULL)
		return;
	if (len*nctypelen(atype) > 255)
		return;
	if (ncattget (tag->nc_id, vfield, *flag, (void *)buf) < 0)
		return;
	if (atype == NC_FLOAT)
		ncbadval = *(float *)buf;
	else
		ncbadval = atoi(dnc_ValueToString((void *)buf, atype, len));
	if (ncbadval != badval)
	{
		for (i = 0; i < ndata; i++)
			if (data[i] == ncbadval)
				data[i] = badval;
		dc_SetFieldAttr (dc, fid, VATT_MISSING,
			 dnc_ValueToString ((void *)&badval, NC_FLOAT, 1));
	}
}
#endif /* APPLY_BADVALUE */




#endif /* notyet */
