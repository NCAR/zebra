/*
 * The definition of the transparent data class sample attribute interface.
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
# include <stdio.h>
# include <string.h>

# include <defs.h>
# include <message.h>
# include "DataStore.h"
# include "DataChunkP.h"

RCSID ("$Id: dc_TrAttr.c,v 3.2 1996-12-06 00:40:48 granger Exp $")

/* XXX Make sure any other transparent ADE keys are less than ST_ATTR! */
# define ST_ATTR	1000	/* Per-sample attributes		*/



void
dc_SetSampleAttrArray (dc, sample, key, type, nval, values)
DataChunk *dc;
int sample;
char *key;
DC_ElemType type;
int nval;
void *values;
{
	if (! dc_ReqSubClass (dc, DCP_Transparent, "SetSampleAttrArray"))
		return;
	dca_AddAttrArray (dc, DCP_Transparent, ST_ATTR + sample, 
			  key, type, nval, values);
}




void *
dc_GetSampleAttrArray (dc, sample, key, type, nval)
DataChunk *dc;
int sample;
char *key;
DC_ElemType *type;
int *nval;
{
	void *values;

	if (! dc_ReqSubClass (dc, DCP_Transparent, "GetSampleAttrArray"))
		return NULL;
	if ((values = dca_GetAttrArray (dc, DCP_Transparent, ST_ATTR + sample, 
					key, type, nval)))
		return (values);
	return (dc_GetGlobalAttrArray (dc, key, type, nval));
}




int
dc_ProcSampleAttrArrays (dc, sample, pattern, func, arg)
DataChunk *dc;
int sample;
char *pattern;
int (*func) (/* char *key, void *vals, int nval, DC_ElemType, void *arg */);
void *arg;
{
	if (! dc_ReqSubClass (dc, DCP_Transparent, "ProcSampleAttrArrays"))
		return (0);
	return (dca_ProcAttrArrays (dc, DCP_Transparent, ST_ATTR + sample,
				    pattern, func, arg));
}




int
dc_GetNSampleAttrs (dc, sample)
DataChunk *dc;
int sample;
{
	if (! dc_ReqSubClass (dc, DCP_Transparent, "GetNSampleAttrs"))
		return (0);
	return (dca_GetNAttrs (dc, DCP_Transparent, ST_ATTR + sample));
}




void
dc_SetSampleAttr (dc, sample, key, value)
DataChunk *dc;
int sample;
char *key, *value;
/*
 * Add a per-sample attribute to this data chunk.
 */
{
	if (! dc_ReqSubClass (dc, DCP_Transparent,"SetSampleAttr"))
		return;
	dca_AddAttr (dc, DCP_Transparent, ST_ATTR + sample, key, value);
}



void
dc_RemoveSampleAttr (dc, sample, key)
DataChunk *dc;
int sample;
char *key;
/*
 * Remvoe a per-sample attribute from this data chunk.
 */
{
	if (! dc_ReqSubClass (dc, DCP_Transparent, "RemoveSampleAttr"))
		return;
	dca_RemoveAttr (dc, DCP_Transparent, ST_ATTR + sample, key);
}




char *
dc_GetSampleAttr (dc, sample, key)
DataChunk *dc;
int sample;
char *key;
/*
 * Look up a per-sample attribute.
 */
{
	char *value;

	if (! dc_ReqSubClass (dc, DCP_Transparent,"GetSampleAttr"))
		return(NULL);
	if ((value = dca_GetAttr (dc, DCP_Transparent, ST_ATTR + sample, key)))
		return (value);
	return (dc_GetGlobalAttr (dc, key));
}




void *
dc_GetSaAttrBlock (dc, sample, len)
DataChunk *dc;
int sample, *len;
/*
 * Get the per-sample attributes out as an opaque chunk.
 */
{
	if (! dc_ReqSubClass (dc, DCP_Transparent, "GetSaAttrBlock"))
		return(NULL);
	return (dca_GetBlock (dc, DCP_Transparent, ST_ATTR + sample, len));
}



void
dc_SetSaAttrBlock (dc, sample, block, len)
DataChunk *dc;
void *block;
int sample, len;
/*
 * Store a per-sample attribute block back.
 */
{
	if (! dc_ReqSubClass (dc, DCP_Transparent, "SetSaAttrBlock"))
		return;
	dca_PutBlock (dc, DCP_Transparent, ST_ATTR + sample, block, len);
}




char **
dc_GetSampleAttrList(dc, sample, pattern, values, natts)
DataChunk *dc;
int sample;
char *pattern;
void **values[];
int *natts;
{
	if (! dc_ReqSubClass (dc, DCP_Transparent, "GetSampleAttrList"))
		return (NULL);
	return(dca_GetAttrList(dc, DCP_Transparent, ST_ATTR + sample,
			       pattern, values, natts));
}



char **
dc_GetSampleAttrKeys (dc, sample, natts)
DataChunk *dc;
int sample;
int *natts;
{
/*
 * Returns a list of keys for the sample attributes for this sample.
 * Also puts into natt the number of global attributes for this dc.
 * The returned array of attribute keys is only valid until the next call
 * of any of the Get*AttrList or Get*AttrKeys functions.
 */
	if (! dc_ReqSubClass (dc, DCP_Transparent, "GetSampleAttrKeys"))
		return (NULL);
	return(dca_GetAttrList(dc, DCP_Transparent, ST_ATTR + sample,
			       NULL, NULL, natts));
}

 

