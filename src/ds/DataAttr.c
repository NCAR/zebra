/*
 * DataChunk global attribute methods, public and private.
 */

/*		Copyright (C) 1987,88,89,90,91 by UCAR
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

# include <unistd.h>
# include <string.h>

# include <defs.h>
# include <message.h>
# include "DataStore.h"
# include "DataChunkP.h"

RCSID ("$Id: DataAttr.c,v 3.1 1996-11-19 08:10:37 granger Exp $")

/*
 * ADE Codes for the raw data object.
 */
# define ST_GLOBATTR	3822


void
dc_SetGlobalAttrArray (dc, key, type, nval, values)
DataChunk *dc;
char *key;
DC_ElemType type;
int nval;
void *values;
/*
 * Store a global attribute into this data chunk.
 */
{
	dca_AddAttrArray (dc, DCP_Raw, ST_GLOBATTR, key, type, nval, values);
}




void *
dc_GetGlobalAttrArray (dc, key, type, nval)
DataChunk *dc;
char *key;
DC_ElemType *type;
int *nval;
/*
 * Store a global attribute into this data chunk.
 */
{
	return (dca_GetAttrArray (dc, DCP_Raw, ST_GLOBATTR, key, type, nval));
}




void
dc_SetGlobalAttr (dc, key, value)
DataChunk *dc;
char *key, *value;
/*
 * Store a global attribute into this data chunk.
 */
{
	dca_AddAttr (dc, DCP_Raw, ST_GLOBATTR, key, value);
}



char *
dc_GetGlobalAttr (dc, key)
DataChunk *dc;
char *key;
/*
 * Look up a global attribute.
 */
{
	return (dca_GetAttr (dc, DCP_Raw, ST_GLOBATTR, key));
}




int
dc_CmpGlobalAttr (dc1, dc2, diffs, len)
DataChunk *dc1;
DataChunk *dc2;
char *diffs;
int len;
/*
 * Generate a string describing differences between global attributes.
 */
{
	return (dca_CmpAttrArrays(dc1, dc2, DCP_Raw, ST_GLOBATTR, diffs, len));
}




int
dc_ProcessAttrArrays (dc, pattern, func, arg)
DataChunk *dc;
char *pattern;
int (*func) (/* char *key, void *vals, int nval, DC_ElemType, void *arg */);
void *arg;
/*
 * Go through and look at a bunch of attributes.  If PATTERN is non-null,
 * it is a regular expression used to filter out things.
 */
{
	return (dca_ProcAttrArrays (dc, DCP_Raw, ST_GLOBATTR, 
				    pattern, func, arg));
}




void
dc_RemoveGlobalAttr (dc, key)
DataChunk *dc;
char *key;
/*
 * Remove this global attribute key from this data chunk.
 */
{
	dca_RemoveAttr (dc, DCP_Raw, ST_GLOBATTR, key);
}




int
dc_ProcessAttrs (dc, pattern, func)
DataChunk *dc;
char *pattern;
int (*func) ();
/*
 * Go through and look at a bunch of attributes.  If PATTERN is non-null,
 * it is a regular expression used to filter out things.
 */
{
	return (dca_ProcAttrs (dc, DCP_Raw, ST_GLOBATTR, pattern, func));
}




void *
dc_GetGlAttrBlock (dc, len)
DataChunk *dc;
int *len;
/*
 * Get the global attributes out as an opaque chunk.
 */
{
	return (dca_GetBlock (dc, DCP_Raw, ST_GLOBATTR, len));
}



void
dc_SetGlAttrBlock (dc, block, len)
DataChunk *dc;
void *block;
int len;
/*
 * Store a global attribute block back.
 */
{
	dca_PutBlock (dc, DCP_Raw, ST_GLOBATTR, block, len);
}




int
dc_GetNGlobalAttrs(dc)
DataChunk *dc;
/*
 * Return the number of global attributes in a datachunk.  
 */
{
	return (dca_GetNAttrs (dc, DCP_Raw, ST_GLOBATTR));
}



char **
dc_GetGlobalAttrList(dc, pattern, values, natts)
DataChunk *dc;
char *pattern;
void **values[];
int *natts;
{
	return(	dca_GetAttrList(dc, DCP_Raw, ST_GLOBATTR,
				pattern, values, natts));
}



char **
dc_GetGlobalAttrKeys (dc, natts)
DataChunk *dc;
int *natts;
/*
 * Return an array of pointers to the global attribute keys.
 * The array is only valid until the next call to any one of
 * the Get*AttrList or Get*AttrKeys functions.
 */
{
	return( dca_GetAttrList(dc, DCP_Raw, ST_GLOBATTR,
				NULL, NULL, natts));
}



/* ================================================================ */

/*
 * Private global attributes, accessible only to other classes.
 */

# define STP_GLOBAL	(3844)


void
dcp_SetGlobalAttrArray (dc, key, type, nval, values)
DataChunk *dc;
char *key;
DC_ElemType type;
int nval;
void *values;
/*
 * Store a private global attribute into this data chunk.
 */
{
	dca_AddAttrArray (dc, DCP_Raw, STP_GLOBAL, key, type, nval, values);
}




void *
dcp_GetGlobalAttrArray (dc, key, type, nval)
DataChunk *dc;
char *key;
DC_ElemType *type;
int *nval;
/*
 * Store a private global attribute into this data chunk.
 */
{
	return (dca_GetAttrArray (dc, DCP_Raw, STP_GLOBAL, key, type, nval));
}


/*====================================================================*/


void
dc_DumpAttrArrays (dc)
DataChunk *dc;
{
	printf ("Global attributes:\n");
	dc_ProcessAttrArrays (dc, NULL, dca_PrintAttrArray, "\n");
	printf ("Private global attributes:\n");
	dca_ProcAttrArrays (dc, DCP_Raw, STP_GLOBAL, NULL, 
			    dca_PrintAttrArray, "\n");
}

