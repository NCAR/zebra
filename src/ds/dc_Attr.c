/*
 * Attribute-handling code for the data chunk system.
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
MAKE_RCSID ("$Id: dc_Attr.c,v 1.5 1993-05-04 21:42:11 granger Exp $")


/*
 * The structure which makes up an attribute ADE.
 */
typedef struct _AttrADE 
{
	int	aa_Alloc;	/* Length allocated		*/
	int	aa_Used;	/* Length used			*/
	char	aa_Data[1];	/* The actual attr data		*/
} AttrADE;

# define INITIAL_SIZE 128


/*
 * Local routines.
 */
static char	*dca_FindKey FP ((AttrADE *, char *));
static void	dca_RemoveKey FP ((AttrADE *, char *));



void
dca_AddAttr (dc, class, code, key, value)
DataChunk *dc;
DataClass class;
int code;
char *key, *value;
/*
 * Add this attribute to the data chunk.
 */
{
	AttrADE *ade;
	int len = strlen (key) + strlen (value) + 2;
/*
 * Find the ADE; if it doesn't exist, create it.
 */
	if (! (ade = (AttrADE *) dc_FindADE (dc, class, code, 0)))
	{
		int alen = sizeof (AttrADE) + INITIAL_SIZE - 1;
		ade = (AttrADE *) malloc (alen);
		ade->aa_Alloc = INITIAL_SIZE;
		ade->aa_Used = 0;
		dc_AddADE (dc, ade, class, code, alen, TRUE);
	}
/*
 * Make sure this key is not already defined here.
 */
	dca_RemoveKey (ade, key);
/*
 * Make sure there is enough space for the new attribute.
 */
	if ((ade->aa_Used + len) >= ade->aa_Alloc)
	{
	/*
	 * Make sure we add at least enough space for this attribute, as
	 * well as some extra space for later additions.  The goal is to
	 * make realloc infrequent without consuming too much memory... so
	 * we're increasing the ade->aa_Data array to (len +
	 * ade->aa_Alloc*2) bytes, which means our new ade structure
	 * requires 'newlen' bytes:
	 */
		int newlen = sizeof (AttrADE) + len + ade->aa_Alloc*2 - 1;
		ade = (AttrADE *) realloc (ade, newlen);
	/*
	 * Note that the amount allocated (aa_Alloc) is actually 
	 *    (newlen - sizeof(AttrADE) + 1) = len + ade->aa_Alloc*2
	 * since the first byte of the data (aa_Data[1]) is part of the
	 * AttrADE structure and accounted for in sizeof(AttrADE).
	 * aa_Alloc only measures the size of the char aa_Data[] array.
	 * It does not include space for the other members of the ade.
	 */
		ade->aa_Alloc *= 2;
		ade->aa_Alloc += len;
		dc_ChangeADE (dc, ade, class, code, newlen);
	}
/*
 * Now we just append and we are done.
 */
	sprintf (ade->aa_Data + ade->aa_Used, "%s=%s", key, value);
	ade->aa_Used += len;
}



void
dca_RemoveAttr (dc, class, code, key)
DataChunk *dc;
DataClass class;
int code;
char *key;
/*
 * Remove this attribute from the data chunk.
 */
{
	AttrADE *ade;
/*
 * Find the ADE; if it doesn't exist, we can't very well remove anything,
 * can we?
 */
	if (! (ade = (AttrADE *) dc_FindADE (dc, class, code, 0)))
		return;
/*
 * Remove this key
 */
	dca_RemoveKey (ade, key);
}




static char *
dca_FindKey (ade, key)
AttrADE *ade;
char *key;
/*
 * Search out this key in this attribute block.
 */
{
	char *begin = ade->aa_Data, *cp, *kp;
/*
 * As long as there is data, we look.
 */
	while ((begin - ade->aa_Data) < ade->aa_Used)
	{
	/*
	 * See if this is the one.
	 */
		for (cp = begin, kp = key; *kp; kp++, cp++)
			if (*cp != *kp)
				break;
	/*
	 * If we found it, return here.
	 */
		if (*kp == '\0' && *cp == '=')
			return (begin);
	/*
	 * Nope, move on to the next one.
	 */
	 	do
			begin++;
		while (*begin != '\0');
		begin++;
	}
	return (NULL);
}




static void
dca_RemoveKey (ade, key)
AttrADE *ade;
char *key;
/*
 * Make sure this key does not exist here.
 */
{
	char *loc, *next;
	char *src, *dest;
/*
 * If it already doesn't exist, life is easy.
 */
	if ((loc = dca_FindKey (ade, key)) == NULL)
		return;
/*
 * It exists.  Find the beginning of the next one.
 */
	for (next = loc; *next != '\0'; next++)
		;
	next++;
/*
 * If need be, shift the data after this entry up, and decrement the
 * length.
 * We can't use memcpy because copy regions may overlap,
 * and Sun doesn't have ANSI memmove()
 */
	for (src = next, dest = loc; 
	     src - ade->aa_Data < ade->aa_Used;
	     ++src, ++dest)
	{
		*dest = *src;
	}
	ade->aa_Used -= next - loc;
}





char *
dca_GetAttr (dc, class, code, key)
DataChunk *dc;
DataClass class;
int code;
char *key;
/*
 * Look up this attribute.
 */
{
	AttrADE *ade;
	char *kp;
/*
 * Look for our block.
 */
	if (! (ade = (AttrADE *) dc_FindADE (dc, class, code, NULL)))
		return (NULL);
/*
 * Now find this key.  If it's there, skip to the value and return it.
 */
	if (! (kp = dca_FindKey (ade, key)))
		return (NULL);
	while (*kp != '=')
		kp++;
	return (kp + 1);
}





int
dca_ProcAttrs (dc, class, code, pattern, func)
DataChunk *dc;
DataClass class;
int code, (*func)();
char *pattern;
/*
 * Plow through all the attributes.
 */
{
	AttrADE *ade;
	char *re_comp (), *re_exec (), *cp, *value;
/*
 * Look for our block.
 */
	if (! (ade = (AttrADE *) dc_FindADE (dc, class, code, NULL)))
		return (0);
/* 
 * If we have a pattern, compile it now.
 */
	if (pattern)
		re_comp (pattern);
/*
 * Go through all of the attributes now.
 */
	cp = ade->aa_Data;
	while ((cp - ade->aa_Data) < ade->aa_Used)
	{
		int ret = 0;
	/*
	 * Find the associated value for this key.
	 */
		for (value = cp; *value != '='; value++)
			;
		*value++ = '\0';	/* Clear = for now. */
	/*
	 * Pass it to the function.
	 */
		if (! pattern || re_exec (cp))
			ret = (*func) (cp, value);
		value[-1] = '=';
		if (ret)
			return (ret);
	/*
	 * Move on to the next one.
	 */
		for (cp = value; *cp; cp++)
			;
		cp++;
	}
	return (0);
}






void *
dca_GetBlock (dc, class, code, len)
DataChunk *dc;
DataClass class;
int code, *len;
/*
 * Pull out the attributes in one big chunk.
 */
{
	AttrADE *ade;
/*
 * Find the ADE, then return it.
 */
	if (! (ade = (AttrADE *) dc_FindADE (dc, class, code, 0)))
		return (0);
	*len = ade->aa_Used;
	return (ade->aa_Data);
}



void
dca_PutBlock (dc, class, code, block, len)
DataChunk *dc;
DataClass class;
int code, len;
void *block;
/*
 * Put back an attribute block obtained with dca_GetBlock.
 */
{
	AttrADE *ade;
	int alen;

	alen = sizeof (AttrADE) + len - 1;
	ade = (AttrADE *) malloc (alen);
	ade->aa_Alloc = ade->aa_Used = len;
	memcpy (ade->aa_Data, block, len);
	dc_AddADE (dc, ade, class, code, alen, TRUE);
}





int
dca_GetNAttrs(dc, class, code)
DataChunk *dc;
DataClass class;
int code;
/*
 * Get number of attrs in a chunk
 */
{
	AttrADE *ade;
	char *cp;
	int n = 0;
/*
 * Look for our block.
 */
	if (! (ade = (AttrADE *) dc_FindADE (dc, class, code, NULL)))
		return (0);
/* 
 * Now loop over the whole attribute block, counting the number of
 * null-terminations we find
 */
	cp = ade->aa_Data;
	while ((cp - ade->aa_Data) < ade->aa_Used)
	{
		for (; *cp != '\0'; cp++)
			;
		n++;
		cp++;
	}
	return(n);
}




char **
dca_GetAttrList(dc, class, code, pattern, values, natts)
DataChunk *dc;
DataClass class;
int code;
char *pattern;
char **values[];
int *natts;
/*
 * Generate an array of keys and corresponding values for the attributes of
 * this class and code.  Returns the pointer to the array of key strings.
 * And on return, *values points to the array of attribute values and
 * *natts holds the number of attributes.  The key and values arrays will
 * be NULL-terminated.  If natts or values is NULL, no values will be
 * returned.  If pattern is non-NULL, only the keys which match the
 * pattern, and their values, will be returned.  The arrays and the strings
 * are stored locally, and are only valid until the next call of this
 * function.  To copy the data for longer use, you'd have to copy the keys
 * array, the values array, and all of the strings pointed to by each
 * element of both of those arrays.  If there is an error or no atts,
 * returns NULL, and 0 in *natts.  *natts will always return >= zero.
 */
{
	static char *local_data = NULL;
	static char **local_keys = NULL;
	static char **local_vals = NULL;
	int num_atts, count;
	AttrADE *ade;
	char *re_comp (), *re_exec (), *cp, *value;

	if (natts)
		*natts = 0;
/*
 * Look for our block.
 */
	if (! (ade = (AttrADE *) dc_FindADE (dc, class, code, NULL)))
		return (NULL);
/* 
 * If we have a pattern, compile it now.
 */
	if (pattern)
		re_comp (pattern);
/*
 * Get some memory. Rather than realloc we free and malloc since we
 * we don't need to copy the old stuff.
 */
	if (local_data) free(local_data);
	if (local_keys) free(local_keys);
	if (local_vals)	free(local_vals);

	num_atts = dca_GetNAttrs(dc, class, code);
	if (! num_atts)
		return (NULL);
	local_data = (char *)malloc(ade->aa_Used);
	local_keys = (char **)malloc((num_atts+1) * sizeof(char *));
	local_vals = (char **)malloc((num_atts+1) * sizeof(char *));
	memcpy(local_data, ade->aa_Data, ade->aa_Used);
/*
 * Go through the local copy of the attributes, collecting keys and 
 * values and converting '=' to '\0'
 */
	cp = local_data;
	count = 0;
	while ((cp - local_data) < ade->aa_Used)
	{
	/*
	 * Find the associated value for this key.
	 */
		for (value = cp; *value != '='; value++)
			;
		*value++ = '\0';
		if (! pattern || re_exec (cp))
		{
			local_keys[count] = cp;
			local_vals[count] = value;
			count++;
		}
	/*
	 * Move on to the next one.
	 */
		for (cp = value; *cp; cp++)
			;
		cp++;
	}
	local_vals[count] = NULL;
	local_keys[count] = NULL;
	if (natts) *natts = count;
	if (values) *values = local_vals;
	if (! count)
		return(NULL);
	return (local_keys);
}

