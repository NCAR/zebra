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

# include <sys/types.h>
# include <memory.h>
# include <string.h>

# include <defs.h>
# include <message.h>
# include "DataStore.h"
# include "ds_fields.h"
# include "DataChunk.h"
# include "DataChunkP.h"
# include <zl_regex.h> /* System-independent regex functions in Zeb library */

#ifndef lint
MAKE_RCSID ("$Id: dc_Attr.c,v 1.12 1995-02-10 01:16:57 granger Exp $")
#endif

/*--------------------------------------------------------------------
 * Quick explanation of how attributes work now that they can have 
 * arrays of values of a particular type.
 *--------------------------------------------------------------------
 * Each attribute occupies a number of bytes which is a multiple of
 * DC_ElemTypeMaxSize, so that each attribute is aligned in aa_Data.
 * The first sizeof(AttrRec) bytes maps to an attribute record, which
 * contains the length of the entire attribute, the type of its
 * values, the number of values, the offset of the key in the record,
 * the values themselves, and finally the key itself, in that order.
 * Some blank space may be necessary following the key to align the
 * following the attribute record.  If the attribute type is DCT_String,
 * the values array includes a null-terminator, and NValues includes the
 * number of characters in the string as well as the null terminator.
 * It is illegal to add more than one value of type DCT_String in the
 * dca_AddAttrArray funcitons.
 *--------------------------------------------------------------------
 */


/*
 * The structure which makes up an attribute ADE.  NOTE: Since each
 * attribute must be aligned, aa_Data must be aligned withing the
 * AttrADE structure.  Try to keep a multiple of 8 bytes between
 * aa_Data and the beginning of the structure.
 */
typedef struct _AttrADE 
{
	int	aa_Alloc;	/* Length allocated		*/
	int	aa_Used;	/* Length used			*/
	char	aa_Data[1];	/* The actual attr data		*/
} AttrADE;


typedef struct _AttrRec
{
	int		ar_Len;		/* Length of this record*/
	DC_ElemType	ar_Type;	/* Att value's type	*/
	int		ar_NValues;	/* Number of values	*/
	int		ar_Key;		/* Offset to key string */
	/* Values array begins here, followed by key */
} AttrRec;
	
#define AR_KEY(rec) 	((char *)(rec) + (rec)->ar_Key)
#define AR_VALUES(rec)	((void *)((char *)(rec) + sizeof(AttrRec)))
#define AR_LEN(rec)	((rec)->ar_Len)
#define AR_TYPE(rec)	((rec)->ar_Type)

#define INITIAL_SIZE 128

/*
 * Local routines.
 */
static AttrRec	*dca_FindKey FP ((AttrADE *, char *));
static void	dca_RemoveKey FP ((AttrADE *, char *));
static AttrRec	*dca_FirstRec FP ((AttrADE *ade));
static AttrRec	*dca_NextRec FP ((AttrADE *ade, AttrRec *rec));
static AttrADE 	*dca_NewAttr FP ((DataChunk *dc, AttrADE *ade, DataClass class,
				  int code, int len, int *next));
static AttrADE 	*dca_CreateADE FP ((DataChunk *dc, DataClass class, int code));




void
dca_AddAttrArray (dc, class, code, key, type, nval, values)
DataChunk *dc;
DataClass class;
int code;
char *key;
DC_ElemType type;
int nval;
void *values;
/*
 * Store 'nval' elements of type 'type' in the attribute 'key'.  Note that
 * it is possible to store an attribute with zero values.  In this case the
 * key itself is returned as the value, NValues will always be zero, and
 * the type will be set to DCT_String.
 */
{
	AttrADE *ade;
	AttrRec *rec;
	int len, next;
	int vlen;

	/*
	 * If the type is DCT_String, then nval must be one.  Otherwise
	 * nval must be greater than zero.
	 */
	if (type == DCT_String && nval != 1)
	{
		msg_ELog (EF_PROBLEM, 
			  "AttrArray: Cannot set more than one string");
		return;
	}
	else if (nval < 0)
	{
		msg_ELog (EF_PROBLEM,
			  "AttrArray: must add zero or more values for key");
	}
	if (! (ade = (AttrADE *) dc_FindADE (dc, class, code, NULL)))
		ade = dca_CreateADE (dc, class, code);
	else
		dca_RemoveKey (ade, key);
	/*
	 * Figure out how much space we'll need for this attribute:
	 * the record, the values, and the key.
	 */
	len = sizeof(AttrRec);
	if (nval == 0 || values == NULL)
	{
		nval = 0;
		values = NULL;
	}
	if (type != DCT_String)
		vlen = nval * dc_SizeOfType(type);
	else if (values)
		vlen = strlen((char *)values) + 1;
	else
		vlen = 0;
	len += vlen;
	len += strlen(key) + 1;
	ade = dca_NewAttr (dc, ade, class, code, len, &next);
	rec = (AttrRec *)(ade->aa_Data + next);
	/*
	 * Copy the information into our record.  If we have no values,
	 * the key becomes the value, so the type becomes DCT_String.
	 */
	if (nval == 0)
		rec->ar_Type = DCT_String;
	else
		rec->ar_Type = type;
	rec->ar_NValues = nval;
	rec->ar_Key = sizeof(AttrRec) + vlen;
	strcpy ((char *)rec + rec->ar_Key, key);
	if (vlen && values)
		memcpy (AR_VALUES(rec), values, vlen);
}




void *
dca_GetAttrArray (dc, class, code, key, vtype, nval)
DataChunk *dc;
DataClass class;
int code;
char *key;
DC_ElemType *vtype;
int *nval;
/*
 * Returns pointer to beginning of array of attribute values.  The type of
 * the values is returned in *type, the number in *nval
 */
{
	AttrADE *ade;
	AttrRec *rec;

	if (! (ade = (AttrADE *) dc_FindADE (dc, class, code, NULL)))
		return (NULL);
	rec = dca_FindKey (ade, key);
	if (! rec)
		return (NULL);
	if (nval)
		*nval = rec->ar_NValues;
	if (vtype)
		*vtype = rec->ar_Type;
	return (AR_VALUES(rec));
}





void
dca_AddAttr (dc, class, code, key, value)
DataChunk *dc;
DataClass class;
int code;
char *key, *value;
/*
 * Add this string attribute to the data chunk.
 */
{
	dca_AddAttrArray (dc, class, code, key, DCT_String, 1, value);
}




char *
dca_GetAttr (dc, class, code, key)
DataChunk *dc;
DataClass class;
int code;
char *key;
/*
 * Look up this key and return its string value.
 */
{
	DC_ElemType type;
	int nval;
	void *values;

	values = dca_GetAttrArray (dc, class, code, key, &type, &nval);
	if (values && (type != DCT_String))
	{
		msg_ELog(EF_PROBLEM, 
			 "GetAttr: cannot get %s attribute value as string",
			 dc_TypeName(type));
		return (NULL);
	}
	else
		return ((char *)values);
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
/*
 * If all of the attributes have been deleted, no sense keeping the ADE
 * around, especially if it holds alot of allocated memory.  Next time we'll
 * start over from scratch.  We shouldn't do this in dca_RemoveKey because
 * dca_RemoveKey may be called when changing an existing key, and the ADE
 * will still be expected to be around.
 */
	if (ade->aa_Used == 0)
		dc_RemoveADE (dc, class, code);
}




static AttrADE *
dca_CreateADE (dc, class, code)
DataChunk *dc;
DataClass class;
int code;
/*
 * Create the attribute ADE for this class and code, giving an initial
 * allocation of space for attributes.
 */
{
	AttrADE *ade;
	int alen;

#ifdef DEBUG
	printf ("creating AttrADE, class %d, code %d\n", class, code);
#endif
	alen = sizeof (AttrADE) + INITIAL_SIZE - 1;
	ade = (AttrADE *) malloc (alen);
	ade->aa_Alloc = INITIAL_SIZE;
	ade->aa_Used = 0;
	dc_AddADE (dc, ade, class, code, alen, TRUE);
	return (ade);
}




static AttrADE *
dca_NewAttr (dc, ade, class, code, len, new)
DataChunk *dc;
AttrADE *ade;
DataClass class;
int code;
int len;
int *new;
/*
 * Make sure there is enough space for a new attribute requiring 'len' bytes.
 * Align the key and return the offset to the new attr in 'next'.
 */
{
	AttrRec *rec;
	int next;	/* offset into ade->aa_Data of the new attr */
/*
 * This attribute should automatically be aligned.  Make sure we take up
 * enough space in this one so that the next attribute will be aligned as
 * well.
 */
	next = ade->aa_Used;
	len = ALIGN(len, DC_ElemTypeMaxSize);
	if (next + len > ade->aa_Alloc)
	{
	/*
	 * Make sure we add at least enough space for this attribute, as
	 * well as some extra space for later additions.  The goal is to
	 * make realloc infrequent without consuming too much memory... so
	 * we're increasing the ade->aa_Data array to (ade->aa_Alloc*2 + len)
	 * bytes, which means our new ade structure requires 'newlen' bytes:
	 */
		int newlen = sizeof (AttrADE) + len + (2 * ade->aa_Alloc) - 1;
		ade = (AttrADE *) realloc (ade, newlen);
	/*
	 * Note that the amount allocated (aa_Alloc) is actually 
	 *    (newlen - sizeof(AttrADE) + 1) = len + 2*ade->aa_Alloc
	 * since the first byte of the data (aa_Data[1]) is part of the
	 * AttrADE structure and accounted for in sizeof(AttrADE).
	 * aa_Alloc only measures the size of the char aa_Data[] array.
	 * It does not include space for the other members of the ade.
	 */
		ade->aa_Alloc = (2 * ade->aa_Alloc) + len;
		dc_ChangeADE (dc, ade, class, code, newlen);
	}
/*
 * Set the space designated for this attribute; quells testcenter.
 */
	memset (ade->aa_Data + next, 0, len);
	rec = (AttrRec *)(ade->aa_Data + next);
	rec->ar_Len = len;
	ade->aa_Used += len;
	*new = next;
	return (ade);
}




static AttrRec *
dca_FirstRec (ade)
AttrADE *ade;
/*
 * Return a pointer to the first attribute record, or NULL if there is none
 */
{
	if (ade->aa_Used == 0)
		return (NULL);
	else
		return ((AttrRec *)ade->aa_Data);
}




static AttrRec *
dca_NextRec (ade, rec)
AttrADE *ade;
AttrRec *rec;
/*
 * Return a pointer to the beginning of the next attribute record.
 */
{
	int next;

	next = ((char *)rec - ade->aa_Data) + rec->ar_Len;
	if (next >= ade->aa_Used)
		return (NULL);
	else
		return ((AttrRec *)(ade->aa_Data + next));
}




static AttrRec *
dca_FindKey (ade, key)
AttrADE *ade;
char *key;
/*
 * Search out this key in this attribute block.
 */
{
	AttrRec *rec;
/*
 * As long as there is data, we look.
 */
	rec = dca_FirstRec (ade);
	while (rec)
	{
	/*
	 * See if this is the one.
	 */
		if (!strcmp (AR_KEY(rec), key))
			return (rec);
	/*
	 * Nope, move on to the next one.
	 */
		rec = dca_NextRec (ade, rec);
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
	AttrRec *rec, *next_rec;
	char *src, *dest;
	int len;
/*
 * If it already doesn't exist, life is easy.
 */
	if ((rec = dca_FindKey (ade, key)) == NULL)
		return;
/*
 * It exists.  Find the beginning of the next one.
 */
	len = rec->ar_Len;
	next_rec = dca_NextRec (ade, rec);
/*
 * If need be, shift the data after this entry up, and decrement the
 * length.  Note that this works because we know each attribute record
 * is already aligned so the shifted records will also be aligned.
 */
	if (next_rec)
	{
		for (src = (char *)next_rec, dest = (char *)rec; 
		     src - ade->aa_Data < ade->aa_Used;
		     ++src, ++dest)
		{
			*dest = *src;
		}
	}
	ade->aa_Used -= len;
}





int
dca_ProcAttrArrays (dc, class, code, pattern, func, arg)
DataChunk *dc;
DataClass class;
int code;
char *pattern;
int (*func)(/* char *key, void *vals, int nval, DC_ElemType, void *arg */);
void *arg;
/*
 * Plow through all the attributes arrays.  To allow the attributes to be
 * modified (i.e. removed) by the function, we first copy the ADE and its
 * data.  We traverse the copy in case the original changes.  Then we
 * can free the copy.
 */
{
	AttrADE *ade, *copy;
	AttrRec *rec;
	int len;
	int ret;
	char *stat;
/*
 * Look for our block.
 */
	if (! (ade = (AttrADE *) dc_FindADE (dc, class, code, NULL)))
		return (0);
/*
 * If we have no attributes, don't bother
 */
	if (ade->aa_Used == 0)
		return (0);
/*
 * Copy the block.
 */
	len = sizeof(AttrADE) + ade->aa_Used - 1;
	copy = (AttrADE *)malloc( len );
	memcpy ((void *)copy, (void *)ade, len);
	copy->aa_Alloc = ade->aa_Used;
/* 
 * If we have a pattern, compile it now.
 */
	if (pattern)
	{
	        if ((stat = zl_re_comp (pattern)))
		{
			msg_ELog (EF_PROBLEM,"regexp '%s': %s", pattern, stat);
			pattern = NULL;
		}
	}
/*
 * Go through all of the attributes now.
 */
	ret = 0;
	rec = dca_FirstRec (copy);
	while (rec)
	{
	/*
	 * Pass it to the function.
	 */
		if (! pattern || zl_re_exec (AR_KEY(rec)))
			ret = (*func) (AR_KEY(rec), AR_VALUES(rec), 
				       rec->ar_NValues, AR_TYPE(rec), arg);
		if (ret)
			break;
	/*
	 * Move on to the next one.
	 */
		rec = dca_NextRec (copy, rec);
	}
	free (copy);
	return (ret);
}





static int
dca_ProcStringArrays (key, value, nval, type, arg)
char *key;
void *value;
int nval;
DC_ElemType type;
void *arg;
{
	int (*func)() = (int (*)())arg;

	if (type == DCT_String)
		return ((*func)(key, (char *)value));
	return (0);
}




int
dca_ProcAttrs (dc, class, code, pattern, func)
DataChunk *dc;
DataClass class;
int code;
char *pattern;
int (*func)();
/*
 * Plow through all the attributes with string values.
 */
{
	return (dca_ProcAttrArrays (dc, class, code, pattern, 
				    dca_ProcStringArrays, (void *)func));
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
		return NULL;
	*len = ade->aa_Used;
	return ((void *)ade->aa_Data);
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

	/*
	 * We need to convert any old-style blocks to the new format, so
	 * we loop over the keys in the block and add them one at a time.
	 */
	if (!strchr((char *)block, '='))
	{
		alen = sizeof (AttrADE) + len - 1;
		ade = (AttrADE *) malloc (alen);
		ade->aa_Alloc = ade->aa_Used = len;
		memcpy (ade->aa_Data, block, len);
		dc_AddADE (dc, ade, class, code, alen, TRUE);
	}
	else
	{
		char *cp;
		char *key = (char *)block;

		while (key - (char *)block < len)
		{
			cp = key;
			while (*cp != '=')
				cp++;
			*cp++ = '\0';
			dca_AddAttr (dc, class, code, key, cp);
			*(cp-1) = '=';
			key += strlen(key) + 1;
		}
	}
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
	AttrRec *rec;
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
	rec = dca_FirstRec (ade);
	while (rec)
	{
		rec = dca_NextRec (ade, rec);
		n++;
	}
	return (n);
}




char **
dca_GetAttrList(dc, class, code, pattern, values, natts)
DataChunk *dc;
DataClass class;
int code;
char *pattern;
void **values[]; 	/* An array of void ptrs passed by reference */
int *natts;
/*
 * Generate an array of keys and corresponding values for the attributes of
 * this class and code.  Returns the pointer to the array of key strings.
 * And on return, *values points to the array of attribute values and
 * *natts holds the number of attributes.  The key and values arrays will
 * be NULL-terminated.  If natts or values is NULL, no values will be
 * returned.  If pattern is non-NULL, only the keys which match the
 * pattern, and their values, will be returned.  The arrays are stored
 * locally, and are only valid until the next call of this function or
 * until any attributes are removed or deleted for this class and code.  To
 * copy the data for longer use, you'd have to copy the keys array, the
 * values array, and all of the strings pointed to by each element of both
 * of those arrays.  If there is an error or no atts, returns NULL, and 0
 * in *natts.  *natts will always return >= zero.
 */
{
	static char **local_keys = NULL;
	static void **local_vals = NULL;
	int num_atts, count;
	AttrADE *ade;
	AttrRec *rec;
	char *stat;

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
	{
	        if ((stat = zl_re_comp (pattern)))
		{
			msg_ELog (EF_PROBLEM,"regexp '%s': %s", pattern, stat);
			pattern = NULL;
		}
	}
/*
 * Get some memory. Rather than realloc we free and malloc since we
 * we don't need to copy the old stuff.
 */
	if (local_keys) free(local_keys);
	if (local_vals)	free(local_vals);

	num_atts = dca_GetNAttrs(dc, class, code);
	if (num_atts == 0)
		return (NULL);
	local_keys = (char **)malloc((num_atts+1) * sizeof(char *));
	local_vals = (void **)malloc((num_atts+1) * sizeof(void *));
/*
 * Go through the local copy of the attributes, collecting keys
 */
	rec = dca_FirstRec (ade);
	count = 0;
	while (rec)
	{
	/*
	 * Find the associated value for this key.
	 */
		if (! pattern || zl_re_exec (AR_KEY(rec)))
		{
			local_keys[count] = AR_KEY(rec);
			local_vals[count] = (void *)AR_VALUES(rec);
			count++;
		}
	/*
	 * Move on to the next one.
	 */
		rec = dca_NextRec (ade, rec);
	}
	local_vals[count] = NULL;
	local_keys[count] = NULL;
	if (natts) *natts = count;
	if (values) *values = local_vals;
	if (! count)
		return(NULL);
	return (local_keys);
}

