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
MAKE_RCSID ("$Id: dc_Attr.c,v 1.2 1992-06-09 19:22:16 corbet Exp $")


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
		int newlen = sizeof (AttrADE) + ade->aa_Alloc*2 - 1;
		ade = (AttrADE *) realloc (ade, newlen);
		ade->aa_Alloc *= 2;	/* Assume this will suffice */
		dc_ChangeADE (dc, ade, class, code, newlen);
	}
/*
 * Now we just append and we are done.
 */
	sprintf (ade->aa_Data + ade->aa_Used, "%s=%s", key, value);
	ade->aa_Used += len;
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
 */
	if ((next - ade->aa_Data) < ade->aa_Used)
		memcpy (loc, next, ade->aa_Used - (next - ade->aa_Data));
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
