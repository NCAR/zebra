/*
 * The elementary DC element interface routines.
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

# include <string.h>

# include <defs.h>
# include <config.h>
# include <message.h>
# include "DataStore.h"

RCSID ("$Id: dc_Elements.c,v 3.2 1996-11-26 22:36:42 granger Exp $")

static const char *DC_ElemTypeNames[] =
{
	"unknown",
	"float",
	"double",
	"long double",
	"char",
	"unsigned char",
	"short int",
	"unsigned short",
	"int",
	"unsigned int",
	"long int",
	"unsigned long",
	"string",
	"Boolean",
	"ZebTime",
	"void *"
};

static const int DC_ElemTypeSizes[] = 
{
	0,
	sizeof(float),
	sizeof(double),
	sizeof(LongDouble),
	sizeof(char),
	sizeof(unsigned char),
	sizeof(short int),
	sizeof(unsigned short),
	sizeof(int),
	sizeof(unsigned int),
	sizeof(long int),
	sizeof(unsigned long),
	sizeof(char *),
	sizeof(unsigned char),
	sizeof(ZebTime),
	sizeof(void *)
};

static const int DC_NumTypes = 
	sizeof (DC_ElemTypeSizes) / sizeof (DC_ElemTypeSizes[0]);


int
dc_SizeOfType (type) 
DC_ElemType type;
{
	return ((((int)(type) >= 0) && ((int)(type) < DC_NumTypes)) ? 
		DC_ElemTypeSizes[(int)(type)] : 0);
}


const char *
dc_TypeName (type)
DC_ElemType type;
{
	return (((((int)(type) >= 0) && ((int)(type) < DC_NumTypes)) ?
		 DC_ElemTypeNames[(int)(type)] : "invalid"));
}


/* ----------------------------------------------------------------------
 * Utility routines for converting between elements, void pointers, and
 * strings.
 * ----------------------------------------------------------------------
 */

void
dc_AssignElement (e, ptr, type)
DC_Element *e;
void *ptr;
DC_ElemType type;
/*
 * De-reference a void pointer to type and store it into a DC_Element union.
 * Unknown types are copied opaquely if they fit, otherwise the union is
 * filled with zeros.
 */
{
	if (! ptr)
	{
		memset ((void *)e, 0, sizeof (*e));
		return;
	}
	switch (type)
	{
	   case DCT_Float:
		e->dcv_float = *(float *)ptr;
		break;
	   case DCT_Double:
		e->dcv_double = *(double *)ptr;
		break;
	   case DCT_LongDouble:
		e->dcv_longdbl = *(LongDouble *)ptr;
		break;
	   case DCT_Char:
		e->dcv_char = *(char *)ptr;
		break;
	   case DCT_UnsignedChar:
		e->dcv_uchar = *(unsigned char *)ptr;
		break;
	   case DCT_ShortInt:
		e->dcv_shortint = *(short *)ptr;
		break;
	   case DCT_UnsignedShort:
		e->dcv_ushort = *(unsigned short *)ptr;
		break;
	   case DCT_Integer:
		e->dcv_int = *(int *)ptr;
		break;
	   case DCT_UnsignedInt:
		e->dcv_uint = *(unsigned int *)ptr;
		break;
	   case DCT_LongInt:
		e->dcv_longint = *(long int *)ptr;
		break;
	   case DCT_UnsignedLong:
		e->dcv_ulong = *(unsigned long *)ptr;
		break;
	   case DCT_String:
		e->dcv_string = *(char **)ptr;
		break;
	   case DCT_Boolean:
		e->dcv_boolean = *(unsigned char *)ptr;
		break;
	   case DCT_ZebTime:
		e->dcv_zebtime = *(ZebTime *)ptr;
		break;
	   case DCT_VoidPointer:
		e->dcv_pointer = ptr;
		break;
	   case DCT_Element:
		*e = *(DC_Element *)ptr;
		break;
	   default:
		if (dc_SizeOfType (type) <= sizeof(*e))
			memcpy ((void *)e, ptr, dc_SizeOfType (type));
		else
			memset ((void *)e, 0, sizeof (*e));
		break;
	}
}



int
dc_CompareElement (e, f, type)
DC_Element *e;
DC_Element *f;
DC_ElemType type;
/*
 * Compare the two elements of identical types.  Return -1 if (e < f),
 * 0 if (e == f) and 1 if (e > f).
 */
{
#define CMP(a,b) \
	(((a)<(b))?(-1):(((a)==(b))?(0):(1)))
	switch (type)
	{
	   case DCT_Float:
		return (CMP(e->dcv_float, f->dcv_float));
		break;
	   case DCT_Double:
		return (CMP(e->dcv_double, f->dcv_double));
		break;
	   case DCT_LongDouble:
		return (CMP(e->dcv_longdbl, f->dcv_longdbl));
		break;
	   case DCT_Char:
		return (CMP(e->dcv_char, f->dcv_char));
		break;
	   case DCT_UnsignedChar:
		return (CMP(e->dcv_uchar, f->dcv_char));
		break;
	   case DCT_ShortInt:
		return (CMP(e->dcv_shortint, f->dcv_shortint));
		break;
	   case DCT_UnsignedShort:
		return (CMP(e->dcv_ushort, f->dcv_ushort));
		break;
	   case DCT_Integer:
		return (CMP(e->dcv_int, f->dcv_int));
		break;
	   case DCT_UnsignedInt:
		return (CMP(e->dcv_uint, f->dcv_uint));
		break;
	   case DCT_LongInt:
		return (CMP(e->dcv_longint, f->dcv_longint));
		break;
	   case DCT_UnsignedLong:
		return (CMP(e->dcv_ulong, f->dcv_ulong));
		break;
	   case DCT_String:
		return (strcmp(e->dcv_string, f->dcv_string));
		break;
	   case DCT_Boolean:
		return (CMP(e->dcv_boolean, f->dcv_boolean));
		break;
	   case DCT_ZebTime:
		return (TC_Less(e->dcv_zebtime, f->dcv_zebtime) ? (-1) :
			((TC_Eq(e->dcv_zebtime, f->dcv_zebtime)) ? 0 : -1));
		break;
	   case DCT_VoidPointer:
		return (CMP(e->dcv_pointer, f->dcv_pointer));
		break;
	   default:
		return (0);
		break;
	}
#undef CMP
}



int
dc_CompareValue (e, f, type)
void *e;
void *f;
DC_ElemType type;
{
	DC_Element a, b;

	dc_AssignElement (&a, e, type);
	dc_AssignElement (&b, f, type);
	return (dc_CompareElement (&a, &b, type));
}



void
dc_AssignValue (ptr, e, type)
void *ptr;
DC_Element *e;
DC_ElemType type;
/* 
 * Assign an element union containing the given type to space pointed
 * to by ptr.  Usually ptr is actually a pointer to the type which 
 * was cast to (void *) when passed into the function.
 */
{
	switch (type)
	{
	   case DCT_Float:
		*(float *)ptr = e->dcv_float;
		break;
	   case DCT_Double:
		*(double *)ptr = e->dcv_double;
		break;
	   case DCT_LongDouble:
		*(LongDouble *)ptr = e->dcv_longdbl;
		break;
	   case DCT_Char:
		*(char *)ptr = e->dcv_char;
		break;
	   case DCT_UnsignedChar:
		*(unsigned char *)ptr = e->dcv_uchar;
		break;
	   case DCT_ShortInt:
		*(short *)ptr = e->dcv_shortint;
		break;
	   case DCT_UnsignedShort:
		*(unsigned short *)ptr = e->dcv_ushort;
		break;
	   case DCT_Integer:
		*(int *)ptr = e->dcv_int;
		break;
	   case DCT_UnsignedInt:
		*(unsigned int *)ptr = e->dcv_uint;
		break;
	   case DCT_LongInt:
		*(long int *)ptr = e->dcv_longint;
		break;
	   case DCT_UnsignedLong:
		*(unsigned long *)ptr = e->dcv_ulong;
		break;
	   case DCT_String:
		*(char **)ptr = e->dcv_string;
		break;
	   case DCT_Boolean:
		*(unsigned char *)ptr = e->dcv_boolean;
		break;
	   case DCT_ZebTime:
		*(ZebTime *)ptr = e->dcv_zebtime;
		break;
	   case DCT_VoidPointer:
		ptr = e->dcv_pointer;
		break;
	   case DCT_Element:
		*(DC_Element *)ptr = *e;
		break;
	   default:
		if (dc_SizeOfType (type) <= sizeof(*e))
			memcpy (ptr, (void *)e, dc_SizeOfType (type));
		break;
	}
}



const char *
dc_ElemToString (e, type)
DC_Element *e;
DC_ElemType type;
/*
 * Convert an element to a string, and return the string.  The returned
 * buffer space is only valid until the next call to this function or to
 * dc_ValueToString.  If the type is a string, allocate dynamic space if
 * the string won't fit in static.  If the type is unknown, the byte values
 * are printed in hex.
 */
{
	static char *dest = NULL;
	static char buf[128]; 	/* holds all numbers and times, yes? */
	unsigned char *hex;

	/*
	 * If we used dynamic memory last time, free it.
	 */
	if ((dest != buf) && (dest != NULL))
		free (dest);
	dest = buf;		/* default to writing in static memory */
	switch (type)
	{
	   case DCT_Float:
		sprintf (dest, "%g", e->dcv_float);
		break;
	   case DCT_Double:
		sprintf (dest, "%g", e->dcv_double);
		break;
	   case DCT_LongDouble:
		sprintf (dest, "%Lg", e->dcv_longdbl);
		break;
	   case DCT_Char:
		sprintf (dest, "%c", e->dcv_char);
		break;
	   case DCT_UnsignedChar:
		sprintf (dest, "%#hx", e->dcv_uchar);
		break;
	   case DCT_ShortInt:
		sprintf (dest, "%hi", e->dcv_shortint);
		break;
	   case DCT_UnsignedShort:
		sprintf (dest, "%hu", e->dcv_ushort);
		break;
	   case DCT_Integer:
		sprintf (dest, "%d", e->dcv_int);
		break;
	   case DCT_UnsignedInt:
		sprintf (dest, "%u", e->dcv_uint);
		break;
	   case DCT_LongInt:
		sprintf (dest, "%li", e->dcv_longint);
		break;
	   case DCT_UnsignedLong:
		sprintf (dest, "%lu", e->dcv_ulong);
		break;
	   case DCT_String:
		/*
		 * Make sure we have enough space for arbitrary strings
		 */
		if (strlen(e->dcv_string) >= sizeof(buf))
			dest = (char *)malloc(strlen(e->dcv_string) + 1);
		strcpy (dest, e->dcv_string);
		break;
	   case DCT_Boolean:
		sprintf (dest, "%s", (e->dcv_boolean) ? "true" : "false");
		break;
	   case DCT_ZebTime:
		if (e->dcv_zebtime.zt_MicroSec)
			TC_EncodeTime (&e->dcv_zebtime, TC_FullUSec, dest);
		else
			TC_EncodeTime (&e->dcv_zebtime, TC_Full, dest);
		break;
	   case DCT_VoidPointer:
		sprintf (dest, "%#0x", (int) e->dcv_pointer);
		break;
	   default:
		hex = (unsigned char *)e;
		if (5 * sizeof (*e) >= sizeof(buf))
			dest = (char *)malloc(5 * sizeof (*e) + 1);
		dest[0] = '\0';
		while (hex < (unsigned char *)e + sizeof(*e))
			sprintf (dest+strlen(dest), "%#0x ", (int)*(hex++));
		break;
	}
	return (dest);
}



const char *
dc_ValueToString (ptr, type)
void *ptr;
DC_ElemType type;
/*
 * Convert the void pointer to an element union and then to a string.
 * The return value is identical to dc_ElemToString.
 */
{
	DC_Element e;

	dc_AssignElement (&e, ptr, type);
	return (dc_ElemToString(&e, type));
}



const char *
dc_PrintElement (e, type)
DC_Element *e;
DC_ElemType type;
/*
 * Same as dc_ElemToString except special characters are converted to their
 * escaped sequences.
 */
{
	static char *dest = NULL;
	static char buf[256];
	const char *result, *src;
	char *obj;

	if ((dest != buf) && (dest != NULL))
		free (dest);
	dest = buf;		/* default to writing in static memory */
	result = dc_ElemToString (e, type);
	/*
	 * We don't really have anything to do unless the type is
	 * character or string.
	 */
	if (type != DCT_String && type != DCT_Char)
		return (result);
	/*
	 * We'll need, at the most, twice as much space for backslashes
	 */
	if (2*strlen(result) >= sizeof(buf))
		dest = (char *)malloc((2 * strlen(result)) + 1);
	dest[0] = '\0';
	src = result;
	obj = dest;
	while (*src)
	{
		switch (*src)
		{
		   case '\t':
			strcat (obj, "\\t");
			break;
		   case '\0':
			strcat (obj, "\\0");
			break;
		   case '\n':
			strcat (obj, "\\n");
			break;
		   case '\b':
			strcat (obj, "\\b");
			break;
		   case '\f':
			strcat (obj, "\\f");
			break;
		   case '\r':
			strcat (obj, "\\r");
			break;
		   default:
			*obj = *src;
			--obj;
		}
		++src;
		obj += 2;
	}
	*obj = '\0';
	return (dest);
}



const char *
dc_PrintValue (ptr, type)
void *ptr;
DC_ElemType type;
/*
 * Same as dc_PrintElement except it first converts a pointer to void
 * to an element before calling dc_PrintElement
 */
{
	DC_Element e;

	dc_AssignElement (&e, ptr, type);
	return (dc_PrintElement (&e, type));
}




/*
 * For convenience, this could be changed to convert to long double and all
 * other conversions can just convert from long double to their desired
 * target.
 */
int
dc_ConvertFloat (f, ptr, type)
float *f;
void *ptr;
DC_ElemType type;
/*
 * De-reference a void pointer to type, cast it to float and store it.
 * Return zero if we fail.
 */
{
	int ret = 1;

	switch (type)
	{
	   case DCT_Float:
		*f = *(float *)ptr;
		break;
	   case DCT_Double:
		*f = (float) *(double *)ptr;
		break;
	   case DCT_LongDouble:
		*f = (float) *(LongDouble *)ptr;
		break;
	   case DCT_Char:
		*f = (float) *(char *)ptr;
		break;
	   case DCT_UnsignedChar:
		*f = (float) *(unsigned char *)ptr;
		break;
	   case DCT_ShortInt:
		*f = (float) *(short *)ptr;
		break;
	   case DCT_UnsignedShort:
		*f = (float) *(unsigned short *)ptr;
		break;
	   case DCT_Integer:
		*f = (float) *(int *)ptr;
		break;
	   case DCT_UnsignedInt:
		*f = (float) *(unsigned int *)ptr;
		break;
	   case DCT_LongInt:
		*f = (float) *(long int *)ptr;
		break;
	   case DCT_UnsignedLong:
		*f = (float) *(unsigned long *)ptr;
		break;
#ifdef notdef	/* would this be atof() or byte->float? */
	   case DCT_String:
		*f = *(char **)ptr;
		break;
#endif
	   case DCT_Boolean:
		*f = (float) *(unsigned char *)ptr;
		break;
	   case DCT_ZebTime:
		*f = (float) (((ZebTime *)ptr)->zt_Sec);
		break;
#ifdef notdef
	   case DCT_VoidPointer:
		*f = (float) ptr;
	        break;
	   case DCT_Element:
		*e = *(DC_Element *)ptr;
		break;
#endif
	   default:
		*f = 0;
		ret = 0;
		break;
	}
	return (ret);
}





void *
dc_DefaultBadval (type)
DC_ElemType type;
/*
 * Return a pointer to a default bad value for the given element type.
 */
{
	static unsigned char cbv = '\0';
	static short sbv = (short)CFG_DC_DEFAULT_BADVAL;
	static int ibv = (int)CFG_DC_DEFAULT_BADVAL;
	static float fbv = (float)CFG_DC_DEFAULT_BADVAL;
	static double dbv = (double)CFG_DC_DEFAULT_BADVAL;
	static LongDouble ldbv = (LongDouble)CFG_DC_DEFAULT_BADVAL;

	switch (type)
	{
	   case DCT_Float:
		return (&fbv);
		break;
	   case DCT_Double:
		return (&dbv);
		break;
	   case DCT_LongDouble:
		return (&ldbv);
		break;
	   case DCT_Char:
		return (&cbv);
		break;
	   case DCT_UnsignedChar:
		return (&cbv);
		break;
	   case DCT_ShortInt:
		return (&sbv);
		break;
	   case DCT_UnsignedShort:
		return (&sbv);
		break;
	   case DCT_Integer:
		return (&ibv);
		break;
	   case DCT_UnsignedInt:
		return (&ibv);
		break;
	   case DCT_LongInt:
		return (&ibv);
		break;
	   case DCT_UnsignedLong:
		return (&ibv);
		break;
	   case DCT_String:
		return (&cbv);
		break;
	   case DCT_Boolean:
		return (&cbv);
		break;
	   case DCT_ZebTime:
		return (NULL);
		break;
	   default:
		return (NULL);
		break;
	}
	return (NULL);
}


