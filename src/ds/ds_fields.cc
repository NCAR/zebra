//
// Fields handling interface using the new C++ Field class.  This module 
// replaces the old Fields.c, but provides the same functions.
//
//		Copyright (C) 1996 by UCAR
//	University Corporation for Atmospheric Research
//		   All rights reserved
//
// No part of this work covered by the copyrights herein may be reproduced
// or used in any form or by any means -- graphic, electronic, or mechanical,
// including photocopying, recording, taping, or information storage and
// retrieval systems -- without permission of the copyright owner.
// 
// This software and any accompanying written materials are provided "as is"
// without warranty of any kind.  UCAR expressly disclaims all warranties of
// any kind, either express or implied, including but not limited to the
// implied warranties of merchantibility and fitness for a particular purpose.
// UCAR does not indemnify any infringement of copyright, patent, or trademark
// through use or modification of this software.  UCAR does not provide 
// maintenance or updates for its software.
//

# include <ctype.h>
# include <sys/types.h>
# include "Field.h"
extern "C"
{
#	include <message.h>
}
# include "ds_fields.h"

MAKE_RCSID ("$Id")


//
// Our field cache
//
static Field **FList = 0;
static int MaxFlds = 0;
static int NFlds = 0;

//
// Warn about field names not acceptable to netCDF?
//
static int Warnings = 0;

//
// An empty string
//
static char *EmptyString = "";

//
// Local prototypes
//
static int F_CheckName( const char *name );


//
// Begin the C interface functions here
//
extern "C" 
{

void
F_Init( void )
{
#ifdef CFG_WARN_FIELD_NAMES
    Warnings = 1;
#else
    Warnings = 0;
#endif // CFG_WARN_FIELD_NAMES
}



void
F_Warnings( int on )
{
    Warnings = on;
}

	
	
void
F_Closure( void )
/*
 * Reset to zero fields and destroy our field cache.
 */
{
    delete[] FList;
    FList = 0;
    NFlds = MaxFlds = 0;
}




void
F_Reset( void )
/*
 * Forget all fields and return to our original initialized state.
 */
{
    F_Closure();
    F_Init();
}




FieldId
F_Lookup( const char* name )
//
// Turn this name into a field ID.  The name is parsed in the same way as
// in field derivation definitions:
//
//	<name>[<type>][<units>][<description>]
//
// where the name can be truncated at any point when all the remaining pieces
// are empty.  E.g., all of the following are legal:
//
//	tdry
//	tdry[temp][degC][temperature]
//	tdry[][degC]
//	tdry[temp]
//	tdry[temp][]
//	tdry[temp][][]
//
// Note that the first four will yield different fields, while the last
// three are all equivalent.
//
{
    char *openbracket, *closebracket;
    char *fname, *ftype = 0, *funits = 0, *fdesc = 0;
//
// Copy the string (we may overwrite some of the characters here with nulls)
//
    char *copy = new char[strlen( name ) + 1];
    strcpy( copy, name );
//
// Field name first
// 
    fname = copy;
//
// If there's an open bracket somewhere, we have to parse for other stuff...
//
    if ((openbracket = strchr( copy, '[' )) != 0)
    {
    //
    // Get the type
    //
	if ((closebracket = strchr( openbracket, ']' )) == 0)
	{
	    msg_ELog( EF_PROBLEM, "Badly formed field name '%s'", name );
	    return BadField;
	}

	ftype = openbracket + 1;
	*openbracket = 0;	// null terminate fname
	*closebracket = 0;	// null terminate ftype

	openbracket = closebracket + 1;
    //
    // Maybe units
    //
	if (*openbracket)
	{
	    if (*openbracket != '[' ||
		(closebracket = strchr( openbracket, ']' )) == 0)
	    {
		msg_ELog( EF_PROBLEM, "Badly formed field name '%s'", name );
		return BadField;
	    }

	    funits = openbracket + 1;
	    *closebracket = 0;	// null terminate funits

	    openbracket = closebracket + 1;
	}
    //
    // Maybe a description, too
    //
	if (*openbracket)
	{
	    if (*openbracket != '[' ||
		(closebracket = strchr( openbracket, ']' )) == 0)
	    {
		msg_ELog( EF_PROBLEM, "Badly formed field name '%s'", name );
		return BadField;
	    }

	    fdesc = openbracket + 1;
	    *closebracket = 0;	// null terminate funits

	    openbracket = closebracket + 1;
	}
    //
    // If there's anything else, besides whitespace, it's a problem
    //
	while (*openbracket && isspace(*openbracket))
	    ++openbracket;
	if (*openbracket)
	{
	    msg_ELog( EF_PROBLEM, "Badly formed field name '%s'", name );
	    return BadField;
	}
    }
//
// Create a Field and return the associated FieldId
//
    Field fld( fname, ftype, funits, fdesc );
    delete[] copy;
    
    return F_FieldId( fld );
}



FieldId
F_Declared( const char* name )
//
// Try to find this field.  Return BadField if not found rather than
// implicitly declaring it.
//
{
    return F_Lookup( name );
}



FieldId
F_DeclareField( const char* name, const char* desc, const char* units )
//
// Define this field to the system.
//
{
    Field	fld( name, 0, units, desc );
    return ( F_FieldId( fld ) );
}



FieldId
F_Field( const char* name, const char* type, const char* desc, 
	 const char* units )
//
// Define this typed field to the system.
//
{
    Field	fld( name, type, units, desc );
    return ( F_FieldId( fld ) );
}



FieldId
F_Alias( const char* name, const char* alias )
//
// Cause "alias" to be equivalent to the existing field "name".
// Fails and returns BadField if "name" does not exist.
//
{
	msg_ELog (EF_INFO, "F_Alias() is no more, so stop calling it!");
	return BadField;
}




char*
F_GetName( FieldId id )
//
// Turn this ID back into just the field name.  The string returned should be 
// treated as read-only.
//
{
    const Field* fld = (const Field*)id;

    if (fld && fld->Name())
	return (char*)fld->Name(); // kludge: cast Name() to drop const
    else
	return EmptyString;
}



char*
F_GetTypeName( FieldId id )
//
// Get the field type name for this ID.  The string returned should be 
// treated as read-only.
//
{
    const Field* fld = (const Field*)id;

    if (fld && fld->TypeName())
	return (char*)fld->TypeName(); // kludge: cast TypeName() to drop const
    else
	return EmptyString;
}



char*
F_GetFullName( FieldId id )
//
// Return the full format field name for the given id, with the type,
// units, and description if any.  The string returned should be treated as
// read-only, and is valid until the next call to F_GetName();
//
{
    const Field* fld = (const Field*)id;

    if (! fld)
	return NULL;
    else
	return (char*)fld->FullName(); // gotta cast from const char*
}




char*
F_GetDesc( FieldId id )
//
// Turn this ID back into a description.
//
{
    const Field* fld = (const Field*)id;

    if (fld && fld->Desc())
	return (char*)fld->Desc(); // kludge: cast Desc() to drop const
    else
	return EmptyString;
}


char*
F_GetUnits( FieldId id )
//
// Turn this ID into a units string.
//
{
    const Field* fld = (const Field*)id;

    if (fld && fld->Units())
	return (char*)fld->Units(); // kludge: cast Units() to drop const
    else
	return EmptyString;
}



zbool
F_CanYield( FieldId src, FieldId wanted, double *slope, double *intercept )
//
// Return true iff 'src' can yield 'wanted', with at most a linear conversion
//
{
    const Field* fsrc = (const Field*)src;
    const Field* fwanted = (const Field*)wanted;
    return fsrc->CanYield( *fwanted, slope, intercept );
}



FieldId
F_FieldId( const Field& f )
//
// Return the FieldId associated with a Field.  We have to keep our own cache
// of Fields here since we must guarantee that equivalent Fields yield exactly
// the same FieldId.
//
{
//
// Return the FieldId out of our cache if it's already there.  Binary
// search since we keep the array sorted.
//
    int	bottom = 0;
    int	top = NFlds - 1;

    while (bottom <= top)
    {
	int	middle = (bottom + top) / 2;
	int	cmp = f.CompareTo( *FList[middle] );
	if (cmp == 0)
	    return (FieldId)(FList[middle]);
	else if (cmp < 0)
	    top = middle - 1;
	else // cmp > 0
	    bottom = middle + 1;
    }

    int	loc = top + 1;
//
// Nope, gotta add it to the cache, so make sure we have enough space first
//
    if (NFlds == MaxFlds)
    {
	MaxFlds += 64;
	Field	**newFList = new Field*[MaxFlds];
	for (int f = 0; f < NFlds; f++)
	    newFList[f] = FList[f];
	delete[] FList;
	FList = newFList;
    }
//
// Copy this field into our cache
//
    memmove( FList + loc + 1, FList + loc, (NFlds - loc) * sizeof (Field*) );
    FList[loc] = new Field( f );
    NFlds++;
//
// If requested, give a warning for field names that aren't netCDF friendly
//
    if (Warnings)
	F_CheckName( f.Name() );
    
    return (FieldId)(FList[loc]);
}


} // end of extern "C"


//
// C++ interface stuff below
//

static int
F_CheckName( const char *name )
//
// Check this name for legality.  Return zero if it passes, nonzero else.
//
{
    const char *c = name;
    int check = 0;

    if (strlen (name) >= CFG_FIELD_NAME_LEN)
    {
	msg_ELog( EF_PROBLEM, "declare field '%s': %s %d characters", name, 
		  "name longer than", CFG_FIELD_NAME_LEN );
	++check;
    }

    c = name;
    while (*c)
    {
	if (! isalnum (*c) && (*c != '_') && (*c != '-'))
	    break;
	c++;
    }

    if (*c)
    {
	msg_ELog( EF_PROBLEM, "declare field '%s': %s", name, 
		  "illegal characters" );
	++check;
    }

    return (check);
}
