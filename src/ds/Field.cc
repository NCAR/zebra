//
// The Zebra data store field class
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

# include <iostream>
# include <string.h>
# include <stdio.h>
# include "Field.h"
extern "C"
{
#	include <udunits.h>
#	include <message.h>
}

	

RCSID("$Id: Field.cc,v 3.6 2002-09-17 20:00:18 granger Exp $")

//
// The predefined field types.  Shared via Field.h.
//
const char* FT_Temp = "T";
const char* FT_Pres = "P";
const char* FT_RH = "rh";
const char* FT_DP = "T_d";
const char* FT_WSpd = "wspd";
const char* FT_WDir = "wdir";
const char* FT_UWind = "uwind";
const char* FT_VWind = "vwind";


//
// Function to maintain a class-wide string cache
//
static const char* CachedString( const char* string );


//
// Have we initialized udunits yet?
//
static int UDUnitsReady = 0;




Field::Field( const char* fname, const char* ftype, const char* funits, 
	      const char* fdesc )
{
//
// Get CachedString copies of the given name, type, units string, and
// description.  We treat zero-length strings the same as null pointers.
//
    name = (fname && fname[0]) ? CachedString( fname ) : 0;
    type = (ftype && ftype[0]) ? CachedString( ftype ) : 0;
    desc = (fdesc && fdesc[0]) ? CachedString( fdesc ) : 0;
    units = (funits && funits[0]) ? CachedString( funits ) : 0;
//
// If we have a units string, try to build ud_units.  If no units string, or
// if it isn't understood by utScan(), then leave ud_units as a null pointer.
//	
    if (units)
    {
	if (! UDUnitsReady)
	{
	    utInit( 0 );
	    UDUnitsReady = 1;
	}
    //
    // Check for '<' or '>' in the units name beforehand, since I know
    // those generate syntax errors in utScan().
    //
	ud_units = new utUnit;
	if (strchr( funits, '<' ) || strchr( funits, '>' ) ||
	    utScan( funits, ud_units ) != 0)
	{
	    msg_ELog( EF_DEBUG, "Non-standard units '%s' for field %s",
		      units, FullName() );
	    delete( ud_units );
	    ud_units = 0;
	}
    }
    else
	ud_units = 0;
}



Field::~Field( void )
{
    delete ud_units; 
};



const char*
Field::FullName( void ) const
//
// Return the "complete" field name, with type, units, and description.
// The string returned is valid until the next execution of CompleteName 
// by any Field.
//
{
    static char fullname[256];

    if (desc)
	sprintf( fullname, "%s[%s][%s][%s]", name ? name : "", 
		 type ? type : "", units ? units : "", desc );
    else if (units)
	sprintf( fullname, "%s[%s][%s]", name ? name : "", 
		 type ? type : "", units );
    else if (type)
	sprintf( fullname, "%s[%s]", name ? name : "", type );
    else
	sprintf( fullname, "%s", name ? name : "(Null Field)" );
    
    return( fullname );
}



    
const Field&
Field::operator =( const Field& src )
{
//
// name, type, units, and desc are cached strings, so we can copy directly
//
    name = src.name;
    type = src.type;
    units = src.units;
    desc = src.desc;
//
// Generate a separate ud_units, though.  We don't test the utScan() call,
// since it presumably worked for our source field...
//
    delete ud_units;

    if (src.ud_units)
    {
	ud_units = new utUnit;
	utScan( units, ud_units );
    }
    else
	ud_units = 0;

    return src;
}




char
Field::CanYield( const Field& wanted, double *slope, double *intercept ) const
//
// Return true iff this field can yield the field 'wanted', with at most a 
// linear units conversion.  If true is returned, then the slope and intercept
// for the units conversion are also returned.
//
{
//
// We must match name, desc, and type exactly if they are specified in
// 'wanted'.  If wanted units are given, we must either match them or be 
// able to convert to them.
//
    double	dummy, *s, *i;

    s = slope ? slope : &dummy;
    i = intercept ? intercept : &dummy;
//
// Initialize slope and intercept for the case where unit names are 
// exactly the same and we short-circuit the call to utConvert()
//
    *s = 1.0;
    *i = 0.0;

    return ((!wanted.name || (wanted.name == name)) &&
	    (!wanted.desc || (wanted.desc == desc)) &&
	    (!wanted.type || (wanted.type == type)) &&
	    (!wanted.units || (wanted.units == units) ||
	     (ud_units && wanted.ud_units &&
	      utConvert(ud_units, wanted.ud_units, s, i) == 0)));
}




std::ostream&
Field::PutTo( std::ostream& s ) const
{
    if (name)
	s << name;

    if (type || units || desc)
    {
	s << "[" << (type ? type : "") << "]";
	if (units || desc)
	{
	    s << "[" << (units ? units : "") << "]";
	    if (desc)
		s << "[" << desc << "]";
	}
    }

    return( s );
}


int
Field::CompareTo( const Field& f ) const
//
// Simple comparison function to allow for sorting Fields.  Return an integer
// less than zero if this is "less than" f, zero if they are "equal", and
// greater than zero if this is "greater than" f.
//
{
    int	cmp;

    if (name || f.name)
    {
	if (! name && f.name)
	    return -1;
	else if (name && ! f.name)
	    return 1;
	else if ((cmp = strcmp( name, f.name )) != 0)
	    return cmp;
    }

    if (type || f.type)
    {
	if (! type && f.type)
	    return -1;
	else if (type && ! f.type)
	    return 1;
	else if ((cmp = strcmp( type, f.type )) != 0)
	    return cmp;
    }
    
    if (units || f.units)
    {
	if (! units && f.units)
	    return -1;
	else if (units && ! f.units)
	    return 1;
	else if ((cmp = strcmp( units, f.units )) != 0)
	    return cmp;
    }

    if (desc || f.desc)
    {
	if (! desc && f.desc)
	    return -1;
	else if (desc && ! f.desc)
	    return 1;
	else if ((cmp = strcmp( desc, f.desc )) != 0)
	    return cmp;
    }

    return 0;
}
	


int
Field::operator ==( const Field& f ) const
//
// Equality operator
//
{
    return ((name == f.name) && (type == f.type) && (units == f.units) &&
	    (desc == f.desc));
}
	


std::ostream&
operator<<( std::ostream& s, const Field& f )
{
    return f.PutTo(s);
}




static const char*
CachedString( const char* string )
//
// If possible, return the string matching 'string' from our cache.  Otherwise,
// make a new cache entry and return that.  This ensures that everybody who
// asks for a given string gets exactly the same pointer.  Hence, string
// equivalence comparisons between strings obtained from CachedString() are
// simple pointer equality tests.
//
{
    static char** stringcache = NULL;
    static int nstrings = 0;
    static int maxstrings = 0;
//
// Return the string out of our cache if it's already there.  Binary
// search since we keep the array sorted.
//
    int	bottom = 0;
    int	top = nstrings - 1;

    while (bottom <= top)
    {
	int	middle = (bottom + top) / 2;
	int	cmp = strcmp( string, stringcache[middle] );
	if (cmp < 0)
	    top = middle - 1;
	else if (cmp == 0)
	    return stringcache[middle];
	else // cmp > 0
	    bottom = middle + 1;
    }
//
// Nope.  Need to add a new string to the cache.
//
    int	loc = top + 1;

    if (nstrings == maxstrings)
    {
    //
    // We're out of space so allocate a bigger array and copy the old
    // one over.
    //
	maxstrings += 100;
	char** newcache = new char*[maxstrings];
	memcpy( newcache, stringcache, nstrings * sizeof (char*) );
	delete[] stringcache;
	stringcache = newcache;
    }

    memmove( stringcache + loc + 1, stringcache + loc, 
	     (nstrings - loc) * sizeof (char*) );
    stringcache[loc] = new char[strlen(string) + 1];
    strcpy( stringcache[loc], string );
    nstrings++;
    return( stringcache[loc] );
}
