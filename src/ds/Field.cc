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

# include <iostream.h>
# include <stdio.h>
# include <string.h>
# include "Field.h"



//
// The one field type everybody needs to know about.  Shared via Field.h.
//
FieldType	FT_Undefined("Undefined");

//
// Prototypes for non-member functions
//
const char* CachedString (const char* string);



Field::Field(const char* fname, const char* fdesc, const char* funits)
{
//
// We don't have a field type, so create this field with type FT_Undefined
//
	*this = Field(fname, FT_Undefined, fdesc, funits);
}




Field::Field(const char* fname, const char *type, const char* fdesc, 
	     const char* funits)
{
	*this = Field(fname, FieldType(type), fdesc, funits);
}




Field::Field(const char* fname, const FieldType& ftype, const char* fdesc, 
	     const char* funits)
{
	name = fname ? CachedString(fname) : 0;
	desc = fdesc ? CachedString(fdesc) : 0;
	if (funits)
	{
		units = CachedString(funits);
		ud_units = new(utUnit);
		if (utScan(funits, ud_units) != 0)
		{
			delete(ud_units);
			ud_units = 0;
		}
	}
	else
	{
		units = 0;
		ud_units = 0;
	}

	type = ftype;
}



bool
Field::CanYield(const Field& wanted, double *slope, double *intercept) const
//
// Return true iff this field can yield the field 'wanted', with at most a 
// units conversion.  If true is returned, then the slope and intercept
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
// Initialize slope and intercept for the case where unit names are the same
// and we short-circuit the call to utConvert()
//
	*s = 1.0;
	*i = 0.0;

	return ((!wanted.name || (wanted.name == name)) &&
		(!wanted.desc || (wanted.desc == desc)) &&
		(wanted.type == type) &&
		(!wanted.units || (wanted.units == units) ||
		 (ud_units && wanted.ud_units &&
		  utConvert(ud_units, wanted.ud_units, s, i) == 0)));
}

	
	

FieldType::FieldType (const char* tname)
{
	name = CachedString(tname);
}




const char*
CachedString(const char* string)
//
// If possible, return the string matching 'string' from our cache.  Otherwise,
// make a new cache entry and return that.  This allows that everybody who
// asks for a given string gets exactly the same pointer.  Hence, string
// equivalence comparisons between strings obtained from CachedString() are
// simple pointer equality tests.
//
{
	static char** stringcache = NULL;
	static int nstrings = 0;
	static int maxstrings = 0;
//
// Return the string out of our cache if it's already there.  Dumb linear
// search here.  If this gets too costly, we'll have to make the cache a
// sorted array.
//
	for (int i = 0; i < nstrings; i++)
		if (! strcmp (string, stringcache[i]))
			return (stringcache[i]);
//
// Nope.  Need to add a new string to the cache.
//
	if (nstrings == maxstrings)
	{
		maxstrings += 100;
		char** newcache = new (char*)[maxstrings];
		memcpy (newcache, stringcache, nstrings * sizeof (char*));
		delete (stringcache);
		stringcache = newcache;
	}

	stringcache[nstrings] = new char[strlen(string) + 1];
	strcpy (stringcache[nstrings], string);
	return (stringcache[nstrings++]);
}

	
	       

	
		
