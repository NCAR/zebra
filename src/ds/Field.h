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

# include <stdlib.h>
extern "C"
{
# include <udunits.h>
}

class FieldType;
class Field;

//
// Everybody needs to know about FT_Undefined (shared from Field.cc)
//
extern FieldType	FT_Undefined;



class FieldType
{
    public:
	FieldType (const char* tname);
	FieldType (void) { *this = FT_Undefined; }
	inline const char* Name(void) const { return(name); }
	inline bool operator==(const FieldType& fld) const
	{
		return (name == fld.name);
	}
    private:
	const char*	name;
};



class Field
{
    public:
	Field(const char* fname, const char* fdesc, const char* funits);
	Field(const char* fname, const char* type, const char* fdesc, 
	      const char* funits);
	Field(const char* fname, const FieldType& type, const char* fdesc, 
	      const char* funits);
	inline const char* Name(void) const { return (name); }
	inline const char* Desc(void) const { return (desc); }
	inline const char* Units(void) const { return (units); }
	bool CanYield(const Field &wanted, double *slope = NULL, 
		      double *intercept = NULL) const;
    private:
	const char*	name;
	const char*	desc;
	const char*	units;
	utUnit*		ud_units;
	FieldType	type;
};
