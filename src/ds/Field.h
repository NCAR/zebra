//
// The Zebra data store field class
// $Id: Field.h,v 3.6 2002-09-17 20:00:18 granger Exp $
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

# ifndef __zebra_Field_h_
# define __zebra_Field_h_

# include <stdlib.h>
# include <iostream>

class utUnit;
class Field;

//
// The predefined field types from Field.cc 
//
extern const char	*FT_Temp, *FT_Pres, *FT_RH, *FT_DP;
extern const char	*FT_WSpd, *FT_WDir, *FT_UWind, *FT_VWind;

std::ostream& operator<<( std::ostream& s, const Field& f );

class Field
{
public:
    Field( const char* fname = 0, const char* type = 0, 
	   const char* funits = 0, const char* fdesc = 0 );
// use our assignment operator for copying
    Field( const Field& src ) { ud_units = 0; *this = src; };
    ~Field( void );
    inline const char* Name( void ) const { return (name); }
    inline const char* TypeName( void ) const { return (type); }
    inline const char* Desc( void ) const { return (desc); }
    inline const char* Units( void ) const { return (units); }
    const char* FullName( void ) const;
    char CanYield( const Field &wanted, double *slope = 0, 
		   double *intercept = 0) const;
    std::ostream& PutTo( std::ostream& s ) const;  
    int CompareTo( const Field& f ) const;
    const Field& operator =( const Field& src );
    int operator ==( const Field& f ) const;
    inline int operator !=( const Field& f ) const 
    { 
	return (! (*this == f)); 
    };
private:
    const char*	name;
    const char*	desc;
    const char*	units;
    const char*	type;
    utUnit*		ud_units;
};


# endif // __zebra_Field_h_
