//
// The Zebra derivation table class
//
//		Copyright (C) 1997 by UCAR
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
# include "DerivTable.h"


DerivTable::DerivTable( void )
{
    flds = 0;
    root_nodes = 0;
    nderiv = maxderiv = 0;
}




DerivTable::~DerivTable( void )
{
    delete flds;
    delete[] root_nodes;
}




void
DerivTable::AddDerivation( const Field& f, DerivNode* dnode )
{
//
// Get a bigger derivation list if necessary
//
    if (nderiv == maxderiv)
    {
	maxderiv += 20;

	Field* newflist = new Field[maxderiv];
	for (int i = 0; i < nderiv; i++)
	    newflist[i] = flds[i];

	delete[] flds;
	flds = newflist;

	DerivNode** newdlist = new DerivNode*[maxderiv];
	memcpy( newdlist, root_nodes, nderiv * sizeof( DerivNode* ) );
	delete[] root_nodes;
	root_nodes = newdlist;
    }
//
// Add this new derivation to the list
//
    root_nodes[nderiv] = dnode;
    flds[nderiv] = f;
    nderiv++;

# ifdef VERBOSE
    cout << f << " = " << *dnode << ";\n\n";
# endif
}



const DerivNode*
DerivTable::NthDerivation( Field& f, int n )
//
// If it exists, return the nth derivation that can yield f, otherwise 0.
//
{
    int	which = 0;

    for (int i = 0; i < nderiv; i++)
    {
	if (flds[i].CanYield( f ))
	{
	    if (which == n)
		return root_nodes[i];
	    else
		which++;
	}
    }
    return 0;
}
