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

# include <iostream>
# include "DerivTable.h"

RCSID ("$Id")

DerivTable::DerivTable( void )
{
    flds = 0;
    root_nodes = 0;
    nderiv = maxderiv = 0;
}




DerivTable::~DerivTable( void )
{
    delete[] flds;
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



DerivNode*
DerivTable::NthDerivation( const Field& f, const int n ) const
//
// If it exists, return the nth derivation that can yield f, otherwise 0.
// We return a copy that must be deleted by the caller.
//
{
    int	which = 0, i;
    double slope, intercept;

    for (i = 0; i < nderiv; i++)
    {
	if (flds[i].CanYield( f, &slope, &intercept ))
	{
	    if (which < n)
		which++;
	    else
		break;
	}
    }
//
// We failed if we got to the end of our list of derivations
//
    if (i == nderiv)
	return 0;
//
// We have the nth derivation, so apply the slope and intercept if necessary
// and return
//
    DerivNode *dtree = root_nodes[i]->Copy();
    
    if (slope != 1.0)
	dtree = new OpDNode( "*", new ConstDNode( slope ), dtree );

    if (intercept != 0.0)
	dtree = new OpDNode( "+", new ConstDNode( intercept ), dtree );

    return dtree;
}
