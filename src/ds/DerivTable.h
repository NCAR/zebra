//
// The Zebra DerivTable, a class for a list of derivation trees
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

# ifndef __zebra_DerivTable_h_
# define __zebra_DerivTable_h_

# include "Field.h"
# include "DerivNode.h"

class DerivTable
{
public:
    DerivTable( void );
    ~DerivTable( void );
//
// When AddDerivation() is called, the passed DerivNode becomes the property
// of the DerivTable
//
    void AddDerivation( const Field& fld, DerivNode* root );
    const DerivNode* NthDerivation( Field& fld, int n );
private:
    Field*	flds;		// list of derivable fields
    DerivNode** root_nodes;	// list of associated derivation trees
    int	nderiv;			// current # of entries in the derivation list
    int	maxderiv;		// space in the derivation list
};


# endif //__zebra_DerivTable_h_

