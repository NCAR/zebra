//
// dsPlatform class methods
//
/*		Copyright (C) 1998 by UCAR
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

# include "dsPlatform.h"

dsPlatform::dsPlatform (const char *name, int pindex)
//
// Create a platform.
//
{
	pname = new char[strlen(name) + 1];
	strcpy (pname, name);
	index = pindex;
}



dsPlatform::dsPlatform (const dsPlatform& old)
//
// Initializer.
//
{
	pname = new char[strlen (old.pname) + 1];
	strcpy (pname, old.pname);
	index = old.index;
}



float
dsPlatform::space () const
//
// How much space?
//
{
	int df;
	float total = 0;

	for (df = 0; df < files.ncontained (); df++)
		total += files.nth(df).size ();
	return (total / (float)(1024 * 1024));
}
