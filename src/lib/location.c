/* Library routines and data dealing with Location structures */

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

#include "defs.h"

const Location LOC_NONE = { -99999, -99999, -99999 };

#ifdef notdef	/* macro in defs.h for now */
int loc_Bad (loc)
Location *loc;
/*
 * Return nonzero if this location does not make any sense, based upon
 * the lat and lon values.
 */
{
	return (loc->l_lat < -90 || loc->l_lat > 90 ||
		loc->l_lon > 180 || loc->l_lon < -180);

}
#endif

