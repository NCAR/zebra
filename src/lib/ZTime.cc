/* ZTime -*- C++ -*- implementation. */
/*		Copyright (C) 1987-2000 by UCAR
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

# include "ZTime.h"
# include <iostream>

RCSID ("$Id: ZTime.cc,v 2.3 2002-09-17 20:00:19 granger Exp $")

/* ================================================================
 * So far the lone C++ implementation, put here to avoid including
 * iostream in ZTime.h.
 */

std::ostream & 
operator<< (std::ostream &out, const ZebraTime &t)
{
	out << TC_AscTime (&t, TC_Full);
	return (out);
}


