/*
 * Consume McIDAS-X GVAR area files into the data store.
 */
/*		Copyright (C) 1995 by UCAR
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

# include <unistd.h>

# include <defs.h>
# include <message.h>

RCSID("$Id: gvarIngest.c,v 1.3 1997-03-11 19:39:14 granger Exp $")

# include "SatUI.h"


/*
 * Define a few symbols needed by some of the GVAR McIDAS modules
 */
long	*uc = NULL, *neguc = NULL, *ttyfd = NULL;


int
main (argc, argv)
int argc;
char **argv;
/*
 * Ingest a GVAR image
 */
{
	EnterUI ("gvarIngest", "GVAR", argc, argv);
	exit (0);
}

