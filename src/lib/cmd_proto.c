/*
 * Command execution protocol code.
 */

/*		Copyright (C) 1987,88,89,90,91,92 by UCAR
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

# include <ui.h>
# include "defs.h"
# include <config.h>
# include "message.h"

MAKE_RCSID ("$Id: cmd_proto.c,v 1.5 1996-09-11 14:33:00 granger Exp $")



void
cp_SetupCmdProto ()
/*
 * Set up to execute incoming commands.
 */
{
	cp_SetupCmdHandler (ui_perform);
}


