/*
 * Command execution code for clients which only want to send
 * commands and will not need to perform them.  These routines
 * do not require the UI library.
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

# include <string.h>

# include "defs.h"
# include <config.h>
# include "message.h"

RCSID ("$Id: cmd_exec.c,v 2.3 1996-09-11 14:32:58 granger Exp $")


/*
 * The remote command handler set by the application
 */
static int (*CP_Handler) FP((char *cmd)) = NULL;

/*
 * Internal command protocol handler
 */
static int cp_RunCommand FP((Message *));


void
cp_Exec (process, command)
char *process, *command;
/*
 * Send the given command to this process.
 */
{
	msg_send (process, MT_COMMAND, FALSE, command, strlen (command) + 1);
}



void
cp_SetupCmdHandler (fn)
int (*fn) FP((char *));
/*
 * Set up to execute incoming commands.
 */
{
	CP_Handler = fn;
	msg_AddProtoHandler (MT_COMMAND, cp_RunCommand);
	
}



static int
cp_RunCommand (msg)
Message *msg;
/*
 * Deal with a command protocol packet.
 */
{
	if (CP_Handler)
		(*CP_Handler) (msg->m_data);
	return (0);
}
