/*
 * Set up configuration variables so that they are available at the UI level.
 * This is its own module so that programs which don't need UI don't need
 * to link with it.
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
# include <unistd.h>
# include <string.h>

# include <config.h>
# include <ui_symbol.h>
# include "defs.h"
# include "message.h"
# include "setup.h"

RCSID("$Id: ConfigUI.c,v 2.2 1997-02-14 07:31:58 granger Exp $")



void
SetupConfigVariables ()
/*
 * Set up the configuration variables.  You must have called ui_init first.
 */
{
	stbl vtable = usy_g_stbl ("ui$variable_table");
	SValue v;
	
	InitDirVariables ();
/*
 * Now set the variables.
 */
	v.us_v_ptr = Basedir;
	usy_s_symbol (vtable, "c$basedir", SYMT_STRING, &v);
	v.us_v_ptr = Libdir;
	usy_s_symbol (vtable, "c$libdir", SYMT_STRING, &v);
	v.us_v_ptr = Bindir;
	usy_s_symbol (vtable, "c$bindir", SYMT_STRING, &v);
	v.us_v_ptr = Projdir;
	usy_s_symbol (vtable, "c$projdir", SYMT_STRING, &v);
/*
 * Data dir is separate.  RDSS doesn't change, I don't think.
 */
	v.us_v_ptr = Datadir;
	usy_s_symbol (vtable, "c$datadir", SYMT_STRING, &v);
	v.us_v_ptr = RDSSDIR;
	usy_s_symbol (vtable, "c$rdssdir", SYMT_STRING, &v);
/*
 * Lastly, make our message handler name and session available
 */
	v.us_v_ptr = (char *) msg_myname ();
	usy_s_symbol (vtable, "c$msgname", SYMT_STRING, &v);
	v.us_v_ptr = (char *) msg_SessionName ();
	usy_s_symbol (vtable, "c$session", SYMT_STRING, &v);
}






void
dm_SetupVariables ()
/*
 * Set up some UI indirect variables
 */
{
	stbl vtable = usy_g_stbl ("ui$variable_table");

	usy_c_indirect (vtable, "window_name", WindowName, SYMT_STRING,
			CFG_MSGNAME_LEN);
	usy_c_indirect (vtable, "display_manager", DisplayManager, 
			SYMT_STRING, CFG_MSGNAME_LEN);
	/*
	 * For compatibility with old gp UI code, which expects 'ourname'
	 * to hold the window name.
	 */
	usy_c_indirect (vtable, "ourname", WindowName, SYMT_STRING, 
			CFG_MSGNAME_LEN);
}

