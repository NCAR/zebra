/*
 * Set up configuration variables so that they are available at the UI level.
 */
static char *rcsid = "$Id: ConfigVars.c,v 1.1 1991-12-20 20:03:06 kris Exp $";
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
# include <config.h>
# include <ui_symbol.h>





void
SetupConfigVariables ()
/*
 * Set up the configuration variables.  You must have called ui_init first.
 */
{
	stbl vtable = usy_g_stbl ("ui$variable_table");
	SValue v;

	v.us_v_ptr = BASEDIR;
	usy_s_symbol (vtable, "c$basedir", SYMT_STRING, &v);

	v.us_v_ptr = RDSSDIR;
	usy_s_symbol (vtable, "c$rdssdir", SYMT_STRING, &v);

	v.us_v_ptr = LIBDIR;
	usy_s_symbol (vtable, "c$libdir", SYMT_STRING, &v);

	v.us_v_ptr = BINDIR;
	usy_s_symbol (vtable, "c$bindir", SYMT_STRING, &v);

	v.us_v_ptr = DATADIR;
	usy_s_symbol (vtable, "c$datadir", SYMT_STRING, &v);
}
