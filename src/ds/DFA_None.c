/*
 * Define some simple placeholder routines for formats whose methods
 * are not compiled.
 */
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

# include <stdio.h>

# include <defs.h>
# include <config.h>
# include <message.h>

RCSID ("$Id: DFA_None.c,v 3.2 1996-12-06 00:40:02 granger Exp $")

# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"
# include "dfa.h"
# include "DataFormat.h"



/* ARGSUSED */
int
fmt_QueryNotCompiled (file, begin, end, nsample)
char *file;
ZebTime *begin;
ZebTime *end;
int *nsample;
{
	msg_ELog (EF_PROBLEM, "cannot query file %s: format not compiled",
		  file);
	return (0);
}



/* ARGSUSED */
int
fmt_OpenNotCompiled (of, file, dp, write)
OpenFile *of;
char *file;
DataFile *dp;
int write;
{
	msg_ELog (EF_PROBLEM, "cannot open file %s: format not compiled",
		  file);
	return (0);
}



/* ARGSUSED */
int
fmt_CreateNotCompiled (ofp, fname, dfile, dc, details, ndetail)
OpenFile *ofp;
char *fname;
DataFile *dfile;
DataChunk *dc;
dsDetail *details;
int ndetail;
{
	msg_ELog (EF_PROBLEM, "cannot create file %s: format not compiled",
		  fname);
	return (0);
}


