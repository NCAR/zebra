/*
 * Time conversions and other utilities.
 */
static char *rcsid = "$Id: TCvt.c,v 2.1 1991-09-13 15:01:58 corbet Exp $";
/*		Copyright (C) 1987,88,89,90,91 by UCAR
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

# include "defs.h"
# include <sys/types.h>
# include <sys/time.h>





void
TC_SysToFcc (sys, fcc)
long sys;
time *fcc;
/*
 * Convert a system time to an fcc time.
 */
{
	struct tm *t = gmtime (&sys);

	fcc->ds_yymmdd = t->tm_year*10000 + (t->tm_mon + 1)*100 + t->tm_mday;
	fcc->ds_hhmmss = t->tm_hour*10000 + t->tm_min*100 + t->tm_sec;
}





long
TC_FccToSys (fcc)
time *fcc;
/*
 * Convert an FCC time into a system time.
 */
{
	struct tm t;

	t.tm_year = fcc->ds_yymmdd/10000;
	t.tm_mon = (fcc->ds_yymmdd/100) % 100 - 1;
	t.tm_mday = fcc->ds_yymmdd % 100;
	t.tm_hour = fcc->ds_hhmmss/10000;
	t.tm_min = (fcc->ds_hhmmss/100) % 100;
	t.tm_sec = fcc->ds_hhmmss % 100;
	t.tm_zone = (char *) 0;
	t.tm_wday = t.tm_isdst = t.tm_yday = 0;

	return (timegm (&t));
}

