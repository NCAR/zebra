/*
 * Centralize the dsDetails convenience routines since they can be shared
 * by client and daemon and therefore should not be in Appl.c
 */

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

#include <unistd.h>
#include <stdio.h>
#include <string.h>

#include <defs.h>
#include <message.h>
#include <timer.h>
#include "DataStore.h"

RCSID ("$Id: Details.c,v 3.1 1996-11-19 08:11:21 granger Exp $")



int
ds_GetDetail (key, details, ndetail, v)
char *key;
dsDetail *details;
int ndetail;
SValue *v;
/*
 * Try to find this detail value in the list, returning TRUE iff it is
 * there.  If V is NULL, the detail value is not returned.
 */
{
	for (; ndetail > 0; details++, ndetail--)
	{
		if (! strcmp (key, details->dd_Name))
		{
			if (v)
				*v = details->dd_V;
			return (TRUE);
		}
	}
	return (FALSE);
}



int
ds_GetFloatDetail (key, details, ndetail, f)
char *key;
dsDetail *details;
int ndetail;
float *f;
{
	SValue v;

	if (ds_GetDetail (key, details, ndetail, &v))
	{
		*f = v.us_v_float;
		return (TRUE);
	}
	else
		return (FALSE);
}



int
ds_GetIntDetail (key, details, ndetail, i)
char *key;
dsDetail *details;
int ndetail;
int *i;
{
	SValue v;

	if (ds_GetDetail (key, details, ndetail, &v))
	{
		*i = v.us_v_int;
		return (TRUE);
	}
	else
		return (FALSE);
}



char *
ds_GetStringDetail (key, details, ndetail)
char *key;
dsDetail *details;
int ndetail;
{
	SValue v;

	if (ds_GetDetail (key, details, ndetail, &v))
		return ((char *)v.us_v_ptr);
	else
		return (NULL);
}



int
ds_SetDetail (key, details, n)
char *key;
dsDetail *details;
int n;
{
	details[n].dd_Name = key;
	details[n].dd_V.us_v_ptr = NULL;
	return (n+1);
}



int
ds_SetIntDetail (key, i, details, n)
char *key;
int i;
dsDetail *details;
int n;
{
	details[n].dd_Name = key;
	details[n].dd_V.us_v_int = i;
	return (n+1);
}



int
ds_SetStringDetail (key, s, details, n)
char *key;
char *s;
dsDetail *details;
int n;
{
	details[n].dd_Name = key;
	details[n].dd_V.us_v_ptr = s;
	return (n+1);
}



int
ds_SetFloatDetail (key, f, details, n)
char *key;
double f;
dsDetail *details;
int n;
{
	details[n].dd_Name = key;
	details[n].dd_V.us_v_float = (float)f;
	return (n+1);
}




