/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 FBYTE4.C 21-Feb-92,9:20:00,`DAVES' First release for flipping bytes     */
/* 2 FBYTE4.C 27-Feb-92,12:41:52,`USER' Released for McIDAS-X Only           */
/* 3 FBYTE4.C 1-Mar-94,10:03:52,`RUSSD' Standardize function names           */
/* 4 FBYTE4.C 31-Mar-94,22:49:24,`BARRYR' Add proprietary statement          */
/* 5 FBYTE4_.C 2-May-94,16:37:20,`USER' Released                             */
/* 6 FBYTE4_.C 19-Feb-96,15:47:02,`DWS' reglue: modified file                */
/* 7 FBYTE4_.C 20-Feb-96,11:52:36,`USER' Released                            */
/**** McIDAS Revision History *** */

#include "mcidas.h"
#include "mcidasp.h"

   /* buffer - INTEGER*4 array to switch bytes  */
   /* n      - INTEGER number of 4 byte switches */

void
fbyte4_(void *buffer, Fint *n)
{
	char           *buf = buffer;
	Fint            i;
	char           *locbuf;
	char            a;
	char            b;
	char            c;
	char            d;

	for (i = 0; i < *n; i++)
	{
		locbuf = buf;
		a = *buf;
		++buf;
		b = *buf;
		++buf;
		c = *buf;
		++buf;
		d = *buf;
		++buf;
		*locbuf = d;
		++locbuf;
		*locbuf = c;
		++locbuf;
		*locbuf = b;
		++locbuf;
		*locbuf = a;
	}
}
