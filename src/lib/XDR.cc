/*
 * Implementation of the XDR class and subclasses.
 */

#include "XDR.hh"

#include <defs.h>

RCSID("$Id: XDR.cc,v 1.2 1997-08-01 19:34:36 granger Exp $")


void StdioXDR::create (FILE *fp, xdr_op op)
{
	results = 0;
	if (fp)
	{
		xdrstdio_create (&xdrs, fp, op);
		reset ();
	}
}


void MemoryXDR::create (void *addr, long len, xdr_op op)
{
	results = 0;
	if (addr && len > 0)
	{
		xdrmem_create (&xdrs, (char *)addr, len, op);
		reset ();
	}
}

