/*
 * Implementation of the XDR class and subclasses.
 */

#include "XDR.hh"

#include <defs.h>

RCSID("$Id: XDR.cc,v 1.1 1997-05-07 17:21:04 granger Exp $")


void StdioXDR::create (FILE *fp, xdr_op op)
{
	xdrstdio_create (&xdrs, fp, op);
	reset ();
}


