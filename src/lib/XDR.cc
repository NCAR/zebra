/*
 * Implementation of the XDR class and subclasses.
 */

#include "XDR.hh"

#include <defs.h>

RCSID("$Id: XDR.cc,v 1.3 1997-11-24 10:20:19 granger Exp $")


void StdioXDR::create (xdr_op op)
{
	results = 0;
	if (file)
	{
		xdrstdio_create (&xdrs, file, op);
		reset ();
	}
}


void MemoryXDR::create (xdr_op xop)
{
	results = 0;
	if (addr && len > 0)
	{
		xdrmem_create (&xdrs, (char *)addr, len, xop);
		reset ();
	}
	else
	{
		addr = 0;
		len = 0;
	}
}


void MemoryXDR::create (void *address, long length)
{
	// The XDR man pages for xdrmem_create suggest that we do not
	// need to recreate the XDR structure if the address has not
	// changed and the length has increased.  Someday this could be
	// a way to avoid excessive destory() and create() cycles.

	results = 0;
	if (this->addr)
		destroy ();
	this->addr = address;
	this->len = length;
	create (this->op);
}

