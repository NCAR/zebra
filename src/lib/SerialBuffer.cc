/*
 * SerialBuffer methods.
 */

#include <defs.h>


#include "SerialBuffer.hh"



SerialBuffer::~SerialBuffer ()
{ 
	if (cs)
		delete cs;
	if (es)
		delete es;
	if (ds)
		delete ds;
}


