/*
 * Implementation of default methods for the Serializable and
 * Translatable virtual interfaces.
 */

#include <defs.h>

RCSID ("$Id: Serializable.cc,v 1.1 1997-11-24 10:11:11 granger Exp $")

#include "Serializable.hh"
#include "SerialBuffer.hh"


int
Translatable::encode (SerialBuffer &buf)
{
	buf.Need (this->size (buf));
	this->translate (*(buf.encodeStream ()));
	return (0);
}


int
Translatable::decode (SerialBuffer &buf)
{
	this->translate (*(buf.decodeStream ()));
	return (0);
}


long
Translatable::size (SerialBuffer &buf)
{
	SerialCountStream *cs = buf.countStream ();
	this->translate (*cs);
	return (cs->Count ());
}


