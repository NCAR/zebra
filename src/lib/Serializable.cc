/*
 * Implementation of default methods for the Serializable and
 * Translatable virtual interfaces.
 */

//#include <defs.h>
//#undef bool

//RCSID ("$Id: Serializable.cc,v 1.3 1998-05-15 19:37:05 granger Exp $")

#include "Serializable.hh"
#include "SerialBuffer.hh"


int
Translatable::encode (SerialBuffer &buf)
{
	buf.Need (this->encodedSize (buf));
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
Translatable::encodedSize (SerialBuffer &buf)
{
	SerialCountStream *cs = buf.countStream ();
	this->translate (*cs);
	return (cs->Count ());
}


