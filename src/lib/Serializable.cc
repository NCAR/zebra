/*
 * Implementation of default methods for the Serializable and
 * Translatable virtual interfaces.
 */

#include <defs.h>

RCSID ("$Id: Serializable.cc,v 1.2 1998-02-25 22:17:29 burghart Exp $")

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


