/*
 * Implementation of simple Buffer.
 */

/** Do we try to implement a malloc/copy-on-write-past-buffer behavior?
    Create the buffer on existing space, but if we need more space malloc
    our own and copy the existing into the new buffer?  */

#include <stdlib.h>

#include "Buffer.hh"

//#include <defs.h>
//RCSID ("$Id: Buffer.cc,v 1.3 1998-05-15 19:36:50 granger Exp $")


const int Buffer::BUFFER_ZONE = 256;


/* ----- Constructors ----- */

Buffer::Buffer (long len, int _zone) : 
	zone(_zone), own(1)
{
	init ();
	Need (len);
}



/*
 * Create a buffer on top of existing bytes
 */
Buffer::Buffer (void *buf, long len, int _zone) : 
	zone(_zone), own(0)
{
	init ();
	length = len;
	buffer = (char *)buf;
}



// Do not change the state of 'own', so that we know when 
// we're supposed to be allocating our own memory
void
Buffer::init ()
{
	length = 0;
	buffer = 0;
	position = 0;
}



int
Buffer::Need (long need)
{
	int ok = 1;
	int needlen = position + need + zone;
	char *curbuf = this->buffer;	// So we can detect changes
	long curlen = this->length;

	if (needlen <= 0)
		needlen = BUFFER_ZONE;

	if (! own)
	{
		if (position + need > length)
			ok = 0;
	}
	else if (! buffer && needlen > 0)
	{
		length = needlen;
		buffer = (char *) malloc (length);
	}
	else if (needlen > length)
	{
		// Try a simple growth increment of half current length
		//
		if (length + length/2 > needlen)
			length += length/2;
		else
			length = needlen;
		buffer = (char *) realloc (buffer, length);
	}
	if (buffer == 0) 	// something got messed up!
	{
		init ();
		ok = 0;
	}
	if (curlen != length || curbuf != buffer)
		Relocate (buffer, length);

	return (ok);
}


void
Buffer::Clear ()
{
	if (buffer && own)
		free (buffer);
	init ();
	Need (0);
}


Buffer::~Buffer ()
{
	if (buffer && own)
		free (buffer);
}

