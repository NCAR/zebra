/*
 * SerialStream method implementations.
 */

//#include <defs.h>
//#undef bool

//RCSID ("$Id: SerialStream.cc,v 1.6 1998-10-20 20:44:45 granger Exp $")

#include "SerialStream.hh"
#include "SerialBuffer.hh"
#include "XDR.hh"

/*
 * ---- SerialStream base class ----
 */

#ifdef notdef
int
SerialStream::translate (void *data, xdr_translator xp)
{
	xdrs->setpos (sbuf->Position ());
	xdrs->translate (data, xp);
	sbuf->Seek (xdrs->getpos());
	return (0);
}
#endif

#define SS_METHOD(T) \
int SerialStream::translate (T &tp) \
{ \
	  xdrs->setpos (sbuf->Position()); \
	  *xdrs << tp; \
	  sbuf->Seek (xdrs->getpos()); \
	  return (0); \
}

SS_METHOD(char);
SS_METHOD(u_char);
SS_METHOD(int);
SS_METHOD(u_int);
SS_METHOD(short);
SS_METHOD(u_short);
SS_METHOD(float);
SS_METHOD(double);
SS_METHOD(quad_t);
SS_METHOD(u_quad_t);

#undef SS_METHOD

void
SerialStream::Seek (long position)
{
	if (xdrs)
		xdrs->setpos (position);
}
		

void
SerialStream::Relocate (void *buffer, long length)
{
	if (xdrs)
	{
		// xdrs->destroy ();
		xdrs->create (buffer, length);
	}
}


SerialStream::~SerialStream ()
{
	// We don't delete sbuf because we didn't create it
	if (xdrs)
		delete xdrs;
}


/*
 * ---- Encoding ----
 */

const char SerialEncodeStream::zero[4] = { '\0', '\0', '\0', '\0' };


SerialEncodeStream::SerialEncodeStream (SerialBuffer &buf) :
	SerialStream (buf)
{
	// We need an encoding xdr stream
	xdrs = new MemWriteXDR (buf.getBuffer(), buf.Length());
}


int
SerialEncodeStream::translate (string &s)
{
	int len = s.length();
	int pad = 4 - (len % 4);
	len += pad;
	*this << len;
	sbuf->Write (s.c_str(), len-pad);
	sbuf->Write (zero, pad);
	return 0;
}



int
SerialEncodeStream::cstring (char *s, long /*max*/)
{
	// Write the string into the buffer, with null
	// characters to round out to a multiple of 4 bytes.
	// The first integer contains the length, INCLUDING the null
	// terminators, so if the length is zero the pointer is NULL.
	// A length of one is an empty string.

	if (s)
	{
		int len = strlen (s);
		int pad = 4 - (len % 4);
		len += pad;
		*this << len;
		sbuf->Write (s, len-pad);
		sbuf->Write (zero, pad);
	}
	else
	{
		int len = 0;
		*this << len;
	}
	return (0);
}



void
SerialEncodeStream::opaque (void *data, long len)
{
	sbuf->Write (data, len);
}


int
SerialEncodeStream::translate (Serializable &object)
{
	// Encode this object onto the buffer
	object.encode (*sbuf);
	return (0);
}


/* 
 * ---- Decoding ----
 */

SerialDecodeStream::SerialDecodeStream (SerialBuffer &buf) :
	SerialStream (buf)
{
	// We need a decoding memory xdr stream
	xdrs = new MemReadXDR (buf.getBuffer(), buf.Length());
}


int
SerialDecodeStream::translate (string &s)
{
	int len;

	*this >> len;
	s = static_cast<char *>(sbuf->Advance (len));
	return 0;
}



int
SerialDecodeStream::cstring (char *s, long max)
{
	int len;

	*this >> len;
	int end = (len <= max) ? len : max;
	sbuf->Read (s, end);
	if (len > max)
		sbuf->Skip (len - max);
	if (end > 0)
		s[end-1] = '\0';
	return (0);
}


void
SerialDecodeStream::opaque (void *data, long len)
{
	sbuf->Read (data, len);
}


int
SerialDecodeStream::translate (Serializable &object)
{
	// Decode this object onto the buffer
	object.decode (*sbuf);
	return (0);
}


/*
 * ---- Counting ----
 */

/* Static members */

long
SerialCountStream::Size (int n)
{
	return XDRStream::Length (n);
}

/* Size translators */

#define SIZE_TRANSLATE(T) \
int \
SerialCountStream::translate (T &tp) \
{ \
	  count += XDRStream::Length (tp); \
	  return (0); \
}

SIZE_TRANSLATE(char);
SIZE_TRANSLATE(u_char);
SIZE_TRANSLATE(int);
SIZE_TRANSLATE(u_int);
SIZE_TRANSLATE(short);
SIZE_TRANSLATE(u_short);
SIZE_TRANSLATE(float);
SIZE_TRANSLATE(double);
SIZE_TRANSLATE(quad_t);
SIZE_TRANSLATE(u_quad_t);

#undef SIZE_TRANSLATE

#ifdef notdef
/*
 * Generic size translator given an XDR routine
 */
int
SerialCountStream::translate (void *data, xdr_translator xp)
{
	count += XDRStream::Length (xp, data);
	return (0);
}
#endif


int
SerialCountStream::translate (string &s)
{
	int len = s.length();
	*this << len;
	Add (len + 4 - (len % 4));
	return 0;
}



int
SerialCountStream::cstring (char *s, long /*max*/)
{
	int len = 0;
	*this << len;		// Size of int length flag
	if (s)			// Size of padded string if non-NULL
	{
		len = strlen (s); 
		Add (len);
		Add (4 - (len % 4));
	}
	return (0);
}



void
SerialCountStream::opaque (void * /*data*/, long len)
{
	Add (len);
}



int
SerialCountStream::translate (Serializable &object)
{
	// Add this object's encoded size to the count
	count += object.encodedSize (*sbuf);
	return (0);
}
