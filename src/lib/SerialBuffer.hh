/*
 * $Id: SerialBuffer.hh,v 1.5 2004-10-22 22:42:28 burghart Exp $
 *
 * A simple Buffer subclassed and complicated with methods to help
 * serialize and deserialize objects to/from the buffer.
 */
#ifndef _SerialBuffer_hh_
#define _SerialBuffer_hh_

#include "Buffer.hh"
#include "Serializable.hh"
#include "SerialStream.hh"

// There could be two ways to override the serial streams used to
// encode/decode objects.  Specific Translatable classes could override the
// encodeStream(), decodeStream(), and countStream() methods of the
// Translatable interface.  Or, a SerialBuffer subclass could override the
// encodeStream(), decodeStream(), and countStream() methods of
// SerialBuffer, thereby changing the streams used by all objects
// translated on that buffer which HAVE NOT already overridden the default
// Translatable methods.
//
// To implement the above, the *Stream() methods of SerialBuffer need to
// call the object methods to get the appropriate stream, and the 
// default *Stream() methods in Translatable need to create the usual
// Serial streams.

///
/** Supports alternatives for converting an object into a serial stream of
    bytes, including the Translatable interface.  An object which intends
    to write itself into a serial buffer must support the serializable
    interface.
    */
class SerialBuffer : public Buffer
{
public:

	/// Constructors, with and without an existing byte buffer
	SerialBuffer (void *buf, long len) : 
		Buffer(buf, len), 
		cs(0), es(0), ds(0)
	{ }

	/// Create a buffer with an initial length, at least as long as BUFFER
	SerialBuffer (long len = BUFFER_ZONE, int zone_ = BUFFER_ZONE) :
		Buffer(len, zone_),
		cs(0), es(0), ds(0)
	{ }

	// ---- The interface for encoding/decoding objects on this buffer

	/// Encode a serializable object onto this buffer
	int encode (Serializable &obj)
	{
		return (obj.encode (*this));
	}

	/// Decode a serializable object from this buffer
	int decode (Serializable &obj)
	{
		return (obj.decode (*this));
	}

	/// Compute the serialized size of this object on this buffer
	long encodedSize (Serializable &obj)
	{
		return (obj.encodedSize (*this));
	}

	/// Return a counting stream on this buffer
	SerialCountStream *countStream ()
	{
		if (! cs)
		{
			cs = new SerialCountStream (*this);
		}
		cs->Zero ();		// Zero the counter
		return (cs);
	}

	/// Return an encoding stream for this buffer
	SerialEncodeStream *encodeStream ()
	{
		if (! es)
		{
			es = new SerialEncodeStream (*this);
		}
		// Make sure the stream's position syncs with ours

		return (es);
	}

	/// Return a decoding stream for this buffer
	SerialDecodeStream *decodeStream ()
	{
		if (! ds)
		{
			ds = new SerialDecodeStream (*this);
		}
		return (ds);
	}

	virtual ~SerialBuffer ();

protected:
	SerialCountStream *cs;
	SerialEncodeStream *es;
	SerialDecodeStream *ds;

	/*
	 * Sub-streams are expected to sync their stream positions with
	 * the buffer before any access to the stream, and then update
	 * the buffer position.  Only relocations are handled here, since
	 * that's a more costly operation and we don't want to be doing
	 * it on every translation.
	 */

	/*
	 * Override the Relocate() method from Buffer so that
	 * we can keep our serial streams in sync with buffer changes.
	 */
	virtual void Relocate (void *buf, long len)
	{
		if (es)
			es->Relocate (buf, len);
		if (ds)
			ds->Relocate (buf, len);
	}

};


template <class T>
inline long serialCount (SerialBuffer &sbuf, const T &t)
{
	SerialCountStream *cs = sbuf.countStream();
	*cs << const_cast<T &>(t);
	return (cs->Count());
}



template <class T>
inline SerialBuffer & operator<< (SerialBuffer &sbuf, const T &t)
{
	SerialEncodeStream *es = sbuf.encodeStream();
	sbuf.Need (serialCount (sbuf, t));
	*es << const_cast<T &>(t);
	return (sbuf);
}



template <class T>
inline SerialBuffer & operator>> (SerialBuffer &sbuf, T &t)
{
	SerialDecodeStream *ds = sbuf.decodeStream();
	*ds >> t;
	return (sbuf);
}





#endif /* _SerialBuffer_hh_ */

