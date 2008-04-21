/*
 * $Id: Serializable.hh,v 1.2 1998-02-25 22:17:30 burghart Exp $
 *
 * The abstract class which defines the methods (interface) which a
 * serializable class must implement.
 */
#ifndef _Serializable_hh_
#define _Serializable_hh_

// We don't want to actually include SerialBuffer.hh just to declare our
// interface.  It doesn't do any good to inline anything here anyway, since
// they're all virtual methods.

class SerialBuffer;
class SerialStream;

///
/** Abstract virtual interface which serializable objects must inherit.
    */
class Serializable
{
public:
	/// 
	/** Serialize yourself onto this SerialBuffer.
	 */
	virtual int encode (SerialBuffer &buf) = 0;

	/// 
	/** Decipher your state from this SerialBuffer.  
	 */
	virtual int decode (SerialBuffer &buf) = 0;

	/// Return the encoded size of yourself
	virtual long encodedSize (SerialBuffer &buf) = 0;

        virtual ~Serializable()
        {}

};


///
/** Translatable objects can use a single translate() method to serialize
    themselves to and from a SerialStream, and they can take advantage of a
    default implementation of the encodedSize() method which uses a counting
    stream.  Perhaps a better name for this interface would be
    "serial-stream-able".  The default implementations of the encode(),
    decode(), and encodedSize() methods call the object's translate() method on
    the appropriate stream from the serial buffer.
    */
class Translatable : public Serializable
{
public:
	/// 
	/** Serialize yourself onto this SerialBuffer.  The default
	    implementation calls the SerialBuffer's translate method,
	    which in turn calls this object's translate method with an
	    encoding SerialStream.
	    */
	virtual int encode (SerialBuffer &buf);

	/// 
	/** Decipher your state from this SerialBuffer.  The default
	    implementation calls the SerialBuffer's translate method, 
	    which then calls this object's translate method with a
	    SerialStream which will decode from the buffer.
	    */
	virtual int decode (SerialBuffer &buf);

	/// 
	/** Translate this object onto a SerialStream (either an encode or
	    decode stream).  This method must be provided by the subclass.
	    */
	virtual void translate (SerialStream &ss) = 0;

	///
	/** The default encodedSize() implementation for a translatable
	    object simply translates the object onto a special counting
	    stream, which does not actually move any data.
	    */
	virtual long encodedSize (SerialBuffer &buf);

	/*
	 * Classes which want to use their own SerialStream subclasses
	 * for translations should override these methods, whose
	 * default implementations just return the default serial
	 * stream from the SerialBuffer.
	 */

	// virtual SerialStream *encodeStream (SerialBuffer &buf);
	// virtual SerialStream *decodeStream (SerialBuffer &buf);
	// virtual SerialStream *countStream (SerialBuffer &buf);
};




#ifdef notdef
// 
// A Serializable object which has a default constructor.
// This class provides a constructor which takes a SerialBuffer.

class InstantSerial : public Serializable // Get it? Instant cereal.
{
public:
	/* This should be the last constructor called, and it just
	   turns around and calls the object's decoder. */
	InstantSerial (SerialBuffer &buf)
	{
		this->decode (buf);
	}
};
#endif

#endif /* _Serializable_hh_ */

