/*
 * $Id: SerialStream.hh,v 1.9 2004-10-22 22:42:29 burghart Exp $
 *
 */
#ifndef _SerialStream_hh_
#define _SerialStream_hh_

# include <rpc/rpc.h>

class Serializable;
class SerialBuffer;
class MemoryXDR;
// # ifndef __sgi__
// class XDR;
// # endif

/*
 * We want basic string serialization built-in.
 */
#include <string>

/*
 * SerialStreams are responsible for keeping their position in sync with
 * their buffer, so all access which might move the XDRStream is surrounded
 * with calls to xdrs->setpos() and sbuf->Seek().  We're using Seek()
 * instead of setPosition() because we're assuming that space in the buffer
 * for objects being translated will have already been assured, either
 * in the buffer zone or by a call to Need().
 */


///
/** This is a helper class for translating Serializable objects to and from
    a SerialBuffer.  An overloaded translate method is bound according to
    the type being translated.  This model is very similar to and heavily
    dependent on the XDRStream class.
    */
class SerialStream
{
public:
	typedef std::string string;

	// typedef int (*xdr_translator)(XDR *, void *);

	///
	/** The constructor requires a reference to the SerialBuffer
	    which will serve as either input or output.
	    */
	SerialStream (SerialBuffer &buf) :
		sbuf (&buf), xdrs(0)
	{ }

	virtual ~SerialStream ();

	/*
	 * Most SerialStream methods simply pass the argument on to
	 * the XDR encoder or decoder stream which the subclass creates,
	 * then updates the buffer's position.
	 */
#define SS_TRANSLATE(T) \
	virtual int translate (T &tp)

	SS_TRANSLATE(char);
	SS_TRANSLATE(unsigned char);
	SS_TRANSLATE(int);
	SS_TRANSLATE(unsigned int);
	SS_TRANSLATE(short);
	SS_TRANSLATE(unsigned short);
	SS_TRANSLATE(long);
	SS_TRANSLATE(unsigned long);
	SS_TRANSLATE(float);
	SS_TRANSLATE(double);
#if __WORDSIZE != 64
	SS_TRANSLATE(quad_t);
	SS_TRANSLATE(u_quad_t);
#endif

#undef SS_TRANSLATE

#ifdef notdef
	///
	/** Types which have a xdr translation procedure can use this
	    method.  The SERIAL_XDR_OPERATOR(Type, XDRTranslator) macro
	    defines the binary operators << and >> to use this method for
	    the given type and XDR routine.
	    */
	virtual int translate (void *data, xdr_translator xp);
#endif

	virtual int translate (string &s) = 0;

	///
	/** A default method for translating strings must be implemented by
	    subclasses.  's' points to the character array in memory, and
	    'maxlen' is the maximum length of the array, including the
	    space for the null terminator.  (Maximum *string* length is
	    len-1).
	    */
	virtual int cstring (char *s, long maxlen) = 0;
	
	virtual int translate (Serializable &object) = 0;

	/* Add binary stream operators << and >> for every type for which
	 * there is a translate method.
	 */
#define SS_OPERATOR(T) \
	SerialStream &operator<< (T &tp) { translate (tp); return (*this); }; \
	SerialStream &operator>> (T &tp) { translate (tp); return (*this); }

	SS_OPERATOR(char);
	SS_OPERATOR(unsigned char);
	SS_OPERATOR(int);
	SS_OPERATOR(unsigned int);
	SS_OPERATOR(short);
	SS_OPERATOR(unsigned short);
	SS_OPERATOR(float);
	SS_OPERATOR(double);
	SS_OPERATOR(string);
	SS_OPERATOR(long);
	SS_OPERATOR(u_long);
#if __WORDSIZE != 64
	SS_OPERATOR(quad_t);
	SS_OPERATOR(u_quad_t);
#endif
	SS_OPERATOR(Serializable);

#undef SS_OPERATOR

	///
	/** The opaque method simply copies a fixed-length array of bytes
	    into and out of the serial buffer without any translation and
	    without any allocation.
	    */
	virtual void opaque (void *data, long len) = 0;

	/* Update the position of our stream */
	void Seek (long position);
		
	/* The buffer has changed, so our stream needs to be recreated */
	void Relocate (void *buffer, long length);

	/* Return our SerialBuffer */
	SerialBuffer *serialBuffer ()
	{
		return sbuf;
	}

protected:

	SerialBuffer *sbuf;
	MemoryXDR *xdrs;

};


#ifdef notdef
#define SERIAL_XDR_OPERATOR(T, XP) \
inline SerialStream &operator<< (SerialStream &ss, T &tp) \
{ \
	ss.translate ((void *)&tp, (SerialStream::xdr_translator)XP); \
	return (ss); \
}; \
inline SerialStream &operator>> (SerialStream &ss, T &tp) \
{ \
	ss.translate ((void *)&tp, (SerialStream::xdr_translator)XP); \
	return (ss); \
}
#endif

/*
 * Emulate "translatability" by defining serial stream operators on a class
 * with a translate(SerialStream &) method, thereby avoiding the overhead
 * of a virtual table pointer on lightweight objects.
 */

#define SERIAL_STREAMABLE(T) \
inline SerialStream & operator<< (SerialStream &ss, T &object) \
{ \
	object.translate (ss); \
	return (ss); \
} \
inline SerialStream & operator>> (SerialStream &ss, T &object) \
{ \
	object.translate (ss); \
	return (ss); \
}


///
/** The subclass of SerialStream which writes objects onto an encoding
    memory XDR stream and implements an encoding for strings.
    */
class SerialEncodeStream : virtual public SerialStream
{
public:

	SerialEncodeStream (SerialBuffer &sbuf);

	/* Inherit all of the SerialStream methods for primitive types */

	virtual int translate (string &s);

	int translate (Serializable &object);

	///
	/** Encode a null-terminated fixed-length array of characters. */
	inline int cstring (char *s)
	{
		return cstring (s, 0);
	}

	virtual int cstring (char *s, long /*maxlen*/);

	void opaque (void *data, long len);

private:
	static const char zero[4];

};



///
/** The decoding subclass of SerialStream.
 */
class SerialDecodeStream : virtual public SerialStream
{
public:

	SerialDecodeStream (SerialBuffer &sbuf);

	/* Inherit all of the SerialStream methods for primitive types */

	virtual int translate (string &s);

	int translate (Serializable &object);

	///
	/** Decode a null-terminated string into a fixed-length array. */
	virtual int cstring (char *s, long maxlen);

	void opaque (void *data, long len);

};



///
/** A helper class which counts the bytes which are translated through
    it rather than actually moving any bytes.  It overrides all of the
    serial stream methods to increment a counter instead.

    This subclass adds three methods to the SerialStream interface:
    Zero() resets the counter to zero, Count() returns the count,
    and Add() just adds the given length to the count.
    */
class SerialCountStream : virtual public SerialStream
{
public:
	// A class method for returning the size of a type
	static long Size (int n);

public:
	SerialCountStream (SerialBuffer &buf) :
		SerialStream (buf), count(0)
	{ }

	inline void Zero();

	inline long Count();

	inline void Add (long add);

	/*
	 * Now override the SerialStream methods for particular types.
	 * We don't need to override the method for Serializable objects,
	 * since it already does what we need it to: call translate on
	 * the object using ourself (a counter) as the stream.  Likewise,
	 * the operators << and >> call translate(), so that is the only
	 * method we need to override.
	 */
#define SIZE_TRANSLATE(T) \
	virtual int translate (T &tp)

	SIZE_TRANSLATE(char);
	SIZE_TRANSLATE(unsigned char);
	SIZE_TRANSLATE(int);
	SIZE_TRANSLATE(unsigned int);
	SIZE_TRANSLATE(short);
	SIZE_TRANSLATE(unsigned short);
	SIZE_TRANSLATE(float);
	SIZE_TRANSLATE(double);
	SIZE_TRANSLATE(long);
	SIZE_TRANSLATE(u_long);
#if __WORDSIZE != 64
	SIZE_TRANSLATE(quad_t);
	SIZE_TRANSLATE(u_quad_t);
#endif

#undef SIZE_TRANSLATE

	virtual void opaque (void *data, long len);

	/* We don't really need the max length, so provide a convenient
	   method to use directly on this stream type, which just calls
	   the virtual method with a meaningless max length. */
	inline int cstring (char *s)
	{
		return cstring (s, 0);
	}

	virtual int cstring (char *s, long /*maxlen*/);

	virtual int translate (string &s);

	virtual int translate (Serializable &object);

#ifdef notdef
	///
	/** Override the translate() method for unknown types
	    and use the given XDR procedure to get the length.
	    This will be the method called by the operators
	    << and >> when used on unknown types for a counting 
	    stream.
	    */
	virtual int translate (void *data, xdr_translator xp);
#endif

protected:

	long count;

};


inline void
SerialCountStream::Zero()
{ 
	count = 0;
}


inline void
SerialCountStream::Add (long add)
{ 
	count += add;
}


inline long
SerialCountStream::Count()
{
	return count;
}

#ifdef notyet			// g++ reports "not implemented yet"...
/*
 * This might be a more interesting way to do this, but it still requires
 * a class to be a subclass of Translatable with virtual methods.  Better
 * might be a SerialStreamable class with no virtual methods, which is
 * just an indication that the class has a translate method.  Until g++
 * implements it, though, we're stuck with the above macro.
 */
template <Translatable T>	// i.e. template <SerialStreamable T>
inline SerialStream & operator<< (SerialStream &ss, T &object)
{
	object.translate (ss);
	return (ss);
}
#endif


#endif /* _SerialStream_hh_ */

