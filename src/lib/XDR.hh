/*
 * $Id: XDR.hh,v 1.3 1997-11-24 10:20:20 granger Exp $
 *
 * C++ interface to the xdr layer.
 */
#ifndef _XDR_HH_
#define _XDR_HH_

#include <stdlib.h>
#include <rpc/types.h>
#include <rpc/xdr.h>


/*
 * Declare an XDR translator type, possibly different from xdrproc_t,
 * since we know this one includes arguments.
 */
///
typedef bool_t (*XDRTranslator)(XDR *, void *);

/*
 * Functionally, xdr encoders and decoders are the same.
 */
///
typedef XDRTranslator XDREncoder;
///
typedef XDRTranslator XDRDecoder;

/*
 * NOTE that only create() is virtual, and it is the method
 * which makes this class abstract.  The other xdr instance
 * methods are not virtual, since so far none of the subclasses
 * need to override those methods, so we can hopefully save a little
 * time by allowing static binding.
 *
 * Someday we may want to allow the base class to be instantiated
 * onto an existing xdr stream, e.g. XDRStream(XDR *xdrs), for
 * which subclasses would create their own XDR structures and pass
 * pointers to it to the superclass.
 */

///
/**
  Abstract base class for XDR stream subclasses.
 */
class XDRStream
{
public:

	// Constructors

	inline 
	XDRStream::XDRStream() : results(0) { }

	// ---------------- Class members which do not need a stream

	/// Free any allocated memory in a decoded structure.
	static void Free (XDRTranslator xp, void *data);

	/// Amount of space required to encode 'data' into XDR
	static inline unsigned long
	Length (XDRTranslator xp, const void *data)
	{
		return (xdr_sizeof ((xdrproc_t)xp, (char *)data));
	}

	/**
	  Overloaded CLASS methods for returning the xdr size
	  of a primitive type.
	 */
#define XDR_LENGTH(T) \
	static unsigned long Length (T & tp) \
	{ \
		return (Length ((XDRTranslator)xdr_##T , &tp)); \
	}

#ifdef notdef
	static unsigned long Length (T *tp) \
	{ \
		return (Length (xdr_##T , tp)); \
	};

	static unsigned long Length (char & tp)
	{
		return (Length (xdr_char , &tp));
	};

	// XDR_LENGTH(char);
#endif

	XDR_LENGTH(char);
	XDR_LENGTH(u_char);
	XDR_LENGTH(int);
	XDR_LENGTH(u_int);
	XDR_LENGTH(long);
	XDR_LENGTH(u_long);
	XDR_LENGTH(short);
	XDR_LENGTH(u_short);
	XDR_LENGTH(float);
	XDR_LENGTH(double);

#undef XDR_LENGTH

	// ---------------- Instance members

protected:

	XDR xdrs;
	bool_t results;

	bool_t check (bool_t status) { results &= status; return (status); }

	virtual void create (xdr_op op) = 0;

public:

	/// Destructor
	virtual ~XDRStream() { destroy (); }

	/// Destroy the current XDR stream
	virtual void destroy () { xdr_destroy (&this->xdrs); }

	/// Return result of operations since last reset.
	bool_t success() const { return (results); }

	/// Reset error flag.
	void reset() { results = 1; }

	/// Position control, not implemented for all streams
	unsigned int getpos ()
	{ 
		return (xdr_getpos (&this->xdrs));
	}

	/// Position control, not implemented for all streams
	bool_t setpos (const unsigned int pos)
	{ 
		return (xdr_setpos (&this->xdrs, (unsigned int)pos));
	}

	/* Lots of overloaded methods for the usual xdr
	 * procedures.  The << and >> operators are actually the
	 * same, but the convention is to use << for ENCODING TO
	 * the stream and >> for DECODING FROM the stream.  I
	 * suppose someday I could make this really complicated
	 * and use subclasses to distinguish encoding and
	 * decoding... whatever... 
	 */

#define XDR_METHOD(T) \
	bool_t translate (T &tp) \
	{ \
		return (check (xdr_##T (&this->xdrs, &tp))); \
	}; \
	XDRStream &operator<< (T &tp) { translate (tp); return (*this); } \
	XDRStream &operator>> (T &tp) { translate (tp); return (*this); } \

#ifdef notdef
	XDRStream &operator<< (T *tp) { translate (tp); return (*this); } \
	XDRStream &operator>> (T *tp) { translate (tp); return (*this); }
#endif

	XDR_METHOD(char);
	XDR_METHOD(u_char);
	XDR_METHOD(int);
	XDR_METHOD(u_int);
	XDR_METHOD(long);
	XDR_METHOD(u_long);
	XDR_METHOD(short);
	XDR_METHOD(u_short);
	XDR_METHOD(float);
	XDR_METHOD(double);

#undef XDR_METHOD

	/// User-supplied type translations
	bool_t translate (void *data, XDRTranslator xp)
	{
		return (check ((*xp) (&this->xdrs, data)));
	}

};


// Applications can add operators << and >> for their own types
// with this macro, given a type T and its xdr procedure XP.

#define XDR_OPERATOR(T, XP) \
XDRStream &operator<< (XDRStream &xdr, T &tp) \
{ \
	xdr.translate ((void *)&tp, XP); \
	return (xdr); \
} \
XDRStream &operator>> (XDRStream &xdr, T &tp) \
{ \
	xdr.translate ((void *)&tp, XP); \
	return (xdr); \
}

#ifdef notdef
XDRStream &operator<< (XDRStream *xdr, T &tp) \
{ \
	xdr->translate ((void *)&tp, XP); \
	return (*xdr); \
} \
XDRStream &operator>> (XDRStream *xdr, T &tp) \
{ \
	xdr->translate ((void *)&tp, XP); \
	return (*xdr); \
} \

#endif

// Simpler macro which creates the standard xdr procedure name given
// the type name.

#define XDR_TYPE_OPERATOR(T) \
XDR_OPERATOR(T, xdr_##T)

///
/**
  The XDR subclass built on stdio streams.
 */
class StdioXDR : public XDRStream
{
public:

	// Inherit the destructor

	virtual void destroy () 
	{
		file = 0;
		XDRStream::destroy ();
	}

protected:

	/// Protected constructor
	StdioXDR (FILE *fp, xdr_op op) : XDRStream(), file(fp)
	{
		StdioXDR::create (op);
	}

	virtual void create (xdr_op op);

	FILE *file;
};


///
/**
  A class for reading from a stdio xdr stream.
 */
class ReadXDR : public StdioXDR
{
public:
	/// Constructor

	ReadXDR (FILE *fp) : StdioXDR (fp, XDR_DECODE) { }

	// Inherit the destructor
};


///
/**
  A class for writing to a stdio stream.
 */
class WriteXDR : public StdioXDR
{
public:
	/// Constructor

	WriteXDR (FILE *fp) : StdioXDR (fp, XDR_ENCODE) { }

	// Inherit the destructor

};


///
/**
 * An XDR subclass built on top of a range of memory.
 */
class MemoryXDR : public XDRStream
{
public:

	/// Recreate the stream on a new range of memory
	void create (void *addr, long len);

	// Inherit the destructor

	virtual void destroy () 
	{
		addr = 0;	
		len = 0;
		XDRStream::destroy ();
	}

protected:

	/// Protected constructor
	MemoryXDR (xdr_op xop) : 
		XDRStream (), op(xop), addr(0), len(0)
	{ }

	/// Protected constructor
	MemoryXDR (void *a, long l, xdr_op xop) : 
		XDRStream (), op(xop), addr(a), len(l)
	{
		MemoryXDR::create (op);
	}

	virtual void create (xdr_op op);

	xdr_op op;
	void *addr;
	long len;
};


///
/**
  A class for reading from a memory stream.
 */
class MemReadXDR : public MemoryXDR
{
public:
	/// Constructor
	MemReadXDR (void *a, long l) : 
		MemoryXDR (a, l, XDR_DECODE) { }

	/// Constructor for stream which will be created later
	MemReadXDR () : MemoryXDR (XDR_DECODE) { }

	// Inherit the destructor

};


///
/**
  A class for writing to a memory stream.
 */
class MemWriteXDR : public MemoryXDR
{
public:
	/// Constructor

	MemWriteXDR (void *a, long l) : 
		MemoryXDR (a, l, XDR_ENCODE) { }

	/// Constructor for stream which will be created later:

	MemWriteXDR () : MemoryXDR (XDR_ENCODE) { }

	// Inherit the destructor

};


// Inline method implementations

inline void 
XDRStream::Free (XDRTranslator xp, void *data)
{
	xdr_free ((xdrproc_t)xp, (char *)data);
}



#endif /* _XDR_HH_ */

