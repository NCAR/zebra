/* -*- mode: c++; c-basic-offset: 8; -*-
 * $Id: XDR.hh,v 1.6 1998-03-16 17:50:36 burghart Exp $
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

	inline XDRStream() : results(0) { }

	// ---------------- Class members which do not need a stream

	/// Free any allocated memory in a decoded structure.
	static void Free (XDRTranslator xp, void *data);

	/// Amount of space required to encode specific types into XDR
#define XDR_LENGTH_METHOD(_T_,size) \
	static inline unsigned long Length (_T_ x) { return (size); }

	XDR_LENGTH_METHOD(char, 4);
	XDR_LENGTH_METHOD(u_char, 4);
	XDR_LENGTH_METHOD(int, 4);
	XDR_LENGTH_METHOD(u_int, 4);
	XDR_LENGTH_METHOD(short, 4);
	XDR_LENGTH_METHOD(u_short, 4);
	XDR_LENGTH_METHOD(float, 4);
	XDR_LENGTH_METHOD(double, 8);
	XDR_LENGTH_METHOD(long, sizeof(long));
	XDR_LENGTH_METHOD(u_long, sizeof(unsigned long));
	XDR_LENGTH_METHOD(quad_t, 8);
	XDR_LENGTH_METHOD(u_quad_t, 8);

# undef XDR_LENGTH_METHOD


# ifdef notdef	// maybe when xdr_sizeof is implemented under Irix and Linux
	/// Generic length function, given a translator
	static inline unsigned long Length (xdrproc_t xp, void *data)
	{ 
		return xdr_sizeof (xp, data); 
	}
# endif


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

	XDR_METHOD(char);
	XDR_METHOD(u_char);
	XDR_METHOD(int);
	XDR_METHOD(u_int);
	XDR_METHOD(short);
	XDR_METHOD(u_short);
	XDR_METHOD(float);
	XDR_METHOD(double);
	XDR_METHOD(quad_t);
	XDR_METHOD(u_quad_t);

#undef XDR_METHOD

	template <int N>
	bool_t
	my_xdr_u_long(u_long& tp);

	template <int N>
	bool_t
	my_xdr_long(long& tp);

	/**
	 * Translate methods for longs are different, since we encode
	 * as a different XDR type depending upon the architecture.
	 * Someday the better alternative might be to always encode
	 * longs as 8 bytes, so they can be decoded on machines where
	 * native longs are 8 bytes.  However, for now, for the case
	 * of zebra, we won't worry so much about architecture
	 * portability.  Especially since that change would make
	 * different versions of zebra incompatible on the same
	 * machine.
	 **/
	bool_t translate (unsigned long &tp);

	XDRStream &operator<< (unsigned long &tp)
	{ 
		translate (tp); return (*this);
	}
	XDRStream &operator>> (unsigned long &tp)
	{
		translate (tp); return (*this);
	}

	bool_t translate (long &tp);

	XDRStream &operator<< (long &tp)
	{ 
		translate (tp); return (*this);
	}
	XDRStream &operator>> (long &tp)
	{
		translate (tp); return (*this);
	}


	/// User-supplied type translations
	bool_t translate (void *data, XDRTranslator xp)
	{
		return (check ((*xp) (&this->xdrs, data)));
	}

};

template<>
inline bool_t
XDRStream::
my_xdr_u_long<4>(u_long& tp)
{
	return check(xdr_uint32_t (&this->xdrs, (uint32_t*)&tp));
}

template<>
inline bool_t
XDRStream::
my_xdr_u_long<8>(u_long& tp)
{
	return check(xdr_u_hyper (&this->xdrs, (u_quad_t*)&tp));
}

inline bool_t
XDRStream::
translate (unsigned long &tp)
{
	return my_xdr_u_long<sizeof(unsigned long)>(tp);
};



template<>
inline
bool_t
XDRStream::
my_xdr_long<4>(long& tp)
{
	return check(xdr_int32_t (&this->xdrs, (int32_t*)&tp));
}

template<>
inline
bool_t
XDRStream::
my_xdr_long<8>(long& tp)
{
	return check(xdr_hyper (&this->xdrs, (quad_t*)&tp));
}

inline bool_t
XDRStream::
translate (long &tp)
{
	return my_xdr_long<sizeof(long)>(tp);
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

