/*
 * $Id: XDR.hh,v 1.2 1997-08-01 19:34:37 granger Exp $
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
typedef bool_t (*XDREncoder)(XDR *, void *);
/*
 * Functionally, xdr encoders and decoders are the same.
 */
typedef XDREncoder XDRDecoder;

/*
 * Abstract base class for XDR stream subclasses.
 */
class XDRStream
{
public:

	// ---------------- Class members which do not need a stream

	// Free any allocated memory in a decoded structure.
	static void Free (XDREncoder xp, void *data)
	{
		xdr_free ((xdrproc_t)xp, (char *)data);
	}

	// Amount of space required to encode 'data' into XDR
	static unsigned long Length (XDREncoder xp, const void *data)
	{
		return (xdr_sizeof ((xdrproc_t)xp, (char *)data));
	}

	// ---------------- Instance members

	// Constructors

	XDRStream() : results(0) { }

	// Destructors

	virtual ~XDRStream() { destroy (); }
	virtual void destroy () { xdr_destroy (&this->xdrs); }

	// Return result of operations since last clear.

	bool_t success() const { return (results); }
	void reset() { results = 1; }

	// Position control, not implemented for all streams
	virtual unsigned int getpos ()
	{ 
		return (xdr_getpos (&this->xdrs));
	}
	virtual bool_t setpos (const unsigned int pos)
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
	bool_t translate (T & tp) \
	{ \
		return (check (xdr_##T (&this->xdrs, &tp))); \
	}; \
	XDRStream &operator<< (T &tp) { translate (tp); return (*this); } \
	XDRStream &operator>> (T &tp) { translate (tp); return (*this); }

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

	// User-supplied type translations

	bool_t translate (void *data, XDREncoder xp)
	{
		return (check ((*xp) (&this->xdrs, data)));
	}

protected:

	XDR xdrs;
	bool_t results;

	bool_t check (bool_t status) { results &= status; return (status); }
};


// Applications can add << operators for their own types
// with this macro, given a type T and its xdr procedure XP.

#define XDR_OPERATOR(T, XP) \
XDRStream operator<< (XDRStream *xdr, T &tp) \
{ \
	xdr->translate ((void *)&tp, XP); \
	return (*xdr); \
} \
XDRStream &operator>> (XDRStream *xdr, T &tp) \
{ \
	xdr->translate ((void *)&tp, XP); \
	return (*xdr); \
} \
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

#define XDR_ADDTYPE(T) \
XDR_OPERATOR(T, xdr_##T)

/*
 * The XDR subclass built on stdio streams.
 */
class StdioXDR : public XDRStream
{
public:

	// Constructors

	StdioXDR (FILE *fp, xdr_op op) : XDRStream()
	{
		create (fp, op);
	}

	void create (FILE *fp, xdr_op op);

	// Inherit the destructor

};


/*
 * A class for reading from a stdio stream.
 */
class ReadXDR : public StdioXDR
{
public:
	// Constructors

	ReadXDR (FILE *fp) : StdioXDR (fp, XDR_DECODE) { }

	// Inherit the destructor

};


/*
 * A class for writing to a stdio stream.
 */
class WriteXDR : public StdioXDR
{
public:
	// Constructors

	WriteXDR (FILE *fp) : StdioXDR (fp, XDR_ENCODE) { }

	// Inherit the destructor

};


/*
 * An XDR subclass built on top of a range of memory.
 */
class MemoryXDR : public XDRStream
{
public:

	// Constructors

	MemoryXDR (void *addr, long len, xdr_op op) : XDRStream ()
	{
		create (addr, len, op);
	}

	MemoryXDR () : XDRStream () { }

	void create (void *addr, long len, xdr_op op);

	// Inherit the destructor

};


/*
 * A class for reading from a memory stream.
 */
class MemReadXDR : public MemoryXDR
{
public:
	// Constructors

	MemReadXDR (void *addr, long len) : 
		MemoryXDR (addr, len, XDR_DECODE) { }

	// Stream will be created later:

	MemReadXDR () : MemoryXDR () { }

	void create (void *addr, long len)
	{
		MemoryXDR::create (addr, len, XDR_DECODE);
	}

	// Inherit the destructor

};


/*
 * A class for writing to a memory stream.
 */
class MemWriteXDR : public MemoryXDR
{
public:
	// Constructors

	MemWriteXDR (void *addr, long len) : 
		MemoryXDR (addr, len, XDR_ENCODE) { }

	// Stream will be created later:

	MemWriteXDR () : MemoryXDR () { }

	void create (void *addr, long len)
	{
		MemoryXDR::create (addr, len, XDR_ENCODE);
	}

	// Inherit the destructor

};



#endif /* _XDR_HH_ */

