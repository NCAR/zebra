/*
 * $Id: Buffer.hh,v 1.3 1997-12-28 05:57:34 granger Exp $
 * 
 * Simple expandable sequential buffer.
 */
#ifndef _Buffer_hh_
#define _Buffer_hh_

#include <string.h>	// memmove()

///
/** Buffer of bytes with routines to grow the buffer as needed.  The
    buffer is guaranteed to keep a given amount of space available at
    the end of the buffer; the amount can be set and retrieved through
    a method.  The default is in Buffer::BUFFER_ZONE.
 */
class Buffer
{
public:
	static const int BUFFER_ZONE;

	/* ----- Constructors ----- */

	/// Create on an existing array of bytes, default to no buffer zone
	Buffer (void *buf, long len, int _zone = 0);

	/// Create a buffer with length at least as long as BUFFER_ZONE
	Buffer (long len = 0, int _zone = BUFFER_ZONE);

	/// Destructor
	virtual ~Buffer ();

	/// Reset the buffer position to the beginning
	void Reset ()
	{ 
		Seek (0);
	}

	/// Seek to the end of the buffer
	void seekEnd ()
	{
		Seek (Length());
	}

	/// 
	/** Move relative to the current position, such as after a 
	  direct read or write. */
	int seekOffset (int offset)
	{
		return (setPosition (position + offset));
	}

	/// Clear the existing buffer and reset, but preserve the zone size.
	void Clear ();

	/// Set the size of the buffer zone
	void setZone (long z)
	{
		if (z >= 0)
			this->zone = z;
	}

	/// Get the size of the buffer zone
	long Zone ()
	{
		return (zone);
	}

	/// 
	/** Express a need for this much more space, either to read or
	    to write.  If it returns zero the space is not available.
	    */
	/* NOT DONE YET: If we need to grow outside space that is not our own,
	   allocate our own space and copy the existing buffer into it.
	   Sort of a copy-on-grow, I guess.  I'm not sure this is needed. */
	int Need (long len);

	/// Return a pointer to the buffer, and its length if desired
	void *getBuffer (long *len = 0)
	{
		if (len) *len = Length(); 
		return (buffer); 
	}

	/// 
	/** Return a pointer into the buffer at its current position,
	    and optionally the amount of buffer space remaining.
	    */
	void *Peek (long *len = 0)
	{
		if (len) *len = Length() - Position();
		return (buffer + position);
	}

	/// 
	/** Set/extend the buffer position and return a pointer to it,
	    and optionally the amount of buffer space remaining.
	    */
	void *Peek (long pos, long *len = 0)
	{
		if (! setPosition (pos))
			return (0);
		if (len) *len = Length() - Position();
		return (buffer + position);
	}

	/// 
	/** Return a pointer to the current buffer position, then advance
	    the buffer position by the given size.  If growth fails, return
	    0 and leave position unchanged.  If size is negative, the
	    return pointer points to the buffer at the new position, allowing
	    the buffer to be accessed in reverse.
	    */
	void *Advance (long size)
	{
		void *block;
		if (size > 0  && ! Need (size))
		{
			block = 0;
		}
		else
		{
			Seek (position + size);
			if (size >= 0)
				block = buffer + (this->position - size);
			else
				block = buffer + this->position;
		}
		return (block);
	}

	///
	/** Move a range of bytes within the buffer, such as to make room
	    somewhere or compact unused space.  Overlapping moves are ok.
	    Moves beyond the size of the buffer will increase buffer size.
	    Returns a pointer to the destination in the buffer, 0 on error.
	    Seeks() to the destination and expands the buffer
	    if necessary.  The position after success is the destination,
	    a pointer to which is returned by the function.
	    */
	void *Move (int to, int from, int n)
	{
		if (to < 0 || from < 0 || n < 0 || from+n > length)
			return 0;
		Seek (to);
		if (! Need (n))
			return 0;
		if (to == from || n == 0)
			return (buffer + to);
		return (memmove (buffer + to, buffer + from, n));
		
	}

	/// Return the length of the buffer
	long Length ()
	{ 
		return length;
	}

	/// 
	/** Return the current offset into the buffer, the point to which
	    data should next be written. */
	long Position () { return position; }

	/// 
	/** Set the position.  If less than zero, the position will be set
	    to zero.  If greater than length, the buffer will be expanded
	    to accomdate.  Returns zero if it doesn't work.  */
	int setPosition (long np)
	{
		if (np < 0)
			np = 0;
		// Advance buffer zone if necessary
		if ((np + zone > length) &&
		    ! Need (np - this->position))
		{
			return (0);
		}
		Seek (np);
		return (1);
	}

	///
	/** Return non-zero when the current position is at or past the
	    end of the buffer.
	    */
	int End ()
	{
		return (position >= length);
	}

	/// 
	/** Write a block of bytes to the current buffer position.
	    A negative length can be used to write bytes preceding the
	    current position.  (See Advance()).
	    */
	int Write (const void *bytes, long len)
	{
		char *buf = (char *) Advance (len);
		if (buf)
			memcpy (buf, (void *)bytes, len);
		return (buf != 0);
	}

	/// 
	/** Read a block of bytes from the current buffer position.
	    A negative length can be used to read bytes preceding the
	    current position.  (See Advance()).
	    */
	int Read (void *bytes, long len)
	{
		char *buf = (char *) Advance (len);
		if (buf)
			memcpy (bytes, buf, len);
		return (buf != 0);
	}

	/** All changes to position and length are done through Seek() and
	    Need().  Need() calls Relocate() if the location or length of
	    the buffer changes.  Subclasses can pick up on changes by
	    overriding these methods and calling the superclass method.
	    This method DOES NOT extend the buffer to positions past
	    the current length; it assumes the caller knows there is enough
	    length for the new position.
	    */
	virtual void Seek (long pos)
	{
		position = pos;
	}

protected:

	virtual void Relocate (void * /*buffer*/, long /*length*/)
	{ }

private:

	long length;
	char *buffer;
	long position;
	long zone;	// Space to guarantee at current position
	int own;	// Non-zero if the buffer space is our own

	virtual void init ();
};

#endif /* _Buffer_hh_ */

