/*
 * Byte order swapping functions for two-byte and four-byte signed and
 * unsigned ints.  Two's complement is assumed for all signed values.
 */
# include <errno.h>
# include <string.h>
# include "byteorder.h"


long 
FromBigI4 (void *vsrc)
/*
 * Convert a 4-byte big-endian signed int, pointed to by src, into a long
 * in local representation.  This is written to be alignment-safe, so it
 * doesn't have same-size/same-endian optimizations.  It should work on
 * machines where sizeof(long) is 4 or greater. 
 */
{
    long l = 0;
    char *src = (char *)vsrc;
    char *c = (char *)(&l);
/*
 * Get the four bytes in the right place
 */
    if (BigEndian())
	memcpy (c + sizeof (long) - 4, src, 4);
    else
    {
	c[0] = src[3];
	c[1] = src[2];
	c[2] = src[1];
	c[3] = src[0];
    }
/*
 * Sign extend if sizeof(long) > 4 and src's sign bit is set
 */
    if (sizeof(long) > 4 && (src[0] & 0x80))
    {
	if (BigEndian())
	    memset (c, 0xff, sizeof (long) - 4);
	else
	    memset (c + 4, 0xff, sizeof (long) - 4);
    }
/*
 * We should have all the bits right now, so return our long
 */
    return l;
}


	
long 
FromLittleI4 (void *vsrc)
/*
 * Convert a 4-byte little-endian signed int, pointed to by src, into a long
 * in local representation.  This is written to be alignment-safe, so it
 * doesn't have same-size/same-endian optimizations.  It should work on
 * machines where sizeof(long) is 4 or greater. 
 */
{
    long l = 0;
    char *src = (char *)vsrc;
    char *c = (char *)(&l);
/*
 * Get the four bytes in the right place
 */
    if (LittleEndian())
	memcpy (c, src, 4);
    else
    {
	c[sizeof (long) - 4] = src[3];
	c[sizeof (long) - 3] = src[2];
	c[sizeof (long) - 2] = src[1];
	c[sizeof (long) - 1] = src[0];
    }
/*
 * Sign extend if sizeof(long) > 4 and src's sign bit is set
 */
    if (sizeof(long) > 4 && (src[3] & 0x80))
    {
	if (BigEndian())
	    memset (c, 0xff, sizeof (long) - 4);
	else
	    memset (c + 4, 0xff, sizeof (long) - 4);
    }
/*
 * We should have all the bits right now, so return our long
 */
    return l;
}



    
unsigned long 
FromBigUI4 (void *vsrc)
/*
 * Convert a 4-byte big-endian unsigned int, pointed to by src, into an
 * unsigned long in local representation.  This is written to be
 * alignment-safe, so it doesn't have same-size/same-endian optimizations.
 * It should work on machines where sizeof(long) is 4 or greater. 
 */
{
    unsigned long l = 0;
    char *src = (char *)vsrc;
    char *c = (char *)(&l);
/*
 * Get the four bytes in the right place
 */
    if (BigEndian())
	memcpy (c + sizeof (long) - 4, src, 4);
    else
    {
	c[0] = src[3];
	c[1] = src[2];
	c[2] = src[1];
	c[3] = src[0];
    }
/*
 * We should have all the bits right now, so return our unsigned long
 */
    return l;
}



	
unsigned long 
FromLittleUI4 (void *vsrc)
/*
 * Convert a 4-byte little-endian unsigned int, pointed to by src, into an
 * unsigned long in local representation.  This is written to be
 * alignment-safe, so it doesn't have same-size/same-endian optimizations.
 * It should work on machines where sizeof(long) is 4 or greater. 
 */
{
    unsigned long l = 0;
    char *src = (char *)vsrc;
    char *c = (char *)(&l);
/*
 * Get the four bytes in the right place
 */
    if (LittleEndian())
	memcpy (c, src, 4);
    else
    {
	c[sizeof (long) - 4] = src[3];
	c[sizeof (long) - 3] = src[2];
	c[sizeof (long) - 2] = src[1];
	c[sizeof (long) - 1] = src[0];
    }
/*
 * We should have all the bits right now, so return our long
 */
    return l;
}



    
long 
FromBigI2 (void *vsrc)
/*
 * Convert a 2-byte big-endian signed int, pointed to by src, into a long
 * in local representation.  This is written to be alignment-safe, so it
 * doesn't have same-size/same-endian optimizations.  It should work on
 * machines where sizeof(long) is 2 or greater. 
 */
{
    long l = 0;
    char *src = (char *)vsrc;
    char *c = (char *)(&l);
/*
 * Get the four bytes in the right place
 */
    if (BigEndian())
	memcpy (c + sizeof (long) - 2, src, 2);
    else
    {
	c[0] = src[1];
	c[1] = src[0];
    }
/*
 * Sign extend if sizeof(long) > 2 and src's sign bit is set
 */
    if (sizeof(long) > 2 && (src[0] & 0x80))
    {
	if (BigEndian())
	    memset (c, 0xff, sizeof (long) - 2);
	else
	    memset (c + 2, 0xff, sizeof (long) - 2);
    }
/*
 * We should have all the bits right now, so return our long
 */
    return l;
}


	
long 
FromLittleI2 (void *vsrc)
/*
 * Convert a 2-byte little-endian signed int, pointed to by src, into a long
 * in local representation.  This is written to be alignment-safe, so it
 * doesn't have same-size/same-endian optimizations.  It should work on
 * machines where sizeof(long) is 2 or greater. 
 */
{
    long l = 0;
    char *src = (char *)vsrc;
    char *c = (char *)(&l);
/*
 * Get the four bytes in the right place
 */
    if (LittleEndian())
	memcpy (c, src, 2);
    else
    {
	c[sizeof (long) - 2] = src[1];
	c[sizeof (long) - 1] = src[0];
    }
/*
 * Sign extend if sizeof(long) > 2 and src's sign bit is set
 */
    if (sizeof(long) > 2 && (src[1] & 0x80))
    {
	if (BigEndian ())
	    memset (c, 0xff, sizeof (long) - 2);
	else
	    memset (c + 2, 0xff, sizeof (long) - 2);
    }
/*
 * We should have all the bits right now, so return our long
 */
    return l;
}



    
unsigned long
FromBigUI2 (void *vsrc)
/*
 * Convert a 2-byte big-endian unsigned int, pointed to by src, into an
 * unsigned long in local representation.  This is written to be
 * alignment-safe, so it doesn't have same-size/same-endian optimizations.
 * It should work on machines where sizeof(long) is 2 or greater. 
 */
{
    unsigned long l = 0;
    char *src = (char *)vsrc;
    char *c = (char *)(&l);
/*
 * Get the four bytes in the right place
 */
    if (BigEndian())
	memcpy (c + sizeof (long) - 2, src, 2);
    else
    {
	c[0] = src[1];
	c[1] = src[0];
    }
/*
 * We should have all the bits right now, so return our unsigned long
 */
    return l;
}



	
unsigned long 
FromLittleUI2 (void *vsrc)
/*
 * Convert a 2-byte little-endian unsigned int, pointed to by src, into an
 * unsigned long in local representation.  This is written to be
 * alignment-safe, so it doesn't have same-size/same-endian optimizations.
 * It should work on machines where sizeof(long) is 2 or greater. 
 */
{
    unsigned long l = 0;
    char *src = (char *)vsrc;
    char *c = (char *)(&l);
/*
 * Get the four bytes in the right place
 */
    if (LittleEndian())
	memcpy (c, src, 2);
    else
    {
	c[sizeof (long) - 2] = src[1];
	c[sizeof (long) - 1] = src[0];
    }
/*
 * We should have all the bits right now, so return our long
 */
    return l;
}




int 
ToBigI4 (long val, void *vdest)
/*
 * Convert the given val into a 4-byte big-endian signed int, and stuff it
 * into dest.  On error, set errno to EDOM (domain error) and return zero.
 */
{
    char *dest = (char *)vdest;
    char *c = (char *)(&val);
/*
 * Make sure we're in the limits of 32-bit representation.
 * NOTE: gcc accepts (-2147483647 - 1) as a long constant, but lies and says
 * -21478483648 is out of range.  Whatever.
 */
    if ((val < (-2147483647 - 1)) || (val > 2147483647))
    {
	errno = EDOM;
	return 0;
    }
/*
 * Stuff the least significant four bytes of val into dest in the correct order
 */
    if (BigEndian())
	memcpy (dest, c + sizeof (long) - 4, 4);
    else
    {
	dest[0] = c[3];
	dest[1] = c[2];
	dest[2] = c[1];
	dest[3] = c[0];
    }

    return (1);
}

    

int 
ToBigUI4 (unsigned long val, void *vdest)
/*
 * Convert the given val into a 4-byte big-endian unsigned int, and stuff it
 * into dest.  On error, set errno to EDOM (domain error) and return zero.
 */
{
    char *dest = (char *)vdest;
    char *c = (char *)(&val);

    if (val > 4294967295u)
    {
	errno = EDOM;
	return 0;
    }
/*
 * Stuff the least significant four bytes of val into dest in the correct order
 */
    if (BigEndian())
	memcpy (dest, c + sizeof (long) - 4, 4);
    else
    {
	dest[0] = c[3];
	dest[1] = c[2];
	dest[2] = c[1];
	dest[3] = c[0];
    }

    return (1);
}




int 
ToLittleI4 (long val, void *vdest)
/*
 * Convert the given val into a 4-byte little-endian signed int, and stuff it
 * into dest.  On error, set errno to EDOM (domain error) and return zero.
 */
{
    char *dest = (char *)vdest;
    char *c = (char *)(&val);
/*
 * Make sure we're in the limits of 32-bit representation.
 * NOTE: gcc accepts (-2147483647 - 1) as a long constant, but lies and says
 * -21478483648 is out of range.  Whatever.
 */
    if ((val < (-2147483647 - 1)) || (val > 2147483647))
    {
	errno = EDOM;
	return 0;
    }
/*
 * Stuff the least significant four bytes of val into dest in the correct order
 */
    if (LittleEndian())
	memcpy (dest, c, 4);
    else
    {
	dest[0] = c[sizeof(long) - 1];
	dest[1] = c[sizeof(long) - 2];
	dest[2] = c[sizeof(long) - 3];
	dest[3] = c[sizeof(long) - 4];
    }

    return (1);
}

    

int 
ToLittleUI4 (unsigned long val, void *vdest)
/*
 * Convert the given val into a 4-byte little-endian unsigned int, and stuff it
 * into dest.  On error, set errno to EDOM (domain error) and return zero.
 */
{
    char *dest = (char *)vdest;
    char *c = (char *)(&val);
/*
 * Make sure we're in the limits of 32-bit representation.
 */
    if (val > 4294967295u)
    {
	errno = EDOM;
	return 0;
    }
/*
 * Stuff the least significant four bytes of val into dest in the correct order
 */
    if (LittleEndian())
	memcpy (dest, c, 4);
    else
    {
	dest[0] = c[sizeof(long) - 1];
	dest[1] = c[sizeof(long) - 2];
	dest[2] = c[sizeof(long) - 3];
	dest[3] = c[sizeof(long) - 4];
    }

    return (1);
}




int 
ToBigI2 (long val, void *vdest)
/*
 * Convert the given val into a 2-byte big-endian signed int, and stuff it
 * into dest.  On error, set errno to EDOM (domain error) and return zero.
 */
{
    char *dest = (char *)vdest;
    char *c = (char *)(&val);
/*
 * Make sure we're in the limits of 16-bit representation.
 */
    if ((val < -32768) || (val > 32767))
    {
	errno = EDOM;
	return 0;
    }
/*
 * Stuff the least significant two bytes of val into dest in the correct order
 */
    if (BigEndian())
	memcpy (dest, c + sizeof (long) - 2, 2);
    else
    {
	dest[0] = c[1];
	dest[1] = c[0];
    }

    return (1);
}

    

int 
ToBigUI2 (unsigned long val, void *vdest)
/*
 * Convert the given val into a 2-byte big-endian unsigned int, and stuff it
 * into dest.  On error, set errno to EDOM (domain error) and return zero.
 */
{
    char *dest = (char *)vdest;
    char *c = (char *)(&val);
/*
 * Make sure we're in the limits of 16-bit representation.
 */
    if (val > 65535)
    {
	errno = EDOM;
	return 0;
    }
/*
 * Stuff the least significant two bytes of val into dest in the correct order
 */
    if (BigEndian())
	memcpy (dest, c + sizeof (long) - 2, 2);
    else
    {
	dest[0] = c[1];
	dest[1] = c[0];
    }

    return (1);
}




int 
ToLittleI2 (long val, void *vdest)
/*
 * Convert the given val into a 2-byte little-endian signed int, and stuff it
 * into dest.  On error, set errno to EDOM (domain error) and return zero.
 */
{
    char *dest = (char *)vdest;
    char *c = (char *)(&val);
/*
 * Make sure we're in the limits of 16-bit representation.
 */
    if ((val < -32768) || (val > 32767))
    {
	errno = EDOM;
	return 0;
    }
/*
 * Stuff the least significant two bytes of val into dest in the correct order
 */
    if (LittleEndian())
	memcpy (dest, c, 2);
    else
    {
	dest[0] = c[sizeof(long) - 1];
	dest[1] = c[sizeof(long) - 2];
    }

    return (1);
}

    

int
ToLittleUI2 (unsigned long val, void *vdest)
/*
 * Convert the given val into a 2-byte little-endian unsigned int, and stuff it
 * into dest.  On error, set errno to EDOM (domain error) and return zero.
 */
{
    char *dest = (char *)vdest;
    char *c = (char *)(&val);
/*
 * Make sure we're in the limits of 16-bit representation.
 */
    if (val > 65535)
    {
	errno = EDOM;
	return 0;
    }
/*
 * Stuff the least significant two bytes of val into dest in the correct order
 */
    if (LittleEndian())
	memcpy (dest, c, 2);
    else
    {
	dest[0] = c[sizeof(long) - 1];
	dest[1] = c[sizeof(long) - 2];
    }

    return (1);
}
