/* $Id: byteorder.h,v 2.9 1999-03-01 02:04:43 burghart Exp $ */
/*
 * Byte order testing and swapping functions.  Two's complement is assumed
 * for all signed values.
 */
# ifndef _zebra_byteorder_h_
# define _zebra_byteorder_h_

# ifdef __cplusplus
extern "C"
{
# else
#   include "defs.h"	/* for "inline" handling only */
# endif

/*
 * Boolean endianness tests
 */
static inline int LittleEndian (void);
static inline int BigEndian (void);

/*
 * In-place byte-swapping functions
 */
static inline void swap4 (void *p);
static inline void swap2 (void *p);

/*
 * The FromXXX functions convert 'src' from representation XXX and return
 * the integer value in local representation.
 * 
 * The ToXXX functions convert 'val' from local integer representation to
 * representation XXX.  If successful, the results are written into 'dest' and
 * 1 is returned.  If not successful, 'dest' is unchanged, 0 is returned, and
 * 'errno' (from errno.h) is set to EDOM (domain error).
 */

/*
 * 4-byte integer handling
 */
long FromBigI4 (void *src);
long FromLittleI4 (void *src);
unsigned long FromBigUI4 (void *src);
unsigned long FromLittleUI4 (void *src);

int ToBigI4 (long val, void *dest);
int ToLittleI4 (long val, void *dest);
int ToBigUI4 (unsigned long val, void *dest);
int ToLittleUI4 (unsigned long val, void *dest);

/*
 * 2-byte integer handling
 */
long FromBigI2 (void *src);
long FromLittleI2 (void *src);
unsigned long FromBigUI2 (void *src);
unsigned long FromLittleUI2 (void *src);

int ToBigI2 (long val, void *dest);
int ToLittleI2 (long val, void *dest);
int ToBigUI2 (unsigned long val, void *dest);
int ToLittleUI2 (unsigned long val, void *dest);


/*
 * The function definitions (not just the declarations) have to be local for 
 * inlines to work, so here they are...
 */

static inline void 
swap4 (void *p)
{
    char tmp;
    char *cp = (char *)p;

    tmp = cp[0]; cp[0] = cp[3]; cp[3] = tmp;
    tmp = cp[1]; cp[1] = cp[2]; cp[2] = tmp;
}

    
static inline void 
swap2 (void *p)
{
    char tmp;
    char *cp = (char *)p;

    tmp = cp[0]; cp[0] = cp[1]; cp[1] = tmp;
}



/*
 * This crude (but effective and fast) byte order testing will work only
 * on true big- or little-endian machines.  Please no mid-endian weirdness..
 * It should work on machines with 16-bit or larger ints.
 */
static inline int
LittleEndian (void)
{
    unsigned int i = 1;
    char *bytes = (char *)&i;

    return (bytes[0] == 1);
}


static inline int
BigEndian (void)
{
    unsigned int i = 1;
    char *bytes = (char *)&i;

    return (bytes[0] != 1);
}

# ifdef __cplusplus
} // end extern "C"
# endif

# endif /* !_zebra_byteorder_h_ */
