/* $Id: byteorder.h,v 2.1 1995-02-08 22:49:51 corbet Exp $ */
/*
 * Byte swapping, where called for.
 */

/*
 * Thus far, only linux needs byte swapping.  This is, nonetheless, almost
 * certainly the wrong test; we need to get endianness together.
 */

# ifdef linux
static
#  ifdef __GNUC__
inline
#  endif
void swap4 (stuff)
void *stuff;
/*
 * 4-byte swap.
 */
{
	char *cp = (char *) stuff, t;

	t = cp[0]; cp[0] = cp[3]; cp[3] = t;
	t = cp[1]; cp[1] = cp[2]; cp[2] = t;
}
# define LITTLE_ENDIAN
# undef BIG_ENDIAN
# else /* linux */

# define swap4(stuff) (stuff)
# define BIG_ENDIAN
# undef LITTLE_ENDIAN

# endif /* linux */
