/* $Id: byteorder.h,v 2.4 1997-05-13 22:16:36 ishikawa Exp $ */
/*
 * Byte swapping, where called for.
 */

/*
 * Thus far, only linux needs byte swapping.  This is, nonetheless, almost
 * certainly the wrong test; we need to get endianness together.
   Added DEC/Alpha OSF/1 LITTLE_ENDIAN machine. Still is not complete
   because Alpha is a 64 bit machine.
 */

# if defined (linux) || defined (__osf__) || defined (LITTLE_ENDIAN)
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

static
#  ifdef __GNUC__
inline
#  endif
void swap2 (stuff)
void *stuff;
/*
 * 2-byte swap.
 */
{
	char *cp = (char *) stuff, t;

	t = cp[0]; cp[0] = cp[1]; cp[1] = t;
}
# ifndef LITTLE_ENDIAN
# define LITTLE_ENDIAN
# endif
# undef BIG_ENDIAN
# else /* linux */

# define swap4(stuff) (stuff)
# define swap2(stuff) (stuff)
# ifndef BIG_ENDIAN
# define BIG_ENDIAN
# endif
# undef LITTLE_ENDIAN

# endif /* linux */
