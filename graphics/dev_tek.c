/* 5/87 jc */
/*
 * Handle tektronix-specific functions, mostly funny number encoding.
 */
# include "param.h"
# include "tty.h"

# define HBIT 0x40	/* Bit to set for high integers	*/
# define LBIT 0x20	/* Low order integer bit	*/
# define NEGBIT 0x10	/* Sign bit	*/

void
tek_xy (x, y, tag, extra)
int x, y;
struct tty_tag *tag;
int extra;
/*
 * Send the given numbers out as XY coords.
 * Entry:
 *	X, Y	are the coordinates to encode.
 *	TAG	is the tag of the terminal to which the data are to be sent.
 *	EXTRA	is true for high-resolution devices that use the extra byte.
 * Exit:
 *	The data have been sent.
 */
{
	int b;
	char obuf[10], *op = obuf;

	if (extra)
	{
		*op++ = ((y & 0xF80) >> 7) | LBIT;
		*op++ = HBIT | LBIT | ((y & 0x3) << 2) | (x & 0x3);
		*op++ = HBIT | LBIT | ((y & 0x7C) >> 2);
		*op++ = ((x & 0xF80) >> 7) | LBIT;
		*op++ = HBIT | ((x & 0x7C) >> 2);
	}
	else
	{
		*op++ = ((y & 0x3E0) >> 5) | LBIT;
		*op++ = HBIT | LBIT | (y & 0x1F);
		*op++ = ((x & 0x3E0) >> 5) | LBIT;
		*op++ = (x & 0x1F) | HBIT;
	}
 	*op = '\0';
	gtty_out (tag, obuf);
}





void
tek_int (value, tag)
int value;
struct tty_tag *tag;
/*
 * Output the given integer value to the tek terminal.
 */
{
	int neg;
	char obuf[10], *op = obuf;
/*
 * Get a positive value.
 */
	if (value < 0)
	{
		value = - value;
		neg = TRUE;
	}
	else
		neg = FALSE;
/*
 * Send out the top five bits, if necessary.
 */
	if (value & 0x7C00)
		*op++ = ((value & 0x7C00) >> 10) | HBIT;
/*
 * Send out the middle six, if necessary.
 */
	*op++ = ((value & 0x3F0) >> 4) | HBIT;
/*
 * Always send out the bottom four
 */
	*op++ = value & 0xF | (neg ? LBIT : LBIT | NEGBIT);
/*
 * Actually ship to the device.
 */
 	*op = '\0';
	gtty_out (tag, obuf);
}

