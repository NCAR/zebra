/*
 * Define structures and the like for dealing with GCS packets.
 */

/*
 * Asynchronous, RS-422 differential sent LSB first at 19,200 baud,
 * 8 data bits, no parity, one stop bit.  No flow control signals are
 * included in the design.
 */


/*
 * Data types:
 *
 * INTEGER	16-bit signed number sent MSByte first.
 * REAL		4-byte floating point in ANSI/IEEE std 754 short format.
 *		Sign bit first, seven MSBits of exponent, then the LSBit
 *		of the mantissa in bit two, followed by the rest of the
 *		mantissa.
 */

#include <defs.h>

#ifdef DLE
#undef DLE	/* We don't want the definition in defs.h */
#endif
#define DLE	((unsigned char)0x10)	/* Data Link Escape character 	*/
#define ETX	((unsigned char)0x03)	/* End of TeXt character	*/

#define MaxPacketBytes	64		/* Max number of bytes in packet*/

/*
 * Packet ordering:
 *
 * <DLE><id> .... data bytes in packet .... <DLE><ETX>
 */

/*
 * The only packet in existence so far.  Sent at 1 second intervals, id 0x21
 */

typedef struct _GCSpacket21 {
	float	gcs_lat;	      	/* radians, +north 		*/
	float	gcs_lon;		/* radians, +east		*/
	float 	gcs_alt;		/* meters, MSL			*/
	float 	gcs_time;		/* time of fix, INS, GPS time 	*/
	float 	gcs_pitch;		/* radians, +up			*/
	float	gcs_roll;		/* radians, +right wing down 	*/
	float	gcs_heading;		/* radians			*/
	float	gcs_airspeed;		/* airspeed in knots		*/
} GCSpacket21;

/*
 * The second count in gcs_time begins with zero each Sunday morning at 
 * midnight. A negative value indicates that time has not yet been acquired.
 */


