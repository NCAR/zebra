/*
 * Ethernet info.
 */


/*
 * What we read off the net.
 */
typedef struct s_ENHeader
{
	struct ether_header en_hdr;	/* The ethernet header		*/
	unsigned short en_type;		/* FIRST or CONTINUE		*/
	unsigned long en_seq;		/* Sequence number		*/
	unsigned short en_fperrad;	/* Frames per radial		*/
	unsigned short en_number;	/* Number of this frame in rad	*/
	unsigned short en_res[4];	/* Reserved junk		*/
	unsigned short en_g_first;	/* First gate			*/
	unsigned short en_g_last;	/* Last gate			*/
} ENHeader;


/*
 * Packet types.
 */
# define PT_FIRST	1
# define PT_CONTINUE	2
