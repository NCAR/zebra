/*
 * Ethernet info.
 */
/*		Copyright (C) 1987,88,89,90,91 by UCAR
 *	University Corporation for Atmospheric Research
 *		   All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced
 * or used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular purpose.
 * UCAR does not indemnify any infringement of copyright, patent, or trademark
 * through use or modification of this software.  UCAR does not provide 
 * maintenance or updates for its software.
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
