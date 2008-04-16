/*HI************************************************************************
 *
 * sunrise_head.h -- header for the head library
 *
 ***************************************************************************
 *
 * HEADER INFORMATION
 *
 *	Software Suite		- SUNRISE
 *	Package			- Misc Lib
 *
 *	Reference number	- SP1/HDR/03030110
 *	
 *	Revision number		- $Revision: 2.3 $
 *	Release state		- $State: Exp $
 *
 *	Author, designer	- Ian Mcafee 
 *
 *	Modification date	- $Date: 2008-04-16 18:26:54 $
 *	Modified by		- $Author: granger $
 * 
 * COPYRIGHT NOTICE
 *
 * 	Copyright (c) 1991 by Lassen Research
 *	All Rights Reserved
 *
 *	This program is copyright by Lassen Research, Chico, California,
 *	95928, (916) 343-6421.  It is licensed for use on a specific cpu
 *	and is not to be transferred or otherwise divulged.  Copies or
 *	modifications of this program must carry this copyright notice.
 * 
 * DESCRIPTION
 *
 * $Source: /code/cvs/rdss/zebra/source/src/ingest/radar/sunrise_head.h,v $
 *
 * MODIFICATION RECORD
 *
 * Revision 2.2  1999/03/01 02:04:39  burghart
 * It's time to take the plunge.  This is the merge of the large file database
 * changes made for the U. of Washington, specifically Branch_Seattle_980430.
 * See the logs of that branch for details.
 *
 * Revision 2.1  1993/08/18  15:34:56  burghart
 * Created new adrad_ingest program to deal with data from Texas A&M's ADRAD
 * (Aggie Doppler Radar).  The Rasterize() function in Rasterize.c was
 * changed slightly to allow for interleaved or non-interleaved data.  Changes
 * in radar_ingest.c are only to adapt to the new interface to Rasterize() and
 * also a new interface to IX_GetWFrame() (in /zeb/src/lib/ImageXfr.c).
 *
 * Revision 1.6  1992/02/21  19:02:05  hussam
 * added level 3 header
 * Hussam
 *
 * Revision 1.5  1990/09/28  15:33:32  ian
 * made the HEAD_XDR_SIZE a long.
 *
 * Revision 1.4  90/09/28  14:42:44  ian
 * added the XDR_HEAD_SIZE define so that the xdr size of the
 * structure is known.
 * 
 * Revision 1.3  90/07/27  13:07:42  ian
 * got rid of one of the spares because neither read_head or write_head
 * used it.
 * 
 * Revision 1.2  90/07/27  11:52:17  ian
 * replace one of the spares with an id number.
 * 
 * Revision 1.1  90/06/26  15:01:36  ian
 * Initial revision
 * 
 **************************************************************************/

/**static char rcs_id[] = "$Id: sunrise_head.h,v 2.3 2008-04-16 18:26:54 granger Exp $";*/

#ifndef _SUNRISE_HEAD_
#define _SUNRISE_HEAD_

#include "sunrise_time.h"

/* different sunrise file types						*/
#define SUNRISE_VOL	0
#define SUNRISE_PROD	1
#define SUNRISE_OTHER	2

/* the size, in bytes, of the struct when it is converted to XDR	*/
#define HEAD_XDR_SIZE ((long)192)

struct sunrise_head /* 160 bytes in all					*/
{
	char magic[8];			/* magic number			*/
	struct time_structure mdate;	/* last modification		*/
	struct time_structure cdate;	/* creation date		*/
	int type;			/* see #defines above		*/
	char mwho[16];			/* last person to modify	*/
	char cwho[16];			/* person who created file	*/
	int protection;			/* is file protected?		*/
	int checksum;			/* data bcc			*/
	char description[40];		/* file description		*/
	int id;				/* for external use only	*/
	int spares[12];
};

#endif _SUNRISE_HEAD_
