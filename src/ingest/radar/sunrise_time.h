/*HI***************************************************************************
 *
 * 	sunrise_time.h -- header file for the time format
 *
 ******************************************************************************
 *
 * HEADER INFORMATION
 *
 *	Software Suite		- RADEX
 *	Package			- Global
 *
 *	Reference number	- SP1/HDR/02000054
 *	
 *	Revision number		- $Revision: 2.3 $
 *	Release state		- $State: Exp $
 *
 *	Author, designer	- I. McAfee
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
 *	Defines a time structure to hold the date. 
 *
 * MODIFICATION RECORD
 *
 * Revision 2.2  1999/03/01 02:04:39  burghart
 * It's time to take the plunge.  This is the merge of the large file database
 * changes made for the U. of Washington, specifically Branch_Seattle_980430.
 * See the logs of that branch for details.
 *
 * Revision 2.1  1993/08/18  15:34:59  burghart
 * Created new adrad_ingest program to deal with data from Texas A&M's ADRAD
 * (Aggie Doppler Radar).  The Rasterize() function in Rasterize.c was
 * changed slightly to allow for interleaved or non-interleaved data.  Changes
 * in radar_ingest.c are only to adapt to the new interface to Rasterize() and
 * also a new interface to IX_GetWFrame() (in /zeb/src/lib/ImageXfr.c).
 *
 * Revision 8.1  92/04/27  10:31:48  stafford
 * Version used during FAT
 * 
 * Revision 1.4  92/04/09  14:01:18  amca
 * Added module reference number
 * 
 * Revision 1.3  92/04/09  12:02:31  amca
 * After level three documentation header added and after a full merge of
 * the include directory with the latest code from Cowes. 
 * Full recompilation of RADEX made to ensure integrity of changes
 * 
 * Revision 1.2  92/04/03  14:27:42  kenb
 * Version after merging code from Chico
 * 
 * Revision 1.1  91/10/02  00:19:31  kenb
 * Initial revision
 *
 * $Source: /code/cvs/rdss/zebra/source/src/ingest/radar/sunrise_time.h,v $
 *
 ******************************************************************************/


#ifndef _SUNRISE_TIME_
#define _SUNRISE_TIME_
struct time_structure
{
	unsigned char year;	/* 00-99				*/
	unsigned char month;	/* 1-12					*/
	unsigned char day;	/* 1-31					*/
	unsigned char hour;	/* 0-23					*/
	unsigned char minute;	/* 0-59					*/
	unsigned char second;	/* 0-59					*/
	unsigned char dummy[2];
};
#endif

/*MODULE END*******************************************************************/
