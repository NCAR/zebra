/*
 * Deal with getting radar beams into the system.
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
static char *rcsid = "$Id: adradInput.c,v 2.2 1994-02-02 19:29:39 burghart Exp $";

# include <sys/types.h>
# include <sys/time.h>
# include <sys/file.h>
# include <sys/stropts.h>
# include <defs.h>
# include <message.h>
# include "HouseKeeping.h"
# include "radar_ingest.h"
/*
 * Adrad includes for xdr, etc.
 */
#include "raw.h"
#include "portable.h"
#include "sunrise_head.h"
#include "cvrt.h"
#include "xdr.h"


static int zero = 0;
int Nbeam=0;

static char InSource[200];

/*
 * The beam structure we pass back.
 */
static Beamst Bst;
Housekeeping b_hk;
struct volume_summary vol;
static void InCroak ();
/*
 * file input stuff
 */
XDR xdrstrm;
FILE *f;
# define BUFLEN 32000
# ifdef notdef
	static unsigned char raydata[BUFLEN];
# else
	unsigned char	*raydata = 0;
# endif

/*
 * other adrad stuff
 */
#define OK 0
#define ERROR -1
#define FAIL -1





void 
FileInput (file)
char	*file;
/*
 * Make a note of file input.
 */
{
	strcpy (InSource, file);
}

/* the file is opened here for reading	*/
extern int read_head();

void
SetupInput ()
/*
 * Set up the input source.
 */
{
struct sunrise_head             head;

	/* here we open the file pointer for access by the xdr stream	*/
 	if((f=fopen(InSource, "r"))==(FILE *)NULL)
       		 {
			msg_ELog (EF_EMERGENCY, "Bogus file source %s",
				  InSource);
			die ();
       		 }

        /* make sure file is at start                           */
	rewind(f);
	/* sunrise header is useless	*/
        read_head(f, &head);
        /* open the xdr  stream for reading     */
        xdrstdio_create(&xdrstrm, f, XDR_DECODE);

        /* first we need to read the adrad volume header        */
        if(accessvolhead(&xdrstrm,vol,RAW_VERSION)!=0)
                printf("Err:version number mismatch\n");
      		  
}

Beam 
GetBeam ()
/*
 * Return a beam to the rasterizer. Routine modified for adrad data.
 */
{
struct ray_header               tmpray;
int numfields=0,status,size,i;
float londeg,latdeg,tmp;

	Nbeam++;
	if( accessrayhead( &xdrstrm, &tmpray ) != OK )
		return (NULL);

	/* find numfields	*/
	if(tmpray.flags.uz)
		numfields++;
	if(tmpray.flags.cz)
		numfields++;
	if(tmpray.flags.vel)
		numfields++;
	if(tmpray.flags.wid)
		numfields++;
	if(tmpray.flags.zdr)
		numfields++;

	Bst.b_hk=&b_hk;

	/* a lot of lines adding things to the bst struct	*/
	b_hk.log_rec_num=Nbeam;		/* logical record # for this beam */
	b_hk.rec_type=0;		/* record type 0 = data */
	b_hk.year=tmpray.year;		/* last two digits */
	b_hk.month=tmpray.month;
	b_hk.day=tmpray.day;
	b_hk.hour=tmpray.hour;
	b_hk.minute=tmpray.minute;
	b_hk.second=tmpray.second;
	b_hk.azimuth=bin2angle(tmpray.vangle)*DEG_TO_BIN;
	b_hk.elevation=bin2angle(tmpray.fanglea)*DEG_TO_BIN;
	b_hk.rhozero1=0;		/* range to leading edge of 1st gate*/
	b_hk.rhozero2=tmpray.rangeg1;	/*= rhozero1 + rhozero2/1000 (in km) */
	b_hk.gate_spacing=tmpray.gatewid;	/* gate spacing (m) = */
	b_hk.gates_per_beam=tmpray.numgates;	/* gates per beam = 1024 */
	b_hk.sweep_index=tmpray.sweep;	/* identifies the sweep (scan) in */
					/* the volume scan (#'s 1 - 16) */
	switch(tmpray.sweep_type)
	{
		case SWEEP_POINT:
			b_hk.scan_mode=5;
			break;
		case SWEEP_PPI:
			b_hk.scan_mode=8;
			break;
		case SWEEP_RHI:
			b_hk.scan_mode=3;
			break;
		case SWEEP_SEC:
			b_hk.scan_mode=1;
			break;
		default:
       		 {
			msg_ELog (EF_EMERGENCY, "bad scan type %d", 
				  tmpray.sweep_type);
			die ();
       		 }
	}

	b_hk.fixed=bin2angle(tmpray.fanglet)*DEG_TO_BIN; /* Fixed angle */
	b_hk.vol_count=tmpray.volume;	/* running count of full or partial
				         * volume scans since last start of 
				 	 * operations */
	b_hk.altitude=vol.radinfo.antenna_height;
	latdeg=(vol.radinfo.latitude.degree+vol.radinfo.latitude.minute/60.0
	    +vol.radinfo.latitude.second/3600.0)*LAT_CF;
	londeg=(vol.radinfo.longitude.degree+vol.radinfo.longitude.minute/60.0
	    +vol.radinfo.longitude.second/3600.0)*LON_CF;
	b_hk.latitude=latdeg;	/* degrees * LAT_CF */
	b_hk.longitude=londeg;	/* degrees * LON_CF */
	b_hk.parm_per_gate=numfields;

        /* read in uncompressed data */
	if (! raydata)
		raydata = (unsigned char *) malloc (BUFLEN * sizeof (char));

        status=xdr_bytes(&xdrstrm,&raydata,&size,BUFLEN);
	if(!(status&&(size==numfields*tmpray.numgates)))
       	 {
		msg_ELog (EF_EMERGENCY, "xdr_bytes failure");
		die ();
       	 }

	for (i = 0; i < numfields; i++)
	{
		Bst.b_gdesc[i].gd_data = raydata + i * tmpray.numgates;
		Bst.b_gdesc[i].gd_ngate = tmpray.numgates;
	}

	return (&Bst);
}

static void
InCroak (s)
char *s;
/*
 * Deal with some sort of failure.
 */
{
	perror (s);
	ui_printf ("\n");
	die ();
}


/*** begin lassen specific routines for adrad ingest	*/

/******************************************************************************
 *
 * read_head.c - read a sunrise header from an open file pointer
 *
 ******************************************************************************/

/******************************************************************************
 *
 * FUNCTION DESCRIPTION
 *
 *	Reads a sunrise header block from the open file pointer referred
 *	to by 'f` into the sunrise header data structure pointed to by 'head'.
 *
 * FUNCTION CALL NOTATION
 *
 *	result = read_head (f, head)
 *	Arguments :	f	: pointer to an open file.
 *			head	: sunrise header data structure where header 
 *				  from the specified file is written to.
 *
 * RETURN VALUE
 *	ok, fail.
 * MODULE COHESION
 *
 *	Called by		- Globally available library function to SUNrise
 *
 *	Calling			- system strcpy(),
 *			 	  xdrstdio_create(),
 *				  xdr_string(),
 *				  xdr_destroy(),
 *				  xdr_u_char(),
 *				  xdr_int(),
 *				  xdr_string().
 *
 ******************************************************************************/

int read_head(f,head)
struct sunrise_head *head;
FILE *f;
{
	int ret=1, i;
	char *tmp;
	XDR xdrstrm;

	strcpy(head->magic, "");

	xdrstdio_create(&xdrstrm, f, XDR_DECODE);

	/*
	** read in the header
	** If the first string returns an error, then this is probably
	** not a valid header, so destroy the XDR stream (in my version)
	** and return an error NOW, rather than attempting to read the
	** rest of the header.
	*/
	tmp=head->magic;
	if((ret&=xdr_string(&xdrstrm, &tmp, 8))==0)
	{
		return(FAIL);
	}

	ret&=xdr_u_char(&xdrstrm, &head->mdate.year);
	ret&=xdr_u_char(&xdrstrm, &head->mdate.month);
	ret&=xdr_u_char(&xdrstrm, &head->mdate.day);
	ret&=xdr_u_char(&xdrstrm, &head->mdate.hour);
	ret&=xdr_u_char(&xdrstrm, &head->mdate.minute);
	ret&=xdr_u_char(&xdrstrm, &head->mdate.second);
	ret&=xdr_u_char(&xdrstrm, &head->cdate.year);
	ret&=xdr_u_char(&xdrstrm, &head->cdate.month);
	ret&=xdr_u_char(&xdrstrm, &head->cdate.day);
	ret&=xdr_u_char(&xdrstrm, &head->cdate.hour);
	ret&=xdr_u_char(&xdrstrm, &head->cdate.minute);
	ret&=xdr_u_char(&xdrstrm, &head->cdate.second);
	ret&=xdr_int(&xdrstrm, &head->type);
	tmp=head->mwho;
	ret&=xdr_string(&xdrstrm, &tmp, 16);
	tmp=head->cwho;
	ret&=xdr_string(&xdrstrm, &tmp, 16);
	ret&=xdr_int(&xdrstrm, &head->protection);
	ret&=xdr_int(&xdrstrm, &head->checksum);
	tmp=head->description;
	ret&=xdr_string(&xdrstrm, &tmp, 40);
	ret&=xdr_int(&xdrstrm, &head->id);
	for(i=0;i<12;i++)
		ret&=xdr_int(&xdrstrm, &head->spares[i]);

	return(ret==1?OK:FAIL);
}

/******************************************************************************
 *
 * FUNCTION DESCRIPTION -accessrayhead
 *
 *	This function reads the values from the ray_header structure into an
 *	XDR stream.
 *
 * FUNCTION CALL NOTATION
 *
 *	result = accessrayhead( xdrptr, rayptr )
 *	Arguments :	xdrptr	: Pointer to XDR stream.
 *			rayptr	: Pointer to ray_header structure.
 *
 * RETURN VALUE
 *	error, ok.
 * MODULE COHESION
 *
 *	Called by		- Globally available library function to SUNrise
 *
 *	Calling			- xdr_u_short(),
 *				  xdr_u_char().
 *
 ******************************************************************************/
int accessrayhead (xdrptr, rayptr)
XDR	*xdrptr;
struct ray_header	*rayptr;
{
	register int	i, ret = 1;

	/*  Read in the ray header  */
	ret&=xdr_u_short( xdrptr, &rayptr->vangle);
	ret&=xdr_u_short( xdrptr, &rayptr->fanglet);
	ret&=xdr_u_short( xdrptr, &rayptr->fanglea);
	ret&=xdr_u_short( xdrptr, &rayptr->a_start);
	ret&=xdr_u_short( xdrptr, &rayptr->a_stop);
	ret&=xdr_u_char( xdrptr, &rayptr->max_height);
	ret&=xdr_u_char( xdrptr, &rayptr->volume);
	ret&=xdr_u_char( xdrptr, &rayptr->sweep);
	ret&=xdr_u_char( xdrptr, &rayptr->sweep_type);
	ret&=xdr_u_short( xdrptr, &rayptr->gatewid);
	ret&=xdr_u_short( xdrptr, &rayptr->rangeg1);
	ret&=xdr_u_short( xdrptr, &rayptr->numgates);
	ret&=xdr_u_short( xdrptr, &rayptr->prf);
	ret&=xdr_u_short( xdrptr, &rayptr->prflow);
	ret&=xdr_u_short( xdrptr, &rayptr->n_pulses);
	ret&=xdr_u_char( xdrptr, &rayptr->p_width);
	ret&=xdr_u_char( xdrptr, &rayptr->cfilter);
	ret&=xdr_u_short( xdrptr, &rayptr->status);
	ret&=xdr_u_short( xdrptr, &rayptr->flags);	/* a bit field */

	for(i=0;i<NUMOFFSETS;i++)
		ret&=xdr_u_short( xdrptr, &rayptr->offset[i]);

	for(i=0;i<NUMSPARES;i++)
		ret&=xdr_u_short( xdrptr, &rayptr->spares[i]);

	ret&=xdr_u_char( xdrptr, &rayptr->year);
	ret&=xdr_u_char( xdrptr, &rayptr->month);
	ret&=xdr_u_char( xdrptr, &rayptr->day);
	ret&=xdr_u_char( xdrptr, &rayptr->hour);
	ret&=xdr_u_char( xdrptr, &rayptr->minute);
	ret&=xdr_u_char( xdrptr, &rayptr->second);

	/*  Check if an error occurred  */
	if( ret != 1 )
		return( ERROR );
	return( OK );
}

/******************************************************************************
 *
 * FUNCTION DESCRIPTION - accessvolhead
 *
 *	This function reads in the volume header into the XDR stream.
 *
 * FUNCTION CALL NOTATION
 *
 *	result = accessvolhead( xdrptr, volptr, version )
 *	Arguments :	xdrptr		: Pointer to XDR stream.
 *			volptr		: Pointer to volume_summary structure.
 *			version		: Version number of volume.
 *
 * RETURN VALUE
 *	error, ok.
 * MODULE COHESION
 *
 *	Called by		- Globally available library function to SUNrise
 *
 *	Calling			- xdr_u_short(),	system fprintf(),
 *				  system return(),	system xdr_short(),
 *				  xdr_u_int(),		xdr_u_char(),
 *				  xdr_string().
 *
 ******************************************************************************/
int accessvolhead( xdrptr, volptr, version )
	XDR			*xdrptr;
	struct volume_summary	*volptr;
	int			version;
{
	register int	i,j;
	register int	ret = TRUE;
	char		*tmp;
	unsigned short	us_tmp;
	int		size;

	/*  Read the version number   */
	ret&=xdr_u_short(xdrptr, &volptr->version);

	/*
	 *  If the version number is to be verified, verify it and if it is
	 *	not correct we are probably trying to read in an incorrect
	 *	version of the volume so return an error.
	 */
	if( version > 0 ) {

		if( volptr->version != version ) {
			(void)fprintf(stderr,
				"Incompatible version of volume().\n" );
			(void)fprintf(stderr,
			"File version: %u.%u \t Program version: %u.%u\n",
				volptr->version/10, volptr->version%10,
				version/10, version%10 );
			return( ERROR );
		}
	}

	/*  Read in the volume header  */
	ret&=xdr_short(xdrptr, &volptr->filled);
	ret&=xdr_u_int(xdrptr, &volptr->volume);
	ret&=xdr_u_short(xdrptr, &volptr->sweep);
	ret&=xdr_u_short(xdrptr, &volptr->sweep_type);
	ret&=xdr_u_short(xdrptr, &volptr->max_height);
	ret&=xdr_u_short(xdrptr, &volptr->status);
	ret&=xdr_u_short(xdrptr, &volptr->min_fangle);
	ret&=xdr_u_short(xdrptr, &volptr->max_fangle);
	ret&=xdr_u_short(xdrptr, &volptr->min_var);
	ret&=xdr_u_short(xdrptr, &volptr->max_var);
	ret&=xdr_u_short(xdrptr, &volptr->a_start);
	ret&=xdr_u_short(xdrptr, &volptr->a_stop);
	ret&=xdr_u_short(xdrptr, &volptr->numsweeps);
	for(i=0;i<30;i++)
		ret&=xdr_u_short(xdrptr, &volptr->fangles[i]);
	ret&=xdr_u_short(xdrptr, &volptr->gatewid);
	ret&=xdr_u_short(xdrptr, &volptr->rangeg1);
	for(i=0;i<30;i++)
		ret&=xdr_u_short(xdrptr, &volptr->numgates[i]);
	ret&=xdr_u_short(xdrptr, &volptr->maxgates);
	ret&=xdr_u_short(xdrptr, &us_tmp);
	ret&=xdr_u_short(xdrptr, &us_tmp);
	ret&=xdr_u_short(xdrptr, &us_tmp);
	ret&=xdr_u_short(xdrptr, &us_tmp);
	ret&=xdr_u_short(xdrptr, &volptr->prf);
	ret&=xdr_u_short(xdrptr, &volptr->prflow);
	ret&=xdr_u_int(xdrptr, &volptr->freq);
	ret&=xdr_u_short(xdrptr, &volptr->n_pulses);

	for(i=0;i<30;i++)
		for(j=0;j<NUMOFFSETS;j++)
			ret&=xdr_u_short(xdrptr, &volptr->offset[i][j]);
	for(i=0;i<30;i++)
		for(j=0;j<NUMSPARES;j++)
			ret&=xdr_u_short(xdrptr, &volptr->spares[i][j]);

	ret&=xdr_u_char(xdrptr, &volptr->year);
	ret&=xdr_u_char(xdrptr, &volptr->month);
	ret&=xdr_u_char(xdrptr, &volptr->day);
	ret&=xdr_u_char(xdrptr, &volptr->shour);
	ret&=xdr_u_char(xdrptr, &volptr->sminute);
	ret&=xdr_u_char(xdrptr, &volptr->ssecond);
	ret&=xdr_u_char(xdrptr, &volptr->ehour);
	ret&=xdr_u_char(xdrptr, &volptr->eminute);
	ret&=xdr_u_char(xdrptr, &volptr->esecond);
	ret&=xdr_u_short(xdrptr, &volptr->volflags);
	
	/* now get the radar info structure				*/
	tmp=volptr->radinfo.radar_name;
	ret&=xdr_string(xdrptr, &tmp, 8);
	tmp=volptr->radinfo.site_name;
	ret&=xdr_string(xdrptr, &tmp, 8);
	ret&=xdr_short(xdrptr, &volptr->radinfo.antenna_height);
	ret&=xdr_short(xdrptr, &volptr->radinfo.latitude.degree);
	ret&=xdr_short(xdrptr, &volptr->radinfo.latitude.minute);
	ret&=xdr_short(xdrptr, &volptr->radinfo.latitude.second);
	ret&=xdr_short(xdrptr, &volptr->radinfo.longitude.degree);
	ret&=xdr_short(xdrptr, &volptr->radinfo.longitude.minute);
	ret&=xdr_short(xdrptr, &volptr->radinfo.longitude.second);

	/*
	 *  The return value is still TRUE if there was no error in reading
	 *	in the data
	 */
	if( ret != TRUE )
		return( ERROR );
	return( OK );
}

