/*
 * UF input module
 */

/*		Copyright (C) 1993 by UCAR
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

# include <math.h>
# include <errno.h>
# include <fcntl.h>
# include <defs.h>
# include <message.h>
# include <DataStore.h>
# include "HouseKeeping.h"
# include "radar_ingest.h"


int	Nbeam = 0;

int	Fd;
char	*InSource;
char	TapeSource = FALSE;

/*
 * The beam structure we pass back.
 */
Beamst		Bst;
Housekeeping	Hk;
char		*Databuf;

# define BUFLEN	16000


void 
FileInput (file)
char	*file;
/*
 * Make a note of file input.
 */
{
	InSource = (char *) malloc (strlen (file) + 1);
	TapeSource = FALSE;
	strcpy (InSource, file);
}




void 
TapeInput (file)
char	*file;
/*
 * Set to a tape input source.
 */
{
	InSource = (char *) malloc (strlen (file) + 1);
	TapeSource = TRUE;
	strcpy (InSource, file);
}




void
SetupInput ()
/*
 * Set up the input source. Get things ready to read beam by beam.
 */
{
    if ((Fd = open (InSource, O_RDONLY)) < 0)
    {
		msg_ELog (EF_EMERGENCY, "Error %d opening '%s'", errno, 
			  InSource);
		die ();
    }
}




Beam 
UFGetBeam (f_request, scale, n_request)
int	*f_request, n_request;
ScaleInfo	*scale;
/*
 * Read the next UF beam and return it in an RP7-style beam structure.
 * Return NULL when EOF is reached.
 *
 * Entry:
 *	f_request:	List of offsets to desired fields.
 *	scale:		List of scale factors to be applied to the 
 *			requested fields.
 *	n_request:	Number of fields requested
 * Exit:
 *	The next UF beam is converted into an RP7-style beam structure 
 *	containing only the requested fields.  The data are returned non-
 *	interleaved.
 */
{
	int	numfields = 0, i, j;
	float	londeg, latdeg, fcount;
	int	status, rec_len, rec_len2;
	short	raw_rec[BUFLEN];
	short	*fldhdr[MAXFIELD], *flddata[MAXFIELD];
	short	*fhdr, *dp, *datahdr, uf_badval;
	int	numgates, nread;

	Nbeam++;
/*
 * Leading record length (file sources only)
 */
	if (TapeSource)
		rec_len = sizeof (raw_rec);
	else
	{
		if (! TapeSource && (status = read (Fd, &rec_len, 4)) != 4)
		{
		/*
		 * Return NULL if we're at EOF
		 */
			if (status == 0)
				return (NULL);
		/*
		 * Otherwise complain
		 */
			msg_ELog (EF_EMERGENCY, "rec_len read error %s", 
				  InSource);
			die ();
		}
	}

	if (rec_len > sizeof (raw_rec))
	{
		msg_ELog (EF_EMERGENCY, "Not enough space for %d byte record",
			  rec_len);
		die ();
	}
/*
 * Log message once in a while
 */
	if (! (Nbeam % 500))
		msg_ELog (EF_DEBUG, "%d beams read", Nbeam);
/*
 * The record itself
 */
	nread = read (Fd, raw_rec, rec_len);

	if (TapeSource)
	{
		if (nread == 0)
			return (NULL);
	}
	else
	{
		if (nread != rec_len)
		{
			msg_ELog (EF_EMERGENCY, 
				  "Error %d reading %d bytes (%dth record)", 
				  errno, rec_len, Nbeam);
			die ();
		}
	}
/*
 * Trailing record length (file sources only)
 */
	if (! TapeSource)
	{
		if (read (Fd, &rec_len2, 4) != 4)
		{
			msg_ELog (EF_EMERGENCY, 
				  "trailing rec_len read error %s", 
				  InSource);
			die ();
		}

		if (rec_len != rec_len2)
		{
			msg_ELog (EF_EMERGENCY, 
			  "rec_len mismatch: %d vs. %d (at %dth record)",
			  rec_len, rec_len2, Nbeam);
			die ();
		}
	}
/*
 * Get pointers to the various headers
 */
	datahdr = raw_rec + raw_rec[4] - 1;
	numfields = datahdr[0];

	for (i = 0; i < numfields; i++)
		fldhdr[i] = raw_rec + datahdr[4 + 2 * i] - 1;
/*
 * Make sure the request list is reasonable
 */
	for (i = 0; i < n_request; i++)
	{
		if (f_request[i] >= numfields)
		{
			msg_ELog (EF_EMERGENCY, 
				  "%dth field requested, only %d present",
				  f_request[i] + 1, numfields);
			die ();
		}
	}
/*
 * Associate our housekeeping structure with the beam structure
 */
	Bst.b_hk = &Hk;
/*
 * Stuff things into housekeeping
 */
	Hk.log_rec_num = raw_rec[5];
	Hk.rec_type = 0;		/* record type 0 = data */
	Hk.parm_per_gate = n_request;

	Hk.year = raw_rec[25];		/* last two digits */
	Hk.month = raw_rec[26];
	Hk.day = raw_rec[27];
	Hk.hour = raw_rec[28];
	Hk.minute = raw_rec[29];
	Hk.second = raw_rec[30];

	Hk.azimuth = ((float) raw_rec[32] / 64.0) * DEG_TO_BIN;
	Hk.elevation = ((float) raw_rec[33] / 64.0) * DEG_TO_BIN;
	Hk.fixed = ((float) raw_rec[35] / 64.0) * DEG_TO_BIN;

	Hk.sweep_index = raw_rec[9];
	Hk.vol_count = raw_rec[6];

	latdeg = (raw_rec[18] + raw_rec[19] / 60.0 + 
		  (raw_rec[20] / 64.0) / 3600.0);
	londeg = (raw_rec[21] + raw_rec[22] / 60.0 + 
		  (raw_rec[23] / 64.0) / 3600.0);
/*
 * KLUGE: We use the normal DEG_TO_BIN conversion here, since LAT_CF and 
 * LON_CF only yield unambiguous values from 0-90 and 0-180, and we need 
 * southern and hemispheres...  The recipient of this beam must be careful
 * to use the right value in unpacking these.
 */
	Hk.latitude = latdeg * DEG_TO_BIN;
	Hk.longitude = londeg * DEG_TO_BIN;
	Hk.altitude = raw_rec[24];
/*
 * Scan type (we only really care about PPI, RHI, and SUR)
 */
	switch (raw_rec[34])
	{
	    case 1:
		Hk.scan_mode = SM_PPI;
		break;
	    case 3:
		Hk.scan_mode = SM_RHI;
		break;
	    case 8:
		Hk.scan_mode = SM_SUR;
		break;
	    default:
		Hk.scan_mode = SM_CAL; /* others don't matter */
	}
/*
 * Gate spacing, number of gates, and range to first gate
 * (We use info from the first field here and assume they're all the same)
 */
	fhdr = fldhdr[0];
	
	Hk.gate_spacing = fhdr[4];		/* gate spacing (m) */
	Hk.rhozero1 = fhdr[2];       		/* range to leading edge of */
	Hk.rhozero2 = fhdr[3] - fhdr[4] / 2;	/* 1st gate = rhozero1 +    */
						/* rhozero2/1000 (in km)    */
	numgates = Hk.gates_per_beam = fhdr[5];
/*
 * Allocate data space
 */
	if (Databuf)
		Databuf = (char *) realloc (Databuf, numfields * numgates);
	else
		Databuf = (char *) malloc (numfields * numgates);
/*
 * Chunk descriptor: first gate, number of gates, and pointer to the data.
 */
	Bst.b_npart = 1;

	Bst.b_gdesc[0].gd_first = 1;
	Bst.b_gdesc[0].gd_ngate = numgates;
	Bst.b_gdesc[0].gd_data = (unsigned char *) Databuf;
/*
 * Do the per-field stuff
 */
	uf_badval = datahdr[44];

	for (i = 0; i < n_request; i++)
	{
		float	ufscale, val;
		int	count;
		
		fhdr = fldhdr[f_request[i]];
		dp = raw_rec + fhdr[0] - 1;	/* pointer to data */
	/*
	 * Move the signed two byte data over to our unsigned one byte array
	 * by converting to engineering units and then applying the user's
	 * scale and bias.
	 */
		ufscale = fhdr[1];

		for (j = 0; j < numgates; j++)
		{
			if (dp[j] != uf_badval)
			{
				val = dp[j] / ufscale;
				fcount = (val - scale[i].s_Offset) / 
					scale[i].s_Scale;
			/*
			 * I wish the HP had nint()...
			 */
				count = (int)(fcount + 
					      ((fcount < 0.0) ? -0.5 : 0.5));
			}
			else
				count = 0xff;
		/*
		 * Make sure the resulting count fits into an unsigned char,
		 * otherwise use 0xff
		 */
			Databuf[j + i*numgates] = (count >= 0 && count < 256) ?
					(unsigned char) count : 0xff;
		}
	/*
	 * Stash the scale and bias.
	 */
		Hk.parm_info[i].pi_scale = 100 * scale[i].s_Scale;
		Hk.parm_info[i].pi_bias = 100 * scale[i].s_Offset;
	}

	return (&Bst);
}
