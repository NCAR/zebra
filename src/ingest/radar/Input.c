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
static char *rcsid = "$Id: Input.c,v 2.9 1995-06-23 19:39:12 corbet Exp $";

# ifdef notdef
# include <sys/types.h>
# include <sys/time.h>
# include <sys/socket.h>
# include <sys/file.h>
# include <sys/ioctl.h>
# include <sys/stropts.h>
# include <net/if.h>
# include <netinet/in.h>
# include <net/nit_if.h>
# include <net/nit_pf.h>
# include <net/packetfilt.h>
# include <netinet/if_ether.h>
# endif

# include <defs.h>
# include <message.h>
# include "HouseKeeping.h"
# include "radar_ingest.h"
# include "BeamBuffer.h"
/* # include "Ethernet.h" */

static int zero = 0;

static enum
{
	FileSource, NetSource, TapeSource, NoSource
} InputType = NoSource;
static char InSource[200];

/*
 * File input stuff.  BUFLEN is the default size of Tbuffer; the size can
 * be changed via the indirect variable buffer_len
 */
# define BUFLEN 32768
static unsigned short *Tbuffer = NULL;
static int buffer_len = BUFLEN;

/*
 * The beam structure we pass back.
 */
static Beamst Bst;






void
InputInitialize ()
/*
 * Add an indirect variable which we will use to set our buffer size.
 */
{
	stbl vtable = usy_g_stbl ("ui$variable_table");

	usy_c_indirect (vtable, "buffer_len", &buffer_len, SYMT_INT, 0);
}



void 
FileInput (file)
char *file;
/*
 * Make a note of file input.
 */
{
	InputType = FileSource;
	strcpy (InSource, file);
}




void
NetInput (interface)
char *interface;
/*
 * Make a note of network input.
 */
{
	InputType = NetSource;
	strcpy (InSource, interface);
}





void
SetupInput ()
/*
 * Set up the input source and our buffer space.
 */
{
	int len;

	if (! Tbuffer)
	{
		len = buffer_len * sizeof(unsigned short);
		msg_ELog (EF_DEBUG, "setting read buffer to %d bytes", len);
		Tbuffer = (unsigned short *) malloc (len);
		if (! Tbuffer)
		{
			msg_ELog (EF_EMERGENCY, 
				  "could not allocate input buffer");
			die ();
		}
	}
/*
 * Branch out depending on the type of input we have.
 */
	switch (InputType)
	{
	/*
	 * If nothing, gripe.
	 */
	   case NoSource:
	   	msg_ELog (EF_EMERGENCY, "No input source to radar_ingest");
		die ();
	/*
	 * Network sources.
	 */
	   case NetSource:
		OpenEthernet (InSource);
		break;
	/*
	 * File sources.
	 */
	   case FileSource:
                /* if (! mtfmnt_ (&zero, InSource)) */
		zero = 1;
		if (! mtmnt_ (&zero))
		{
			msg_ELog (EF_EMERGENCY, "Bogus file source %s",
				InSource);
			die ();
		}
		Bst.b_npart = 1;
		Bst.b_gdesc[0].gd_first = 1;
	}
}



Beam 
GetBeam ()
/*
 * Return a beam to the rasterizer.
 */
{
	int len = buffer_len, status;
	static char 	file[200];
	static int	nlog = 0, curlog = 0, logreclen = 0, neof = 0;

	NBeam++;
/*
 * Ethernet gets.
 */
	if (InputType == NetSource)
		return (GetEtherBeam (&Bst));
/*
 * Files.  We allow packed beams here.
 */
	else
	{
		if (++curlog < nlog)
		{
			Bst.b_hk = (Housekeeping *)
				(Tbuffer + curlog * logreclen);
		}
		else
		{
			mtread_ (&zero, Tbuffer, &len);
			mtwait_ (&zero, &status, &len);
			if (status == 1)
			{
				if (neof++)
					die ();
				return (GetBeam ());
			}
			if (status)
			{
				mtclose_ (&zero);
				ui_string_prompt ("New file: ", 0, file, 
						  "none");

				if (! strcmp (file, "none"))
					die ();
				if (! mtfmnt_ (&zero, file))
					die ();
				return (GetBeam ());
			}

			Bst.b_hk = (Housekeeping *) Tbuffer;

			curlog = 0;
			nlog = Bst.b_hk->num_log_rcd >> 8;
			logreclen = Bst.b_hk->sz_cur_log;
		}
	/*
	 * If we are doing beam buffering, kludge it through now.
	 */
		if (Using_BB)
		{
			unsigned char *buf = BB_GetWriteBuffer ();
			memcpy (buf, Bst.b_hk, logreclen);
			BB_WriteDone (logreclen);
			Bst.b_hk = (Housekeeping *) buf;
		}
	/*
	 * Finish fixing things up.
	 */
		neof = 0;
		Bst.b_gdesc[0].gd_ngate = Bst.b_hk->gates_per_beam;
		Bst.b_gdesc[0].gd_data = ((unsigned char *) Bst.b_hk) +
			Bst.b_hk->sz_hsk;
		return (&Bst);
	}
}






