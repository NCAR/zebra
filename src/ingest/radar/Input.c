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
static char *rcsid = "$Id: Input.c,v 2.6 1994-09-06 20:30:38 burghart Exp $";

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


# include <defs.h>
# include <message.h>
# include "HouseKeeping.h"
# include "radar_ingest.h"
# include "Ethernet.h"

static int zero = 0;

static enum { FileSource, NetSource, NoSource } InputType = NoSource;
static char InSource[200];

/*
 * File input stuff.
 */
# define BUFLEN 32768
static unsigned short Tbuffer[BUFLEN];

/*
 * The beam structure we pass back.
 */
static Beamst Bst;

/*
 * Ethernet input info.
 */
static int NitDev = -1;
# define EBUFLEN 1600		/* Max ether packet		*/
# define NEBUF	10		/* Number of ethernet buffers	*/
static unsigned char EBuf[NEBUF][EBUFLEN];
static int CurrBuf = -1;		/* Current ethernet buffer	*/
static int Recycle = FALSE;


/*
 * Forwards.
 */
# ifdef __STDC__
	static void OpenEthernet (void);
	static void InCroak (char *);
	static ENHeader * GetEtherPacket (void);
	static Beam GetEtherBeam (void);
	static void RecyclePacket (void);
# else
	static void RecyclePacket ();
	static void OpenEthernet ();
	static void InCroak ();
	static ENHeader * GetEtherPacket ();
	static Beam GetEtherBeam ();
# endif




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
 * Set up the input source.
 */
{
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
		OpenEthernet ();
		break;

	/*
	 * File sources.
	 */
	   case FileSource:
		if (! mtfmnt_ (&zero, InSource))
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
	int len = BUFLEN, status;
	static char 	file[200];
	static int	nlog = 0, curlog = 0, logreclen = 0;

	NBeam++;
/*
 * Ethernet gets.
 */
	if (InputType == NetSource)
		return (GetEtherBeam ());
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
		
			
		Bst.b_gdesc[0].gd_ngate = Bst.b_hk->gates_per_beam;
		Bst.b_gdesc[0].gd_data = (unsigned char *) 
			(Tbuffer + curlog * logreclen + Bst.b_hk->sz_hsk);
		return (&Bst);
	}
}






static void
OpenEthernet ()
/*
 * Open up the ethernet device.
 */
{
	struct ifreq ifr;
	int flag = NI_PROMISC, offset;
	struct packetfilt filter;
	unsigned short *fp;
	struct ether_header *header = 0;
/*
 * Open up NIT.
 */
	if ((NitDev = open ("/dev/nit", O_RDONLY)) < 0)
		InCroak ("/dev/nit open");
/*
 * Now hook into our actual interface and put it in promiscuous mode.
 */
	/* strcpy (ifr.ifr_name, "le1"); */
	strcpy (ifr.ifr_name, InSource);
	if (ioctl (NitDev, NIOCBIND, &ifr) < 0)
		InCroak ("Interface BIND");
	if (ioctl (NitDev, NIOCSFLAGS, &flag) < 0)
		InCroak ("Promiscuous mode");
/*
 * Set up the packet filter.
 */
	if (ioctl (NitDev, I_PUSH, "pf") < 0)
		InCroak ("Packet filter push");
	offset = (int) (&header->ether_type)/sizeof (short);
	ui_printf ("Offset is %d\n", offset);
	fp = filter.Pf_Filter;
	*fp++ = ENF_PUSHWORD + offset;
	*fp++ = ENF_PUSHLIT | ENF_EQ;
	*fp++ = htons ((unsigned short) 0x6006);
	filter.Pf_FilterLen = fp - filter.Pf_Filter;
	if (ioctl (NitDev, NIOCSETF, &filter) < 0)
		InCroak ("Packet filter setup");
/*
 * Flush out any junk that may have accumulated, and we're ready.
 */
	if (ioctl (NitDev, I_FLUSH, FLUSHR) < 0)
		InCroak ("NIT flush");
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





static Beam
GetEtherBeam ()
/*
 * Get a beam from the ethernet.
 */
{
	ENHeader *hdr, *cont;
	int npacket;
/*
 * Wait until we get a begin packet.
 */
	do 
		hdr = GetEtherPacket ();
	while (hdr && hdr->en_type != PT_FIRST);
/*
	ui_printf ("First, seq %ld, f/r %d, num %d, g %d->%d\n", hdr->en_seq,
		hdr->en_fperrad, hdr->en_number, hdr->en_g_first, 
		hdr->en_g_last);
*/
/*
 * Fill in the first beam structure.
 */
	Bst.b_hk = (Housekeeping *) (hdr + 1);
	Bst.b_npart = hdr->en_fperrad;
	Bst.b_gdesc[0].gd_first = hdr->en_g_first;
	Bst.b_gdesc[0].gd_ngate = hdr->en_g_last;
	Bst.b_gdesc[0].gd_data = Bst.b_hk->sz_hsk*sizeof (short) +
				(unsigned char *) Bst.b_hk;
/*
 * Now we go through and get the rest of them.
 */
	for (npacket = 2; npacket <= hdr->en_fperrad; npacket++)
	{
		GDesc *gd = Bst.b_gdesc + npacket - 1;
	/*
	 * Get the next packet.
	 */
		cont = GetEtherPacket ();
/*
		ui_printf (" Cont, seq %ld, num %d, g %d->%d\n", cont->en_seq,
			cont->en_number, cont->en_g_first, cont->en_g_last);
*/
	/*
	 * If it doesn't match what we are expecting, so it goes.
	 */
		if (cont->en_number != npacket)
		{
			RecyclePacket ();
			return (GetEtherBeam ());
		}
	/*
	 * Fill in another chunk.
	 */
		gd->gd_first = cont->en_g_first;
		gd->gd_ngate = cont->en_g_last - cont->en_g_first + 1;
		gd->gd_data = (unsigned char *) (cont + 1);
	}
/*
 * Done!
 */
 	return (&Bst);
}





static ENHeader *
GetEtherPacket ()
/*
 * Return the next packet off the net.
 */
{
	unsigned char *buf;
	struct sockaddr saddr;
	struct strbuf control, data;
	int flags = 0;
	static unsigned long lastseq = 0;
	ENHeader *ehdr;
/*
 * If the last packet is to be recycled, just send it back again.
 */
	if (Recycle)
	{
		Recycle = FALSE;
		return ((ENHeader *) EBuf[CurrBuf]);
	}
/*
 * Select a buffer.
 */
	if (++CurrBuf >= NEBUF)
		CurrBuf = 0;
	buf = EBuf[CurrBuf];
/*
 * Fill in the structures for getmsg.
 */
	control.maxlen = sizeof (saddr);
	control.buf = (char *) &saddr;
	data.maxlen = EBUFLEN;
	data.buf = (char *) buf;
/*
 * Now we try to pull something from the net.
 */
	if (getmsg (NitDev, &control, &data, &flags) < 0)
		InCroak ("getmsg");
/*
 * We got it.  Return the info.
 */
	ehdr = (ENHeader *) buf;
	if (lastseq > 0 && ehdr->en_seq != lastseq + 1)
		NMissed++;
	lastseq = ehdr->en_seq;
	return ((ENHeader *) buf);
}




static void
RecyclePacket ()
/*
 * See to it that the last packet gets read again.
 */
{
	Recycle = TRUE;
}
