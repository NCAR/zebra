/*
 * Separate out ethernet handling stuff since it's really the only unportable
 * part of the ingestor.
 */
# include <defs.h>
# include <message.h>
# include "HouseKeeping.h"
# include "radar_ingest.h"

# if defined(SYSV) || defined (SVR4)
/*
 * NIT stuff only works under Sunos.  Put in some stubs to keep the linker
 * happy and be done with it.
 */
void OpenEthernet ()
{ }

Beam GetEtherBeam (beam)
Beam beam;
{
	return (0);
}


# else /* is sunos */

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


# include "Ethernet.h"

MAKE_RCSID ($Id: EtherInput.c,v 2.1 1995-04-07 21:05:20 corbet Exp $)

/*
 * Ethernet input info.
 */
static int NitDev = -1;
# define EBUFLEN 1600		/* Max ether packet		*/
# define NEBUF	10		/* Number of ethernet buffers	*/
static unsigned char EBuf[NEBUF][EBUFLEN];
static int CurrBuf = -1;		/* Current ethernet buffer	*/
static int Recycle = FALSE;



static void InCroak FP ((char *));
static ENHeader * GetEtherPacket FP ((void));
static void RecyclePacket ((void));




void
OpenEthernet (interface)
char *interface;
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
	strcpy (ifr.ifr_name, interface);
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





Beam
GetEtherBeam (beam)
Beam beam;
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
	beam->b_hk = (Housekeeping *) (hdr + 1);
	beam->b_npart = hdr->en_fperrad;
	beam->b_gdesc[0].gd_first = hdr->en_g_first;
	beam->b_gdesc[0].gd_ngate = hdr->en_g_last;
	beam->b_gdesc[0].gd_data = beam->b_hk->sz_hsk*sizeof (short) +
				(unsigned char *) beam->b_hk;
/*
 * Now we go through and get the rest of them.
 */
	for (npacket = 2; npacket <= hdr->en_fperrad; npacket++)
	{
		GDesc *gd = beam->b_gdesc + npacket - 1;
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
 	return (&beam);
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




# endif /* Humungo ifdef */
