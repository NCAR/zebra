/*
 * Separate out ethernet handling stuff since it's really the only unportable
 * part of the ingestor.
 *
 * This variant uses the "pcap" packet capture library from LBL -- snarf
 * from ftp.ee.lbl.gov.
 */
# include <defs.h>
# include <message.h>
# include "HouseKeeping.h"
# include "radar_ingest.h"

# include <sys/types.h>
# include <sys/socket.h>
# include <net/if.h>
# include <netinet/in.h>
# include <netinet/if_ether.h>

# include <pcap.h>
# include "Ethernet.h"

MAKE_RCSID ("$Id: PCapInput.c,v 2.1 1995-04-07 21:05:26 corbet Exp $")

/*
 * Ethernet input info.
 */
# define EBUFLEN 10240		/* Max beam len			*/
# define NEBUF	10		/* Number of beams		*/
static unsigned char EBuf[NEBUF][EBUFLEN];
static int CurrBuf = -1;		/* Current  buffer	*/
static int Recycle = FALSE;
static ENHeader *OldPacket = 0;	/* Recycled packet 	*/
static int OldPackLen = 0;	/* Length of this packet */

/*
 * The "device" from which packets come.
 */
pcap_t *Device;



static void InCroak FP ((char *));
static ENHeader * GetEtherPacket FP ((int *));
static void RecyclePacket FP ((ENHeader *, int));




void
OpenEthernet (interface)
char *interface;
/*
 * Open up the ethernet device.
 */
{
	char ebuf[PCAP_ERRBUF_SIZE];
	struct bpf_program filter;
/*
 * Get the device opened.
 */
	if ((Device = pcap_open_live (interface, 1500, TRUE, 1, ebuf)) == NULL)
		InCroak (ebuf);
/*
 * Compile up our filter expression.  I'm too damn lazy to get the real
 * netmask from the system, so I just wired class C.  I don't think it's
 * relevant to this filter anyway.
 */
	if (pcap_compile (Device, &filter, "ether proto 0x6006", TRUE,
			0xffffff00) < 0)
		InCroak ("pcap_compile failure");
	pcap_setfilter (Device, &filter);
/*
 * That's it!
 */
}	






static void
InCroak (s)
char *s;
/*
 * Deal with some sort of failure.
 */
{
	ui_printf ("%s\n", s);
	die ();
}





Beam
GetEtherBeam (beam)
Beam beam;
/*
 * Get a beam from the ethernet.
 */
{
	unsigned char *buf;
	ENHeader *hdr, *cont;
	int npacket, npart, len, offset;
/*
 * Wait until we get a begin packet.
 */
	do 
		hdr = GetEtherPacket (&len);
	while (hdr && hdr->en_type != PT_FIRST);
/*
	ui_printf ("First, seq %ld, f/r %d, num %d, g %d->%d\n", hdr->en_seq,
		hdr->en_fperrad, hdr->en_number, hdr->en_g_first, 
		hdr->en_g_last);
*/
/*
 * Select a buffer.
 */
	if (++CurrBuf >= NEBUF)
		CurrBuf = 0;
	buf = EBuf[CurrBuf];
/*
 * Copy this packet over (sigh).
 */
	memcpy (buf, hdr + 1 /* skip to data */, len - sizeof (*hdr));
	offset = len - sizeof (*hdr);
/*
 * Fill in the first beam structure.
 */
	beam->b_hk = (Housekeeping *) buf;
	beam->b_npart = 1;	/* We are reassembling */
	npart = hdr->en_fperrad;
	beam->b_gdesc[0].gd_first = hdr->en_g_first;
	beam->b_gdesc[0].gd_ngate = hdr->en_g_last;
	beam->b_gdesc[0].gd_data = beam->b_hk->sz_hsk*sizeof (short) +
				(unsigned char *) beam->b_hk;
/*
 * Now we go through and get the rest of them.
 */
	for (npacket = 2; npacket <= npart; npacket++)
	{
	/*
	 * Get the next packet.
	 */
		cont = GetEtherPacket (&len);
/*
		ui_printf (" Cont, seq %ld, num %d, g %d->%d\n", cont->en_seq,
			cont->en_number, cont->en_g_first, cont->en_g_last);
*/
	/*
	 * If it doesn't match what we are expecting, so it goes.
	 */
		if (cont->en_number != npacket)
		{
			RecyclePacket (cont, len);
			return (GetEtherBeam (beam));
		}
	/*
	 * Fill in another chunk.
	 */
		memcpy (buf + offset, cont + 1, len - sizeof (*cont));
		offset += len - sizeof (*cont);
		beam->b_gdesc[0].gd_ngate = cont->en_g_last;
	}
/*
 * Done!
 */
 	return (beam);
}





static ENHeader *
GetEtherPacket (len)
int *len;
/*
 * Return the next packet off the net.
 */
{
	static unsigned long lastseq = 0;
	ENHeader *ehdr;
	struct pcap_pkthdr pcaphdr;
	unsigned char *packet;
/*
 * If the last packet is to be recycled, just send it back again.
 */
	if (Recycle)
	{
		Recycle = FALSE;
		*len = OldPackLen;
		return (OldPacket);
	}
/*
 * Do the snarf.
 */
	if ((packet = (unsigned char *) pcap_next (Device, &pcaphdr)) == 0)
		InCroak ("pcap_next failed");
/*
 * We got it.  Return the info.
 */
	ehdr = (ENHeader *) packet;
	if (lastseq > 0 && ehdr->en_seq != lastseq + 1)
		NMissed++;
	lastseq = ehdr->en_seq;
	*len = pcaphdr.len;
	return ((ENHeader *) packet);
}




static void
RecyclePacket (packet, len)
ENHeader *packet;
/*
 * See to it that the last packet gets read again.  NOTE that we also
 * recycle the buffer here...
 */
{
	Recycle = TRUE;
	OldPacket = packet;
	OldPackLen = len;
	if (--CurrBuf < 0)
		CurrBuf = NEBUF;
}
