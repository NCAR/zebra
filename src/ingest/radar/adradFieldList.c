# include <stdio.h>
# include <errno.h>
# include "raw.h"
# include "portable.h"
# include "sunrise_head.h"
# include "cvrt.h"
# include "xdr.h"

main (argc, argv)
int	argc;
char	**argv;
{
	XDR	xdrstrm;
	FILE	*xfile;
	int	version;
	int	nfld = 0;
	struct volume_summary	volhdr;
	struct ray_header	rayhdr;
	
	if (argc != 2)
	{
		fprintf (stderr, "Usage: %s <filename>\n", argv[0]);
		exit (1);
	}
	
	
	if (! (xfile = fopen (argv[1], "r")))
	{
		fprintf (stderr, "Error %d opening '%s'\n", argv[1]);
		exit (1);
	}
	
	xdrstdio_create (&xdrstrm, xfile, XDR_DECODE);
	sunrise_head (xdrstrm);
	accessvolhead (xdrstrm, &volhdr, RAW_VERSION);
	accessrayhead (xdrstrm, &rayhdr);

	
	if (rayhdr.flags.uz)
		printf ("field 'ureflectivity' offset %d\n", nfld++);
	if (rayhdr.flags.cz)
		printf ("field 'creflectivity' offset %d\n", nfld++);
	if (rayhdr.flags.vel)
		printf ("field 'velocity' offset %d\n", nfld++);
	if (rayhdr.flags.wid)
		printf ("field 'swidth' offset %d\n", nfld++);
	if (rayhdr.flags.zdr)
		printf ("field 'zdr' offset %d\n", nfld++);
}

	


int sunrise_head (xdrstrm)
XDR	xdrstrm;
{
	struct sunrise_head	head;
	int	i, ret = 1;
	char	*tmp;

	strcpy(head.magic, "");

	tmp = head.magic;
	if(! xdr_string(&xdrstrm, &tmp, 8))
	{
		fprintf (stderr, "Error reading SUNRISE header\n");
		exit (1);
	}

	ret &= xdr_u_char(&xdrstrm, &head.mdate.year);
	ret &= xdr_u_char(&xdrstrm, &head.mdate.month);
	ret &= xdr_u_char(&xdrstrm, &head.mdate.day);
	ret &= xdr_u_char(&xdrstrm, &head.mdate.hour);
	ret &= xdr_u_char(&xdrstrm, &head.mdate.minute);
	ret &= xdr_u_char(&xdrstrm, &head.mdate.second);
	ret &= xdr_u_char(&xdrstrm, &head.cdate.year);
	ret &= xdr_u_char(&xdrstrm, &head.cdate.month);
	ret &= xdr_u_char(&xdrstrm, &head.cdate.day);
	ret &= xdr_u_char(&xdrstrm, &head.cdate.hour);
	ret &= xdr_u_char(&xdrstrm, &head.cdate.minute);
	ret &= xdr_u_char(&xdrstrm, &head.cdate.second);
	ret &= xdr_int(&xdrstrm, &head.type);
	tmp = head.mwho;
	ret &= xdr_string(&xdrstrm, &tmp, 16);
	tmp = head.cwho;
	ret &= xdr_string(&xdrstrm, &tmp, 16);
	ret &= xdr_int(&xdrstrm, &head.protection);
	ret &= xdr_int(&xdrstrm, &head.checksum);
	tmp = head.description;
	ret &= xdr_string(&xdrstrm, &tmp, 40);
	ret &= xdr_int(&xdrstrm, &head.id);
	for(i=0;i<12;i++)
		ret  &=  xdr_int(&xdrstrm, &head.spares[i]);

	return (ret);
	
}


die ()
{
	fprintf (stderr, "Error exit\n");
	exit (1);
}
