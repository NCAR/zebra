/*
 * $Id: areadump.c,v 1.5 1997-06-17 09:36:29 granger Exp $
 */

# include <stdio.h>

# include <defs.h>
# include <message.h>
# include <DataStore.h>

# include "Area.h"

void DumpInfo();

#define C_DATE		(1<<0)
#define C_RESOLUTION	(1<<1)
#define C_IMAGE_TYPE	(1<<2)
#define C_IMAGE_SIZES	(1<<3)
#define C_LIMITS	(1<<4)
#define C_ALL		(0xffff)
#define C_NONE		(0)
#define C_DEFAULT	(C_DATE | C_RESOLUTION)



void
usage(argc, argv)
int argc;
char *argv[];
{
	printf ("Usage: %s [-d] [-r] [-t] [-s] [-a] <area_file> ...\n", 
		argv[0]);
	printf ("Defaults to showing date and resolution; the following\n");
	printf ("options can be combined to tailor output:\n");
	printf (" -d  Display dates\n");
	printf (" -r  Resolution info\n");
	printf (" -t  Image type and source\n");
	printf (" -s  Image sizes: lines, elements, bytes\n");
	printf (" -l  Estimate area limits\n");
	printf (" -a  Display all\n");
	printf (" -x  Include debug messages\n");
}



int
main (argc, argv)
int	argc;
char	**argv;
{
	int i;
	AreaFile *af;
	int code = C_NONE;

	if (argc < 2)
	{
		usage (argc, argv);
		exit (1);
	}

	msg_connect (NULL, argv[0]);
	for (i = 1; i < argc; ++i)
	{
		if ((argv[i][0] == '-') && (strlen(argv[i]) == 2))
		{
			switch (argv[i][1])
			{
			   case 'd':
				code |= C_DATE;
				break;
			   case 's':
				code |= C_IMAGE_SIZES;
				break;
			   case 't':
				code |= C_IMAGE_TYPE;
				break;
			   case 'r':
				code |= C_RESOLUTION;
				break;
			   case 'l':
				code |= C_LIMITS;
				break;
			   case 'a':
				code = C_ALL;
				break;
			   case 'x':
				msg_ELPrintMask (EF_ALL);
				break;
			   default:
				printf ("Invalid option %s\n",argv[i]);
				usage (argc, argv);
				exit(1);
			}
			continue;
		} 
		if (! (af = AddFile (NULL, argv[i], NULL)))
			continue;

		DumpInfo (af, (!code)?(C_DEFAULT):(code));
		CloseAreaFile (af);
	}
	exit (0);
}



void
DumpInfo (af, code)
AreaFile *af;
int code;		/* Code detailing what to display */
{
	char imtype[5];
	AreaImage area;
	AreaGrid ag;

	ReadArea (af, &area);
	if (! ReadNavCod (af, &area, NULL, imtype))
	{
		strcpy (imtype, "ERR");
	}
/*
 * Now show all this information we've gotten
 */
	if (code & (C_DATE | C_RESOLUTION))
	{
		printf("%-20s %s", TC_AscTime (&af->when, TC_Full), af->name);
		printf("  X,Y Res: %i,%i km\n", area.xres, area.yres);
	}
	if (code & C_IMAGE_TYPE)
	{
		printf("    Image type:      %-5s", imtype);
		printf("%15s %-5s", "Source:", area.source);
		printf("%25s\n", 
		       (af->doswap ? "Swapping bytes":"No byte swap"));
	}
	if (code & C_IMAGE_SIZES)
	{
		printf("    Bytes/element:   %1i            ", area.nbytes);
		printf("Elements/line: %-4i\n", area.nx);
		printf("    Prefix length:   %-2i bytes     ", area.prefixlen);
		printf("Bytes/line:   %-5i   ", area.linelen);
		printf("Number lines: %-5i\n", area.ny);
	}
	if (code & C_LIMITS)
	{
		InitGrid (&ag);
		SetAreaLimits (af, &ag);
		if (ag.limits)
			printf ("Limits: %.1f %.1f %.1f %.1f; origin %.1f\n",
				ag.minlat, ag.minlon, ag.maxlat, ag.maxlon,
				ag.origin_lat);
		else
			printf ("Limits: Estimate failed.\n");
	}
	if (code == C_ALL)
	{
		printf("Satellite sensor source: %d\n", area.sss);
		printf("Number of channels: %d\n", area.nchans);
		printf("Byte offset to data block: %d\n", area.datablock);
		printf("         navigation block: %d\n", area.navblock);
		printf("        calibration block: %d\n", area.calblock);
		printf("Calibration type: %s\n", area.caltype);
		printf("Memo: %s\n\n", area.memo);
	}
}





# ifdef notdef
/*
 * This gives raw output.  Perhaps it can be returned with a command-line option.
 * For now this section is skipped
 */
	for (i = 0; i < 192; i++)
	{
	/*
	 * Special treatment for the 32 character comment
	 */
		if (i == 24)
		{
			printf ("25-32  ");
			for (c = 0; c < 32; c++)
				printf ("%c", cvals[4*i + c]);

			i += 7;
			printf ("\n");
			continue;
		}
	/*
	 * Print the word number
	 */
		printf ("%5d  ", i < 64 ? i+1 : i-63);
	/*
	 * Three of the entries are character info
	 */
		if (i == 51 || i == 52 || i == 64)
		{
			printf ("%c%c%c%c\n", cvals[4*i], cvals[4*i + 1], 
				cvals[4*i + 2], cvals[4*i + 3]);
			continue;
		}
	/*
	 * If we get here, the word of interest contains an integer, so
	 * we need to swap bytes around.
	 */
		memcpy (bytes, cvals + 4*i, 4);

		temp = bytes[0];
		bytes[0] = bytes[3];
		bytes[3] = temp;

		temp = bytes[1];
		bytes[1] = bytes[2];
		bytes[2] = temp;
	/*
	 * Print the value
	 */
		printf ("%d\n", *ival);
	}		
# endif /* notdef */
