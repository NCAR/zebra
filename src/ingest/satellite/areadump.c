/*
 * $Id: areadump.c,v 1.1 1993-06-25 08:12:33 granger Exp $
 */

# include <stdio.h>
# include "defs.h"

void DumpInfo();
void GetFileTime();
void swapfour();

#define C_DATE		(1<<0)
#define C_RESOLUTION	(1<<1)
#define C_IMAGE_TYPE	(1<<2)
#define C_IMAGE_SIZES	(1<<3)
#define C_ALL		(0xffff)
#define C_NONE		(0)
#define C_DEFAULT	(C_DATE | C_RESOLUTION)

int	Mdays[] = {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};


void
usage(argc, argv)
int argc;
char *argv[];
{
	printf ("Usage: %s [-d] [-r] [-t] [-s] [-a] <area_file> ...\n", 
		argv[0]);
	printf ("Defaults to showing date and resolution, but the following\n");
	printf ("options can be combined to tailor output:\n");
	printf (" -d  Display dates\n");
	printf (" -r  Resolution info\n");
	printf (" -t  Image type and source\n");
	printf (" -s  Image sizes: lines, elements, bytes\n");
	printf (" -a  Display all\n");
}



main (argc, argv)
int	argc;
char	**argv;
{
	FILE	*infile;
	char	*cvals, bytes[4], temp;
	int	*header, *nav_cod;
	int	i, c, *ival;
	ZebTime	zt;
	char	buf[50];
	int	code = C_NONE;

	cvals = (char *) malloc (768 * sizeof (char));

	if (argc < 2)
	{
		usage (argc, argv);
		exit (1);
	}

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
			   case 'a':
				code = C_ALL;
				break;
			   default:
				printf ("Invalid option %s\n",argv[i]);
				usage (argc, argv);
				exit(1);
			}
		} 
		else if ((infile = fopen (argv[i], "r")) == 0)
		{
			perror(argv[i]);
			continue;
		}
		else
		{
			fread (cvals, 1, 768, infile);  
			/* 256 byte header + 512 byte nav codicil */
			fclose (infile);
			DumpInfo (argv[i], cvals, (!code)?(C_DEFAULT):(code));
		}
	}
}



void
DumpInfo(filename, cvals, code)
	char *filename;
	char *cvals;
	int code;		/* Code detailing what to display */
{
	int *header = (int *)cvals;
	int *nav_cod = header + 64;
	char buf[50];
	char source[5], *c;
	char imtype[4];
	ZebTime zt;
	int Yres, Xres, Nbytes, Ny, Nx, Prefixlen, Linelen;
	int i;

	GetFileTime(header, &zt);
/*
 * NOTE: We don't swap in those portions which contain text
 */
	swapfour (header + 5, 15);
	swapfour (header + 32, 19);

	strncpy (imtype, nav_cod, 4);
	imtype[4] = '\0';
/*
 * Resolution (# of satellite units per image unit)
 */
	Yres = header[11];
	Xres = header[12];
	Nbytes = header[10];
/*
 * Image size (Nx x Ny), bytes per element and prefix length
 */
	Ny = header[8];
	Nx = header[9];
	Prefixlen = header[14];

	Linelen = Nx * Nbytes + Prefixlen;
/*
 * Source name from header word 51 (convert to lower case and remove spaces)
 */
	strncpy (source, header + 51, 4);
	source[4] = '\0';
	for (i = 0; i < 4; i++)
	{
		c = source + i;
		if (*c == ' ')
			*c = '\0';
		else
			*c = tolower (*c);
	}
/*
 * Now show all this information we've gotten
 */
	TC_EncodeTime(&zt, TC_Full, buf);
	if (code & (C_DATE | C_RESOLUTION))
	{
		printf("%-20s %s",buf,filename);
		printf("  X,Y Res: %i,%i km\n", Xres, Yres);
	}
	if (code & C_IMAGE_TYPE)
	{
		printf("    Image type:      %-5s", imtype);
		printf("%15s %-5s\n","Source:",source);
	}
	if (code & C_IMAGE_SIZES)
	{
		printf("    Bytes/element:   %1i            ", Nbytes);
		printf("Elements/line: %-4i\n",Nx);
		printf("    Prefix length:   %-2i bytes     ",Prefixlen);
		printf("Bytes/line:   %-5i   ",Linelen);
		printf("Number lines: %-5i\n",Ny);
	}
}




void
GetFileTime (header, t)
int	*header;
ZebTime *t;
/*
 * Print the time taken from the area file header
 */
{
	int	year, month, day, hour, minute, second;
/*
 * Do the appropriate byte swapping.
 */
	swapfour (header, 5);
/*
 * Extract the date.
 */
	year = header[3] / 1000;
	if ((year % 4) == 0)
		Mdays[2] = 29;	/* February has 29 days in leap years */

	day = header[3] % 1000;
	month = 1;
	while (day > Mdays[month])
		day -= Mdays[month++];
	Mdays[2] = 28;		/* return to 28 days in case next file
				 * is in a different year */
/*
 * Time
 */
	hour = header[4] / 10000;
	minute = (header[4] / 100) % 100;
	second = header[4] % 100;
/*
 * Build a zeb time out of the pieces and we're done
 */
	TC_ZtAssemble (t, year, month, day, hour, minute, second, 0);
}




void
swapfour (array, count)

int	*array, count;
/*
 * Swap byte order (0123 -> 3210) for 'count' longwords in 'array'
 */
{
	int	i;
	char	*bytes, swapped[4];

	for (i = 0; i < count; i++)
	{
		bytes = (char *) &(array[i]);
		swapped[0] = bytes[3];
		swapped[1] = bytes[2];
		swapped[2] = bytes[1];
		swapped[3] = bytes[0];
		memcpy (bytes, swapped, 4);
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
