# include <stdio.h>

main (argc, argv)
int	argc;
char	**argv;
{
	FILE	*infile;
	char	*cvals, bytes[4], temp;
	int	i, c, *ival;

	cvals = (char *) malloc (768 * sizeof (char));
	ival = (int *) bytes;


	if (argc < 2)
	{
		printf ("Usage: %s <area_file>\n", argv[0]);
		exit (1);
	}

	if ((infile = fopen (argv[1], "r")) == 0)
	{
		printf ("Unable to open '%s'\n", argv[1]);
		exit (1);
	}

	fread (cvals, 1, 768, infile);
	fclose (infile);

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
}

