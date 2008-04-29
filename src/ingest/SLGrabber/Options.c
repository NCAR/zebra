/* $Id: Options.c,v 2.2 2000-07-23 06:32:22 granger Exp $ */

#include "Options.h"
#include <string.h>

static int *Argc;
static char **Argv;
static const char **Options;
static char *Arg;
static int I;
static int Abbreviations;

char Option_Flag = '\0';

void
OptionSetup (int *argc, char *argv[], const char *options[])
/*
 * The options array is a null terminated array of options to be
 * matched against the command line.
 */
{
    Argc = argc;
    Argv = argv;
    Options = options;
    Arg = 0;
    I = 0;
    Abbreviations = 1;
}


/*
 * Setup Options handling with a flag to set whether abbreviations
 * are allowed or not.  Abbreviations default to on.  Non-zero 'abbrevs'
 * implies abbreviations are accepted, zero implies the full option must
 * match.
 */
void
OptionSetupAbbrevs (int *argc, char *argv[], const char *options[], 
		    int abbrevs)
{
    OptionSetup (argc, argv, options);
    Abbreviations = abbrevs;
}




static void
RemoveOptions(int *argc, char *argv[], 
	      int i,		/* position to start removing args from */
	      int n)		/* number of args to remove */
{
	int j;

	(*argc) -= n;
	for (j = i; j < *argc; ++j) 
	   argv[j] = argv[j+n];
}


/*
 * Return the second character (assumed first non-minus) of the next option
 * matched on the command line.  If the option has an argument, note the
 * argument.  Remove the option and its argument, if any, from the command
 * line.
 */
int
OptionNext ()
{
    int ret = -1;

    while (ret < 0 && ++I < *Argc)
    {
	int len = strlen (Argv[I]);
	if (len >= 2)
	{
	    int o = 0;
	    while (Options[o])
	    {
		if (Options[o] != OptionArgument &&
		    (Options[o+1] != OptionArgument ||
		     I+1 < *Argc))
		{
		    int equal;
		    if (Abbreviations)
			equal = (strncmp (Argv[I], Options[o], len) == 0);
		    else
			equal = (strcmp (Argv[I], Options[o]) == 0);
		    if (equal)
		    {
			ret = Options[o][1];
			if (Options[o+1] == OptionArgument)
			{
			    Arg = Argv[I+1];
			    RemoveOptions (Argc, Argv, I, 2);
			}
			else
			    RemoveOptions (Argc, Argv, I, 1);
			--I;
			break;
		    }
		}
		++o;
	    }
	}
    }
    return ret;
}


char *
OptionArg ()
{
    return Arg;
}
