/*----------------------------------------------------------------------*\
 *		Time logging routines					*
 *		UCAR/NCAR/ATD/RDP/G. Forrest Cook	6/11/90		*
\*----------------------------------------------------------------------*/

# include <sys/types.h>
# include <time.h>
# include <errno.h>
# include <stdio.h>
# include <string.h>

/*----------------------------------------------------------------------*\
 *		Send a GMT timestamp and message to stdout		*
 *		UCAR/NCAR/ATD/RDP/G. Forrest Cook	5/8/90		*
\*----------------------------------------------------------------------*/
void
log_gmt (msg)
char *msg;
{
	char tstr[40];
	time_t t = time ((time_t *) 0);
	char *atime = asctime(gmtime(&t));

	strcpy (tstr, atime);		/* get a local copy of time */
	tstr [strlen(tstr)-1] = '\0';	/* remove cr/lf */

	printf ("%s (GMT): %s\n", tstr, msg);
	fflush (stdout);		/* write msg to logfile */
}

/*----------------------------------------------------------------------*\
 *	Return the current GMT time string				*
\*----------------------------------------------------------------------*/

char *gmt_str ()
{
	static char tstr[40];
	time_t t = time ((time_t *) 0);
	char *atime = asctime(gmtime(&t));

	strcpy (tstr, atime);			/* get a local copy of time */
	strcpy (&tstr[strlen(tstr)-1], " (GMT)");	/* rm cr/lf, add gmt */

	return (tstr);
}

/*----------------------------------------------------------------------*\
 * Print GMT time, a local message, and the system error message.	*
\*----------------------------------------------------------------------*/

void gmt_perr (str)
char *str;
{
	char ostr[120];

	strcpy (ostr, gmt_str());

	if (strlen(str) > 80)
		printf ("\nstring too long in gmt_perr\n");
	else	strcat (ostr, str);

	perror (ostr);
}

/*----------------------------------------------------------------------*/
