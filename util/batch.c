# define ___   0


int 
batch ()
/*
 * Return true if we are running as a batch job.
 */
{
# ifdef CRAY
	return (-1);	/* Always batch on CRAY */
# else
/*
 * Assume that, if we are talking to a terminal, we are
 * running interactively.  We look at stderr, since input/output streams
 * could be directed elsewhere.
 */
	return (isatty (2) == 0);
# endif
}
