
/*
 * Quick test of regular expression compatibility
 */
#include "zl_regex.h"

static char *rcsid = 
   "$Id: re_test.c,v 2.1 1994-01-05 20:16:37 granger Exp $";

main()
{
	int errors = 0;
	char *stat;

	/*
	 * Just compile a few strings and try to match.  Report
	 * any discrepancies.
	 */
	if (stat = zl_re_comp("[abc]"))
	{
		printf ("%s\n", stat);
		++errors;
	}
	else if (zl_re_exec("lkjjklkajji") != 1)
		++errors;
	else if (zl_re_exec("  fgh ") != 0)
		++errors;

	if (errors)
		printf ("%d errors found!\n");
	else
		printf ("OK.\n");
}

		
