
/*
 * Quick test of regular expression compatibility
 */
#include "zl_regex.h"

RCSID ("$Id: re_test.c,v 2.2 1995-05-24 22:34:22 granger Exp $")

int
main()
{
	int errors = 0;
	char *stat;

	/*
	 * Just compile a few strings and try to match.  Report
	 * any discrepancies.
	 */
	if ((stat = zl_re_comp("[abc]")))
	{
		printf ("regex error: %s\n", stat);
		++errors;
	}
	else if (zl_re_exec("lkjjklkajji") != 1)
		++errors;
	else if (zl_re_exec("  fgh ") != 0)
		++errors;

	if ((stat = zl_re_comp("^.234.*5$")))
	{
		printf ("regex error: %s\n", stat);
		++errors;
	}
	else if (zl_re_exec("12345") != 1)
		++errors;
	else if (zl_re_exec("x234xxxx5") != 1)
		++errors;
	else if (zl_re_exec(" ") != 0)
		++errors;
	else if (zl_re_exec("xx2345") != 0)
		++errors;
	else if (zl_re_exec("x2345yyy") != 0)
		++errors;

	return (errors);
}

		
