/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 STRREPL.C 19-Feb-96,15:31:52,`DWS' reglue: new file                     */
/* 2 STRREPL.C 20-Feb-96,12:13:28,`USER' Released                            */
/* 3 STRREPL.C 22-Mar-96,12:04:22,`DWS' reglue: modified file                */
/* 4 STRREPL.C 25-Mar-96,14:03:28,`USER' Released                            */
/**** McIDAS Revision History *** */

#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>

#include "mcidasp.h"

#define HASH '#'
#define HASHSTR "#"
#define TOOMUCH_RECURSION 5


static M0buf   *buf = 0;	/* permanent resizeable bufferspace */


/*
 * This function returns a pointer to internal storage containing the
 * value of the string or a null pointer if the lookup could not
 * be performed.
 *
 * Currently lbget is used, so the number 160 is known.
 */

#define LBGET_MAXLEN 160

static char    *
table_entry(const char *index, int size)
{
	extern Fint     lbget_(const char *, char *, FsLen, FsLen);
	static char     buffer[LBGET_MAXLEN + 1];

	/* look up string in string table */
	if (lbget_(index, buffer, size, LBGET_MAXLEN) < 0)
	{
		/* lookup failed */
		return 0;
	}

	/* remove trailing blanks to produce c-idiom string */
	buffer[fslen(buffer, LBGET_MAXLEN)] = '\0';
	return buffer;
}

/*
 * this recursive function does string replacement;
 * successful passage past this routine results in bytes
 * being added to buf.
 */

static int
string_replace(const char *source, int level)
{
	extern Fint     ksys_(Fint *);
	int             rc;
	const char     *ptr;	/* working variable along source    */

	/* check for excess recursion */
	if (level > TOOMUCH_RECURSION)
		return -2;

	/*
	 * keep doing something as long as there are any hash marks
	 * left
	 */
	while ((ptr = strchr(source, HASH)) != (char *) 0)
	{

		/*
		 * copy from begining of source to just before ptr
		 * into output
		 * 
		 * this might be zero bytes, but the M0buf routines
		 * handle it OK
		 */
		M0bufcat(buf, source, ptr - source);
		source = ptr + 1;

		/*
		 * if there are two in a row, just skip them both in
		 * the source, and put one in the destination
		 */
		if (*(source) == HASH)
		{
			source++;
			M0bufcat(buf, HASHSTR, sizeof HASHSTR - 1);
			continue;
		}

		/*
		 * we know that source points to the beginning of a
		 * string variable;  we now want ptr to point to the
		 * end of it
		 */
		ptr = source;
		if(isgraph(*ptr)) ptr++;
		while (isupper(*ptr) || isdigit(*ptr))
			ptr++;

		/*
		 * now we have delimited the string variable
		 * 
		 * it starts at source, and ends one before ptr
		 */

		/*
		 * there are two special cases
		 * 
		 * #UC(number) and #SYS(number)
		 */
		if (strncmp(source, "UC(", 3) == 0)
		{
			char            number[100];

			ptr++;
			source = ptr;

			/* delimit end of number, or end of input */
			while (*ptr != ')' && *ptr != '\0')
				ptr++;
			if (*ptr == ')')
				ptr++;

			/* check bounds */
			if (atoi(source) < -101 || atoi(source) > 8192)
			{
				M0bufrealloc(buf, 0);
				source -= 4;
				M0bufcat(buf, source, strlen(source));
				return -1;
			}

			/* turn Ucword into string */
			sprintf(number, "%ld", (long) Mcluc(atoi(source)));
			M0bufcat(buf, number, strlen(number));

			/* otherwise check for next special case */
		}
		else if (strncmp(source, "SYS(", 4) == 0)
		{
			char            number[100];
			Fint            ival;

			ptr++;
			source = ptr;

			/* delimit end of number, or end of input */
			while (*ptr != ')' && *ptr != '\0')
				ptr++;
			if (*ptr == ')')
				ptr++;

			ival = atoi(source);

			sprintf(number, "%d", ksys_(&ival));
			M0bufcat(buf, number, strlen(number));

			/* normal string, out of string table */
		}
		else
		{
			char           *string;

			string = table_entry(source, (int) (ptr - source));

			/*
			 * if string is NULL, return failure, and
			 * point dest to the offending string
			 */
			if (string == 0)
			{
				M0bufrealloc(buf, 0);
				M0bufcat(buf, source, ptr - source);
				return -1;
			}

			/*
			 * lookup was successful.  recurse
			 */
			{
				char           *temp;

				temp = stralloc(string, (void *)0);
				rc = string_replace(temp, level + 1);
				free(temp);
				if (rc != 0)
					return rc;
			}
		}

		/* point beyond end of string variable */
		source = ptr;

	}

	/* tack on any bytes that remain */
	M0bufcat(buf, source, strlen(source));
	return 0;
}


/*
*$ Name:
*$    Mcstrrepl - do McIDAS string replacement
*$
*$ Interface:
*$    #include "mcidas.h"
*$
*$    int
*$    Mcstrrepl(const char *source, char **dest)
*$
*$ Inputs:
*$    source     - command string which may contain #
*$
*$ Input abd Output:
*$    none
*$
*$ Output:
*$    dest       - pointer to pointer to string made by
*$                 resolving any # expressions in source
*$
*$ Return values:
*$    0 - success.  dest contains resolution of # values
*$   -1 - failure, a #variable was not found
*$        in this case, dest points to the offending #variable name
*$   -2 - failure, #variables were nested too deeply
*$        in this case, dest points to the replacement string up to
*$        the point where the nesting became too deep
*$
*$ Remarks:
*$    The storage pointed to by dest is static, and not saved
*$    across consecutive calls.
*$
*$    In Fortran, use lbrepl().
*$
*$ Categories:
*$    utility
*/


int
Mcstrrepl(const char *source, char **dest)
{
	int             rc;


	/* start with an empty buffer */
	if (buf == 0)
	{
		buf = M0bufalloc();
	}
	else
	{
		M0bufrealloc(buf, 0);
	}

	rc = string_replace(source, 0);

	/* now return with dest pointing to data */
	*dest = M0bufptr(buf);
	return rc;
}

/*
*$ Name:
*$    lbrepl    - expand strings using system string table
*$
*$ Interface
*$    integer function
*$    lbrepl(character*1 cmark, character*(*) cin, character*(*) count)
*$
*$ Inputs:
*$    cmark      - string symbol.  Must be #
*$    cin        - array of text to expand
*$
*$ Input and Output:
*$    none
*$
*$ Output:
*$    cout       - cin after replacement
*$
*$ Return values:
*$    0 - success, cout contains string after replacements
*$   -1 - failure, string variable could not be found, cout contains
*$        name of string variable which could not be found
*$   -2 - failure, strings nested too deeply, cout contains the
*$        replacement string up to the point where the nesting became
*$        too deep
*$
*$ Remarks:
*$    In C, use Mcstrrepl().
*$
*$    There is no indication as to whether the string in cout has
*$    been truncated.
*$
*$ Categories:
*$    utility
*/

Fint
lbrepl_(const char *cmark, const char *cin, char *cout,
	FsLen cmarklen, FsLen cinlen, FsLen coutlen)
{
	int             rc;
	char           *result;
	static M0buf   *request = 0;

	assert(cmarklen == 1);	/* first parameter must be of length
				 * 1 */
	assert(*cmark == HASH);	/* only hashmark is allowed in this
				 * implementation */

	/* initialize request */
	if (request == 0)
	{
		request = M0bufalloc();
	}

	/* copy in FORTRAN string */
	M0bufcpy(request, cin, fslen(cin, cinlen));

	/* call c-implementation */
	rc = Mcstrrepl(M0bufptr(request), &result);

	/* copy the result to the user's storage */
	(void) Mcstrtofs(cout, result, coutlen);

	/* propagate return code */
	return rc;
}
