/*----------------------------------------------------------------------*\
 *		Get Number From Console	(Generic Base)			*
\*----------------------------------------------------------------------*/

/*
 *	This function solicits a numeric entry with error checking and retry.
 *	If a carriage return is entered, select is set to min and returned.
 *	This function returns to the caller only after
 *	a valid numeric entry in the range of min to max has been
 *	entered.
 *	A retry error message is printed if the entry is out of range.
 *
 *	On entry: min and max are set to the lowest and highest
 *		  allowable digits to be entered.
 *		  deflt is the default (cr) answer returned
 *		  base is the numeric base of the number 2 - 16.
 *
 *	On exit: A correct entry has been received and select has been set
 *		 equal to the value entered.
 */

# define NDIGS 8	/* Max digit entry */
# define BELL 0x07	/* bell character */
# define ERROR 0	/* value returned for Error */
# define SELERR -1	/* select error flag */
# define SELOK 0	/* select ok flag */

#include <stdio.h>
#include <string.h>

int
get_g_num (min, max, deflt, base)
int min, max, deflt, base;
{
	int select, err;

	if (min > max)		/* Error in calling params */
	{
		printf ("\nERROR (get_g_num): Min > Max\n");
		return (ERROR);
	}

	if ((deflt < min) || (deflt > max))
	{
		printf ("\nERROR (get_g_num): Default is out of range\n");
		return (ERROR);
	}

	if ((base < 2) || (base > 16))	/* Error in calling params */
	{
		printf
		  ("\nERROR (get_g_num): Base is out of range\n");
		return (ERROR);
	}

	select = mlt_dig (deflt, base, &err);	/* get an entry */

	while ((select < min) || (select > max) || (err == SELERR))
	{
		printf ("\n%cOut of range, Re-enter between ", BELL);

		if (base > 10)		printf ("%X and %X", min, max);
		else if (base == 8)	printf ("%o and %o", min, max);
		else			printf ("%d and %d", min, max);

		printf (" in base %d: ", base);
		select = mlt_dig (deflt, base, &err);	/* try again */
	}
	return (select);
}

/*-------------------------------------------------------------------*/
/*
 *	Get an entry from the console and convert it to an integer up to
 *	8 digits long (dddddddd).  If non-numeric characters are entered,
 *	err is set to SELERR to indicate and error, if ok, it is set to SELOK.
 *	Leading and trailing spaces are ignored.
 *
 *	On entry:	deflt contains the number to return for (cr) only.
 *			base contains the numeric base of the entry.
 *			err is undefined.
 *
 *	On exit:	Select is set to the numeric value of the digits
 *			entered from the keyboard.
 *			If a non valid entry is received, err is set to SELERR.
 */

int
mlt_dig (deflt, base, err)
int deflt, base, *err;
{
	int select, count1;
	int subtot = 0;
	int count = 0;			/* digit counter */
	int sign = 1;
	char kentry [NDIGS + 1];	/* input data is stored here */
	char chr;

	*err = SELOK;

	while ((chr = getchar()) != '\n')	/* fill count[] til (cr). */
	{
		if ((chr == ' ') || (chr == '\t')); /* ignore tabs and spaces */
		else if ((chr == '-') && (count == 0))	sign = -1;
		else if (count++ <= NDIGS)	kentry [count] = chr;
	}

	if (count)
	{
		for (count1 = 1; count1 <= count; count1++)
		{
			if (base <= 10)		/* decode base <= 10 numbers */
			{
				if ((kentry [count1] >= '0')
				 && (kentry [count1] <= base + '0' - 1))
					select = kentry [count1] - '0';

				else *err = SELERR;	/* non numeric */
			}
			else		/* decode hex and base > 10 numbers */
			{
				if ((kentry [count1] >= '0')	  /* 0-9 */
				 && (kentry [count1] <= '9'))
					select = kentry [count1] - '0';

				else if ((kentry [count1] >= 'a') /* a-f */
				      && (kentry [count1] <= 'f'))
					select = kentry [count1] - 'a' + 10;

				else if ((kentry [count1] >= 'A') /* A-F */
				      && (kentry [count1] <= 'F'))
					select = kentry [count1] - 'A' + 10;

				else *err = SELERR;
			}
			subtot = (base * subtot) + select;
		}
		select = subtot * sign;
	}
	else 				/* if count = 0 */
	{
		select = deflt;		/* default for (cr) case */
		printf ("Default: ");	/* print default # in correct base */
		if (base == 16)		printf ("%X", deflt);
		else if (base == 10)	printf ("%d", deflt);
		else if (base == 8)	printf ("%o", deflt);
		else			printf ("(decimal): %d", deflt);
	}
	return (select);
}

/*-------------------------------------------------------------------*/
void
cr_hold ()		/* wait until CR is typed */
{
	printf ("\nType RETURN to proceed: ");
	get_g_num (0, 0, 0, 10);
}

/*-------------------------------------------------------------------*/
int
cr_hold_esc ()		/* wait until CR is typed and check for exit */
{
	printf ("\nType RETURN to proceed or -1 to exit: ");

	return (get_g_num (-1, 0, 0, 10));
}

/*----------------------------------------------------------------------*/
void
get_int_param (name, var, min, max)	/* generic parameter entry routine */
char *name;
int *var, min, max;
{
	printf ("\n%s was %d, Enter new value: ", name, *var);
	*var = get_g_num (min, max, *var, 10);
	printf ("\n");
}

/*----------------------------------------------------------------------*/
int
str_prompt (prompt, str, maxlen)	/* generic string entry routine */
char *prompt, *str;
int maxlen;
{
	static char instr[120];
	int sl, retry = 1;

	printf ("%s", prompt);

	if (maxlen > 120)	/* first check for a reasonable maxlen */
	{
		printf
		("\nError in str_prompt: maxlen greater than input array\n");
		return(0);
	}

	do			/* then get the string and test its length */
	{
		gets (instr);
		sl = strlen(instr);
		if (sl <= maxlen) retry = 0;
	} while (retry);

	strcpy (str, instr);	/* finally, copy the input string over */

	return (sl);
}

/*-------------------------------------------------------------------*/
