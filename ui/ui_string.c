/* 3/89 jc */
/*
 * Dynamic string functions.  These routines have usy_ prefixes for historical
 * reasons.
 */
# include "ui.h"
# include "ui_globals.h"
# include "ui_symbol.h"

static char *Rcsid = "$Id: ui_string.c,v 1.6 2000-04-10 20:33:45 burghart Exp $";
/*
 * For small, temporary strings, we maintain some internal lookaside 
 * lists with fixed-size strings.  This way, many malloc/free cycles are
 * avoided.
 *
 * The length of the lists is empirically determined from looking at a couple
 * of applications, particularly ROBOT.  Experience is that almost all
 * dynamic strings are *very* short, so most of the preallocated strings are
 * small ones.
 *
 * The following union is used for keeping track of unallocated strings.
 */

# define SMALLSIZE	32
# define MEDSIZE	64
# define BIGSIZE	128
union st_small
{
	union st_small	*next;
	char data[SMALLSIZE];
};


union st_med
{
	union st_med	*next;
	char data[MEDSIZE];
};

union st_big
{
	union st_big	*next;
	char data[BIGSIZE];
};

/*
 * These are the actual strings.
 */
# define N_SMALL	500
# define N_MED		100
# define N_BIG		50
static union st_small SmallT[N_SMALL], *Small, *SmallEnd;
static union st_med MedT[N_MED], *Med, *MedEnd;
static union st_big BigT[N_BIG], *Big, *BigEnd;
/*
 * Statistics.
 */
static int S_nstring = 0;		/* usy_string calls		*/
static int S_relstring = 0;		/* Released strings		*/
static int S_lenstring = 0;		/* bytes alloc for strings	*/
static int S_lenrel = 0;		/* Released string len		*/
static int S_getvm = 0;			/* Number dynamically allocated */
static int S_pstring = 0;		/* "permanent" strings		*/

/*
 * The number of free entries.  Set to zero initially so that string
 * allocation works before this module is initialized.
 */
static int N_small = 0, N_med = 0, N_big = 0;
static int N_asmall = 0, N_amed = 0, N_abig = 0;

static bool D_string = FALSE;


void
usy_st_init (stable)
stbl stable;
/*
 * Initialize the string tables.
 */
{
	int i;
/*
 * Set the pointers.
 */
	for (i = 0; i < N_SMALL - 1; i++)
		SmallT[i].next = SmallT + i + 1;
	SmallT[i].next = 0;
	for (i = 0; i < N_MED - 1; i++)
		MedT[i].next = MedT + i + 1;
	MedT[i].next = 0;
	for (i = 0; i < N_BIG - 1; i++)
		BigT[i].next = BigT + i + 1;
	BigT[i].next = 0;
/*
 * Initialize the free lists.
 */
	Small = SmallT;
	Med = MedT;
	Big = BigT;
/*
 * Initialize the end markers.
 */
 	SmallEnd = SmallT + N_SMALL;
	MedEnd = MedT + N_MED;
	BigEnd = BigT + N_BIG;
/*
 * Initialize the counters.
 */
 	N_small = N_SMALL;
	N_med = N_MED;
	N_big = N_BIG;
/*
 * Initialize a bunch of indirect variables so that the counts can be seen.
 */
 	usy_c_indirect (stable, "s_nstring", &S_nstring, SYMT_INT, 0);
 	usy_c_indirect (stable, "s_relstring", &S_relstring, SYMT_INT, 0);
 	usy_c_indirect (stable, "s_lenstring", &S_lenstring, SYMT_INT, 0);
 	usy_c_indirect (stable, "s_lenrel", &S_lenrel, SYMT_INT, 0);
	usy_c_indirect (stable, "s_getvm", &S_getvm, SYMT_INT, 0);
	usy_c_indirect (stable, "s_pstring", &S_pstring, SYMT_INT, 0);
	usy_c_indirect (stable, "n_small", &N_small, SYMT_INT, 0);
	usy_c_indirect (stable, "n_asmall", &N_asmall, SYMT_INT, 0);
	usy_c_indirect (stable, "n_med", &N_med, SYMT_INT, 0);
	usy_c_indirect (stable, "n_amed", &N_amed, SYMT_INT, 0);
	usy_c_indirect (stable, "n_big", &N_big, SYMT_INT, 0);
	usy_c_indirect (stable, "n_abig", &N_abig, SYMT_INT, 0);
	usy_c_indirect (Ui_variable_table, "ui$dstring", &D_string,
		SYMT_BOOL, 0);
}






char *
usy_string (const char* text)
/*
 * Return a dynamically-allocated string with this text.
 */
{
	int len = strlen (text) + 1;
	char *ret;
/*
 * Check the lookaside lists first.
 */
	if (len < SMALLSIZE && Small)
	{
		if (D_string)
			ui_printf ("A %d ", Small - SmallT);
		ret = Small->data;
		Small = Small->next;
		N_small--;
		N_asmall++;
	}
	else if (len < MEDSIZE && Med)
	{
		ret = Med->data;
		Med = Med->next;
		N_med--;
		N_amed++;
	}
	else if (len < BIGSIZE && Big)
	{
		ret = Big->data;
		Big = Big->next;
		N_big--;
		N_abig++;
	}
/*
 * Failing that, just allocate something special.
 */
 	else
	{
		ret = getvm (len);
		S_getvm++;
		S_lenstring += len;
	}
/*
 * Copy the data and return the data.
 */
 	memcpy (ret, text, len);
	S_nstring++;
	return (ret);
}




void
usy_rel_string (string)
char *string;
{
	union st_small *s;
	union st_med *m;
	union st_big *b;

	S_relstring++;
/*
 * See if this string is from one of the lookaside lists.
 */
 	if ((s = (union st_small *) string) >= SmallT && s < SmallEnd)
	{
		s->next = Small;
		Small = s;
		if (D_string)
			ui_printf ("D %d ", Small - SmallT);
		N_small++;
	}
	else if ((m = (union st_med *) string) >= MedT && m < MedEnd)
	{
		m->next = Med;
		Med = m;
		N_med++;
	}
	else if ((b = (union st_big *) string) >= BigT && b < BigEnd)
	{
		b->next = Big;
		Big = b;
		N_big++;
	}
	else
	{
		S_lenrel += strlen (string) + 1;
		relvm (string);
	}
} 






char *
usy_pstring (text)
const char *text;
/*
 * Allocate a "permanent" string, in that it's unlikely to be released
 * soon.  Use of this routine for long-lived strings will be more efficient
 * than use of usy_string.
 */
{
	int len = strlen (text) + 1;
	char *ret = getvm (len);

 	memcpy (ret, text, len);
	S_pstring++;
	S_lenstring += len;
	S_nstring++;
	return (ret);
}





dump_str (n)
int n;
{
	ui_printf ("String %d = '%s'\n", n, SmallT[n].data);
	return (TRUE);
}
