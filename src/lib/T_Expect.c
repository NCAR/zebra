/*
 * The TX_ log message expect interface.  Test applications register
 * the log messages which are expected, and we use the msg library log
 * callback functionality to verify that all expected log messages 
 * are received.
 */

#include "defs.h"
#include "zl_regex.h"
#include "message.h"
#include "Test.h"

RCSID("$Id: T_Expect.c,v 2.1 1996-11-19 07:50:36 granger Exp $")


typedef struct _TX_Entry {
	int	te_id;
	int	te_mask;
	int 	te_suppress;
	int	te_count;
	char	te_re[512];
	struct _TX_Entry *te_next;
} TX_Entry;


static TX_Entry *TE_List = NULL;
static TX_Entry *TE_Free = NULL;
static int TX_ID = 0;	
static int TX_InitFlag = 0;

/*
 * Private prototypes
 */
static void TX_InsertEntry FP ((TX_Entry *te));
static TX_Entry *TX_GetEntry FP ((void));
static void TX_RemoveEntry FP ((TX_Entry *te, TX_Entry *prev));
static int TX_Log FP ((int mask, char *msg, void *arg));
static int TX_Purge FP ((int mask, TX_Entry *te, TX_Entry *prev));


void
TX_Init ()
/*
 * Reset our variables and register our log message callback routine.
 */
{
	++TX_InitFlag;
	TX_ClearAll (0);
	TX_ID = 0;
	msg_LogCallback (EF_ALL, TX_Log, NULL);
}



void
TX_Closure ()
/*
 * Free the free list.
 */
{
	while (TE_Free)
	{
		TX_Entry *te = TE_Free;
		TE_Free = te->te_next;
		free (te);
	}
}



static void
TX_Reset ()
/*
 * Reset to our initialized state, removing our callback function.
 * Later entries into the expect interface will call TX_Init.
 */
{
	msg_LogCallback (0, NULL, NULL);
	TX_InitFlag = 0;
}



static int
TX_Log (mask, msg, arg)
int mask;
char *msg;
void *arg;
/*
 * Check whether this message matches any of our registered
 * expectations.  Look the first match only.  If found, remove the entry
 * and return non-zero iff the suppress field of the entry is non-zero.
 */
{
	TX_Entry *te, *prev;
	int suppress = 0;

	for (prev = te = TE_List; (te != NULL); prev = te, te = te->te_next)
	{
		char *status;
		/*
		 * Ignoring the count would make log message matching "hungry".
		 * For now we only match as many as we were told to match.
		 */
		if (te->te_count == 0)
			continue;
		if (! (mask & te->te_mask))
			continue;
		if (! te->te_re[0])		/* automatic match */
			break;
		if ((status = zl_re_comp (te->te_re)) != NULL)
		{
			msg_ELog (EF_PROBLEM, "TX regex error: %s", status);
			TX_RemoveEntry (te, prev);
			te = prev;
			continue;
		}
		if (zl_re_exec (msg))
			break;
	}
	if (te)
	{
		suppress = te->te_suppress;
		--te->te_count;
		msg_ELog (EF_DEBUG, "TX caught%s (%d left): %s",
			  ((suppress) ? " and suppressed" : ""), 
			  te->te_count, msg);
	}
	return (suppress);
}



static void
TX_RemoveEntry (te, prev)
TX_Entry *te;
TX_Entry *prev;
{
	if (te == TE_List)
		TE_List = te->te_next;
	else if (prev)
		prev->te_next = te->te_next;
	te->te_next = TE_Free;
	TE_Free = te;
	/*
	 * If we no longer have any entries to match against, reset.
	 */
	if (! TE_List)
		TX_Reset();
}



static TX_Entry *
TX_GetEntry ()
{
	TX_Entry *te;

	if (TE_Free)
	{
		te = TE_Free;
		TE_Free = te->te_next;
	}
	else
	{
		te = (TX_Entry *) malloc (sizeof (TX_Entry));
	}
	return (te);
}



static void
TX_InsertEntry (te)
TX_Entry *te;
{
	te->te_next = TE_List;
	TE_List = te;
}



int
TX_ExpectMany (mask, suppress, count, re)
int mask;
int suppress;
int count;
char *re;
/*
 * A null regular expression matches everything
 */
{
	TX_Entry *te;

	if (! TX_InitFlag)
		TX_Init();
	te = TX_GetEntry ();
	if (re)
		strcpy (te->te_re, re);
	else
		te->te_re[0] = '\0';
	te->te_suppress = suppress;
	te->te_mask = mask;
	te->te_count = count;
	te->te_id = ++TX_ID;
	TX_InsertEntry (te);
	return (te->te_id);
}



int
TX_Expect (mask, suppress, re)
int mask;
int suppress;
char *re;
{
	return (TX_ExpectMany (mask, suppress, 1, re));
}



int
TX_Catch (re)
char *re;
/*
 * Expect and suppress a message of any mask which matches this regex.
 */
{
	return (TX_Expect (EF_ALL, TRUE, re));
}



int
TX_ClearAll (mask)
int mask;
/*
 * Return zero if there are no expect entries to clear. If 'mask' is nonzero,
 * log warnings about every expected message not received with that mask.
 */
{
	TX_Entry *te;
	int errors = 0;

	if (! TX_InitFlag)
		TX_Init();
	if (! TE_List)
		return (0);
	te = TE_List;
	while (te)
	{
		TX_Entry *next = te->te_next;
		errors += TX_Purge (mask, te, NULL);
		te = next;
	}
	TE_List = NULL;
	return (errors);
}



int
TX_Clear (id, mask)
int id;
int mask;
/*
 * Check whether the given expect entry has cleared.  If it has not,
 * log a message with 'mask', remove it, and return non-zero.  Otherwise
 * just return zero.
 */
{
	TX_Entry *te, *prev;
	int result = 0;

	if (! TX_InitFlag)
		TX_Init();
	te = prev = TE_List;
	while (te)
	{
		if (te->te_id == id)
			break;
		prev = te;
		te = te->te_next;
	}
	if (te)
	{
		result += TX_Purge (mask, te, prev);
	}
	return (result);
}



int
TX_Seen ()
/*
 * Just check that the most recent expect entry has been seen, and if not
 * log a generic problem message.
 */
{
	TX_Entry *te = TE_List;
	int result = 0;

	if (te && (te->te_id == TX_ID))
	{
		result += TX_Purge (EF_PROBLEM, te, NULL);
	}
	return (result);
}



int
TX_Pending ()
/*
 * Check to see whether the most recent expect entry is awaiting a clear.
 * If not return 0, else return 1, but don't actually clear the entry.
 */
{
	TX_Entry *te = TE_List;

	return ((te && (te->te_count <= 0)));
}



int
TX_Problem (count)
int count;
/*
 * We're expecting and wish to suppress some number of problems, and we
 * don't care what the log message says.
 */
{
	return (TX_ExpectMany (EF_PROBLEM | EF_EMERGENCY, TRUE, count, NULL));
}



static int
TX_Purge (mask, te, prev)
int mask;
TX_Entry *te;
TX_Entry *prev;
{
	char buf[512];
	int count;

	count = te->te_count;
	if (count > 0)
	{
		sprintf (buf, "expected %d more %s for %0#x:'%s'",
			 count, (count > 1) ? "matches" : "match",
			 te->te_mask, te->te_re);
	}
	else if (count < 0)
	{
		sprintf (buf, "%d too many matches for %0#x:'%s'",
			 (-count), te->te_mask, te->te_re);
	}
	/*
	 * Remove the entry before we log any messages about it.
	 */
	TX_RemoveEntry (te, prev);
	if (mask && (count != 0))
		msg_ELog (mask, "%s", buf);
	return ((count < 0) ? (-count) : count);
}

