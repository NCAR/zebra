
/*
 * The TP_ profiling interface.
 */
#include <unistd.h>
#include <stdio.h>
#include <limits.h>
#include <string.h>
#include <sys/times.h>
#include <sys/types.h>
#include <sys/param.h>
#include <time.h>

#include "defs.h"
#include "zl_regex.h"
#include "message.h"
#include "Test.h"

RCSID("$Id: T_Profile.c,v 2.1 1996-11-19 07:50:37 granger Exp $")

#define TE_NAME_LEN 40

typedef struct _TP_Entry {
	int	te_id;
	int	te_count;
	char	te_name[TE_NAME_LEN];
	clock_t	te_begin;
	struct tms te_tms;
	struct _TP_Entry *te_next;
} TP_Entry;


static TP_Entry *TE_List = NULL;
static TP_Entry *TE_Free = NULL;
static int TP_ID = 0;	
static int TP_InitFlag = 0;
static int TP_ReportMask = 0;	/* default to no reporting */
static int TP_Enabled = 0;	/* default to disable profiling */
static long ClockTicks = 60;	/* clock ticks per second */

/*
 * Private prototypes
 */
static void TP_InsertEntry FP ((TP_Entry *te));
static TP_Entry *TP_GetEntry FP ((void));
static void TP_RemoveEntry FP ((TP_Entry *te, TP_Entry *prev));
static TP_Entry *TP_FindEntry FP ((int id, TP_Entry **prev_out));
static int TP_Report FP ((int id, const char *name, int final));


static inline float
Clock2ms (clk)
clock_t clk;
{
	return ((((float)clk)*1000.0)/((float)ClockTicks));
}


void
TP_Init ()
/*
 * Reset our variables and register our log message callback routine.
 */
{
	++TP_InitFlag;
	TP_ID = 0;
	TP_ReportMask = 0;
	/*
	 * Try to calculate our clock ticks per second
	 */
#if defined(_SC_CLK_TCK)
	ClockTicks = sysconf(_SC_CLK_TCK);
	msg_ELog (EF_INFO, "%s %i clock ticks per second",
		  "profile: sysconf() returns", ClockTicks);
#elif defined(CLK_TCK)
	ClockTicks = CLK_TCK;
	msg_ELog (EF_INFO, "%s %i clock ticks per second",
		  "profile: CLK_TCK symbol gives", ClockTicks);
#elif defined(HZ)
	ClockTicks = HZ;
	msg_ELog (EF_INFO, "%s %i clock ticks per second",
		  "profile: HZ symbol gives", ClockTicks);
#else
	ClockTicks = 60;
	msg_ELog (EF_INFO, "%s %i per second",
		  "profile: clock ticks UNKNOWN, using default", ClockTicks);
#endif
}


/*
 * Behavior on entry to all public routines
 */
#define TP_ENTER(disabled) \
{ \
	  if (! TP_Enabled) return (disabled); \
	  if (! TP_InitFlag) TP_Init(); \
}
	  

int
TP_Profile (enable)
int enable;
/*
 * Enable profiles.  This "turns on" the profile capability by allowing
 * profiles to be registered and reported, using a default reporting
 * mask.  If profiles are not enabled, all the public entry routines
 * shortcut to immediate returns.
 */
{
	/*
	 * If disabling, clear existing profiles
	 */
	if (TP_Enabled && ! enable)
	{
		TP_ReportProfiles (0);
		TP_PopAll ();
		TP_Enabled = enable;
	}
	if (! TP_Enabled && enable)
	{
		TP_Enabled = enable;
		TP_Init ();		/* may as well re-init */
		TP_ReportProfiles (EF_INFO);
	}
	return (0);
}



int
TP_Begin (name)
const char *name;
{
	TP_Entry *te;

	TP_ENTER(0);
	te = TP_GetEntry ();
	te->te_name[0] = '\0';
	if (name)
	{
		strncpy (te->te_name, name, TE_NAME_LEN);
		te->te_name[TE_NAME_LEN - 1] = '\0';
	}
	te->te_id = ++TP_ID;
	te->te_count = 0;
	te->te_begin = time (0);
	times (&te->te_tms);
	TP_InsertEntry (te);
	return (te->te_id);
}



int
TP_Push (name)
const char *name;
{
	return (TP_Begin (name));
}



int
TP_End (id)
int id;
/*
 * End and report on the profile with this id.
 */
{
	int result;

	TP_ENTER(0);
	if ((result = TP_Report (id, NULL, 1)) < 0)
		msg_ELog (EF_PROBLEM, "profile end: id %d not found", id);
	return (result);
}




int
TP_Pop ()
{
	int result = -1;

	TP_ENTER(0);
	if (! TE_List)
		msg_ELog (EF_PROBLEM, "profile pop: no profiles on stack");
	else if ((result = TP_End (TE_List->te_id)) < 0)
		msg_ELog (EF_PROBLEM, "profile pop: %s",
			  "weird, top id not found by tp_end");
	return (result);
}



int
TP_PopAll ()
/*
 * Pop and report all profiles.  Return less than zero on error.
 */
{
	int errors = 0;

	TP_ENTER(0);
	if (! TE_List)
		return (0);
	while (TE_List)
	{
		if (TP_End (TE_List->te_id) < 0)
			errors = -1;
	}
	return (errors);
}




float
TP_Elapsed (id)
int id;
/* return elapsed wall clock msecs since id began */ 
{
	struct tms now;
	clock_t end;
	float elapsed;
	TP_Entry *te;
		
	TP_ENTER(-1);
	end = time (0);
	times (&now);
	te = TP_FindEntry (id, NULL);
	if (te)
	{
		elapsed = Clock2ms (end - te->te_begin);
	}
	else
	{
		msg_ELog (EF_PROBLEM, "profile elapsed: id %d not found", id);
		elapsed = -1;
	}
	return (elapsed);
}




int
TP_Count (id, count)
int id;
int count;
/* add count repeats to this id, and return the new count */
{
	TP_Entry *te;

	TP_ENTER(-1);
	te = TP_FindEntry (id, NULL);
	if (te)
	{
		te->te_count += count;
		count = te->te_count;
	}
	else
	{
		msg_ELog (EF_PROBLEM, "profile count: id %d not found", id);
		count = -1;
	}
	return (count);
}



int
TP_ReportElapsed (id, name)
int id;
const char *name;
/* report elapsed stats for this id at the named intermediate point
 * return the id on success, less than zero on failure */
{
	TP_ENTER(0);
	if (TP_Report (id, name, 0) < 0)
	{
		msg_ELog (EF_PROBLEM, "profile report: id %d not found", id);
		id = -1;
	}
	return (id);
}



int
TP_ReportProfiles (mask)
int mask;
/*
 * A report mask of zero disables reporting *but not profiling*
 */
{
	TP_ReportMask = mask;
	return (TP_ReportMask);
}



static int
TP_Report (id, name, final)
int id;			/* profile id to find */
const char *name;	/* if null use id's name */
int final;		/* nonzero if this is a final report */
/*
 * Generate a report for this profile id 
 */
{
	TP_Entry *te, *prev;
	char buf[256];
	struct tms now;
	clock_t end;
	float elapsed, real;
		
	/*
	 * Get timing info first so that it doesn't include our search time
	 */
	end = time (0);
	times (&now);
	te = TP_FindEntry (id, &prev);
	if (! te)
		return (-1);
	/* 
	 * For some reason times() always returns zero, contrary to the
	 * documentation, so we use time() to set begin and end real-time
	 * markers.  That resolution is too low for short tests, so revert
	 * to the sum of sys and cpu.  Otherwise always use the tms times
	 * to calculate ms/pass.
	 */
	elapsed = Clock2ms (now.tms_stime - te->te_tms.tms_stime +
			    now.tms_utime - te->te_tms.tms_utime);
	if (end - te->te_begin < 5 /*secs*/)
		real = elapsed;
	else
		real = (float)(end - te->te_begin) * 1000.0;
	sprintf (buf, "%s(%i): real %.1f %s, sys %.1f, cpu %.1f",
		 (name) ? (name) : te->te_name, te->te_id, 
		 (real > 5000) ? real/1000.0 : real,
		 (real > 5000) ? "s" : "ms",
		 Clock2ms (now.tms_stime - te->te_tms.tms_stime),
		 Clock2ms (now.tms_utime - te->te_tms.tms_utime));
	if (te->te_count > 0)
		sprintf (buf+strlen(buf), ": %i @ %.1f ms/pass",
			 te->te_count, (elapsed/(float)te->te_count));
	if (TP_ReportMask)
	{
		msg_ELog (TP_ReportMask, "%s", buf);
	}
	if (final)
		TP_RemoveEntry (te, prev);
	return (id);
}



static void
TP_RemoveEntry (te, prev)
TP_Entry *te;
TP_Entry *prev;
{
	if (te == TE_List)
		TE_List = te->te_next;
	else if (prev)
		prev->te_next = te->te_next;
	te->te_next = TE_Free;
	TE_Free = te;
}



static TP_Entry *
TP_GetEntry ()
{
	TP_Entry *te;

	if (TE_Free)
	{
		te = TE_Free;
		TE_Free = te->te_next;
	}
	else
	{
		te = (TP_Entry *) malloc (sizeof (TP_Entry));
	}
	return (te);
}



static void
TP_InsertEntry (te)
TP_Entry *te;
{
	te->te_next = TE_List;
	TE_List = te;
}



static TP_Entry *
TP_FindEntry (id, prev_out)
int id;
TP_Entry **prev_out;
{
	TP_Entry *te, *prev;

	te = prev = TE_List;
	while (te)
	{
		if (te->te_id == id)
			break;
		prev = te;
		te = te->te_next;
	}
	if (prev_out)
		*prev_out = prev;
	return (te);
}


