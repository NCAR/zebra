/*
 * $Id: apple.c,v 3.18 2004-07-05 18:04:16 granger Exp $
 */

/*
 * Attempt to modularize the testing process:
 *
 * Provide routines for each of the different DataChunk classes which
 * create test DataChunks (perhaps these tests should be in the class
 * itself---perhaps even a DataChunk method).  The caller provides the
 * starting time, whether the chunk is mobile or stationary, and the number
 * of samples wanted, and maybe whether or not to add attributes, etc.
 *
 * Provide routines which store a given DataChunk to a named platform,
 * then fetch the DataChunk and compare it with the original.  This should
 * be independent of platform and datachunk class (especially if DataChunk
 * instance comparison we're also made a class method).

For each data chunk class,
	for each test data datachunk in the class
	   for each filetype which supports the organization and class
	   	define the appropriate platform for the org and filetype
		call the class method for creating a test datachunk
		assign the platform to the datachunk
		store the datachunk
		fetch the datachunk
		compare (report differences between) the stored and
			the fetched datachunks
		destroy the datachunks


Example: each class performs comparisons relevant to its domain.
Transparent tests number of samples, times of each sample, mobility,
locations.  MetData tests the same fields.  Scalar knows how to test data
values.  The compare method calls the superclasses of the datachunk classes
from raw down to the class and returns with a list of diff messages, if
any.

At some point, it would be nice to construct a ds.config file from here
given what test we want to make, and then fork() and exec() a dsDaemon
on the config file ourselves.  Keep the "t_" prefix convention for all test
platforms.

Allow dynamic platform definitions through the ds protocol, define exactly
the set of needed test platforms on the fly.  A generic ds.config file, or
possibly none at all, will suffice.

Bind the test functions to Tcl commands.  Each call to a Tcl command 
registers the desire to run that test, then on 'begin', the ds.config is
generated and the test routines are run.  Could we then pipe some output
to 'expect'?

 *********/

/*
 * Each test routine looks up its required platforms, and if not defined
 * looks them up in the table and defines it.  It then proceeds with the
 * test and returns the appropriate return value.
 *
 * Why so many duplicate platforms?  Why not delete all files from a
 * platform, then use it for the test, then delete the files?  Then other
 * test routines can re-use that platform....
 *
 * Some higher mechanism will pick which tests to run.
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>

#include <defs.h>
#include <zl_symbol.h>		/* just need symbol tables */
#include <message.h>
#include <timer.h>
#include "DataStore.h"
#include "apple.h"


RCSID("$Id: apple.c,v 3.18 2004-07-05 18:04:16 granger Exp $")

extern TestRoutine NSpaceTests[];
extern TestRoutine ZNFTests[];
extern TestRoutine NetCDFTests[];
extern TestRoutine TransparentTests[];
extern TestRoutine DataStoreTests[];
extern TestRoutine GridTests[];
extern TestRoutine AttributeTests[];
extern TestRoutine ScalarTests[];
extern TestRoutine ApplTests[];
extern TestRoutine DataFormatTests[];
extern TestRoutine DataChunkTests[];
extern TestRoutine FieldTests[];
extern TestRoutine MetDataTests[];
extern TestRoutine DerivationTests[];

/*
 * We set a maximum time for the tests and declare a handler for the 
 * alarm clock.
 */
# define MAX_TEST_TIME (2 * 60 * 60) /* max time to complete tests, seconds */
static void OutOfTime (int signal);

/*
 * Choose the modules to link and register.
 */
static TestRoutine *TestModules[] = 
{
	NSpaceTests,
	ZNFTests,
	NetCDFTests,
	TransparentTests,
	DataStoreTests,
	GridTests,
	AttributeTests,
	ScalarTests,
	ApplTests,
	DataFormatTests,
	DataChunkTests,
	FieldTests,
	MetDataTests,
	DerivationTests,
	NULL
};

static stbl TestNames = NULL;

/*
 * Globals.
 */
int DiffDataChunks = 0;
int DumpDataChunks = 0;
int Debug = 0;
int Verbose = 0;
int AllTests = 0;
int NoBuffer = 0;
int DefaultCount = 1;	/* default number of repetitions for each test */
int Errors = 0;		/* global error count */
ZebTime Begin;
int EF_STATS = EF_DEBUG;

TestField TestFields[] = {
	{ "scalar",	"Test Field",			"none" },
	{ "field2",	"Test Field, too",		"none" },
	{ "fld3",	"field #3",			"none" },
	{ "no4",	"Number 4 field",		"none" },
	{ "pres",	"Pressure",		       	"hPa" },
	{ "temp",	"Temperature",			"degC" },
	{ "rh",		"Relative humidity",		"percent" },
	{ "dpt",	"Dewpoint",			"K" }
};


/*
 * NOTICE: For the moment, only platforms which should be cleaned when
 * first defined should be included in this list. 
 */
struct TestPlatform TestPlatforms[] = 
{
	{ "t_dummy_cdf", FTNetCDF, OrgScalar, 60, FALSE },
	{ "t_dummy_znf", FTZeb, OrgScalar, 60, FALSE },
	{ "t_gap_cdf", FTNetCDF, OrgScalar, 60, FALSE },
	{ "t_gap_znf", FTZeb, OrgScalar, 60, FALSE },
	{ "t_deletes", FTZeb, OrgScalar, 1, FALSE },
	{ "t_nspace", FTNetCDF, OrgNSpace, 1000, FALSE },
	{ "t_nsscalar", FTNetCDF, OrgNSpace, 1000, FALSE },
	{ "t_blocks", FTZebra, OrgScalar, 1000, FALSE },
	{ "t_nsblocks", FTNetCDF, OrgNSpace, 1000, FALSE },
	{ "t_test6", FTNetCDF, OrgNSpace, 1000, FALSE },
	{ "t_transparent", FTZebra, OrgTransparent, 500, FALSE },
	{ "t_virtual", FTZebra, OrgScalar, 5000, TRUE },
	{ "t_getfields_cdf", FTNetCDF, OrgScalar, 100, FALSE },
	{ "t_getfields_znf", FTZebra, OrgScalar, 100, FALSE },
	{ "t_nsvsc_scalar", FTNetCDF, OrgScalar, 1000, FALSE },
	{ "t_nsvsc_nspace", FTNetCDF, OrgScalar, 1000, FALSE },
	{ "t_fieldtypes", FTNetCDF, OrgScalar, 100, FALSE },
	{ "t_time_units", FTNetCDF, OrgScalar, 100, FALSE },
	{ "t_time_units_2", FTNetCDF, OrgScalar, 100, FALSE },
	{ "t_att_types_cdf", FTNetCDF, OrgScalar, 100, FALSE },
	{ "t_aeri_types_cdf", FTNetCDF, OrgScalar, 5000, FALSE },
	{ "t_aeri_types_znf", FTZebra, OrgScalar, 5000, FALSE },
	{ "t_1dgrid_cdf", FTNetCDF, Org1dGrid, 1000, FALSE },
	{ "t_1dgrid_znf", FTZebra, Org1dGrid, 1000, FALSE },
	{ "t_irgrid_cdf", FTNetCDF, OrgIRGrid, 24, FALSE },
	{ "t_irgrid_znf", FTZebra, OrgIRGrid, 24, FALSE },
};
	
int NUM_PLATFORMS = (sizeof(TestPlatforms)/sizeof(TestPlatforms[0]));
int NUM_TESTFIELDS = (sizeof(TestFields)/sizeof(TestFields[0]));

float test_data[20000];



static int
msg_handler (msg)
struct message *msg;
{
	msg_ELog (EF_DEBUG, "Message received");
	if ((msg->m_proto == MT_MESSAGE) && (msg->m_len > 0))
	{
		struct mh_template *tm;
		tm = (struct mh_template *) msg->m_data;
		if (tm->mh_type == MH_SHUTDOWN)
		{
			msg_ELog (EF_PROBLEM, "message handler shutting down");
			exit (1);
		}
	}
	return (0);
}


static void
InitializePlatforms()
{
	int i;

	for (i = 0; i < NUM_PLATFORMS; ++i)
	{
		TestPlatforms[i].platid = NoPlatform;
	}
}

	

static void
RegisterModules ()
/*
 * Loop through every module in the arrays of test routines and
 * add each test to our symbol table.
 */
{
	TestRoutine *tr;
	SValue v;
	int i;
	int ntest = 0;

	TestNames = zl_c_stbl ("Tests");
	for (i = 0; TestModules[i]; ++i)
	{
		tr = TestModules[i];
		while (tr->tr_name[0])
		{
			v.us_v_ptr = (char *) tr;	      
			usy_s_symbol(TestNames, tr->tr_name, SYMT_POINTER, &v);
			++tr;
			++ntest;
		}
	}
	msg_ELog (EF_DEBUG, "Registered %d modules and %d tests.", i, ntest);
}



static void
Initialize (standalone)
int standalone;
{
	if (standalone)
	{
		msg_connect (NULL, "");
	}
	else if (!msg_connect (msg_handler, "Will-Tell"))
	{
		msg_ELog (EF_EMERGENCY, "Cannot connect to message manager!");
		exit(1);
	}
/*
 * Try to limit our output to what's important
 */
	if (Debug)
		msg_ELPrintMask (EF_ALL);
	else
		msg_ELPrintMask (EF_INFO | EF_EMERGENCY | EF_PROBLEM);
	if (Verbose)
	{
		msg_ELPrintMask (EF_ALL | EF_DEVELOP);
		DumpDataChunks = 1;
	}
	if (standalone)
	{
		ds_Standalone ();
	}
	else if (!ds_Initialize())
	{
		msg_ELog (EF_EMERGENCY, "Cannot connect nor initialize DS!");
		exit(1);
	}
	InitializePlatforms();
}



#define REPEAT(expr,c) \
{ \
	  int i; \
	  for (i = 0; i < c; ++i) \
  { (expr); TP_Count (id, 1); \
	    if (i+1<(c)) TP_ReportElapsed (id, tr->tr_name); } \
}


static int
RunTest (tr, when, count)
TestRoutine *tr;
ZebTime *when;
int count;
/*
 * Run the given test and return the number of errors it produces.
 */
{
	int errors = 0;
	TestParms tp = (TestParms) (tr->tr_flags & 0x3);
	int id;

	Errors = 0;	/* use global counter to catch "internal" errors */
	if (count < 1)
		count = DefaultCount;
	msg_ELog (EF_INFO, "Running test '%s' %d time%s ...", 
		  tr->tr_name, count, (count == 1) ? "" : "s");
	id = TP_Push (tr->tr_name);
	switch (tp)
	{
	   case TR_PLAT:
		REPEAT((errors += (*tr->tr_test)(when, tr->tr_arg,
						 tr->tr_name)),count);
		break;
	   case TR_BEGIN:
		REPEAT((errors += (*tr->tr_test)(when, tr->tr_name)),count);
		break;
	   default:
		msg_ELog (EF_PROBLEM, "unknown parameter list for test '%s'",
			  tr->tr_name);
		++errors;
		break;
	}
	TP_Pop ();
	errors += Errors;
	Errors = 0;
	if (errors) 
		msg_ELog (EF_PROBLEM, "***** %s failed: %d errors *****", 
			  tr->tr_name, errors);
	else
		msg_ELog (EF_INFO, "%s passed.", tr->tr_name);
	/*
	 * Clear any messages generated by the test.
	 */
	while (msg_poll(0) == 0);
	return (errors);
}



static int
ShowTest (name, type, v, arg)
char *name;
int type;
SValue *v;
int arg;
{
	TestRoutine *tr = (TestRoutine *) v->us_v_ptr;

	if (arg)
		printf ("%-15s %s\n", tr->tr_name, tr->tr_desc);
	else
		printf ("%-15s\n", tr->tr_name);
	return (TRUE);
}



static int
TraverseTest (char *name, int type, SValue *v, long arg)
{
	TestRoutine *tr = (TestRoutine *) v->us_v_ptr;
	int *errors = (int *) arg;

	*errors += RunTest (tr, &Begin, 0);
	return (TRUE);
}



static int
ParseTest (name)
char *name;
/*
 * Parse this test name for parameters or repetitions, then run it.
 */
{
	SValue v;
	int type;
	TestRoutine *tr;
	int count = DefaultCount;
	char buf[256];
char *c;
	int errors =0;

	strcpy (buf, name);
	if ((c = (char *)strchr (buf, '[')) != NULL)
	{
		*c++ = '\0';
		count = atoi(c);
		if (count < 1)
			count = 1;
	}
	if (! usy_g_symbol (TestNames, buf, &type, &v))
	{
		msg_ELog (EF_PROBLEM, "test '%s' not found", buf);
		++errors;
	}
	else
	{
		tr = (TestRoutine *) v.us_v_ptr;
		errors += RunTest (tr, &Begin, count);
	}
	return (errors);
}



int
main (argc, argv)
int argc;
char *argv[];
{
	ZebTime when;
	int i;
	int errors;
	int standalone = 0;

	/*
	 * If we're not done in an hour, something is seriously wrong
	 */
	signal (SIGALRM, OutOfTime);
	alarm (MAX_TEST_TIME);

	if (NoBuffer)
	{
		setvbuf (stdout, NULL, _IONBF, 0);
		setvbuf (stderr, NULL, _IONBF, 0);
	}
	usy_init();
	RegisterModules ();
	for (i = 1; i < argc; ++i)
	{
		if (!strncmp(argv[i],"-debug",strlen(argv[i])))
			Debug = 1;
		else if (!strncmp(argv[i],"-all",strlen(argv[i])))
			AllTests = 1;
		else if (!strncmp(argv[i],"-standalone",strlen(argv[i])))
			standalone = 1;
		else if (!strncmp(argv[i],"-verbose",strlen(argv[i])))
			Verbose = 1;
		else if (!strncmp(argv[i],"-profile",strlen(argv[i])))
			TP_Profile (TRUE);
		else if (!strncmp(argv[i],"-x",strlen(argv[i])))
			EF_STATS = EF_INFO;
		else if (!strncmp(argv[i],"-names",strlen(argv[i])))
		{
			usy_traverse (TestNames, ShowTest, 0, TRUE);
			exit (0);
		}
		else if (!strncmp(argv[i],"-list",strlen(argv[i])))
		{
			usy_traverse (TestNames, ShowTest, 1, TRUE);
			exit (0);
		}
		else if (!strncmp(argv[i],"-count",strlen(argv[i])))
		{
			if ((DefaultCount = (argv[++i] ? atoi(argv[i]) : 1))<1)
				DefaultCount = 1;
		}
		else if (!strncmp(argv[i],"-help",strlen(argv[i])))
		{
			printf ("usage: %s [-debug] [-help] [-all] %s\n",
				argv[0],"[-standalone] [-verbose] [test ...]");
			printf ("  -a   run all tests\n");
			printf ("  -v   voluminous output\n");
			printf ("  -d   debug messages\n");
			printf ("  -p   enable time profiling (-profile)\n");
			printf ("  -c   default repetitions for each test\n");
			printf ("  -x   print debug stats as info\n");
			printf ("  -s   no daemon, no session, no nothin\n");
			printf ("  -n   print names of tests only\n");
			printf ("  -l   list tests with descriptions\n");
			printf ("With no args, prints a list of the tests\n");
			printf ("Test names can take the form test[n]\n");
			printf ("to indicate 'n' repeats of the test\n");
			exit (0);
		}
	}
	if (argc == 1)
	{
		/*
		 * Dump a list of all tests and their characteristics.
		 */
		usy_traverse (TestNames, ShowTest, 1, TRUE);
		exit (0);
	}

	putenv ("DS_DATA_DIR=/net/shared/zebra/project/test/data");
	Initialize (standalone);
	errors = 0;
	TC_ZtAssemble (&when, 93, 1, 1, 0, 0, 0, 0);
	Begin = when;
	for (i = 0; i < sizeof(test_data)/sizeof(test_data[0]); ++i)
		test_data[i] = i;
	/*
	 * Clear messages received after connecting, such as event mask.
	 */
	while (msg_poll(1) == 0);
	if (AllTests)
	{
		usy_traverse (TestNames, TraverseTest, (long)&errors, TRUE);
	}
	else
	{
		/*
		 * For each argument on the command line, look up the test
		 * and run it.
		 */
		i = 1;
		while (i < argc)
		{
			if (argv[i][0] != '-')
				errors += ParseTest (argv[i]);
			else if (argv[i][1] == 'c')
				++i;
			++i;
		}
	}
	/*
	 * When all is said and done, report internal statistics, then
	 * try to release all held memory and exit gracefully.
	 */
	T_MetDataStats();
	ds_ForceClosure();
	TX_Closure();
	if (TestNames)
		zl_z_stbl (TestNames);
	while (msg_poll(0) == 0);
	msg_ELog (EF_INFO, "%d errors.", errors);
	msg_disconnect ();
	return (errors);
}



static void
OutOfTime (int signal)
{
    msg_ELog (EF_PROBLEM, "Tests aborted after taking more than %d seconds!",
	      MAX_TEST_TIME);
    exit (1);
}

    
