
/*
 * ingest.c --- A common ingest interface and support routines for 
 *		Zeb ingest modules
 */

#include "ingest.h"

#ifndef lint
MAKE_RCSID("$Id")
#endif


/*
 * Private prototypes:
 */
static int	incoming FP((struct message *));


/* --------------------------------------------------------------------
 * Global variables :: IngestLogFlags declared external in ingest.h  */

int IngestLogFlags = 0;		/* Specifies what types of messages are
				 * written to the terminal */
char *IngestName;		/* Message name of ingest module */

/* ------------------------------------------------------------------*/



void
IngestLog(flags, va_alist)
int flags;
va_dcl
/*
 * Send messages to the event logger and to stdout, according
 * to the global IngestLogFlags variable
 */
{
	va_list args;
	struct msg_elog *el;
	static char cbuf[1024];
	char *fmt;

	va_start(args);
	fmt = va_arg(args, char *);

/*
 * First check our local debug flag
 */
	if ((flags & EF_EMERGENCY) || (IngestLogFlags & flags))
	{
		printf("%s: ",IngestName);
		vprintf(fmt, args);
		printf("\n");
	}

/*
 * Now create the message to the event logger
 */
	el = (struct msg_elog *) cbuf;
	vsprintf(el->el_text, fmt, args);
	va_end(args);

/*
 * Send the message
 */
	el->el_flag = flags;
	msg_send("Event logger", MT_ELOG, 0, el,
		sizeof(*el) + strlen(el->el_text));
}


static int
incoming (msg)
struct message *msg;
/*
 * Deal with incoming messages.
 */
{
	switch (msg->m_proto)
	{
	   case MT_TIMER:
	   	tl_DispatchEvent ((struct tm_time *) msg->m_data);
		break;
	}
	return (0);
}


void
IngestUsage()
{
	printf ("General ingest options:\n");
	printf ("   -log all|p|d|i	Set the log messages to write out\n");
	printf ("	all: 	all\n");
	printf ("	  p: 	problems\n");
	printf ("	  d: 	debugging\n");
	printf ("	  i: 	informational\n");
	printf ("   -help		Show this information\n");
}


void
IngestParseOptions(argc, argv, usage)
	int *argc;
	char *argv[];
	void (*usage)(/* char *prog_name */);
{
	int i,j;
	int get_msgs;
	char *arg;

	i = 1;
	get_msgs = 0;
	while (i < *argc)
	{
		if (get_msgs)
		{
		   arg = argv[i];
		   if (streq(arg,"all"))
		      IngestLogFlags = 0xff;
		   else
		   {
      		      while ((*arg))
      		      {
         		 switch(*arg)
          		 {
    			    case 'd':
       			       IngestLogFlags |= EF_DEBUG;
			       break;
			    case 'p':
			       IngestLogFlags |= EF_PROBLEM;
			       break;
			    case 'i':
			       IngestLogFlags |= EF_INFO;
			       break;
			    default:
			       fprintf(stderr,"Invalid log flag: %c\n",*arg);
         		 }
			 ++arg;
      		      }
		   }
		   get_msgs = 0;
		}
		else if (streq(argv[i],"-help"))
		{
		   if (usage)
		   	usage(argv[0]);
		   exit(0);
		}
		else if (streq(argv[i],"-log"))
		{
		   get_msgs = 1;
		}
		else
		{
		   ++i;
		   continue;
		}

		/* Remove any options that are found */
		--(*argc);
		for (j = i; j < *argc; ++j) 
		   argv[j] = argv[j+1];
	}
}


void
IngestInitialize(name)
	char *name;		/* Message name of this ingest module */
{

	IngestName = name;

	usy_init ();
	msg_connect (incoming, IngestName);
	if (! ds_Initialize ())
	{
		IngestLog(EF_EMERGENCY,"Error: ds_Initialize() failed.");
		exit(1);
	}
	F_Init();			/* Init field ID table */
}


void
ListAvailableFields()
{
/*
 * Print a list of valid field names, as well as the maximum number of
 * fields which class_ingest can accept on the command line
 */
/*
 * For now we have to cheat by knowing there cannot be more than
 * 128 fields.  It would be nice to have function in the Fields package
 * which returns the number of fields (i.e. the maximum field id)
 */
/*
 * This should probably be changed to just list the fields available 
 * in a named sounding file...  how to do that?
 */
	FieldId field;
	char *field_name;

	for (field = 0; field<128; ++field)
	{
		if ((field_name = F_GetName(field)))
			printf("%-10s%-50s%-10s\n",
				field_name,
				F_GetDesc(field),
				F_GetUnits(field));
	}
}

