/*
 * ingest.h --- Public protoypes and macros for ingest modules
 *
 * $Id: ingest.h,v 1.3 1992-07-22 14:54:47 granger Exp $
 */

# ifndef _ingest_h_
# define _ingest_h_

# include <stdio.h>
# include <varargs.h>
# include <ctype.h>
# include <ui_error.h>
# include "defs.h"
# include "message.h"
# include "timer.h"
# include "DataStore.h"
# include "DataChunk.h"
# include "ds_fields.h"

# ifndef streq
# define streq(a,b) (strcmp(a,b) == 0)
# endif

/* ---------------------------------------------------------------------
 * Ingest Public Prototypes:						*/

extern void	IngestLog ();
extern void	IngestParseOptions FP((int *argc, char *argv[], 
					void (*usage)()));
extern void 	IngestInitialize FP((char *module_name));
extern void	IngestUsage();

extern bool _Ingest_ds_Store FP((DataChunk *dc, bool newfile, 
				dsDetail *details, int ndetail));
extern PlatformId _Ingest_ds_LookupPlatform FP((char *name));

extern void RemoveOptions FP((int *argc, char *argv[], int i, int n));


/*
 * During testing and DryRun mode, the ds functions will be 
 * omitted by the Ingest cover functions _Ingest_ds*
 */
#define ds_Store(a,b,c,d) _Ingest_ds_Store(a,b,c,d)
#define ds_LookupPlatform(a) _Ingest_ds_LookupPlatform(a)
#define ds_DeleteData(a,b) _Ingest_ds_DeleteData(a,b)

/* -------------------------------------------------------------------- */


/*
 * Exported flags so that ingest modules can test their debug state
 */
extern int IngestLogFlags;
extern short NoDataStore;
extern short NoMessageHandler;
extern short NoEventLogger;
extern short DryRun;
extern short DumpDataChunks;

/* -----------------------------------------------------------------
 * These macros allow ingest modules to set their default debug
 * state within the program, such as during development
 */
#define SetNoMessageHandler() {\
		NoDataStore = 1; \
		NoMessageHandler = 1; \
		NoEventLogger = 1; \
		}

#define SetNoEventLogger() {\
		NoEventLogger = 1; \
		}

#define SetNoDataStore() {\
		NoDataStore = 1;  \
		}

#define SetDryRun() {\
		DryRun = 1; \
		SetNoMessageHandler(); \
		}
		

# endif
