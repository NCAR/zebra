/*
 * ingest.h --- Public protoypes and macros for ingest modules
 *
 * $Id: ingest.h,v 1.1 1992-06-05 23:02:57 granger Exp $
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

void	IngestLog FP((int, va_dcl));
void	IngestParseOptions FP((int *argc, char *argv[], void (*usage)()));
void 	IngestInitialize FP((char *module_name));
void	IngestUsage();

/* -------------------------------------------------------------------- */

/*
 * An exported flag so that ingest modules can test their debug state
 */
extern int IngestLogFlags;

# endif
