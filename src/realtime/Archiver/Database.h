/* $Id: Database.h,v 1.5 1999-11-24 00:03:08 granger Exp $
 *
 * Simple database for Archiver dumped files.
 */

#ifndef _zebra_Database_h_
#define _zebra_Database_h_

#include <DataStore.h>

/*
 * A default name for the file databases.
 */
#define DUMPED_FILES "DumpedFiles"

#if __cplusplus
extern "C" {
#endif

/*
 * Note that we are opening ourselves up for possible problems here: When
 * Database.c is compiled, and any C applications which link with it, it is
 * compiled with the "C" version of DataFileCore. C++ programs will be
 * compiled to link with a C++ version (I think? Unless the extern "C"
 * somehow fixes that...).  Anyway, as noted in DataFiles.h, DataFileCore
 * must remain byte-compatible as both a C and C++ struct.
 */

int db_Open (const char *dbname);
int db_Read (const char *dbname);
void db_Close ();
int db_Fetch (const char* fname, DataFileCore *dfc);
int db_Insert (const char* fname, const DataFileCore *dfc);
const char *db_First (DataFileCore *dfc);
const char *db_Next (DataFileCore *dfc);
int db_Remove (const char* fname);

#if __cplusplus
}
#endif

#endif /* _zebra_Database_h_ */
