/* $Id: Database.h,v 1.4 1999-03-01 02:04:53 burghart Exp $
 *
 * Simple database for Archiver dumped files.
 */

int db_Open (const char *dbname);
int db_Read (const char *dbname);
void db_Close ();
int db_Fetch (const char* fname, DataFileCore *dfc);
int db_Insert (const char* fname, const DataFileCore *dfc);
const char *db_First (DataFileCore *dfc);
const char *db_Next (DataFileCore *dfc);
int db_Remove (const char* fname);
