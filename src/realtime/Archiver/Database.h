/* $Id: Database.h,v 1.1 1997-05-13 10:46:24 granger Exp $
 *
 * Simple database for Archiver dumped files.
 */

int db_Open (const char *name);
void db_Close ();
int db_Fetch (DataFileInfo *in, DataFileInfo *out, ZebraTime *zt);
int db_Insert (DataFileInfo *dfi, ZebraTime *zt);

