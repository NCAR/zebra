/* $Id: Database.h,v 1.2 1997-05-13 23:04:37 granger Exp $
 *
 * Simple database for Archiver dumped files.
 */

int db_Open (const char *name);
int db_Read (const char *name);
void db_Close ();
int db_Fetch (DataFileInfo *in, DataFileInfo *out, ZebraTime *zt);
int db_Insert (DataFileInfo *dfi, ZebraTime *zt);
const char *db_First (DataFileInfo *dfi, ZebraTime *zt);
const char *db_Next (DataFileInfo *dfi, ZebraTime *zt);

