/* $Id: Database.h,v 1.3 1997-06-20 21:02:14 granger Exp $
 *
 * Simple database for Archiver dumped files.
 */

int db_Open (const char *name);
int db_Read (const char *name);
void db_Close ();
int db_Fetch (const char *plat, DataFileInfo *in, 
	      DataFileInfo *out, ZebraTime *zt);
int db_Insert (const char *plat, DataFileInfo *dfi, ZebraTime *zt);
const char *db_First (DataFileInfo *dfi, ZebraTime *zt);
const char *db_Next (DataFileInfo *dfi, ZebraTime *zt);

