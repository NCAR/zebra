/*
 * Simple dbm interface for storing datafile structures keyed by
 * their platform namd and filename pair.
 */

#include <ndbm.h>
#include <stdio.h>
#include <fcntl.h>


#include <defs.h>
#include <message.h>
#include <DataStore.h>


static DBM *File = NULL;	/* For now Archiver only opens one database */


typedef struct _Entry
{
	DataFileInfo dfi;
	ZebraTime dump;		/* time this file was dumped */
} 
Entry;



int
db_Open (const char *name)
{
	if (File)
		dbm_close (File);
	File = dbm_open ((char *)name, O_RDWR /* | O_CREAT */, 0664);
	return (File ? 0 : -1);
}



void
db_Close ()
{
	if (File)
		dbm_close (File);
	File = NULL;
}



static const char *
db_Key (const DataFileInfo *dfi, datum *key)
{
	static char buf[512];

	sprintf (buf, "%s/%s", ds_PlatformName (dfi->dfi_Plat), dfi->dfi_Name);
	if (key)
	{
		key->dptr = buf;
		key->dsize = strlen (buf) + 1;
	}
	return (buf);
}



int
db_Insert (DataFileInfo *dfi, ZebraTime *zt)
{
	Entry e;
	datum key, value;

	e.dfi = *dfi;
	e.dump = *zt;
	db_Key (dfi, &key);
	value.dptr = (void *)&e;
	value.dsize = sizeof(Entry);
	return (dbm_store (File, key, value, DBM_REPLACE));
}



int
db_Fetch (const DataFileInfo *in, DataFileInfo *dfi, ZebraTime *zt)
{
	datum key;
	datum value;
	int found = 0;

	db_Key (in, &key);
	value = dbm_fetch (File, key);
	if (value.dptr)
	{
		found = 1;
		if (dfi)
			*dfi = ((Entry *)(value.dptr))->dfi;
		if (zt)
			*zt = ((Entry *)(value.dptr))->dump;
	}
	return (found ? 0 : -1);
}

