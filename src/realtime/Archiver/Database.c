/*
 * Simple dbm interface for storing DataFileCore structures keyed by
 * their full path name.
 */

#include <ndbm.h>
#include <stdio.h>
#include <fcntl.h>


#include <defs.h>
#include <message.h>
#include <DataStore.h>


static DBM *File = NULL;	/* For now Archiver only opens one database */



int
db_Open (const char *name)
{
	if (File)
		dbm_close (File);
	File = dbm_open ((char *)name, O_RDWR | O_CREAT, 0664);
	return (File ? 0 : -1);
}



int
db_Read (const char *name)
/*
 * Read-only open of a database.
 */
{
	if (File)
		dbm_close (File);
	File = dbm_open ((char *)name, O_RDONLY, 0);
	return (File ? 0 : -1);
}



void
db_Close ()
{
	if (File)
		dbm_close (File);
	File = NULL;
}



static void
db_Key (const char* fname, datum *key)
{
    key->dptr = (void*)fname;
    key->dsize = strlen (fname) + 1;
}



int
db_Insert (const char *fname, const DataFileCore *dfc)
{
	datum key, value;

	db_Key (fname, &key);

	value.dptr = (void *)dfc;
	value.dsize = sizeof(DataFileCore);
	return (dbm_store (File, key, value, DBM_REPLACE));
}




static int
db_Value (datum *key, DataFileCore *dfc)
{
	datum value;
	int found = 0;

	value = dbm_fetch (File, *key);
	if (value.dptr)
	{
		found = 1;
		if (dfc)
			*dfc = *(DataFileCore*)(value.dptr);
	}
	return (found);
}




int
db_Fetch (const char* fname, DataFileCore *dfc)
{
	datum key;

	db_Key (fname, &key);
	return (db_Value (&key, dfc));
}



const char *
db_First (DataFileCore *dfc)
{
	datum key;

	key = dbm_firstkey (File);
	if (key.dptr == 0 || ! db_Value (&key, dfc))
		return (NULL);
	return ((const char *)key.dptr);
}



const char *
db_Next (DataFileCore *dfc)
{
	datum key;

	key = dbm_nextkey (File);
	if (key.dptr == 0 || ! db_Value (&key, dfc))
		return (NULL);
	return ((const char *)key.dptr);
}



int
db_Remove (const char *fname)
{
    datum key;

    db_Key (fname, &key);
    return (dbm_delete (File, key));
}
