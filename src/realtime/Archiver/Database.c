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

#include "Database.h"

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





typedef struct s_OldDataFileInfo
{
	char    dfi_Name[80];		/* Name of this file            */
        ZebTime dfi_Begin;              /* Begin time                   */
        ZebTime dfi_End;                /* End time                     */
        int     dfi_NSample;            /* How many samples             */
        int     dfi_Plat;               /* It's platform                */
        char    dfi_Archived;           /* Has been archived?           */
        int     dfi_Next;               /* Next file in chain           */
} OldDataFileInfo;


typedef struct _OldEntry
{
	OldDataFileInfo dfi;
	ZebraTime dump;		/* time this file was dumped */
} 
OldEntry;


static int
db_Value (datum *key, DataFileCore *dfc)
{
	datum value;
	int found = 0;

	value = dbm_fetch (File, *key);
  	if (! value.dptr)
	{
		/* Nothing to do here */
	}
  	else if (value.dsize == sizeof(DataFileCore))
	{
  		found = 1;
 		if (dfc)
		{
			// *dfc = *(DataFileCore*)(value.dptr);
			memcpy ((char *)dfc, (char *)value.dptr, value.dsize);
		}
  	}
	else if (value.dsize == sizeof(OldEntry))
	{
		/* Try a simple conversion from old DataFileInfo struct */
		if (dfc)
		{
			OldEntry here;
			OldDataFileInfo *dfi = &here.dfi;
			memcpy ((char *)&here, (char *)value.dptr,
				sizeof (here));
			strncpy (dfc->dfc_name, dfi->dfi_Name,
				 sizeof(dfc->dfc_name) - 1);
			dfc->dfc_name[sizeof(dfc->dfc_name) - 1] = '\0';
			dfc->dfc_begin = dfi->dfi_Begin;
			dfc->dfc_end = dfi->dfi_End;
			dfc->dfc_rev = TC_ZtToSys (&here.dump);
			dfc->dfc_inode = 0;
			dfc->dfc_ftype = FTUnknown;
			dfc->dfc_nsample = dfi->dfi_NSample;
		}
		found = 1;
	}
	else
	{
		fprintf (stderr, "database error: %s: key %s", 
			 "structure size unknown", key->dptr);
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
