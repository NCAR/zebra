/* 
 * Inget data from a DAP format file.
 */
# include "dap_file.h"
# include <errno.h>
# include <time.h>
# include <sys/time.h>
# include <defs.h>
# include <message.h>
# include <DataStore.h>
# include <config.h>
# include "dap_cmds.h"


# define BLOCK_SIZE	65536
# define NAME_LEN	DNAMLEN
# define END_DAP	-1
# define STRLEN		20
# define MAXFIELDS	100

/*
 * Globals.
 */
static char	*DAPFields[MAXFIELDS];
static char	*ZebFields[MAXFIELDS];
static int	NumFields = 0;
static int	Argc;
static char	**Argv;


/*
 * Routines.
 */
static int	next_dap_rec FP ((int, int, struct Dapdata *));
static int	Die FP ((void));
static float	dap_var_search FP ((struct Dapdata *, struct Daphead1 *,
			char *));
static void	DoFields FP ((FieldId *));
static void	Go FP ((void));
static void	NewField FP ((struct ui_command *));
static int	Dispatcher FP ((int, struct ui_command *));



main( argc, argv )
  int argc;
  char **argv;
{
	char	loadfile[200];
	SValue	v;
/*
 * Check arguments.
 */
	if (argc < 4)
	{
		printf("Usage: dap_ingest header datafile platform [fields]\n");
		exit (1);
	}
/*
 * Save arguements.
 */
	Argc = argc;
	Argv = argv;
/*
 * Hook in.
 */
	msg_connect (Die, "dap_ingest");
	fixdir ("DAPLOADFILE", GetLibDir(), "dap_ingest.lf", loadfile);
	if (argc > 4)
	{
		ui_init (loadfile, FALSE, TRUE);
		v.us_v_ptr = argv[4];
		usy_s_symbol (usy_g_stbl ("ui$variable_table"), "commandfile",
			SYMT_STRING, &v);
	}
	else
		ui_init (loadfile, TRUE, FALSE);
	ds_Initialize ();
/*
 * Time to go into UI mode.
 */
	ui_get_command ("initial", "DAP>", Dispatcher, 0);
	Die ();
}


static int
Dispatcher (junk, cmds)
int     junk;
struct ui_command       *cmds;
/*
 * The command dispatcher.
 */
{
        switch (UKEY (*cmds))
        {
        /*
         * Time to actually do things.
         */
                case DAP_GO:
                        Go ();
                        break;
        /*
         * Define a dap field.
         */
                case DAP_FIELD:
                        NewField (cmds + 1);
                        break;
        /*
         * Complain.
         */
                default:
                        msg_ELog (EF_PROBLEM, "Unknown kw %d", UKEY (*cmds));
                        break;
        }
        return (TRUE);
}


static void
NewField (cmds)
struct ui_command	*cmds;
/*
 * Store a DAP and a corresponding Zeb field name.
 */
{
/*
 * Make sure we don't have too many.
 */
	if (NumFields >= MAXFIELDS)
	{
		msg_ELog (EF_PROBLEM, "Too many fields.");
		return;
	}	
/*
 * Remember the stuff.
 */
	DAPFields[NumFields] = usy_string (UPTR (*cmds));
	ZebFields[NumFields] = usy_string (UPTR (cmds[1]));
	msg_ELog (EF_INFO, "%s -> %s", DAPFields[NumFields], 
		ZebFields[NumFields]);
	NumFields++;
}


static void
Go ()
{
	int	n, i;
	struct Daphead1	*dap_header;
	struct Dapdata	*dap_data;
	int	header_file, data_file;
	PlatformId	pid;
	FieldId		fids[MAXFIELDS];
	float		data[MAXFIELDS];
	ZebTime	zt;
	Location	origin;
	DataChunk	*dc;
/*
 * Open the header file.
 */
	if ((header_file = open (Argv[1], 0)) < 0) 
	{
		msg_ELog (EF_EMERGENCY, "Unable to open %s.", Argv[1] );
		exit (1);
	}
/*
 * Read the header.
 */
	if ((dap_header = (struct Daphead1 *) malloc (BLOCK_SIZE)) == NULL)
	{
		msg_ELog (EF_PROBLEM, "No memory for header.");
		exit (1);
	}
	if ((n = read (header_file, (char *) dap_header, BLOCK_SIZE)) <= 0) 
	{
		msg_ELog (EF_EMERGENCY, "Unable to read DAP header (%d)",
			errno);
		exit (1);
	}
	close (header_file);
/*
 * Open the data file.
 */
	if ((data_file = open (Argv[2], 0)) < 0) 
	{
		msg_ELog (EF_EMERGENCY, "Unable to open %s.", Argv[2]);
		exit (1);
	}
	if ((dap_data = (struct Dapdata *) malloc (BLOCK_SIZE)) == NULL)
	{
		msg_ELog (EF_PROBLEM, "No memory for data.");
		exit (1);
	}
/*
 * Check the platform.
 */
	if ((pid = ds_LookupPlatform (Argv[3])) == BadPlatform)
	{
		msg_ELog (EF_EMERGENCY, "Bad platform %s.", Argv[3]);
		exit (1);
	}
/*
 * Get field id's.
 */
	DoFields (fids);
/*
 * Plow.
 */
	for(;;) 
	{
	/*
	 * Get a record.
	 */
		if (next_dap_rec (data_file, dap_header->nwords * sizeof (int),
		    dap_data) == END_DAP)	
			break;
	/*
	 * Create a data chunk.
	 */
		dc = dc_CreateDC (DCC_Scalar);
		dc->dc_Platform = pid;
		dc_SetScalarFields (dc, NumFields, fids);
	/*
	 * Extract a time.
	 */	
		TC_ZtAssemble (&zt, dap_header->idated[0], 
			dap_header->idated[1], dap_header->idated[2], 
			dap_data->ihr, dap_data->imin,
			dap_data->isec, 0);
	/*
	 * Extract a location.
	 */ 
		origin.l_lat = dap_var_search (dap_data, dap_header, "ALAT"); 
		origin.l_lon = dap_var_search (dap_data, dap_header, "ALON"); 
		origin.l_alt = dap_var_search (dap_data, dap_header, "PALT"); 
	/*
	 * Extract the fields of interest.
	 */
		for (i = 0; i < NumFields; i++)
		{
			data[i] = dap_var_search (dap_data, dap_header,
				DAPFields[i]);	
			dc_AddScalar (dc, &zt, 0, fids[i], data + i);
		}
		dc_SetLoc (dc, 0, &origin);
	/*
	 * Store the data.
	 */
		ds_StoreBlocks (dc, FALSE, NULL, 0);
		dc_DestroyDC (dc);
	}
/*
 * Finish up.
 */
	free (dap_header);
	free (dap_data);
	close (data_file);
}



static void
DoFields (fids)
FieldId	*fids;
/*
 * Get id's for Zeb fields.
 */
{
	int	i;
/*
 * Get id's for the fields of interest.
 */
	for (i = 0; i < NumFields; i++)
		fids[i] = F_Lookup (ZebFields[i]);
}


static int
next_dap_rec (fptr, rec_len, record)
int	fptr;
int	rec_len;
struct Dapdata *record;
/*
 * Read the next record.
 */ 
{
	int	i;
	static int	rec_count=0;

	if ((i = read (fptr, record, rec_len)) < rec_len ) 
	{
		msg_ELog (EF_INFO, "End of DAP data after record %d.", 
			rec_count);
		return (END_DAP);
    	}
	rec_count++;
	return (1);
}

static float
dap_var_search (record, header, fld_name)
struct Dapdata	*record;
struct Daphead1	*header;
char	*fld_name;
/*
 * Return the value of 'fld_name' in DAP record 'record'.
 */
{
	int	i, j;
	char	a[NAME_LEN];
/*
 * Check.
 */
	j = strlen (fld_name);
	if (j > NAME_LEN)
	{
		msg_ELog (EF_PROBLEM, "Bad field name %s", fld_name);
		return (NULL);
	}
/*
 * Pad the field name.
 */
	for (i= 0; i < NAME_LEN; i++)
		a[i] = ' ';
	for (i = 0; i < j; i++)
		a[i] = *fld_name++;
/*
 * Get the value.
 */
	for (i = 0; i < header->nvar; i++)
        	if (strncmp (a, header->names[i], NAME_LEN) == 0)
              		return (record->values[i]);
	return (NULL);

}

 
static int
Die ()
/*
 * Die Gracefully.
 */
{
	msg_ELog (EF_INFO, "Dying.");
	ui_finish ();
	exit (0);
}
