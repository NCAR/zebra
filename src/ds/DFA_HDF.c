/*
 * Access to HDF files.
 */
/*		Copyright (C) 1987,88,89,90,91,92 by UCAR
 *	University Corporation for Atmospheric Research
 *		   All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced
 * or used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular purpose.
 * UCAR does not indemnify any infringement of copyright, patent, or trademark
 * through use or modification of this software.  UCAR does not provide 
 * maintenance or updates for its software.
 */
/* -----
 * The object is this:
 *
 * Upon opening an HDF file, scan it for a Vgroup of class "CDF", and
 * remember the id of that vgroup.
 * Attach to this Vgroup.
 * The variables in the Vgroup are the sub-Vgroups of class "Var".
 * The global attributes are the "Attr" class Vdatas in the CDF vgroup.
 * The variable attributes are the "Attr" class Vdatas in the "Var" 
 *    sub-vgroups.

   So work this like the netCDF interface:  

   Open the file and get its time from a global attribute.
   Generate two arrays which map FieldId to vgroup id and vice versa. 
   To look up info on a variable, all that's needed is the id of the file
   and the id of the variable's vgroup.

   For reading images: since only a single image in the file, cache the
   image?  Read the data on setup and convert the data to bytes, so that we
   know the scale and offset to give the field.  Keep the scale and offset
   of the field cached so we can recalculate byte values quickly when the
   byte array is no longer referenced in the tag.

   --> Bad idea to cache data itself, because we have no idea how many
   files will be open and have no way to age off unused caches.  Just keep
   scale info around.

   For 2d grids, convert short words or bytes to floats using the scale
   and offset in the file.

 */

# include <math.h>
# include <sys/time.h>
# include <string.h>
# include <ctype.h>

/*
 * The HDF interface typedef's bool to int, which conflicts with UI char
 * typedef.  So we just change the name for the hdf header files since we
 * do not use the HDF bool type in this file.
 */
# define bool hdf_int_bool
# include <hdf.h>
# undef bool

# include <defs.h>
# include <config.h>
# include <message.h>

RCSID ("$Id: DFA_HDF.c,v 3.2 1995-07-07 22:49:51 granger Exp $")

#ifdef HDF_INTERFACE 	/* Conditional compilation of HDF interface */

#ifdef HDF_TEST
#define ds_PlatformDataOrg(id) Org2dGrid
#endif

# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"
# include "dfa.h"

# define DEG_TO_RAD(x)	((x)*0.017453292)
# define RAD_TO_DEG(x)	((x)*57.29577951)
# define DEG_TO_KM(x)	((x)*111.3238367) /* on a great circle */
# define KM_TO_DEG(x)	((x)*0.008982802) /* on a great circle */
/*
 * Radius of the earth, in km
 */
static const double R_EARTH = 6378.;
static const double PI = 3.14159265358979323846;


# define MAX_DIMS	128
# define MAX_FIELDS 	CFG_FIELD_MAX_ID

/*
 * Variable attributes retrieved for a field
 */
#define VATT_LONGNAME	"long_name"	/* description of variable */
#define VATT_UNITS	"units"		/* units of variable	   */
#define VATT_MISSING	"missing_value" /* bad value		   */

/*
 * This is our tag structure.
 */
typedef struct _htag
{
	int             h_fid;		/* HDF file ID value		 */
	int		h_vgid;		/* VGroup ID of our CDF group	 */
	ZebTime         h_time;		/* The time of the file		 */
	PlatformId	h_plat;
	DataOrganization h_org;		/* The purported organization	 */
	Location        h_center;	/* Location of center of an image*/
	float		h_pixel;	/* pixel spacing of images	 */
	int		h_nVar;		/* Number of variables in the file */
	FieldId		*h_Fids;	/* The map to field id		 */
	int32		*h_Vids;	/* The map to vgroup id		 */

} Htag;

/*
 * The class/organization compatibility table.  If the desired class
 * and the given file organization appear together here, we can do it.
 */
static struct CO_Compat
{
	DataOrganization	c_org;
	DataClass		c_class;
} COCTable [] =
{
 	{ OrgImage,		DCC_Image },	/* only for bytes so far */
	{ Org2dGrid,		DCC_RGrid },
};
# define N_COC (sizeof (COCTable)/sizeof (struct CO_Compat))

/*
 * The minimum size of a time list before it's worthwhile to do a binary
 * search.
 */
#define MINTIME 10

#ifdef HDF_TEST
Htag *OpenTag = NULL;
#endif

/*
 * Semi-public routines (DFA methods)
 */
int dh_CloseFile FP ((Htag *tag));
int dh_OpenFile FP ((char *fname, DataFile *dp, int write, Htag **rtag));
int dh_SyncFile FP ((Htag *tag));
int dh_QueryTime FP ((char *file, ZebTime *begin, ZebTime *end, int *nsamp));
DataChunk *dh_Setup FP ((GetList *gp, FieldId *fields, int nfield, DataClass));
int dh_GetData FP ((DataChunk *dc, GetList *gp, dsDetail *, int ndetail));
int dh_DataTimes FP ((int df, ZebTime *, TimeSpec which, int n, ZebTime *));
int dh_GetFields FP ((int dfile, ZebTime *t, int *nfld, FieldId *flist));
int dh_CreateFile FP ((char *fname, DataFile *, DataChunk *dc, char **rtag));
int dh_MakeFileName FP ((char *dir, char *platform, ZebTime *zt, char *dest));

#ifdef notdef
/*
 * The HDF format.
 */
    {
	"HDF",	".hdf",
	dh_QueryTime,			/* Query times			*/
	dh_Setup,			/* setup			*/
	dh_OpenFile,			/* Open				*/
	dh_CloseFile,			/* Close			*/
	dh_SynFile,			/* Synchronize			*/
	___,				/* Inquire platforms		*/
	dh_GetData,			/* Get the data			*/
	___,				/* Get IRGrid locations		*/
	___,				/* Get altitude info		*/
	dh_DataTimes,			/* Get data times		*/
	___,				/* Get forecast times		*/
	dh_MakeFileName,		/* Make file name		*/
	dh_CreateFile,			/* Create a new file		*/
	___,				/* Write to file		*/
	___,				/* Write block to a file	*/
	___,				/* Get observation samples	*/
	dh_GetFields,			/* Get fields			*/
	___,				/* Get Attributes		*/
    },
#endif

/*
 * Private prototypes
 */
static int dh_ReadAttr FP ((int32 fid, int32 vgid, const char *name, 
		    int32 *ftype, int32 *count, int32 *isize, void *values));
static int dh_FindAttr FP ((int32 fid, int32 vgid, const char *, int32 type_in,
			    int32 len_in, int32 *isize_out, void *values));
static int dh_DecipherTime FP ((int32 fid, int32 vgid, ZebTime *zt));
static int dh_LoadFields FP ((Htag *tag));
static int dh_GetFieldVar FP ((Htag *tag, FieldId fid));
static int dh_LoadLocation FP ((Htag *tag));
static int dh_LoadPixel FP ((Htag *tag));
static int dh_OFTimes FP ((Htag *tag));
static int32 dh_VgroupClass FP ((int32 fid, const char *name));
static char *dh_GetStringAtt FP ((int fid, int varid, const char *att_name,
				  char *att_val, int len));
static void dh_FreeFields FP ((Htag *tag));
static int dh_OrgClassCompat FP ((DataOrganization org, DataClass class));
static DC_ElemType dh_ElemType FP ((int32 type));
static void dh_ReadVgroupAtts FP((DataChunk *dc, Htag *, int32 vgid, FieldId));
static void dh_ReadGlobalAtts FP((DataChunk *dc, Htag *tag));
static void dh_ReadFieldAtts FP ((DataChunk *dc, Htag *tag, 
				  FieldId *fids, int nfield));
static void dh_ReadRGrid FP ((DataChunk *dc, Htag *tag, FieldId *, int nfld));
static void dh_FillArray FP ((float *array, int len, double val));
static ScaleInfo *dh_GetFieldScales FP ((Htag *tag, int nfield, FieldId *));
static void dh_SetImageFields FP ((Htag *tag, DataChunk *dc, int nfield,
				   FieldId *fields));
static void dh_ReadImage FP ((DataChunk *dc, Htag *tag, FieldId *fields,
			      int nfield));
static void dh_Origin FP ((Htag *tag, int nx, int ny, Location *origin));



static int
dh_ReadAttr (fid, vgid, name, ftype, count, isize, values)
int32 fid;
int32 vgid;
const char *name;
int32 *ftype;
int32 *count;
int32 *isize;
void *values;
/*
 * Search this vgroup for an attribute given by 'name'.  The attribute will
 * be a vdata of class 'Attr' with a single field 'Values'.  If 'values' is
 * NULL, no values are returned, but isize can be used to allocate space
 * for the values for a subsequent call.
 */
{
	char vsclass[128];
	char vsname[128];
	char fields[128];
	char *fname;
	int32 vgkey;
	int32 intr, sz;
	int32 key, id, tag;
	int nfield, i;
	int ntagrefs;

	if ((vgkey = Vattach (fid, vgid, "r")) == FAIL)
	{
		msg_ELog (EF_PROBLEM, "Vattach(%i) failed.", vgid);
		return (-1);
	}
	ntagrefs = Vntagrefs (vgkey);
	for (i = 0; i < ntagrefs; ++i)
	{
		Vgettagref (vgkey, i, &tag, &id);
		if (tag != DFTAG_VH)	/* looking for vdata */
			continue;
		/*
		 * Attach to this vdata and get its key
		 */
		if ((key = VSattach (fid, id, "r")) == FAIL)
			continue;
		VSgetclass (key, vsclass);
		VSinquire (key, count, &intr, fields, &sz, vsname);
		if (strncmp (vsclass, "Attr", 4) || strcmp (vsname, name))
		{
			VSdetach (key);
			continue;
		}
		/*
		 * Now we know a vdata of class 'Attr' and the right name.
		 * It should have only one field ('Values') from which
		 * we get the value of the attribute.
		 */
		nfield = VFnfields (key);
		if (nfield > 1)
			msg_ELog (EF_PROBLEM, 
			  "Attribute %s has more than one field?!", vsname);
		*ftype = VFfieldtype (key, 0);
		*isize = VFfieldisize (key, 0);
		if (values)
		{
			fname = VFfieldname (key, 0);
			VSsetfields (key, fname);
			VSseek (key, 0);
			VSread (key, values, *count, FULL_INTERLACE);
		}
		VSdetach (key);
		break;
	}
	Vdetach (vgkey);
	return ((i < ntagrefs) ? 0 : -1);
}



static int
dh_FindAttr (fid, vgid, name, type_in, len_in, isize_out, values)
int32 fid;
int32 vgid;
const char *name;
int32 type_in;
int32 len_in;
int32 *isize_out;
void *values;
/*
 * Find an attribute of the given name, type, and length, and put the
 * values in 'values'.  Report a warning if not found.
 */
{
	int32 atype;
	int32 alen;
	int32 isize;

	if (dh_ReadAttr (fid, vgid, name, &atype, &alen, &isize, NULL) < 0 ||
	    atype != type_in || alen != len_in)
	{
		msg_ELog (EF_PROBLEM, "%s missing or not type %d",
			  name, type_in);
		return (FALSE);
	}
	if (values &&
	    dh_ReadAttr (fid, vgid, name, &atype, &alen, &isize, values) < 0)
	{
		msg_ELog (EF_PROBLEM, "could not read %s value", name);
		return (FALSE);
	}
	if (isize_out)
		*isize_out = isize;
	return (TRUE);
}



static int
dh_GetDoubleAtt (fid, vgid, name, value)
int32 fid;
int32 vgid;
const char *name;
double *value;
{
	return (dh_FindAttr (fid, vgid, name, DFNT_FLOAT64, 1, NULL, value));
}



static char *
dh_GetStringAtt (fid, varid, att_name, att_val, len)
int fid;
int varid;
const char *att_name;
char *att_val;
int len;
/*
 * Retrieve named attribute from given varid, making sure value is of
 * type NC_CHAR and not longer than len.  Returns att_val if 
 * if successful, else returns NULL without changing att_val[].
 *
 * If len == 0 or att_val == NULL, we'll allocate enough space here and 
 * return that without touching att_val.  The returned string is valid
 * until the next call.
 */
{
	static char *buf = NULL;
	int32 att_type;
	int32 count, isize;

	if (buf)
	{
		free (buf);
		buf = NULL;
	}
	if (dh_ReadAttr (fid, varid, att_name, &att_type, &count, 
			 &isize, NULL) < 0 || 
	    count != 1 || att_type != DFNT_CHAR)
		return (NULL);
	if (! att_val || len == 0)
	{
		buf = (char *) malloc ( isize + 1 );
		dh_ReadAttr (fid, varid, att_name, &att_type, &count, &isize,
			     buf);
		buf[isize] = '\0';
		return (buf);
	}
	else if (isize+1 < len)
	{
		dh_ReadAttr (fid, varid, att_name, &att_type, &count, &isize,
			     att_val);
		att_val[isize] = '\0';
		return (att_val);
	}
	return (NULL);
}




static DC_ElemType
dh_ElemType (type)
int32 type;
/*
 * Convert the HDF type to the corresponding DC_ElemType.
 */
{
	switch(type)
	{
	   case DFNT_UCHAR8:
		return (DCT_UnsignedChar);
	   case DFNT_CHAR8:
		return (DCT_Char);
	   case DFNT_INT16:
		return (DCT_ShortInt);
	   case DFNT_UINT16:
		return (DCT_UnsignedShort);
	   case DFNT_INT32:
		return (DCT_Integer);
	   case DFNT_UINT32:
		return (DCT_UnsignedInt);
	   case DFNT_INT64:
		return (DCT_LongInt);
	   case DFNT_UINT64:
		return (DCT_UnsignedLong);
	   case DFNT_FLOAT32:
		return (DCT_Float);
	   case DFNT_FLOAT64:
		return (DCT_Double);
	   case DFNT_FLOAT128:
		return (DCT_LongDouble);
	   default:
		return (DCT_Unknown);
	}
}




static int
dh_DecipherTime (fid, vgid, zt)
int32 fid;
int32 vgid;
ZebTime *zt;
/*
 * Grab the time of this file from the attributes "pass_date" (long)
 * and "start_time" (float) of the given vgroup.
 * Read them into a UItime and then convert it to ZebTime.
 */
{
	UItime uidate;
	long pass_date;
	double start_time;
	long microsec;

	if (! dh_FindAttr (fid, vgid, "pass_date", DFNT_INT32, 1, NULL, 
			   &pass_date))
		return (FALSE);
	uidate.ds_yymmdd = pass_date;

	if (! dh_GetDoubleAtt (fid, vgid, "start_time", &start_time))
		return (FALSE);
	microsec = (start_time - (long)start_time) * 1e+6;
	uidate.ds_hhmmss = (long) start_time;
	TC_UIToZt (&uidate, zt);
	zt->zt_MicroSec = microsec;
	return (TRUE);
}




static int32
dh_VgroupClass (fid, name)
int32 fid;
const char *name;
/*
 * Search for a vgroup in the HDF file 'fid' whose class begins with the
 * given 'name'.  (e.g. "CDF" will match "CDF0.0")
 */
{
	int32 vgid, vgkey;
	char vgclass[128]; 

	/*
	 * Loop through the vgroups until one matches the class name
	 */
	vgid = -1;
	while ((vgid = Vgetid (fid, vgid)) != FAIL)
	{
		if ((vgkey = Vattach (fid, vgid, "r")) == FAIL)
		{
			msg_ELog (EF_PROBLEM, "Vattach(%i) failed.", vgid);
			continue;
		}
		Vgetclass (vgkey, vgclass);
		Vdetach (vgkey);
		if (! strncmp (name, vgclass, strlen(name)))
			break;
	} 
	return ((vgid != FAIL) ? vgid : -1);
}




int
dh_QueryTime (file, begin, end, nsamp)
char *file;
ZebTime *begin, *end;
int *nsamp;
/*
 * Query the times on this file.  */
{
	int32 fid;
	int32 vgid;
	ZebTime zt;
	int fail = 0;
/*
 * Try opening the file.
 */
	if ((fid = Hopen (file, DFACC_READ, 0)) < 0)
		return (FALSE);
	Vstart (fid);
/*
 * Find the CDF class vgroup.
 */
	if ((vgid = dh_VgroupClass (fid, "CDF")) < 0)
	{
		msg_ELog (EF_PROBLEM, "could not find CDF class vgroup");
		fail = 1;
	}
/*
 * Look up the time.
 */
	if (! fail && dh_DecipherTime (fid, vgid, &zt))
	{
		*begin = zt;
		*end = zt;
		*nsamp = 1;
	}
/*
 * Clean up and return.
 */
	Vend (fid);
	Hclose (fid);
	return (fail ? FALSE : TRUE);
}



/* ARGSUSED */
int
dh_MakeFileName (dir, platform, zt, dest)
char *dir, *platform;
ZebTime *zt;
char *dest;
/*
 * Generate a file name.
 */
{
	msg_ELog (EF_PROBLEM, "dh_MakeFileName: Can't create HDF files yet!");
	return (FALSE);
}



int
dh_CreateFile (fname, dfile, dc, rtag)
char *fname;
DataFile *dfile;
DataChunk *dc;
char **rtag;
/*
 * Create an HDF file.
 */
{
	msg_ELog (EF_PROBLEM, "dh_CreateFile: Can't create HDF files yet!");
	return (FALSE);
}




int
dh_OpenFile (fname, dp, write, rtag)
char *fname;
DataFile *dp;
int write;
Htag **rtag;
/*
 * Try to open this file.
 */
{
	Htag *tag = ALLOC (Htag);
	int32 fid;
/*
 * Try to open the file.
 */
	*rtag = NULL;
	if ((fid = Hopen (fname, DFACC_READ, 0)) < 0)
	{
		msg_ELog (EF_PROBLEM, "hdf: Hopen(%s) failed.", fname);
		free (tag);
		return (FALSE);
	}
	Vstart (fid);
/*
 * Do some initialization.
 */
	tag->h_fid = fid;
	tag->h_org = ds_PlatformDataOrg (dp->df_platform);
	tag->h_plat = dp->df_platform;
	tag->h_Fids = NULL;
	tag->h_Vids = NULL;
	tag->h_nVar = 0;
	tag->h_pixel = -1;	/* Don't know pixel spacing yet */
	if (! dh_SyncFile (tag))
	{
		msg_ELog (EF_PROBLEM, "hdf: dh_Sync(%s) failed.", fname);
		dh_CloseFile (tag);
		return (FALSE);
	}
	*rtag = tag;
	return (TRUE);
}



int
dh_SyncFile (tag)
Htag *tag;
/* 
 * Sync our internal structures with the changed file on disk.  Essentially
 * we sync the HDF API to the disk, and then re-set our tag with the API.
 */
{
	int ret = TRUE;

	Hsync (tag->h_fid);
/*
 * Find our primary vgroup, class CDF
 */
	if ((tag->h_vgid = dh_VgroupClass (tag->h_fid, "CDF")) < 0)
	{
		msg_ELog (EF_PROBLEM, 
			  "HDF abort: could not find CDF vgroup");
		return (FALSE);
	}
/*
 * Deal with the time and field information.
 */
	if (! dh_OFTimes (tag) || ! dh_LoadFields (tag))
	{
		return (FALSE);
	}
/*
 * Load the static location
 */
	if (! dh_LoadLocation (tag))
	{
		return (FALSE);
	}
/*
 * The rest of the setup is organization-specific.  For both images and 2d 
 * grids we need a cell (pixel) size.
 */
	switch (tag->h_org)
	{
	   case OrgImage:
	   case Org2dGrid:
		ret = dh_LoadPixel (tag);
		break;

	   default:
		msg_ELog (EF_PROBLEM, "Can't deal with org %d", tag->h_org);
		ret = FALSE;
		break;
	}
	return (ret);
}



static void
dh_FreeFields (tag)
Htag *tag;
/*
 * Free info associated with our list of fields 
 */
{
	if (tag->h_Fids)
		free (tag->h_Fids);
	if (tag->h_Vids)
		free (tag->h_Vids);
	tag->h_nVar = 0;
}



int
dh_CloseFile (tag)
Htag *tag;
/*
 * Close this file.
 */
{
	dh_FreeFields (tag);
	Vend (tag->h_fid);
	Hclose (tag->h_fid);
	free (tag);
	return (0);
}




static int
dh_OFTimes (tag)
Htag *tag;
/*
 * Deal with the time info in this file.
 */
{
	ZebTime base;

	if (! dh_DecipherTime (tag->h_fid, tag->h_vgid, &base))
	{
		msg_ELog (EF_PROBLEM, "could not decipher time variables");
		return (FALSE);
	}
	tag->h_time = base;
	return (TRUE);
}




static int
dh_LoadFields (tag)
Htag *tag;
/*
 * Pull in the field information and generate our fieldid<-->vgid map.
 */
{
#	define FBLK 64
	char longname[256];	/* anything bigger we'll just ignore */
	char units[256];
	char vclass[256];
	char vname[256];
	int vgkey, key;
	int32 id, vtag;
	int ntagrefs, i;
/*
 * Free any existing info, for the future case when we might be re-syncing.
 */
	dh_FreeFields (tag);
	tag->h_Fids = (FieldId *) malloc (FBLK * sizeof (FieldId));
	tag->h_Vids = (int32 *) malloc (FBLK * sizeof (int32));
/*
 * Pass through the fields.  Attach to the CDF vgroup and collect all
 * vgroups of class "Var".
 */
	if ((vgkey = Vattach (tag->h_fid, tag->h_vgid, "r")) == FAIL)
	{
		msg_ELog (EF_PROBLEM, "dh_LoadFields: Vattach failed.");
		return (-1);
	}
	ntagrefs = Vntagrefs (vgkey);
	if (ntagrefs <= 0)
	{
		msg_ELog (EF_PROBLEM, "%s: no reference tags in CDF vgroup",
			  "hdf warning");
	}
	for (i = 0; i < ntagrefs; ++i)
	{
		Vgettagref (vgkey, i, &vtag, &id);
		if (vtag != DFTAG_VG)
			continue;
		if ((key = Vattach (tag->h_fid, id, "r")) == FAIL)
			continue;
		Vgetclass (key, vclass);
		Vgetname (key, vname);
		Vdetach (key);
		if (strncmp ("Var", vclass, 3) != 0)
			continue;
		tag->h_Vids[tag->h_nVar] = id;
		strcpy (longname, vname);
		strcpy (units, "unknown");
		(void)dh_GetStringAtt (tag->h_fid, id, VATT_LONGNAME, 
				       longname, 256);
		(void)dh_GetStringAtt (tag->h_fid, id, VATT_UNITS, 
				       units, 256);
		tag->h_Fids[tag->h_nVar] = 
			F_DeclareField (vname, longname, units);
		tag->h_nVar++;
		/*
		 * Now see if we need to increase our allocated memory
		 */
		if ((tag->h_nVar % FBLK) == 0)
		{
			tag->h_Fids = (FieldId *) realloc (tag->h_Fids,
				   (tag->h_nVar + FBLK) * sizeof (FieldId));
			tag->h_Vids = (int32 *) realloc (tag->h_Vids,
				   (tag->h_nVar + FBLK) * sizeof (int32));
		}
	}
	Vdetach (vgkey);
	return (TRUE);
}




static int
dh_GetFieldVar (tag, fid)
Htag *tag;
FieldId fid;
/*
 * Find the HDF vgroup id corresponding to this field id.
 */
{
	int var;

	for (var = 0; var < tag->h_nVar; var++)
		if (tag->h_Fids[var] == fid)
			return (tag->h_Vids[var]);
	return (-1);
}




static int
dh_LoadLocation (tag)
Htag *tag;
/*
 * Load the center_lat and center_lon global attributes into the tag locn
 */
{
	double lat, lon;

	if (dh_GetDoubleAtt (tag->h_fid, tag->h_vgid, "center_lat", &lat) &&
	    dh_GetDoubleAtt (tag->h_fid, tag->h_vgid, "center_lon", &lon))
	{
		tag->h_center.l_lat = lat;
		tag->h_center.l_lon = lon;
		tag->h_center.l_alt = 0.0;
		return (TRUE);
	}
	return (FALSE);
}



static int
dh_LoadPixel (tag)
Htag *tag;
/*
 * Load a pixel size from the global 'history' attribute.
 */
{
	float width = 0.0, height = 0.0;
	char *history;
	char *c;

	history = dh_GetStringAtt (tag->h_fid, tag->h_vgid, "history", 0, 0);
	if (! history)
	{
		msg_ELog (EF_PROBLEM, "hdf: no history attribute: %s",
			  "could not determine pixel size");
		return (FALSE);
	}
	/*
	 * Now try to parse the history string for the pixel_ parameters
	 */
	if (! (c = strstr (history, "pixel_width=")))
	{
		msg_ELog (EF_PROBLEM, "hdf: no pixel_width in 'history'");
		return (FALSE);
	}
	c += strlen ("pixel_width=");
	sscanf (c, "%f", &width);

	if (! (c = strstr (history, "pixel_height=")))
	{
		msg_ELog (EF_PROBLEM, "hdf: no pixel_height in 'history'");
		return (FALSE);
	}
	c += strlen ("pixel_height=");
	sscanf (c, "%f", &height);
	if (width != height)
	{
		msg_ELog (EF_PROBLEM, "pixel_height (%f) != pixel_width (%f)",
			  height, width);
		return (FALSE);
	}
	tag->h_pixel = width;
	return (TRUE);
} 




static int
dh_OrgClassCompat (org, class)
DataOrganization org;
DataClass class;
/*
 * Return TRUE iff these two are compatible.
 */
{
	int i;
/*
 * Go through and see if we find the combination in the table.
 */
	for (i = 0; i < N_COC; i++)
		if (class == COCTable[i].c_class && org == COCTable[i].c_org)
			return (TRUE);
	return (FALSE);
}



DataChunk *
dh_Setup (gp, fields, nfield, class)
GetList *gp;
FieldId *fields;
int nfield;
DataClass class;
/*
 * Get set up for this piece of data access.
 */
{
	Htag *tag;
	DataChunk *dc;
/*
 * Start by opening the first file.  We'll need it soon.
 */
#ifndef HDF_TEST
	if (! dfa_OpenFile (gp->gl_dfindex, FALSE, (void *) &tag))
		return (NULL);
#else
	tag = OpenTag;
#endif
/*
 * Make sure this is a combination we can do.
 */
	if (! dh_OrgClassCompat (tag->h_org, class))
	{
		msg_ELog (EF_PROBLEM, "hdf: file org/class mismatch");
		return (NULL);
	}
/*
 * Create a data chunk with the desired organization.
 */
	dc = dc_CreateDC (class);
	dc->dc_Platform = tag->h_plat;
/*
 * Now we try to get everything together.
 */
	switch (class)
	{
	/*
	 * Need scale info for each of the fields
	 */
	   case DCC_Image:
		dh_SetImageFields (tag, dc, nfield, fields);
		break;

	   case DCC_RGrid:
		dc_RGSetup (dc, nfield, fields);
		break;
	/*
	 * Hmm....
	 */
	   default:
	   	msg_ELog (EF_PROBLEM, "Unsupported data class %d", class);
		dc_DestroyDC (dc);
		return (NULL);
	}
/*
 * Read global and field attributes.  Any attributes set in the DataChunk
 * when data is retrieved will override any attributes set here
 * (e.g. a change in the bad value due to a dsDetail)
 */
	dh_ReadGlobalAtts (dc, tag);	/* read global attributes */

	if (class != DCC_Location)	/* read field attributes */
	{
		dh_ReadFieldAtts (dc, tag, fields, nfield);
	}
	return (dc);
}



static void
dh_ReadVgroupAtts (dc, tag, vgid, fid)
DataChunk *dc;
Htag *tag;
int32 vgid;
FieldId fid;
/*
 * Read the attributes from the vgroup id and store them in the datachunk.
 * If fid < 0, store the attributes as global; otherwise, fid is the field
 * id to store the attributes in.
 */
{
	char vsclass[128];
	char vsname[128];
	char fields[128];
	char *fname;
	int32 vgkey;
	int32 intr, sz, ftype, isize;
	int32 key, id, ref;
	int32 count;
	int ntagrefs, i;
	void *values;
/*
 * Attach to the global vgroup and traverse the vdata attributes
 */
	if ((vgkey = Vattach (tag->h_fid, vgid, "r")) == FAIL)
	{
		msg_ELog (EF_PROBLEM, "Vattach(%i) failed.", vgid);
		return;
	}
	ntagrefs = Vntagrefs (vgkey);
	for (i = 0; i < ntagrefs; ++i)
	{
		Vgettagref (vgkey, i, &ref, &id);
		if (ref != DFTAG_VH)	/* looking for vdata */
			continue;
		/*
		 * Attach to this vdata and get its key
		 */
		if ((key = VSattach (tag->h_fid, id, "r")) == FAIL)
			continue;
		VSgetclass (key, vsclass);
		if (strncmp (vsclass, "Attr", 4))
		{
			VSdetach (key);
			continue;
		}
		VSinquire (key, &count, &intr, fields, &sz, vsname);
		/*
		 * Now we know a vdata of class 'Attr' and its name.
		 * It should have only one field ('Values') from which
		 * we get the value of the attribute.
		 */
		ftype = VFfieldtype (key, 0);
		isize = VFfieldisize (key, 0);
		values = (void *) malloc (count * sz + 1);
		fname = VFfieldname (key, 0);
		VSsetfields (key, fname);
		VSseek (key, 0);
		VSread (key, values, count, FULL_INTERLACE);
		if (ftype == DFNT_CHAR)
		{
			((char *)values)[isize] = '\0';
			if (fid < 0)
				dc_SetGlobalAttrArray (dc, vsname, 
						       DCT_String, 1, values);
			else
				dc_SetFieldAttrArray (dc, fid, vsname,
						      DCT_String, 1, values);
		}
		else
		{
			if (fid < 0)
				dc_SetGlobalAttrArray (dc, vsname, 
				       dh_ElemType (ftype), count, values);
			else
				dc_SetFieldAttrArray (dc, fid, vsname, 
				       dh_ElemType (ftype), count, values);
		}
		free (values);
		VSdetach (key);
	}
	Vdetach (vgkey);
}



static void
dh_ReadGlobalAtts (dc, tag)
DataChunk *dc;
Htag *tag;
/*
 * Read the global attributes from the global vgroup
 */
{
	dh_ReadVgroupAtts (dc, tag, tag->h_vgid, /*fid*/ -1);
}



static void
dh_ReadFieldAtts (dc, tag, fids, nfield)
DataChunk *dc;
Htag *tag;
FieldId *fids;
int nfield;
/*
 * Read attributes from each field's vgroup and store in the datachunk.
 */
{
	int f;
	int32 varid;
	
	for (f = 0; f < nfield; f++) 
	{
		/* 
		 * We need the varid for this field
		 */
		if ((varid = dh_GetFieldVar (tag, fids[f])) < 0) 
			continue;
		/*
		 * Now pass off the work of extracting atts from the vgroup
		 */
		dh_ReadVgroupAtts (dc, tag, varid, fids[f]);
	}
}




int
dh_GetData (dc, gp, details, ndetail)
DataChunk *dc;
GetList *gp;
dsDetail *details;
int ndetail;
/*
 * Actually get the data that all that work has been done for.
 */
{
	Htag *tag;
	int nfield;
	FieldId *fids;
	SValue v;
	float badval = 0;
/*
 * Open the data file.
 */
#ifndef HDF_TEST
	if (!dfa_OpenFile (gp->gl_dfindex, FALSE, (void *) &tag))
		return (FALSE);
#else
	tag = OpenTag;
#endif
	fids = dc_GetFields (dc, &nfield);
/*
 * Figure out what bad value flag they want, and make sure it is stored
 * in the DC.
 */
	if (dc->dc_Class != DCC_Location)
	{
		badval = ds_GetDetail ("badval", details, ndetail, &v) ?
				v.us_v_float : 99999.9;
		dc_SetBadval (dc, badval);
	}
/*
 * Verify we're at the right time
 */
	if (! TC_LessEq (gp->gl_begin, tag->h_time) ||
	    ! TC_LessEq (tag->h_time, gp->gl_end))
		return (FALSE);
/*
 * Now we have to split out based on the class they want.
 */
	switch (dc->dc_Class)
	{
	/*
	 * Image
	 */
	   case DCC_Image:
		dh_ReadImage (dc, tag, fids, nfield);
		break;
	/*
	 * RGrid
	 */
	   case DCC_RGrid:
		dh_ReadRGrid (dc, tag, fids, nfield);
		break;

	   default:
	   	msg_ELog (EF_PROBLEM, "Don't handle class %d yet",
			  dc->dc_Class);
		return (FALSE);
	}
	return (TRUE);
}



static int
dh_Variable (fid, varid, vtype_out, ndim_out, dims, dimsizes, natt_out)
int32 fid;
int32 varid;
int32 *vtype_out;
int32 *ndim_out;
int32 *dims;
int32 *dimsizes;
int32 *natt_out;
/*
 * Return some information about this variable.  Return less than zero
 * on failure.
 */
{
	char vclass[128];
	int32 vgkey;
	int32 tag;
	int32 key, id;
	int32 vtype;
	int ndim, natt;
	int i, ntagrefs;
	char *fname;
	intn access, attach;
	int rank;

	ndim = 0;
	natt = 0;
	if ((vgkey = Vattach (fid, varid, "r")) == FAIL)
	{
		msg_ELog(EF_PROBLEM,"dh_Inquire: Vattach(%i) failed.",varid);
		return (-1);
	}
	ntagrefs = Vntagrefs (vgkey);
	for (i = 0; i < ntagrefs; ++i)
	{
		Vgettagref (vgkey, i, &tag, &id);
		switch (tag)
		{
		   case DFTAG_VH:	/* vdata */
			if ((key = VSattach (fid, id, "r")) == FAIL)
				continue;
			VSgetclass (key, vclass);
			if (strncmp (vclass, "Attr", 4) == 0)
				++natt;
			VSdetach (key);
			break;
		   case DFTAG_VG:	/* vgroup */
			if ((key = Vattach (fid, id, "r")) == FAIL)
				continue;
			Vgetclass (key, vclass);
			if (strncmp (vclass, "Dim", 3) == 0)
				dims[ndim++] = id;
			Vdetach (key);
			break;
		   case DFTAG_NDG: /* scientific data */
			/*
			 * Select the SDS by reference and collect info on it 
			 */
			Hfidinquire (fid, &fname, &access, &attach);
			if (DFSDreadref (fname, id) < 0)
			{
				msg_ELog (EF_PROBLEM, "hdf: %s(%s,%i) failed", 
					  "DFSDreadref", fname, id);
				return (-1);
			}
			DFSDgetdims (fname, &rank, dimsizes, MAX_DIMS);
			DFSDgetNT (&vtype);
			break;
		   default:		/* who knows */
			continue;
		}
	}
	Vdetach (vgkey);
	if (ndim_out)
		*ndim_out = ndim;
	if (natt_out)
		*natt_out = natt;
	if (vtype_out)
		*vtype_out = vtype;
	return (0);
}




static int
dh_ReadData (fid, varid, values)
int32 fid;
int32 varid;
void *values;
/*
 * Read data from the SDS for this variable.  Return less than zero
 * on failure.
 */
{
	int32 vgkey;
	int32 tag;
	int32 id;
	int32 dimsizes[MAX_DIMS];
	int i, ntagrefs;
	char *fname;
	intn access, attach;
	int rank;

	if ((vgkey = Vattach (fid, varid, "r")) == FAIL)
	{
		msg_ELog(EF_PROBLEM,"dh_Inquire: Vattach(%i) failed.",varid);
		return (-1);
	}
	ntagrefs = Vntagrefs (vgkey);
	for (i = 0; i < ntagrefs; ++i)
	{
		Vgettagref (vgkey, i, &tag, &id);
		if (tag == DFTAG_NDG)	/* scientific data */
		{
			/*
			 * Select the SDS by reference and collect info on it 
			 */
			Hfidinquire (fid, &fname, &access, &attach);
			if (DFSDreadref (fname, id) < 0)
			{
				msg_ELog (EF_PROBLEM, "hdf: %s(%s,%i) failed", 
					  "DFSDreadref", fname, id);
				return (-1);
			}
			DFSDgetdims (fname, &rank, dimsizes, MAX_DIMS);
			DFSDgetdata (fname, rank, dimsizes, values);
			break;
		}
	}
	Vdetach (vgkey);
	if (i < ntagrefs)
		return (0);
	else
		return (-1);	/* never found an SDS */
}




static void
dh_Origin (tag, nx, ny, origin)
Htag *tag;
int nx, ny;
Location *origin;
/*
 * Convert the distance from the center to the southwest corner into
 * degrees to get the lat/lon coords of the origin.  I think the origin
 * is defined as the *center* of the lower left cell, hence the 0.5 addend.
 */
{
	float ox, oy;
	float del_lat, del_lon;

	ox = ((float)nx / 2.0 - 0.5) * tag->h_pixel;
	oy = ((float)ny / 2.0 - 0.5) * tag->h_pixel;
	del_lat = oy / R_EARTH;
	del_lon = ox / (R_EARTH * cos (DEG_TO_RAD (tag->h_center.l_lat)));
	origin->l_lat = tag->h_center.l_lat - RAD_TO_DEG(del_lat);
	origin->l_lon = tag->h_center.l_lon - RAD_TO_DEG(del_lon);
	origin->l_alt = 0.0;
}




static inline int
dh_TranslateRow (rg, phi1, i)
RGrid *rg;
double phi1;
int i;
{
	double delta_phi, y;
	int m;

	/*
	 * Convert row in eq-cyl grid to km and then to latitude delta.
	 * Use the center of the cell as its location.
	 */
	y = (rg->rg_nY/2.0 - i + 0.5) * rg->rg_Yspacing;
	delta_phi = (y / R_EARTH);
	/*
	 * Project the latitude delta back to y using Mercatur
	 */
#ifdef notdef
	/*
	 * Apparently the center of the image is NOT the center of
	 * the projection, as could be implied from the documentation.
	 */
	y = R_EARTH*log(tan(PI/4.0+delta_phi/2.0))*cos(phi1);
#endif
	y = R_EARTH*log(tan(PI/4.0+delta_phi/2.0));
	m = rg->rg_nY/2.0 - (y / rg->rg_Yspacing) + 0.5;
	return (m);
}



/*
 * To re-project a grid, the x and lon coordinates do not change between
 * Mercatur and equidistant cylindrical projections.  So for each column
 * in the grid we just recalculate the y coordinate.
 *
 * Since the grid is in row major order, just calculate where each row will
 * come from in the source grid and copy it.  Y is independent of X for
 * both Mercatur and eq cyl projections, and both projections use the same
 * X transformation.
 *
 * If I weren't so lazy and behind, the row translations for the top half
 * could be reflected and used for the bottom.  Oh well. 
 */
static void
dh_ProjectGrid (tag, dest, grid, rg, badval)
Htag *tag;
float *dest;
float *grid;
RGrid *rg;
float badval;
{
	double phi1;
	int i, j, m;

	phi1 = DEG_TO_RAD (tag->h_center.l_lat);
	for (i = 0; i < rg->rg_nY; ++i)
	{
		m = dh_TranslateRow (rg, phi1, i);
		if (m >= 0 && m < rg->rg_nY)
			memcpy (dest + i*rg->rg_nX, grid + m*rg->rg_nX,
				rg->rg_nX * sizeof(float));
		else
		{
			for (j = 0; j < rg->rg_nX; ++j)
				dest[i*rg->rg_nX + j] = badval;
		}
	}
}




static void
dh_ProjectImage (tag, dest, grid, rg)
Htag *tag;
unsigned char *dest;
unsigned char *grid;
RGrid *rg;
{
	double phi1;
	int i, m;

	phi1 = DEG_TO_RAD (tag->h_center.l_lat);
	for (i = 0; i < rg->rg_nY; ++i)
	{
		m = dh_TranslateRow (rg, phi1, i);
		if (m >= 0 && m < rg->rg_nY)
			memcpy (dest + i*rg->rg_nX, grid + m*rg->rg_nX,
				rg->rg_nX);
		else
			memset (dest + i*rg->rg_nX, 255, rg->rg_nX);
	}
}




static void
dh_ReadRGrid (dc, tag, fields, nfield)
DataChunk *dc;
Htag *tag;
FieldId *fields;
int nfield;
{
	int i, j, varid;
	ScaleInfo *scales;
	int32 vtype;
	int32 ndims, natts;
	int32 dims[MAX_DIMS];
	int32 dimsizes[MAX_DIMS];
	long nx, ny;
	RGrid info;
	Location origin;
	float *grid, *dest;
	unsigned short *words;
	unsigned char *bytes;
	int nsample = dc_GetNSample (dc);
/*
 * For each field: find its varid.
 * Inquire and verify it has two dimensions, and get field type.
 * Put dimension sizes into RGrid info structure and start/count arrays.
 * Calculate origin from center location, dimension sizes, and pixel size.
 * Allocate and read the image array.
 * Convert short and byte data to float using the scale info.
 * Store the grid into the datachunk.
 */
/*
 * Loop invariants
 */
	info.rg_nZ = 1;
	info.rg_Xspacing = tag->h_pixel;
	info.rg_Yspacing = tag->h_pixel;
	info.rg_Zspacing = 1;
	scales = dh_GetFieldScales (tag, nfield, fields);

	for (i = 0; i < nfield; ++i)
	{
		if ((varid = dh_GetFieldVar (tag, fields[i])) < 0) 
			continue;
		dh_Variable (tag->h_fid, varid, &vtype, &ndims, 
			     dims, dimsizes, &natts);
		if (ndims != 2)
		{
			msg_ELog (EF_PROBLEM, 
				  "hdf: %s: expected 2 dimns in field %s",
				  "ReadRGrid", F_GetName (fields[i]));
			continue;
		}
		/*
		 * The X dimension varies fastest and is the second of the two
		 */
		nx = dimsizes[1];
		ny = dimsizes[0];
		info.rg_nX = nx;
		info.rg_nY = ny;
		dh_Origin (tag, nx, ny, &origin);

		/*
		 * Make space for the grid and get a pointer to that space
		 */
	 	dc_RGAddGrid (dc, nsample, fields[i], &origin,
			      &info, &tag->h_time, (void *) NULL, 0);
		grid = (float *) malloc (nx * ny * sizeof(float));
		dest = dc_GetMData (dc, nsample, fields[i], 0);
		switch (vtype)
		{
		   case DFNT_UCHAR8:
		   case DFNT_CHAR8:
		   case DFNT_INT8:
		   case DFNT_UINT8:
			bytes = (unsigned char *) malloc (nx * ny * 
							  sizeof (char));
			dh_ReadData (tag->h_fid, varid, bytes);
			for (j = 0; j < nx * ny; ++j)
			{
				grid[j] = (float)bytes[j] * scales[i].s_Scale;
				grid[j] += scales[i].s_Offset;
			}
			free (bytes);
			break;
		   case DFNT_INT16:
		   case DFNT_UINT16:
			words = (unsigned short *) 
				malloc (nx * ny * sizeof (unsigned short));
			dh_ReadData (tag->h_fid, varid, words);
			for (j = 0; j < nx * ny; ++j)
			{
				grid[j] = (float)words[j] * scales[i].s_Scale;
				grid[j] += scales[i].s_Offset;
			}
			free (words);
			break;
		   case DFNT_FLOAT32:
			dh_ReadData (tag->h_fid, varid, grid);
			break;
		   default:
			msg_ELog (EF_PROBLEM,
				  "hdf: ReadRGrid: unsupported type %d for %s",
				  vtype, F_GetName (fields[i]));
			dh_FillArray (grid, nx*ny, dc_GetBadval (dc));
			break;
		}
		/*
		 * Once we have a floating point grid we can re-project it
		 * into the datachunk
		 */
		dh_ProjectGrid (tag, dest, grid, &info, dc_GetBadval (dc));
		free (grid);
	}
}




static void
dh_FillArray (array, len, val)
float	*array;
int	len;
double	val;
/*
 * Fill the specified array of length len with val
 */
{
	int	i;

	for (i = 0; i < len; i++)
		array[i] = val;
}



static ScaleInfo *
dh_GetFieldScales (tag, nfield, fields)
Htag *tag;
int nfield;
FieldId *fields;
{
	static ScaleInfo scales[ MAX_FIELDS ];
	int f;
	int32 varid;
	double factor, offset;
	
	for (f = 0; f < nfield; f++) 
	{
		scales[f].s_Scale = 1.0;
		scales[f].s_Offset = 0.0;
	/* 
	 * Before anything else we need the varid for this field
	 */
		if ((varid = dh_GetFieldVar (tag, fields[f])) < 0) 
			continue;
	/*
	 * Look for the scale factor and offset attributes.
	 */
		if (dh_GetDoubleAtt (tag->h_fid, varid, "scale_factor",
				     &factor) >= 0)
			scales[f].s_Scale = (float) factor;
		if (dh_GetDoubleAtt (tag->h_fid, varid, "add_offset",
				     &offset) >= 0)
			scales[f].s_Offset = (float) offset;

	}  /* nfields */
	return (scales);
}



int
dh_DataTimes (index, when, which, n, dest)
int index;
ZebTime *when;
TimeSpec which;
int n;
ZebTime *dest;
/*
 * Find out when data is available.
 */
{
	Htag *tag;
/*
 * Get the file open.
 */
	if (! dfa_OpenFile (index, FALSE, (void *) &tag))
		return (0);
/*
 * Copy out the info.
 */
	if (which == DsBefore)
	{
		if (TC_LessEq (tag->h_time, *when))
			*dest = tag->h_time;
		return (1);
	}
	else if (which == DsAfter)
	{
		if (TC_LessEq (*when, tag->h_time))
			*dest = tag->h_time;
		return (1);
	}
	return (0);
}




/* ARGSUSED */
int
dh_GetFields (dfile, t, nfld, flist)
int dfile;
ZebTime *t;
int *nfld;
FieldId *flist;
/*
 * Return the list of available fields.
 */
{
	Htag *tag;
	int max = *nfld, fld;
/*
 * Open the file.
 */
	*nfld = 0;
	if (! dfa_OpenFile (dfile, FALSE, (void *) &tag))
		return (FALSE);
/*
 * Pass through the fields.
 */
	for (fld = 0; fld < tag->h_nVar && *nfld < max; fld++)
	{
		flist[*nfld] = tag->h_Fids[fld];
		(*nfld)++;
	}
	return (TRUE);
}




static void
dh_SetImageFields (tag, dc, nfield, fields)
Htag *tag;
DataChunk *dc;
int nfield;
FieldId *fields;
/*
 * Loop through these fields and determine the scale info for each one,
 * if not from the variable attributes then from a default.
 */
{
	int i;
	ScaleInfo *scales;
	int32 varid, vtype;
	int32 ndims, natts;
	int32 dims[MAX_DIMS];
	int32 dimsizes[MAX_DIMS];
	
	scales = dh_GetFieldScales (tag, nfield, fields);
	for (i = 0; i < nfield; i++) 
	{
		if ((varid = dh_GetFieldVar (tag, fields[i])) < 0) 
			continue;
		dh_Variable (tag->h_fid, varid, &vtype, &ndims, 
			     dims, dimsizes, &natts);
	/*
	 * Now factor in the scaling to convert the data into bytes.
	 */
		switch (vtype)
		{
		   case DFNT_UCHAR8:
		   case DFNT_CHAR8:
		   case DFNT_INT8:
		   case DFNT_UINT8:
			/* Scale and offset already correct */
			break;
		   case DFNT_INT16:
		   case DFNT_UINT16:
			/* Account for shift to byte values */
			scales[i].s_Scale *= 256.0;
			msg_ELog (EF_INFO, "hdf: field %s: %s",
				  F_GetName (fields[i]),
				  "16-bit data being scaled to 8 bits");
			break;
		   default:
			msg_ELog (EF_PROBLEM, "hdf: field %s: %s type %d",
				  "don't know how to scale", 
				  F_GetName (fields[i]), vtype);
			break;
		}
	}  /* nfields */
/*
 * Now we can set the fields in the image datachunk
 */
	dc_ImgSetup (dc, nfield, fields, scales);
}



static void
dh_ReadImage (dc, tag, fields, nfield)
DataChunk *dc;
Htag *tag;
FieldId *fields;
int nfield;
/*
 * Read the images for the given fields into the datachunk.
 */
{
	int i, j, varid;
	ScaleInfo *scales;
	int32 vtype;
	int32 ndims, natts;
	int32 dims[MAX_DIMS];
	int32 dimsizes[MAX_DIMS];
	long nx, ny;
	RGrid info;
	Location origin;
	unsigned char *image, *dest;
	short *words;
	int nsample = dc_GetNSample (dc);

	info.rg_nZ = 1;
	info.rg_Xspacing = tag->h_pixel;
	info.rg_Yspacing = tag->h_pixel;
	info.rg_Zspacing = 1;
	scales = dh_GetFieldScales (tag, nfield, fields);

	for (i = 0; i < nfield; ++i)
	{
		if ((varid = dh_GetFieldVar (tag, fields[i])) < 0) 
			continue;
		dh_Variable (tag->h_fid, varid, &vtype, &ndims, 
			     dims, dimsizes, &natts);
		if (ndims != 2)
		{
			msg_ELog (EF_PROBLEM, 
				  "hdf: %s: expected 2 dimns in field %s",
				  "ReadImage", F_GetName (fields[i]));
			continue;
		}
		/*
		 * The X dimension varies fastest and is the second of the two
		 */
		nx = dimsizes[1];
		ny = dimsizes[0];
		info.rg_nX = nx;
		info.rg_nY = ny;
		dh_Origin (tag, nx, ny, &origin);

		/*
		 * Make space for the grid and get a pointer to that space
		 */
	 	dc_ImgAddImage (dc, nsample, fields[i], &origin,
				&info, &tag->h_time, (void *) NULL, 0);
		image = (unsigned char *) malloc (nx * ny * sizeof(char));
		dest = dc_GetMData (dc, nsample, fields[i], 0);
		switch (vtype)
		{
		   case DFNT_UCHAR8:
		   case DFNT_CHAR8:
		   case DFNT_INT8:
		   case DFNT_UINT8:
			dh_ReadData (tag->h_fid, varid, image);
			break;
		   case DFNT_INT16:
		   case DFNT_UINT16:
			words = (short *) malloc (nx * ny * sizeof (short));
			dh_ReadData (tag->h_fid, varid, words);
			for (j = 0; j < nx * ny; ++j)
				image[j] = (unsigned char)(words[j] >> 8);
			free (words);
			break;
		   default:
			msg_ELog (EF_PROBLEM,
				  "hdf: ReadImage: unsupported type %d for %s",
				  vtype, F_GetName (fields[i]));
			for (j = 0; j < nx * ny; ++j)
				image[j] = (unsigned char) 255;
			break;
		}
		dh_ProjectImage (tag, dest, image, &info);
		free (image);
	}
}



#ifdef NOT_FINISHED



int
dmf_GetObsSamples (dfile, times, locs, max)
int dfile, max;
ZebTime *times;
Location *locs;
/*
 * Return sample info.
 */
{
	NCTag *tag;
	int i;
/*
 * Get the file open.
 */
	if (!dfa_OpenFile (dfile, FALSE, (void *) &tag))
		return (0);

	*times = tag->nc_time;
	locs->l_lat = 0.0;
	locs->l_lon = 0.0;
	locs->l_alt = 0.0;
	return (1);
}





static nc_type
dmf_NCType (type)
DC_ElemType type;
/*
 * Convert the DC_ElemType type to corresponding nc_type.
 */
{
	switch(type)
	{
	   case DCT_Float:
		return (NC_FLOAT);
	   case DCT_Double:
	   case DCT_LongDouble:
		return (NC_DOUBLE);
	   case DCT_Char:
		return (NC_CHAR);
	   case DCT_UnsignedChar:
		return (NC_BYTE);
	   case DCT_ShortInt:
	   case DCT_UnsignedShort:
		return (NC_SHORT);
	   case DCT_Integer:
	   case DCT_UnsignedInt:
	   case DCT_LongInt:
	   case DCT_UnsignedLong:
		return (NC_LONG);
	   default:
		return (0);
	}
}



static void
dh_SprintfAttr (buf, ftype, count, isize, values)
char *buf;
int32 ftype;
int32 count;
void *values;
{
	void *ptr;
	int j;

	ptr = buf;
	for (j = 0; j < count; ++j)
	{
		switch (ftype)
		{
		   case DFNT_FLOAT32:
			sprintf ("%.2g, ", *((float *)ptr));
			break;
		   case DFNT_FLOAT64:
			sprintf ("%.2lg, ", *((double *)ptr));
			break;
		   case DFNT_INT8:
			sprintf ("%i, ", (int)*((char *)ptr));
			break;
		   case DFNT_INT16:
			sprintf ("%i, ", (int)*((short *)ptr));
			break;
		   case DFNT_INT32:
			sprintf ("%i, ", *((int *)ptr));
			break;
		   case DFNT_UCHAR8:
		   case DFNT_CHAR8:
			strncpy (out, (char *)ptr, isize);
			out[isize] = 0;
			sprintf ("%s", out);
			break;
		   default:
			sprintf ("unknown vdata type");
			j = count;
			break;
		}
		ptr = (char *)ptr + isize;
	}
}



#endif /* NOT_FINISHED */


#ifdef HDF_TEST
/*
 * Test the HDF interface routines
 */

static void
dh_DumpTag (fname, tag)
char *fname;
Htag *tag;
{
	int i;
# 	define Show(fld,fmt) \
	printf ("%15s: " fmt, #fld, tag->h_##fld )

	printf ("Tag '%s'\n", fname);
	Show (fid,"%d");
	Show (vgid,"%d");
	Show (plat,"%d");
	printf ("\n");
	printf ("%15s: %s\n", "time", TC_AscTime (&tag->h_time, TC_Full));
	Show (center.l_lat,"%6.2f");
	Show (center.l_lon,"%6.2f");
	Show (center.l_alt,"%6.2f");
	printf ("\n");
	Show (nVar,"%d");
	Show (org,"%d");
	Show (pixel,"%f");
	printf ("\n");
	for (i = 0; i < tag->h_nVar; ++i)
	{
		printf ("%15s: '%s' (%s)", F_GetName (tag->h_Fids[i]),
		   F_GetDesc (tag->h_Fids[i]), F_GetUnits (tag->h_Fids[i]));
		if (i % 2) printf ("\n");
	}
	printf ("\n");
#	undef Show
}


int
main (argc, argv)
int argc;
char *argv[];
{
	int i;
	ZebTime begin, end;
	char ctime[64];
	int nsamp;
	int ret = 0;
	DataFile df;
	GetList gl;
	DataChunk *dc;
	Htag *tag;

	df.df_rev = 0;
	df.df_inode = 0;
	df.df_index = 0;
	df.df_flags = 0;
	df.df_ftype = 42;
	df.df_FLink = 0;
	df.df_BLink = 0;

	msg_connect (NULL, "hdftest");
	usy_init ();
	F_Init ();
	/*
	 * Expect a list of HDF files to test on the command line.
	 * Query each for a time and print the results, then open
	 * each and dump the tag before closing them.
	 */
	for (i = 1; i < argc; ++i)
	{
		ret |= dh_QueryTime (argv[i], &begin, &end, &nsamp);
		TC_EncodeTime (&begin, TC_Full, ctime);
		printf ("%s (%d samples): %s to %s\n", argv[i],
			nsamp, ctime, TC_AscTime (&end, TC_Full));
		/*
		 * Create a dummy datafile structure
		 */
		df.df_platform = 1234;
		strcpy (df.df_name, argv[i]);
		df.df_nsample = nsamp;
		df.df_begin = begin;
		df.df_end = end;

		/*
		 * Now try to open the file
		 */
		if (! dh_OpenFile (df.df_name, &df, /*write*/ 0, &tag))
		{
			printf ("%s: open failed.\n", df.df_name);
			continue;
		}
		/*
		 * Dump the tag
		 */
		dh_DumpTag (argv[i], tag);

		/*
		 * Test the setup and getdata methods and dump the datachunk
		 */
		OpenTag = tag;
		gl.gl_begin = tag->h_time;
		gl.gl_end = tag->h_time;
		dc = dh_Setup (&gl, tag->h_Fids, tag->h_nVar, DCC_RGrid);
		if (! dc)
		{
			msg_ELog (EF_PROBLEM, "setup failed");
		}
		else
		{
			dh_GetData (dc, &gl, NULL, 0);
			dc_DumpDC (dc);
			dc_DestroyDC (dc);
		}
		dh_CloseFile (tag);	
		OpenTag = NULL;
	}
	return (ret);
}



#endif /* HDF_TEST */


#endif /* HDF_INTERFACE */

