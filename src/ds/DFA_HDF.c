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

#ifndef HDF_INTERFACE 	/* Conditional compilation of HDF interface */

static char *hdfopt[2] = { "@(#)$DFA: HDF_INTERFACE NOT Compiled $",
				   (char *)hdfopt };
# else
static char *hdfopt[2] = { "@(#)$DFA: HDF_INTERFACE Compiled $",
				   (char *)hdfopt };

/*
 * The HDF interface typedef's bool to int, which conflicts with UI char
 * typedef.  So we just change the name for the hdf header files since we
 * do not use the HDF bool type in this file.
 */
# define bool hdf_int_bool
# include <hdf.h>
# undef bool

#endif /* HDF_INTERFACE */

# include <defs.h>
# include <config.h>
# include <message.h>

RCSID ("$Id: DFA_HDF.c,v 3.12 1997-11-21 20:36:06 burghart Exp $")

# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"
# include "dfa.h"
# include "DataFormat.h"

# define DEG_TO_RAD(x)	((x)*0.017453292)
# define RAD_TO_DEG(x)	((x)*57.29577951)
# define DEG_TO_KM(x)	((x)*111.3238367) /* on a great circle */
# define KM_TO_DEG(x)	((x)*0.008982802) /* on a great circle */
/*
 * Radius of the earth, in km
 */
static const double R_EARTH = 6378.;
static const double D_PI = 3.14159265358979323846;

#if defined(sun) && !defined(SVR4) && !defined(__GNUC__)
# define signed
#endif

# define MAX_DIMS	128
# define MAX_FIELDS 	CFG_FIELD_MAX_ID

/*
 * Variable attributes retrieved for a field
 */
#define VATT_LONGNAME	"long_name"	/* description of variable */
#define VATT_UNITS	"units"		/* units of variable	   */
#define VATT_MISSING	"missing_value" /* bad value		   */
#define FILL_VALUE	"_FillValue"	/* TDF sets this one	   */

/*
 * The minimum size of a time list before it's worthwhile to do a binary
 * search.
 */
#define MINTIME 10

#ifdef HDF_INTERFACE
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

#define HTAGP(ofp) (&((HDFOpenFile *)ofp)->hdf_tag)

#endif

typedef struct _HDFOpenFile
{
	OpenFile	open_file;
#ifdef HDF_INTERFACE
	Htag		hdf_tag;
#endif
} 
HDFOpenFile;


static CO_Compat COCTable [] =
{
 	{ OrgImage,		DCC_Image },	/* only for bytes so far */
	{ Org2dGrid,		DCC_RGrid }
};


#ifdef HDF_INTERFACE
/*
 * Semi-public routines (DFA methods)
 */
P_CloseFile (dh_CloseFile);
P_OpenFile (dh_OpenFile);
P_SyncFile (dh_SyncFile);
P_QueryTime (dh_QueryTime);
P_Setup (dh_Setup);
P_GetData (dh_GetData);
P_GetFields (dh_GetFields);
P_GetTimes (dh_GetTimes);

/*
 * The HDF format.
 */
static DataFormat hdfFormatRec =
{
	"HDF",
	FTHDF,
	".hdf",

	COCTable,       		/* org/class compatibility table*/
	N_COC(COCTable),
	sizeof(HDFOpenFile),		/* open file instance size	*/
	TRUE,				/* read-only 			*/

	FORMAT_INIT,

	dh_QueryTime,			/* Query times			*/
	___,				/* Make file name		*/

	dh_Setup,			/* setup			*/
	dh_OpenFile,			/* Open				*/
	dh_CloseFile,			/* Close			*/
	dh_SyncFile,			/* Synchronize			*/
	dh_GetData,			/* Get the data			*/
	___,				/* Get altitude info		*/
	fmt_DataTimes,			/* Get data times		*/
	___,				/* Get forecast times		*/
	___,				/* Create a new file		*/
	___,				/* Write to file		*/
	___,				/* Write block to a file	*/
	___,				/* Get observation samples	*/
	dh_GetFields,			/* Get fields			*/
	___,				/* Get Attributes		*/
	dh_GetTimes,			/* Get times			*/
	___                             /* Get the associated files     */
};

#else /* ! HDF_INTERFACE */

static DataFormat hdfFormatRec =
{
	"HDF-Uncompiled",
	FTHDF,
	".hdf",

	COCTable,       		/* org/class compatibility table*/
	N_COC(COCTable),
	sizeof(HDFOpenFile),
	TRUE,				/* read-only 			*/

	FORMAT_INIT,

	fmt_QueryNotCompiled,		/* Query times			*/
	___,				/* Make file name		*/

	___,				/* setup			*/
	fmt_OpenNotCompiled,		/* Open				*/
	___,				/* Close			*/
	___,				/* Synchronize			*/
	___,				/* Get the data			*/
	___,				/* Get altitude info		*/
	___,				/* Get data times		*/
	___,				/* Get forecast times		*/
	___,				/* Create a new file		*/
	___,				/* Write to file		*/
	___,				/* Write block to a file	*/
	___,				/* Get observation samples	*/
	___,				/* Get fields			*/
	___,				/* Get Attributes		*/
	___				/* Get times			*/
};

#endif /* HDF_INTERFACE */

DataFormat *hdfFormat = (DataFormat *) &hdfFormatRec;

#ifdef HDF_INTERFACE

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
static int dh_Variable FP ((int32 fid, int32 varid, int32 *vtype_out,
			    int32 *ndim_out, int32 *dims, int32 *dimsizes,
			    int32 *natt_out));
static int dh_OFTimes FP ((Htag *tag));
static int32 dh_VgroupClass FP ((int32 fid, const char *name));
static char *dh_GetStringAtt FP ((int fid, int varid, const char *att_name,
				  char *att_val, int len));
static void dh_FreeFields FP ((Htag *tag));
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
static void dh_Origin FP ((Htag *tag, RGrid *info, Location *origin));
static int dh_SyncTag FP ((Htag *tag));



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




static int
dh_QueryTime (file, begin, end, nsamp)
char *file;
ZebTime *begin, *end;
int *nsamp;
/*
 * Query the times on this file.
 */
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




static int
dh_OpenFile (ofp, fname, dp, write)
OpenFile *ofp;
char *fname;
DataFile *dp;
int write;
/*
 * Try to open this file.
 */
{
	Htag *tag = HTAGP (ofp);
	int32 fid;
/*
 * Try to open the file.
 */
	if ((fid = Hopen (fname, DFACC_READ, 0)) < 0)
	{
		msg_ELog (EF_PROBLEM, "hdf: Hopen(%s) failed.", fname);
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
	if (! dh_SyncTag (tag))
	{
		msg_ELog (EF_PROBLEM, "hdf: dh_SyncTag(%s) failed.", fname);
		dh_CloseFile (ofp);
		return (FALSE);
	}
	return (TRUE);
}



static int
dh_SyncFile (ofp)
OpenFile *ofp;
{
	Htag *tag = HTAGP (ofp);

	return (dh_SyncTag (tag));
}



static int
dh_SyncTag (tag)
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



static void
dh_CloseFile (ofp)
OpenFile *ofp;
/*
 * Close this file.
 */
{
	Htag *tag = HTAGP (ofp);

	dh_FreeFields (tag);
	Vend (tag->h_fid);
	Hclose (tag->h_fid);
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
	int32 vtype;
	int32 ndims, natts;
	int32 dims[MAX_DIMS];
	int32 dimsizes[MAX_DIMS];
/*
 * Free any existing info, for the future case when we might be re-syncing.
 */
	dh_FreeFields (tag);
	tag->h_Fids = (FieldId *) malloc (FBLK * sizeof (FieldId));
	tag->h_Vids = (int32 *) malloc (FBLK * sizeof (int32));
/*
 * Pass through the fields.  Attach to the CDF vgroup and collect all
 * vgroups of class "Var" which match our purported organization.
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
		dh_Variable (tag->h_fid, id, &vtype, &ndims, 
			     dims, dimsizes, &natts);
		if (ndims != 2)		/* must match Image or RGrid org */
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




static DataChunk *
dh_Setup (ofp, fields, nfield, class)
OpenFile *ofp;
FieldId *fields;
int nfield;
DataClass class;
/*
 * Get set up for this piece of data access.
 */
{
	Htag *tag = HTAGP (ofp);
	DataChunk *dc;
/*
 * Create a data chunk with the desired organization.
 */
	dc = dc_CreateDC (class);
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
			if (fid == BadField)
				dc_SetGlobalAttrArray (dc, vsname, 
						       DCT_String, 1, values);
			else
				dc_SetFieldAttrArray (dc, fid, vsname,
						      DCT_String, 1, values);
		}
		else
		{
			if (fid == BadField)
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
	dh_ReadVgroupAtts (dc, tag, tag->h_vgid, BadField);
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




static int
dh_GetData (ofp, dc, begin, nsample, details, ndetail)
OpenFile *ofp;
DataChunk *dc;
int begin, nsample;
dsDetail *details;
int ndetail;
{
	Htag *tag = HTAGP (ofp);
	int nfield;
	FieldId *fids;
	SValue v;
	float badval = 0;

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
 * We can have only one sample, so make sure that's all that's requested.
 */
	if (begin != 0 || nsample != 1)
	{
		msg_ELog (EF_PROBLEM, "hdf: getdata on more than sample 0");
	}
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
		msg_ELog(EF_PROBLEM,"dh_Variable: Vattach(%i) failed.",varid);
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
dh_Origin (tag, info, origin)
Htag *tag;
RGrid *info;
Location *origin;
/*
 * Convert the distance from the center to the southwest corner into
 * degrees to get the lat/lon coords of the origin.  I think the origin
 * is defined as the *center* of the lower left cell, hence the 0.5 addend.
 * We want the centers of the two projected grids to coincide, hence we
 * calculate the origin of the EC grid using the eq cyl projection.
 */
{
	float ox, oy;
	float del_lat, del_lon;

	ox = ((float)info->rg_nX / 2.0 - 0.5) * info->rg_Xspacing;
	oy = ((float)info->rg_nY / 2.0 - 0.5) * info->rg_Yspacing;
	del_lat = oy / R_EARTH;
	del_lon = ox / (R_EARTH * cos (DEG_TO_RAD (tag->h_center.l_lat)));
	origin->l_lat = tag->h_center.l_lat - RAD_TO_DEG(del_lat);
	origin->l_lon = tag->h_center.l_lon - RAD_TO_DEG(del_lon);
	origin->l_alt = 0.0;
}




static inline int
dh_TranslateRow (rg, phi1, y0, i)
RGrid *rg;
double phi1, y0;
int i;
{
	double phi, y;
	int m;

	/*
	 * Convert row in eq-cyl grid to km and then to latitude.
	 * Use the center of the cell as its location.
	 */
	y = (rg->rg_nY/2.0 - i - 0.5) * rg->rg_Yspacing;
	phi = (y / R_EARTH) + phi1;
	/*
	 * Project the latitude back to y using Mercatur.  Subtract the
	 * kilometer coordinate of the grid center latitude before
	 * calculating the row.
	 */
	y = R_EARTH*log(tan(D_PI/4.0+phi/2.0))*cos(phi1) - y0;
	m = rg->rg_nY/2.0 - (y / rg->rg_Yspacing) - 0.5;
	return (m);
}



/*
 * Fill an eq cyl grid from a Mercatur grid:
 *
 * The X spacing is assumed to have been adjusted for the eq cyl so that
 * the widths of columns in both grids are equal.  Both grids share the
 * same center, so the grid values along any row are identical between the
 * two grids.  Since the grid is in row major order, just calculate where
 * each row will come from in the source grid and copy it.
 */
static void
dh_ProjectGrid (tag, dest, grid, rg, badval)
Htag *tag;
float *dest;
float *grid;
RGrid *rg;
float badval;
{
	double phi1, y0;
	int i, j, m;

	phi1 = DEG_TO_RAD (tag->h_center.l_lat);
	y0 = R_EARTH*log(tan(D_PI/4.0+phi1/2.0))*cos(phi1);
	for (i = 0; i < rg->rg_nY; ++i)
	{
		m = dh_TranslateRow (rg, phi1, y0, i);
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
	double phi1, y0;
	int i, m;

	phi1 = DEG_TO_RAD (tag->h_center.l_lat);
	y0 = R_EARTH*log(tan(D_PI/4.0+phi1/2.0))*cos(phi1);
	for (i = 0; i < rg->rg_nY; ++i)
	{
		m = dh_TranslateRow (rg, phi1, y0, i);
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
	int32 ftype, isize, count;
	long nx, ny;
	RGrid info;
	Location origin;
	float *grid, *dest;
	unsigned short *words;
	unsigned char *bytes;
	unsigned short sfillval;
	unsigned char cfillval;
	float fillval;
	float badval = dc_GetBadval (dc);
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
		 * The X dimension varies fastest and is the second of the two.
		 * For the Mercatur and EC grid columns to coincide, the
		 * EC X spacing must be scaled by cos(phi) to account for
		 * the two projections having different latitude centers.
		 * (Merc uses equator and EC uses grid center.)
		 */
		nx = dimsizes[1];
		ny = dimsizes[0];
		info.rg_nX = nx;
		info.rg_nY = ny;
		dh_Origin (tag, &info, &origin);

		/*
		 * Make space for the grid and get a pointer to that space
		 */
	 	dc_RGAddGrid (dc, nsample, fields[i], &origin,
			      &info, &tag->h_time, (void *) NULL, 0);
		grid = (float *) malloc (nx * ny * sizeof(float));
		dest = dc_GetMData (dc, nsample, fields[i], 0);
		/*
		 * When converting, be sure to test against missing
		 * value first, BEFORE scaling.  If its a missing value,
		 * then explicitly set it to the datachunk's bad value.
		 */
		switch (vtype)
		{
		   case DFNT_UCHAR8:
		   case DFNT_UINT8:
			bytes = (unsigned char *) malloc (nx * ny * 
							  sizeof (char));
			dh_ReadData (tag->h_fid, varid, bytes);
			if (! dh_ReadAttr (tag->h_fid, varid, FILL_VALUE, 
					   &ftype, &count, &isize, &cfillval))
				cfillval = 0xff;
			for (j = 0; j < nx * ny; ++j)
			{
				if (bytes[j] == cfillval)
					grid[j] = badval;
				else
				{
					grid[j] = (float)bytes[j] * 
						scales[i].s_Scale;
					grid[j] += scales[i].s_Offset;
				}
			}
			free (bytes);
			break;
		   case DFNT_CHAR8:
		   case DFNT_INT8:
			bytes = (unsigned char *) malloc (nx * ny * 
							  sizeof (char));
			dh_ReadData (tag->h_fid, varid, bytes);
			if (! dh_ReadAttr (tag->h_fid, varid, FILL_VALUE, 
					   &ftype, &count, &isize, &cfillval))
				cfillval = 0x80;
			for (j = 0; j < nx * ny; ++j)
			{
				if (bytes[j] == cfillval)
					grid[j] = badval;
				else
				{
					grid[j] = (float)((signed char)
					   bytes[j]) * scales[i].s_Scale;
					grid[j] += scales[i].s_Offset;
				}
			}
			free (bytes);
			break;
		   case DFNT_UINT16:
			words = (unsigned short *) 
				malloc (nx * ny * sizeof (unsigned short));
			dh_ReadData (tag->h_fid, varid, words);
			if (! dh_ReadAttr (tag->h_fid, varid, FILL_VALUE, 
					   &ftype, &count, &isize, &sfillval))
				sfillval = 0xffff;
			for (j = 0; j < nx * ny; ++j)
			{
				if (words[j] == sfillval)
					grid[j] = badval;
				else
				{
					grid[j] = (float)words[j] * 
						scales[i].s_Scale;
					grid[j] += scales[i].s_Offset;
				}
			}
			free (words);
			break;
		   case DFNT_INT16:
			words = (unsigned short *) 
				malloc (nx * ny * sizeof (unsigned short));
			dh_ReadData (tag->h_fid, varid, words);
			if (! dh_ReadAttr (tag->h_fid, varid, FILL_VALUE, 
					   &ftype, &count, &isize, &sfillval))
				sfillval = 0x8000;
			for (j = 0; j < nx * ny; ++j)
			{
				if (words[j] == sfillval)
					grid[j] = badval;
				else
				{
					grid[j] = 
					   (float)((signed short)words[j]) * 
					   scales[i].s_Scale;
					grid[j] += scales[i].s_Offset;
				}
			}
			free (words);
			break;
		   case DFNT_FLOAT32:
			dh_ReadData (tag->h_fid, varid, grid);
			if (dh_ReadAttr (tag->h_fid, varid, FILL_VALUE, 
					 &ftype, &count, &isize, &fillval))
			{
				for (j = 0; j < nx * ny; ++j)
				{
					if (grid[j] == fillval)
						grid[j] = badval;
				}
			}
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




/* ARGSUSED */
static int
dh_GetFields (ofp, sample, nfld, flist)
OpenFile *ofp;
int sample;
int *nfld;
FieldId *flist;
/*
 * Return the list of available fields.
 */
{
	Htag *tag = HTAGP (ofp);
	int max = *nfld, fld;

	*nfld = 0;
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
		dh_Origin (tag, &info, &origin);

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



static ZebTime *
dh_GetTimes (ofp, ntime)
OpenFile *ofp;
int *ntime;
{
	Htag *tag = HTAGP (ofp);

	*ntime = 1;
	return (&tag->h_time);
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


#endif /* HDF_INTERFACE */

