/*
 * My very first HDF program, and it shows.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <hdf.h>

#ifndef lint
static char *rcsid[2] = { (char *)rcsid,
   "$Id: hdflook.c,v 3.1 1995-06-29 21:43:11 granger Exp $" };
#endif

static void show_vdata (int32 fid, int32 id, int depth);
static void show_vgroup (int32 fid, int32 vgid, int depth);
static void show_sd (int32 fid, int32 ref, int depth);
static void indent (int depth);

int
main (argc, argv)
int argc;
char *argv[];
{
	int32 fid; /* id of an opened HDF file */ 
	int32 vgid, vgkey;
	char vgclass[128]; 

	fid = Hopen(argv[1], DFACC_READ, 0); 
	vgid = -1;
	Vstart (fid);
	/*
	 * Look for the CDF0.0 class vgroup and show that one only
	 */
	while ((vgid = Vgetid (fid, vgid)) != FAIL)
	{
		if ((vgkey = Vattach (fid, vgid, "r")) == FAIL)
		{
			printf ("Vattach(%i) failed.\n", (int)vgid);
			continue;
		}
		Vgetclass (vgkey, vgclass);
		Vdetach (vgkey);
		if (! strncmp ("CDF", vgclass, 3))
			show_vgroup (fid, vgid, 0);
	} 
	Vend (fid);
	Hclose (fid);
	return (0);
}



static void
show_vgroup (fid, vgid, depth)
int32 fid;
int32 vgid;
int depth;
{
	char vgclass[128]; 
	char vgname[128]; 
	int32 vgkey;
	int32 ntagrefs, tag, ref, i;

	indent (depth);
	if ((vgkey = Vattach (fid, vgid, "r")) == FAIL)
	{
		printf ("Vattach(%i) failed.\n", (int)vgid);
		return;
	}
	Vgetclass (vgkey, vgclass);
	Vgetname (vgkey, vgname);
	ntagrefs = Vntagrefs (vgkey);
	printf ("vgroup '%s' id %i (class %s): %i tag/ref pairs:\n",
		vgname, (int)vgid, vgclass, (int)ntagrefs);
	for (i = 0; i < ntagrefs; ++i)
	{
		Vgettagref (vgkey, i, &tag, &ref);
		switch (tag)
		{
		   case DFTAG_VH:
			show_vdata (fid, ref, depth + 1);
			break;
		   case DFTAG_VG:
			/* recursively show this vgroup */
			show_vgroup (fid, ref, depth + 1);
			break;
		   case DFTAG_NDG: /* scientific data */
			show_sd (fid, ref, depth + 1);
			break;
		   default:
			indent (depth + 1);
			printf ("unknown tag: %-5i  ref: %-5i\n", 
				(int)tag, (int)ref);
			break;
		}
	}
	Vdetach (vgkey);
}



static void
show_vdata (fid, id, depth)
int32 fid;
int32 id;
int depth;
{
	char vsclass[128];
	char vsname[128];
	char fields[128];
	int32 count, intr, sz;
	int32 key;
	int nfield, i, j;
	void *buf, *ptr;
	char *out;

	indent (depth);
	if ((key = VSattach (fid, id, "r")) == FAIL)
	{
		printf ("VSattach failed.\n");
		return;
	}
	VSgetclass (key, vsclass);
	VSinquire (key, &count, &intr, fields, &sz, vsname);
	printf ("Vdata (%i): %s class: %s count: %i size: %i; '%s'\n",
		(int)id, vsname, vsclass, (int)count, (int)sz, fields);
	nfield = VFnfields (key);
	buf = (void *) malloc (count * sz);
	out = (char *) malloc (count * sz + 1);
	for (i = 0; i < nfield; ++i)
	{
		char *name = VFfieldname (key, i);
		int32 ftype = VFfieldtype (key, i);
		int32 isize = VFfieldisize (key, i);
		int32 esize = VFfieldesize (key, i);

		indent (depth+1);
		printf ("field: %s type: %i isize: %i esize: %i data: ", 
			name, (int)ftype, (int)isize, (int)esize);
		VSsetfields (key, name);
		VSseek (key, 0);
		VSread (key, buf, count, FULL_INTERLACE);
		ptr = buf;
		for (j = 0; j < count; ++j)
		{
			switch (ftype)
			{
			   case DFNT_FLOAT32:
				printf ("%.2g, ", *((float *)ptr));
				break;
			   case DFNT_FLOAT64:
				printf ("%.2g, ", *((double *)ptr));
				break;
			   case DFNT_INT8:
				printf ("%i, ", (int)*((char *)ptr));
				break;
			   case DFNT_INT16:
				printf ("%i, ", (int)*((short *)ptr));
				break;
			   case DFNT_INT32:
				printf ("%i, ", *((int *)ptr));
				break;
			   case DFNT_UCHAR8:
			   case DFNT_CHAR8:
				strncpy (out, (char *)ptr, isize);
				out[isize] = 0;
				printf ("%s", out);
				break;
			   default:
				printf ("unknown vdata type");
				j = count;
				break;
			}
			ptr = (char *)ptr + isize;
		}
		printf ("\n");
	}
	VSdetach (key);
	free (buf);
	free (out);
}




static void
show_sd (fid, ref, depth)
int32 fid;
int32 ref;
int depth;
{
	char *fname;
	intn access, attach;
	int rank;
	int32 dimsizes[64];

	/*
	 * Select the SDS by reference and collect info on it 
	 */
	Hfidinquire (fid, &fname, &access, &attach);
	indent (depth);
	if (DFSDreadref (fname, ref) < 0)
	{
		printf ("DFSDreadref (%s, %i) failed.\n", fname, (int)ref);
		return ;
	}
	DFSDgetdims (fname, &rank, dimsizes, 64);
	printf ("SDS ref: %i; rank: %i\n", (int)ref, rank);
}



static void
indent (depth)
int depth;
{
	printf ("%*s", depth<<2, "");
}

