/*
 * Define some simple placeholder routines for formats whose methods
 * are not compiled.
 */


#ifndef _zebra_NoFormat_h_
#define _zebra_NoFormat_h_

/* ARGSUSED */
static int
fmt_QueryNotCompiled (file, begin, end, nsample)
char *file;
ZebTime *begin;
ZebTime *end;
int *nsample;
{
	msg_ELog (EF_PROBLEM, "cannot query file %s: format not compiled",
		  file);
	return (0);
}

/* ARGSUSED */
static int
fmt_OpenNotCompiled (of, file, dp, write)
OpenFile *of;
char *file;
DataFile *dp;
int write;
{
	msg_ELog (EF_PROBLEM, "cannot open file %s: format not compiled",
		  file);
	return (0);
}

/* ARGSUSED */
static int
fmt_CreateNotCompiled (ofp, fname, dfile, dc, details, ndetail)
OpenFile *ofp;
char *fname;
DataFile *dfile;
DataChunk *dc;
dsDetail *details;
int ndetail;
{
	msg_ELog (EF_PROBLEM, "cannot create file %s: format not compiled",
		  fname);
	return (0);
}

#endif /* !def _zebra_NoFormat_h_ */

