/*
 * $Id: Area.h,v 1.5 2003-01-29 22:22:21 burghart Exp $
 *
 * Prototypes for routies in the Area module
 */

#ifndef _zebra_Area_h_
#define _zebra_Area_h_

#include <defs.h>

typedef struct _AreaImage
{
	int minelem;
	int maxelem;
	int minline;
	int maxline;

	int sss;		/* Satellite sensor source, word 3 */
	int nchans;		/* Number of channels, word 14 */
	int datablock;		/* Data block offset, word 34 */
	int navblock;		/* Nav block offset, word 35 */
	int calblock;		/* Offset to calibration, 0 if none, word 63 */
	char source[5];		/* 4-char image source type, word 52 */
	char caltype[5];	/* Calibration type string, word 53 */
	char memo[33];		/* 32-char memo, words 25 through 32 */

	int nbytes;
	int prefixlen;
	int linelen;
	int xres;
	int yres;
	int nx, ny;
}
AreaImage;


typedef struct _AreaGrid
{
	int gridX;
	int gridY;
	float kmres;
	float minlon, maxlon, minlat, maxlat;
	int limits;		/* non-zero once our lat/lon limits valid */
	float latstep, lonstep;
	float origin_lat;
	int truncate;		/* truncate multi-byte data to one byte */
	int reset;		/* reset grid sizes to zero when finished */
}
AreaGrid;	

#define NO_ORIGIN (-999.0)

typedef struct _AreaFile
{
	char *name;
	char *field;
	FILE *stream;
	int area;		/* Offset to area directory */
	int navoff;		/* Offset to navigation codicil */
	ZebraTime when;
	int doswap;
	struct _AreaFile *prev;
	struct _AreaFile *next;
}
AreaFile;

AreaFile *AddFile (AreaFile *chain, char *fname, char *fld);
AreaFile *RemoveFile (AreaFile *chain, AreaFile *f);
AreaFile *RemoveOldFile (AreaFile *chain, AreaFile *f);
int CountFiles (AreaFile *chain);
void CloseAreaFile (AreaFile *f);
AreaFile *TimeCheck (AreaFile *chain, ZebTime *t);
void *DoFile (AreaFile *f, const AreaGrid *ag, unsigned char *map);
int UserLimits (AreaGrid *ag, double minlat, double minlon, 
		double maxlat, double maxlon);
void InitGrid (AreaGrid *ag);
int SetGrid (AreaGrid *ag, RGrid *rg, Location *loc);
void ResetGrid (AreaGrid *ag);
void SetArea (AreaImage *a, int *header);
void ReadArea (AreaFile *f, AreaImage *area);
int *ReadNavCod (AreaFile *f, AreaImage *area, int *nav_cod, char *imtype);
int SetAreaLimits (AreaFile *f, AreaGrid *ag);
unsigned char *ReadAreaImage (AreaFile *f, AreaImage *area);
#ifdef __zebra_DataStore_h_
DataChunk *SetupDC (AreaFile *chain, const char *platname);
int AreaIngest (AreaFile *chain, AreaGrid *ag, const char *platname);
void SetFieldMap (FieldId fid, ScaleInfo *scale, unsigned char *map);
#endif

# define MAXNAVCHUNKS 5

# define BETWEEN(x,lower,upper)	(((x)-(lower))*((x)-(upper)) <= 0)
# define DEG_TO_RAD(x)	((x)*0.017453292)
# define RAD_TO_DEG(x)	((x)*57.29577951)
# define DEG_TO_KM(x)	((x)*111.3238367) /* on a great circle */
# define KM_TO_DEG(x)	((x)*0.008982802) /* on a great circle */


#endif /* _zebra_Area_h_ */
