/*
 * Please do not edit this file.
 * It was generated using rpcgen.
 */

#ifndef _MAP_H_RPCGEN
#define	_MAP_H_RPCGEN

#include <rpc/rpc.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * $Id: map.h,v 2.2 1997-07-10 22:04:23 granger Exp $
 *
 * Definition of machine-independent binary map format.
 */

enum MapFlag {
	PF_NONE = 0,
	PF_FILL = 1,
	PF_LANDMARK = 2,
	PF_ALTITUDES = 4
};
typedef enum MapFlag MapFlag;

struct Polyline {
	int npoints;
	float lat1;
	float lat0;
	float lon1;
	float lon0;
	int flags;
};
typedef struct Polyline Polyline;

struct Point {
	float lat;
	float lon;
};
typedef struct Point Point;

struct AltPoint {
	float lat;
	float lon;
	float alt;
};
typedef struct AltPoint AltPoint;
/*
 * Prototype and type declarations.
 */

enum MapFormat { MF_UNKNOWN, MF_ASCII, MF_XDR };
typedef enum MapFormat MapFormat;

typedef struct MapFile MapFile;

MapFormat MapGetFormat (MapFile *mf);
int MapWritePoint (MapFile *mf, float *lat, float *lon);
int MapWriteHeader (MapFile *mf, int npoints, 
		    float *lat0, float *lon0, 
		    float *lat1, float *lon1, int flags);
int MapReadPoint (MapFile *mf, float *lat, float *lon);
int MapReadHeader (MapFile *mf, int *npoints, 
		   float *lat0, float *lon0, 
		   float *lat1, float *lon1, int *flags);

MapFile *MapRead (const char *filepath);
MapFile *MapWrite (const char *filepath, MapFormat fmt);
void MapClose (MapFile *mf);


/* the xdr functions */

#if defined(__STDC__) || defined(__cplusplus)
extern  bool_t xdr_MapFlag(XDR *, MapFlag*);
extern  bool_t xdr_Polyline(XDR *, Polyline*);
extern  bool_t xdr_Point(XDR *, Point*);
extern  bool_t xdr_AltPoint(XDR *, AltPoint*);

#else /* K&R C */
extern bool_t xdr_MapFlag();
extern bool_t xdr_Polyline();
extern bool_t xdr_Point();
extern bool_t xdr_AltPoint();

#endif /* K&R C */

#ifdef __cplusplus
}
#endif

#endif /* !_MAP_H_RPCGEN */
