#ifdef RPC_HDR
%/*
% * $Id: map.x,v 2.1 1997-02-10 20:18:10 granger Exp $
% *
% * Definition of machine-independent binary map format.
% */
#endif

enum MapFlag
{
	PF_NONE = 0, 
	PF_FILL = 1, 		/* Polyline is a filled polygon */
	PF_LANDMARK = 2, 
	PF_ALTITUDES = 4	/* Points include altitude, in km above MSL */
};

/*
 * Every polyline in the map starts with this header, followed
 * by <npoints> Points or AltPoints, depending upon presence of PF_ALTITUDES.
 */
struct Polyline
{
	int npoints;
	float lat1;
	float lat0;
	float lon1;
	float lon0;
	int flags;	/* MapFlag OR'ed field */
};


struct Point
{
	float lat;
	float lon;
};


struct AltPoint
{
	float lat;
	float lon;
	float alt;
};

#ifdef RPC_HDR
%/*
% * Prototype and type declarations.
% */
%
%enum MapFormat { MF_UNKNOWN, MF_ASCII, MF_XDR };
%typedef enum MapFormat MapFormat;
%
%typedef struct MapFile MapFile;
%
%MapFormat MapGetFormat (MapFile *mf);
%int MapWritePoint (MapFile *mf, float *lat, float *lon);
%int MapWriteHeader (MapFile *mf, int npoints, 
%		    float *lat0, float *lon0, 
%		    float *lat1, float *lon1, int flags);
%int MapReadPoint (MapFile *mf, float *lat, float *lon);
%int MapReadHeader (MapFile *mf, int *npoints, 
%		   float *lat0, float *lon0, 
%		   float *lat1, float *lon1, int *flags);
%
%MapFile *MapRead (const char *filepath);
%MapFile *MapWrite (const char *filepath, MapFormat fmt);
%void MapClose (MapFile *mf);
%
#endif
