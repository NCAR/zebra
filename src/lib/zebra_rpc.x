%/*
% * $Id: zebra_rpc.x,v 2.1 1999-03-01 15:53:23 burghart Exp $
% *
% * Zebra basic types which require XDR definitions and interfaces.
% */
%

%/*
% * Here is the new zebra time format.  This looks suspiciously like the
% * BSD timeval structure....
% */
struct ZebraTime
{
	long	zt_Sec;			/* Seconds since 1/1/70	*/
	long	zt_MicroSec;		/* Microseconds added to zt_Sec */
};

typedef struct ZebraTime ZebTime;


%/*
% * Locations.
% */
struct Location
{
	float	l_lat;			/* degrees north	*/
	float	l_lon;			/* degrees east		*/
	float	l_alt;			/* altitude, units vary	*/
};


%/*
% * Scale and bias info for integer-encoded fields.
% */
struct ScaleInfo
{
	float	s_Scale;		/* real value = data/s_scale	*/
	float	s_Offset;		/*   + s_Offset		*/
};


%/*
% * Regularly-spaced grids in geographical coordinates
% */
struct RGrid
{
%	/* X dimension spacing		*/
	float	rg_Xspacing;
%	/* Y (north/south) spacing	*/
	float	rg_Yspacing;
%	/* Vertical spacing		*/
	float	rg_Zspacing;
%	/* Dimensions			*/
	int	rg_nX;
	int	rg_nY;
	int	rg_nZ;
};


%/*
% * Altitude units.  If you add new units here, be sure to add associated
% * units strings and formats to altunits.c.
% */
enum AltUnitType
{
	AU_kmMSL,	/* km MSL */
	AU_mMSL,	/* m MSL */
	AU_kmAGL,	/* km AGL */
	AU_mAGL,	/* m AGL */
	AU_mb,		/* mb (pressure altitude) */
	AU_sigma,	/* sigma level (unitless) */
	AU_level,	/* unitless level */
	AU_unknown	/* unknown */
};

