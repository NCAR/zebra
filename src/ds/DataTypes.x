/* -*- C -*- */
#ifdef RPC_HDR
%/*
% * $Id: DataTypes.x,v 3.2 2002-10-24 16:29:59 granger Exp $
% *
% * Simple datastore types required by platform and datafile protocol
% * structures.
% */
%
%# ifndef __zebra_DataTypes_h_
%# define __zebra_DataTypes_h_
%
%# include <defs.h>	/* Primitive zebra library types. */
%
%
%# ifndef TRUE
%#	define TRUE 1
%#	define FALSE 0
%# endif
%
#endif /* RPC_HDR */

/*
 * Possible data organizations.
 */
enum DataOrganization
{
	OrgUnknown	= 0,
	Org2dGrid	= 1,
	OrgIRGrid	= 2,
	OrgScalar	= 3,
	OrgImage	= 4,
	OrgOutline	= 5,
	Org3dGrid	= 6,
	OrgCmpImage	= 7,
        Org1dGrid       = 8,
	OrgTransparent  = 9,
	OrgFixedScalar  = 10,	/* Inflexible scalar for DFA_Zebra */
	OrgNSpace	= 11,
	OrgPolar	= 12
};


/*
 * The file types.  These are tied into the format table definition in
 * DFA, so don't mess with them.
 */
enum FileType
{
	FTUnknown = -1,
	FTNetCDF = 0,
	FTBoundary = 1,
	FTRaster = 2,
	FTCmpRaster = 3,
	FTZebra = 4,
	FTZeb = 4,
	FTGRIB = 5,
	FTGRIBSfc = 6,	/* GRIB surface grids only */
	FTGrads = 7,
	FTGradsModel = 8,
	FTHDF = 9,
	FTSweepFile = 10,	/* Radar sweep files */
	FTOpaque = 11
	/* ... */
};

#ifdef RPC_HDR
%# endif /* ifndef  __zebra_DataTypes_h_ */
#endif /* RPC_HDR */
