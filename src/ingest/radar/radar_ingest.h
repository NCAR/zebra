/* $Id: radar_ingest.h,v 1.1 1991-04-28 17:38:15 corbet Exp $ */
/*
 * Global stuff for the radar ingest module.
 */


/*
 * Beams of data are passed around as this:
 */
typedef struct _GDesc
{
	int	gd_first;		/* First gate		*/
	int	gd_ngate;		/* How many gates	*/
	unsigned char	*gd_data;		/* Where the data is	*/
} GDesc;

# define MAXGD 10
typedef struct _Beam
{
	Housekeeping *b_hk;		/* Housekeeping		*/
	int	b_npart;		/* How many data chunks	*/
	GDesc	b_gdesc[MAXGD];		/* The actual chunks	*/
} Beamst, *Beam;

/*
 * Where the radar is, in pixel and real space.
 */
extern int XRadar, YRadar;
extern float RadarLat, RadarLon;

/*
 * Resolution issues.
 */
extern int XRes, YRes;
extern float AzFill;

/*
 * The sortest sweep that interests us.
 */
extern int MinSweep;

/*
 * The offset in hours to GMT from the time recorded by the radars.
 */
extern int GMTOffset;

extern int NBeam, NMissed;
/*
 * Field and image info.
 */
typedef struct _RDest
{
	int	rd_foffset;		/* Field offset		*/
	unsigned char *rd_image;	/* Image destination	*/
} RDest;


/*
 * The scale factor, in pixels per kilometer.
 */
extern float PixScale;

/*
 * The (temporary) color map.
 */
unsigned char CMap[256];

/*
 * Functions.
 */
# ifdef __STDC__
	void 	SetupInput (void);
	Beam 	GetBeam (void);
	void	Rasterize (Beam, RDest *, int);
	void	OutputSweep (time *, double, int);
# else
# 	define const
	void 	SetupInput ();
	Beam 	GetBeam ();
	void	Rasterize ();
	void	OutputSweep ();
# endif

/*
 * Command constants go here.
 */
# define RIC_GO		1
# define RIC_SOURCE	2
# define RIC_FILE	3
# define RIC_INTERFACE	4
# define RIC_FIELD	5
