/*
 * Radar information
 * $Id: radar.h,v 1.2 1991-07-05 19:50:28 burghart Exp $
 */

# ifndef RADAR_H
# define RADAR_H
# include <ui_param.h>		/* for bool definition */

/*
 * Optimization status: disabled, match time and spatial resolution, match 
 * time only, or match spatial resolution only
 */
typedef enum
{
	MatchBoth = 0, 
	MatchTime, 
	MatchSpatial,
	N_STATUS
} rstatus;

/*
 * Scan types
 */
typedef enum
{
	PPI = 0,
	RHI,
	SUR,
	N_STYPES
} s_type;

/*
 * Radar name length and maximum number of sweeps
 */
# define RNAMELEN	20
# define MAX_SWEEPS	100

/*
 * Everything that can be associated with a given radar (for our purposes)
 */
typedef struct
{
	char	name[RNAMELEN];		/* radar name			*/
	bool	enabled;		/* radar enabled?		*/
	char	*line_out;		/* name of outgoing line	*/
	char	*phone;			/* phone # for sending scans	*/
	rstatus	status;			/* current status		*/
	s_type	scantype;		/* scan type			*/
	float	lat, lon;		/* position			*/
	float	max_h_scanrate;		/* max horizontal scan speed	*/
	float	max_v_scanrate;		/* max vertical scan speed	*/
	float	h_accel;		/* horizontal acceleration	*/
	float	v_accel;		/* vertical acceleration	*/
	int	prf;			/* current PRF			*/
	int	min_hits, max_hits;	/* Hit limits			*/
	int	hits;			/* current number of hits	*/
	bool	fix_hits;		/* Are we using fixed hits?	*/
	bool	fix_step;		/* Fixed steps for non-RHIs?	*/
	bool	inside;			/* Enclosed by the volume?	*/
	float	min_range;		/* smallest range to deal with	*/
	float	min_elev;		/* smallest elevation to use	*/
	float	az_left, az_right;	/* volume azimuth limits	*/
	float	el_bottom, el_top;	/* volume elevation limits	*/
	float	rng_front, rng_back;	/* volume range limits		*/
	float	anglist[MAX_SWEEPS];	/* angle list			*/
	int	nsweeps;		/* length of angle list		*/
	float	scanrate;		/* scan rate			*/
	float	scantime;		/* volume scan time		*/
	float	res_vert, res_horiz;	/* resolutions for the scan	*/
} Radar;

/*
 * Our radar array
 */
extern Radar	Rad[];
extern int	Nradars;

# endif 	/* RADAR_H */
