/*
 * Broadcast packets sent over the net to tell everybody where the
 * radar is.
 */

typedef struct _RadarInfo
{
	time ri_last;
	float ri_az, ri_el, ri_fixed;
	int ri_mode;		/* Scan mode		*/
} RadarInfo;

# define RadarInfoPort	3347
