/*
 * Definitions for the polar plot stuff.
 */

# ifdef __cplusplus
extern "C"
{
# endif
	
typedef void *PPCookie;

extern PPCookie pol_DisplaySetup (int project, int tfill, int transparent,
				  unsigned long transparent_pixel);
extern PPCookie pol_GridSetup (int, DestImage *, double, double, double,
		double);
extern void pol_PlotBeam (PPCookie, PolarBeam *, void *, float, float);
extern void pol_Finished (PPCookie);	

# ifdef __cplusplus
};  /* Extern "C" */
# endif
