/*
 * Description of the P3 composite image file format.
 */

typedef struct P3Header
{
	char	ph_Flight[8];		/* Flight number?	*/
	char	ph_Name[16];		/* Some sort of name	*/
	int	ph_StYear;		/* Start year		*/
	int	ph_StMonth;		/* Start month		*/
	int	ph_StDay;		/* Start day		*/
	int	ph_StHour;		/* You get the picture...*/
	int	ph_StMinute;
	int	ph_StSecond;
	int	ph_EndYear;		/* End year		*/
	int	ph_EndMonth;
	int	ph_EndDay;
	int	ph_EndHour;
	int	ph_EndMinute;
	int	ph_EndSecond;
	int	ph_Nx;			/* Number of X points	*/
	int	ph_Ny;			/* Number of Y points	*/
	int	ph_Lat;			/* Origin lat * 10000	*/
	int	ph_Lon;			/* Origin lon * 10000	*/
	int	ph_XSpacing;		/* X spacing in meters	*/
	int	ph_YSpacing;		/* Y spacing in meters	*/
	int	ph_NSweep;		/* Number of sweeps	*/
	int	ph_Alt;			/* Altitude in meters	*/
	int	ph_Weirdness[3];	/* tilt/roll stuff	*/
	int	ph_BadVal;		/* Missing data flag	*/
	int	ph_IFT;			/* "inflight track data" */
	int	ph_Pad[5];		/* Padding		*/
} P3Header;

/*
 * Data alleged to be stored as two-byte ints, across rows, southernmost
 * row first.
 */

