# include <ui.h>
# include <defs.h>
# include <message.h>
# include <timer.h>
# include <DataStore.h>

# define STRLEN	30
# define BADVAL -32768
# define BUFLEN	1000
# define MAXOURS 10
# define KM_PER_NM (6080.0 / (5280.0 * 0.621))
# define M_PER_FT 0.30480
# define A_FEW 20
# define PI 3.141592654
# define R_EARTH 6372.0


/*
 * Structure to hold converted black box aircraft data.
 */
typedef struct acdata
{
	int	transponder;
	float	altitude;	
	float	latitude, longitude;
} Ac_Data;


/*
 * Structure to hold raw black box aircraft data.
 */
typedef struct packet
{
	short	range;
	short	azimuth;
	short	transponder;
	short	altitude;
} Packet;

/*
 * List of our aircraft and their current transponder codes.
 */
typedef struct ourac
{
	char	platform[STRLEN];
	char	transponder[STRLEN];
} OurAc;

	
/*
 * Global stuff for the aircraft black box ingest module.
 */
char	BlackBox[STRLEN], DialOut[STRLEN], OurAircraft[BUFLEN];
char	*Command = "1";
char	*Startup = "g f2000";
char	*Kill = "6";
char	*SendAll = "2";
char	Ac_Platform[STRLEN];
int	BaudRate; 
float	RangeRes, AzimuthRes;
float	RadarLat, RadarLon;

/*
 * File descriptor of device file where black box is.
 */
int	Fd;

/*
 * The data object to store.
 */
DataObject	Dobj;

/*
 * Command constants go here.
 */
# define AIC_GO		1
# define AIC_DIAL	2
