/* $Id: RasterFile.h,v 1.2 1991-04-11 22:01:05 corbet Exp $ */
/*
 * This is the file for FCC native raster files.  Note that these are not
 * (necessarily) *image* files -- they are simply a form of gridded data.
 */

/*
 * Fields are described by the following:
 */
typedef struct s_RFField
{
	char 	rff_Name[20];		/* Name of the field		*/
	char	rff_Desc[40];		/* It's description		*/
	ScaleInfo rff_Scale;		/* Scaling info			*/
} RFField;


/*
 * The file header.
 */
# define RF_MAGIC	0x910226
# define MaxRFField	10		/* Max number of fields	*/

typedef struct s_RFHeader
{
	int	rf_Magic;		/* == RF_MAGIC			*/
	char	rf_Platform[40];	/* Name of platform stored here */
	int	rf_MaxSample;		/* Max number of samples	*/
	int	rf_NSample;		/* How many we have		*/
	int	rf_NField;		/* How many fields		*/
	RFField rf_Fields[MaxRFField];	/* Fields			*/
} RFHeader;


/*
 * Immediately after the header comes the table of contents, which is
 * dimensioned by the MaxSample header field.
 */
typedef struct s_RFToc
{
	time	rft_Time;		/* Time of this image		*/
	long	rft_Offset;		/* It's place in the file	*/
	RGrid	rft_Rg;			/* Geometry info		*/
	Location rft_Origin;		/* Image origin			*/
} RFToc;
