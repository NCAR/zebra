/* $Id: RasterFile.h,v 2.0 1991-07-18 22:53:23 corbet Exp $ */
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
# define RF_MAGIC	0x910714
# define RF_OLDMAGIC	0x910702
# define MaxRFField	10		/* Max number of fields	*/

typedef struct s_RFHeader
{
	int	rf_Magic;		/* == RF_MAGIC			*/
	char	rf_Platform[40];	/* Name of platform stored here */
	int	rf_MaxSample;		/* Max number of samples	*/
	int	rf_NSample;		/* How many we have		*/
	int	rf_NField;		/* How many fields		*/
	RFField rf_Fields[MaxRFField];	/* Fields			*/
	int	rf_Flags;		/* Flag info			*/
} RFHeader;

# define RFF_COMPRESS	0x0001		/* Images are compressed	*/

/*
 * Immediately after the header comes the table of contents, which is
 * dimensioned by the MaxSample header field.
 */
typedef struct s_RFToc
{
	time	rft_Time;		/* Time of this image		*/
	long	rft_Offset[MaxRFField];	/* It's place in the file	*/
	long	rft_Size[MaxRFField];	/* Length			*/
	RGrid	rft_Rg;			/* Geometry info		*/
	Location rft_Origin;		/* Image origin			*/
	int	rft_AttrLen;		/* Length of attrs		*/
	long	rft_AttrOffset;		/* Where they are		*/
} RFToc;

typedef struct s_OldRFToc
{
	time	rft_Time;		/* Time of this image		*/
	long	rft_Offset[MaxRFField];	/* It's place in the file	*/
	long	rft_Size[MaxRFField];	/* Length			*/
	RGrid	rft_Rg;			/* Geometry info		*/
	Location rft_Origin;		/* Image origin			*/
} OldRFToc;
