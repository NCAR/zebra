/*
 * Access to low-level image stuff.
 */

/* # include <X11/Intrinsic.h> */

# ifdef __cplusplus
extern "C" {
# endif

/*
 * Here is the structure we pass around to represent a destination
 * image array.  With any luck at all, users of these images are not
 * aware of just what kind of array they are using.
 */

typedef enum { RI_ShmImage, RI_XImage, RI_MemImage } RImageType;

typedef struct _DestImage
{
/* Private stuff, please don't mess with it */
	RImageType	di_type;	/* What type of image?	*/
	int		di_x, di_y;	/* Destination position	*/
	XImage		*di_ximage;	/* The ximage we use	*/
	int		di_frame;	/* draw frame we may use */
/* Public stuff, but if you change it you deserve the mess you get */
	int		di_bdepth;	/* Byte depth		*/
	void		*di_image;	/* The image data	*/
	int		di_ioffset;	/* Initial offset	*/
	int		di_bpl;		/* Bytes per scan line	*/
	int		di_w, di_h;	/* Width and height	*/
	int		di_needswap;	/* Need to swap bytes	*/
	int		di_transparent; /* Attempting transparency */
} DestImage;
  

/*
 * Public routines.
 */
extern DestImage *
ri_CreateImage (int frame, int x, int y, int w, int h, int transparent);
extern DestImage *ri_GetDestImage (int, int, int, int, int);
extern DestImage *ri_MakeMemImage (int, int, void *, int, int);	
extern void ri_ShipImage (DestImage *);


# ifdef __cplusplus
};   /* Extern "C" */
# endif
