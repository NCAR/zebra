/*
 * The serial link grabber protocol.
 */

# define SL_DATA	1		/* Data			*/



typedef struct _sldata
{
	int	sl_type;		/* == SL_DATA		*/
	int	sl_len;			/* The length		*/
	char	sl_data[1];		/* Actual data		*/
} sldata;
