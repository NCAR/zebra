/*
 * File describing fixed GENPRO files.
 */

#ifndef __zeb_fixed_gp_h_
#define __zeb_fixed_gp_h_

/*
 * The header structure.
 */

struct gp_header
{
	int	gph_magic;		/* The fixed GENPRO magic number */
	int	gph_nfield;		/* The number of fields in the file */
	int	gph_ntoc;		/* The number of TOC entries	*/
	int	gph_tocoff;		/* Where to find the TOC.	*/
	int	gph_reclen;		/* Data record length		*/
	UItime	gph_begin;		/* Begin time			*/
	UItime	gph_end;		/* end time			*/
};
# define GPH_MAGIC	0x900612


/*
 * The field table contains these entries:
 */
# define MAXFNAME	12
# define MAXFDESC	100

struct gp_field
{
	char	gpf_name[MAXFNAME];	/* The field name		*/
	char	gpf_desc[MAXFDESC];	/* Description of this field	*/
	char	gpf_unit[MAXFNAME];	/* Units of this field		*/
	int	gpf_offset;		/* Offset into the data rec	*/
};



/*
 * The table of contents format.
 */
struct gp_toc
{
	int	gpt_when;		/* The time of this entry	*/
	int	gpt_offset;		/* Where to go in the file	*/
};


/*
 * The data record format.
 */
struct data_rec
{
	int	d_time;			/* Time of this data		*/
	float	d_data[1];		/* The data itself		*/
};

#endif /* !__zeb_fixed_gp_h_ */
