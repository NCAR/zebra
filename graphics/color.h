/*
 * Information pertaining to color management.
 */
struct colors
{
	int c_base;		/* Base of a range of free colors.	*/
	int c_nc;		/* Number of colors available		*/
	struct colors *c_next;	/* Next entry in the chain.		*/
};
