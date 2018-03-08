/* 5/87 jc */
/*
 * The top level workstation description.
 */
struct workstation
{
	struct device *ws_dev;		/* Pointer to device info	*/
	struct overlay *ws_overlay;	/* The overlay chain		*/
	char *ws_tag;			/* Device-level returned tag	*/
	struct pixmap *ws_pmap;		/* Master pixel map.		*/
	struct colors *ws_color;	/* Color map info		*/
};
