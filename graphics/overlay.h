/* 5/87 jc */
/*
 * The description of a graphical overlay.
 */
struct overlay
{
	struct workstation *ov_ws;	/* The workstation for this overlay */
	int ov_priority;		/* Overlay priority		*/
	int ov_flags;			/* Flag word -- see below	*/
	int ov_number;			/* Hardware overlay number	*/
	struct pixmap *ov_pmap;		/* Pixel map, if applicable	*/
	struct oplist *ov_ops;		/* Othewise op list		*/
	struct oplist *ov_aop;		/* Additive op pointer		*/
	int ov_naop;			/* Additive op index		*/
	float ov_x0, ov_x1;		/* World X coords		*/
	float ov_y0, ov_y1;		/* World Y coords		*/
	int ov_cx0, ov_cx1;		/* Clip window X values		*/
	int ov_cy0, ov_cy1;		/* Clip window Y values		*/
	struct overlay *ov_next;	/* Next overlay in list		*/
};



/*
 * The overlay flags.
 */
# define OVF_PIXMAP	0x0001		/* This is a pixel-map overlay	*/
# define OVF_VISIBLE	0x0002		/* This overlay is visible	*/
# define OVF_MODIFIED	0x0004		/* Overlay has been changed	*/
# define OVF_ADDITIVE	0x0008		/* Changes are additive		*/
# define OVF_EMPTY	0x0010		/* No data here			*/
