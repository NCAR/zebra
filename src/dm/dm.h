/* $Id: dm.h,v 1.2 1990-03-19 11:01:34 corbet Exp $ */
/*
 * Display manager stuff.
 */

# define DM_RECONFIG	1
# define DM_SUSPEND	2
# define DM_HELLO	3
# define DM_DIE		4
# define DM_BUTTON	5
# define DM_EXCH	6
# define DM_PDCHANGE	7

/*
 * The message structure sent out by the display manager.
 */
# define PDLEN 40
struct dm_msg
{
	int	dmm_type;		/* the type of this message	*/
	int	dmm_x, dmm_y;		/* New window location		*/
	int	dmm_dx, dmm_dy;		/* New window size		*/
	char	dmm_pdesc[PDLEN];	/* Plot description name	*/
};



/*
 * Hello message to be received from dm clients.
 */
struct dm_hello
{
	int dmm_type;
	Window dmm_win;
};

/*
 * PD change command.
 */
struct dm_pdchange
{
	int	dmm_type;		/* Message type = DM_PDCHANGE	*/
	char	dmm_pdesc[PDLEN];	/* New pd			*/
};


/*
 * Button event message.
 */
struct dm_button
{
	int dmm_type;
	int dmm_button;		/* The number of the button */
};
