/*
 * Implement a null device.
 */
# include "config.h"

# ifdef DEV_NULL

# include "graphics.h"
# include "device.h"


int
nl_open (device, type, ctag, dev)
char *device, *type, **ctag;
struct device *dev;
{
	*ctag = 0;
	return (GE_OK);
}



int nl_ok ()
{
	return (GE_OK);
}


# endif /* DEV_NULL */
