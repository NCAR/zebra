/*
 * $Id: DataStore.h,v 1.1 1990-10-22 16:04:10 corbet Exp $
 *
 * Public data store definitions.
 */


/*
 * Possible data organizations.
 */
typedef enum {
	OrgUnknown	= 0,
	OrgRGrid	= 1,
	OrgIRGrid	= 2,
	OrgScalar	= 3,
	OrgImage	= 4,
	OrgOutline	= 5,
} DataOrganization;
