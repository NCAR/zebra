/*
 * Provide definitions for global variables.
 */
static char *rcsid = "$Id: ui_globals.c,v 1.1 1990-09-07 11:28:11 corbet Exp $";

# ifdef VMS
#	define var globaldef
# 	include <string.h>
# else
#	define var
# endif

# include "ui_param.h"
# include "ui_globals.h"
