# ifdef XSUPPORT
/* 
 * Implementation of forms.
 */
# include <X11/Intrinsic.h>
# include <X11/Xaw/Cardinals.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Label.h>

# include "ui.h"
# include "ui_param.h"
# include "ui_globals.h"
# include "ui_commands.h"
# include "ui_window.h"
# include "ui_error.h"
# include "ui_loadfile.h"

static char *Rcsid = "$Id: ui_wForm.c,v 1.1 1992-01-28 21:25:48 corbet Exp $";
