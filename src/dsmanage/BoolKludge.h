//
// The following ugliness brought to you by the latest GNU C++ library,
// which makes assumptions about the "bool" type that are different from
// those made in defs.h.  This workaround commits us to libg++, but that
// was pretty much the case anyway.
//
# include <_G_config.h>
# ifdef _G_HAVE_BOOL
# undef _G_HAVE_BOOL
# define _G_HAVE_BOOL 0
# endif
//
// Now back to our regularly-scheduled event.
//
