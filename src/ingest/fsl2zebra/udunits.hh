# ifndef _UDUNITS_HH_
# define _UDUNITS_HH_

extern "C" 
{
# include <udunits.h>
}


//
// This null-parameter version of utInit() can be called multiple
// times 
//
static int utInitCalled = 0;

inline int
utInit()
{
    if (! utInitCalled)
    {
	int status = utInit(NULL);
	if (status != 0)
	    return status;
	utInitCalled = 1;
    }
}

# endif // ndef _UDUNITS_HH_
    
