	
#if defined(sun)
        long laddr_(long);
#else
	long laddr(long);
#endif

#if defined(sun)
	long laddr_(long k) 
#else
	long laddr(long k)
#endif
/* *** McIDAS-AIX Revision History ****/
/* *** McIDAS-AIX Revision History ****/
	{
	   return(k);
	}
