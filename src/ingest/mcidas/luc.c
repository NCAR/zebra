#if defined(UNDERSCORE)
  long int luc_(long int *);
#else
  long int luc(long int *);
#endif
#if defined(UNDERSCORE)
  long int luc_(long int *index)
#else
  long int luc(long int *index)
#endif

/**** McIDAS-AIX Revision History *** */
/* 1 LUC.C 25-Sep-90,13:47:26,`DAVES' First release                          */
/* 2 LUC.C 27-Sep-90,12:32:58,`DAVES' First release                          */
/* 3 LUC.C 11-Feb-91,14:55:22,`SMG' add conditional compile option           */
/* 4 LUC.C 9-Jul-91,15:08:10,`SUEG' conform to ANSI standards                */
/* 5 LUC.C 16-Feb-92,14:49:10,`USER' Released for McIDAS-X Only              */
/**** McIDAS-AIX Revision History *** */
/*
   Return value from UC
*/
{
   extern long int *uc;
   extern long int neguc[];
   if ( *index < 0) 
      return(neguc[- (*index)]);
   else
      return(uc[*index]);
}
