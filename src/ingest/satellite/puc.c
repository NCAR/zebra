
#if defined(sun)
  void puc_(int *, int *);
#else
  void puc(int *, int *);
#endif


#if defined(sun)
  void puc_(int *value, int *index)
#else
  void puc(int *value, int *index)
#endif

/**** McIDAS-AIX Revision History *** */
/* 1 PUC.C 25-Sep-90,13:47:28,`DAVES' First release                          */
/* 2 PUC.C 27-Sep-90,12:33:02,`DAVES' First release                          */
/* 3 PUC.C 11-Feb-91,14:55:22,`SMG' add conditional compile option           */
/* 4 PUC.C 5-Mar-91,9:38:14,`DAVES' Add CURAIX                               */
/* 5 PUC.C 6-Mar-91,9:24:48,`SMG' fix if block                               */
/* 6 PUC.C 11-Jul-91,7:50:50,`SUEG' conform to ANSI standards                */
/* 7 PUC.C 19-Sep-91,14:33:02,`SUEG' remove sendevnt call                    */
/* 8 PUC.C 16-Feb-92,14:49:02,`USER' Released for McIDAS-X Only              */
/**** McIDAS-AIX Revision History *** */
/*
   Set value in UC
*/
{
   extern long int *uc;
   extern long int neguc[];
   if (*index >= 0) {
      uc[*index] = *value;
    }
   else
      neguc[ - (*index)] = *value;
}
