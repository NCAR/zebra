/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 FS.C 4-Apr-95,10:17:58,`JUDYK' McIDAS API Fortran/C Interface Functions */
/* 2 FS.C 20-Apr-95,12:59:22,`JUDYK' fix doc from void to int Mcstrtofs(...) */
/* 3 FS.C 2-May-95,16:44:04,`JUDYK' change strlen to STRLEN, etc.            */
/* 4 FS.C 8-May-95,22:53:56,`JUDYK' add const                                */
/* 5 FS.C 6-Jun-95,15:02:08,`USER' Released                                  */
/* 6 FS.C 28-Jun-95,17:18:28,`JUDYK' change ddest_() to Mcdprintf            */
/* 7 FS.C 15-Jan-96,14:48:28,`USER' Released                                 */
/* 8 FS.C 19-Feb-96,15:47:50,`DWS' reglue: modified file                     */
/* 9 FS.C 20-Feb-96,11:52:56,`USER' Released                                 */
/* 10 FS.C 17-Jul-96,11:20:34,`BILLL' Added programmer documementation.      */
/* 11 FS.C 6-Sep-96,10:17:10,`USER' Released                                 */
/**** McIDAS Revision History *** */

/*==============================fs.c==========================================*/
/*
        fs.c       - McIDAS API Fortran/C Interface Functions
                     Functions for handling conversion of fortran character*(*)
                     parameters to C strings, and vice versa.
*/
/*----------------------------------------------------------------------------*/

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "mcidas.h"
#include "m0arg.h"

/*==============================Mcstrtofs=====================================*/
/*
*$ Name:
*$      Mcstrtofs - Copies C string to fixed length, blank-padded, char block.
*$
*$ Interface:
*$      #include "mcidas.h"
*$
*$      int
*$      Mcstrtofs(char* mem, const char* str, int siz_mem) 
*$
*$ Input:
*$      str      - C string to be copied, i.e. source of copy.
*$      siz_mem  - Size of destination character array.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      mem      - Pointer to character array, destination of copy.
*$
*$ Return values:
*$       0       - Copy is successful.
*$      -1       - Truncation error.  (C string length is greater than
*$                 dimension of the destination array.)
*$      < -9999  - failure statuses
*$
*$ Remarks:
*$      This function is used to convert a C string to a Fortran character*(*)
*$      field.
*$      If mem = str, then this function changes the input null-terminated
*$      string to one which is blank-terminated.
*$
*$ Categories:
*$      UTILITY
*/

int                                           /* returned status code         */
Mcstrtofs(                                    /* Copy String to Fortran       */
   char       *  mem,                         /* given pointer to char array  */
   const char *  str,                         /* given C string to be copied  */
   int           siz_mem)                     /* given size of "mem"          */
{
   int           status  = 0;                 /* init to successful status    */
   size_t        len_str = STRLEN(str);       /* compute actual string length */
/*----------------------------------------------------------------------------*/
   if (siz_mem < 0) {                         /* validate given parameter     */
      PROG_ERROR(parameter,                   /* print standard diagnostic    */
         "invalid size for destination char array, siz_mem=%d", siz_mem);
      return PARM_RETERR;                     /* return invalid parm status   */
   }

   if (!mem) {                                /* validate given parameter     */
      PROG_ERROR(parameter,                   /* print standard diagnostic    */
         "missing char *value - can't copy str = %s", str);
      return PARM_RETERR;                     /* return invalid parm status   */
   }
/*----------------------------------------------------------------------------*/

   if (len_str < siz_mem) {                   /* strlen < desired size ?      */

                                   /* blank fill string from null char to end */
      memset(mem + len_str, SPACE, siz_mem - len_str);

   }

/*----------------------------------------------------------------------------*/
   else if (len_str > siz_mem) {              /* strlen > desired size ?      */

/*    Print debug message:
      progname* (Mcstrtofs.c): String of length nnn truncated to character*nn:
      progname* "str........................................."
*/
      Mcdprintf("(%.38s): String of length %d truncated to character*%d:\n",
                __FILE__, len_str, siz_mem );
      Mcdprintf("\"%.*s\"\n", (SIZDEST-3), str);

      status  = -1;                           /* return error status          */
      len_str = siz_mem;                      /* truncate exceeded length     */

   }
/*----------------------------------------------------------------------------*/

   memmove(mem, str, len_str);                /* copy string for its length   */

   return status;                             /* return the status code       */
}
/*==========================end Mcstrtofs=====================================*/

/*==============================Mcarrtofs=====================================*/
/*
*$ Name:
*$      Mcarrtofs - Copies array of C strings to 2-dimensional char array,
*$                  left-justified, blank-padded, and non-null-terminated.
*$
*$ Interface:
*$      #include "mcidas.h"
*$
*$      int
*$      Mcarrtofs(const char* const* str, int nstr,
*$                int nele, int el_size, char* arr)
*$
*$ Input:
*$      str      - Array of C strings to be copied.
*$                 It is a one-dimensional array of pointers to char.
*$      nstr     - Number of C strings, str[nstr].
*$      nele     - Number of string vectors available in the output array,
*$                  arr[nele][el_size].
*$      el_size  - Number of characters available for each string,
*$                  arr[nele][el_size].
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      arr      - Two-dimensional char array, to which strings are copied.
*$
*$ Return values:
*$        0      - All strings were copied to the output array.
*$      < 0      - # of C strings that were truncated, or dropped
*$      < -9999  - failure statuses
*$
*$ Remarks:
*$      This function is used to convert a C string array to a Fortran
*$      character array.
*$      It blank fills the string vectors that are not filled
*$       when nele > nstr.
*$      Uses Mcstrtofs() to convert each string.
*$
*$ Categories:
*$      UTILITY
*/

int                                           /* returned status code         */
Mcarrtofs(                                    /* Copy String Array to Fortran */
   const char * const *  str,                 /* string array to be copied    */
   int                   nstr,                /* given # strings in array     */
   int                   nele,                /* given dimension size of arr  */
   int                   el_size,             /* given output char size       */
   char               *  arr)                 /* char[nele][el_size] array    */
{
   int                   status;              /* returned status code         */
   int                   stat;                /* work status code             */
   ptrdiff_t             lenfill;             /* length of blank filling      */
   char               *  fs  = arr;           /* loop ptr to char[][] array   */
   const char * const *  cs  = str;           /* loop ptr to C string array   */
   const char * const *  end = str + MIN(nstr, nele);      /* loop ending ptr */
/*----------------------------------------------------------------------------*/
   if (!str  &&  nstr > 0) {                  /* validate given parameter     */
      PROG_ERROR(parameter,                   /* print standard diagnostic    */
         "missing char **str - can't convert array of nstr=%d", nstr);
      return PARM_RETERR;                     /* return invalid parm status   */
   }

   if (nele < 0)  nele = 0;
/*----------------------------------------------------------------------------*/

   status = (nstr>nele) ? (nele-nstr) : (0);

   for (;  cs < end;  cs++) {                 /* copy each C string           */
      stat = Mcstrtofs(fs, *cs, el_size);     /*  to output char[][] array    */
      if (FATAL_STAT(stat)) {
         status = stat;
         break;
      }
      status += stat;
      fs     += el_size;                      /* incre ptr to char[][] array  */
   }

                                              /* blank out remaining of array */
   lenfill = arr + (nele * el_size) - fs;
   if (fs  &&  lenfill > 0)  memset(fs, ' ', lenfill);

   return status;                             /* return the status code       */
}
/*==========================end Mcarrtofs=====================================*/

/*==============================Mcfstoarr=====================================*/
/*
*$ Name:
*$      Mcfstoarr - Creates an array of pointers to strings terminated by nulls.
*$                  The strings are in a two dimensional array.
*$                  There may be leading or trailing blanks in each of the
*$                  input strings.
*$
*$ Interface:
*$      #include "mcidas.h"
*$
*$      char**
*$      Mcfstoarr(const char* arr, int nele, int el_size)
*$
*$ Input:
*$      arr      - Two-dimensional char array, containing C strings.
*$      nele     - Number of string vectors in arr - arr[nele][el_size].
*$      el_size  - Number of characters available in arr for each
*$                 string - arr[nele][el_size].
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      Pointer to a newly malloc()ed array, or NULL if error.
*$      The array contains pointers to strings.
*$
*$ Remarks:
*$      Use this function to convert a Fortran character array to an array
*$      of pointers null terminated strings.
*$      Use Mcfreearr() to free the returned array of pointers to strings.
*$
*$ Categories:
*$      UTILITY
*/

char**                                        /* returned C string array      */
Mcfstoarr(                                    /* Copy Fortran to String Array */
   const char *  arr,                         /* char[nele][el_size] array    */
   int           nele,                        /* given dimension size of arr  */
   int           el_size)                     /* given char*(*) size of arr   */
{
   const char *  fs;                          /* loop ptr to char[][] array   */
   char      **  cs;                          /* loop ptr to C string array   */
   char      **  endcs;                       /* ptr to loop end              */
   char      **  str;                         /* malloc()ed string array      */
/*----------------------------------------------------------------------------*/
   if (!arr)       return NULL;               /* return NULL if no array      */
   if (nele <= 0)  return NULL;               /* return NULL if no elements   */

   if (el_size < 0) {                         /* validate given parameter     */
      PROG_ERROR(parameter,                   /* print standard diagnostic    */
         "invalid size for char*(*) array, el_size=%d", el_size);
      return NULL;                            /* return NULL if error         */
   }
/*----------------------------------------------------------------------------*/
                                              /* allocate space for the array */
   MALLOC_RC(str, nele * sizeof(*str), NULL);

   fs    = arr;                               /* init ptr to char[][] array   */
   endcs = str + nele;                        /* init ptr to loop end         */

   for (cs = str;  cs < endcs;  cs++) {       /* for each character*(*),      */
      *cs = fsalloc(fs, el_size);             /*  create a new C string       */
      fs += el_size;                          /* incre ptr to char[][] array  */
   }

   return str;                                /* return ptr to C string array */
}

/*==========================end Mcfstoarr=====================================*/

/*==============================Mcfreearr=====================================*/
/*
*$ Name:
*$      Mcfreearr - Free the given array, and all the memory pointed to
*$                  by the pointers in the array.
*$
*$ Interface:
*$      #include "mcidas.h"
*$
*$      void
*$      Mcfreearr(char** str, int nstr)
*$
*$ Input:
*$      str   - Array of pointers to be freed.
*$      nstr  - Number of pointers to memory which is to be freed.
*$
*$ Input and Output:
*$      none
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      none
*$
*$ Remarks:
*$      Frees all of the array's pointers, and then frees the array itself.
*$
*$ Categories:
*$      UTILITY
*/

void
Mcfreearr(                              /* Free Array of Pointers             */
   char **  str,                        /* given array of pointers            */
   int      nstr)                       /* given number of pointers           */
{
   Mcfreestrs(str, nstr);               /* free each pointer within the array */

   free(str);                           /* free the array itself              */
}
/*==========================end Mcfreearr=====================================*/

/*==============================Mcfreestrs====================================*/
/*
*$ Name:
*$      Mcfreestrs - Free the given array's pointers, without freeing the array.
*$
*$ Interface:
*$      #include "mcidas.h" 
*$
*$      void
*$      Mcfreestrs(char** str, int nstr)
*$
*$ Input:
*$      nstr  - Given number of pointers.
*$
*$ Input and Output:
*$      str   - Given array of pointers to be freed, returns array of NULL's.
*$
*$ Output:
*$      none
*$
*$ Return values:
*$      none
*$
*$ Remarks:
*$      Frees all of the array's pointers, and reinitializes them to NULL.
*$
*$ Categories:
*$      UTILITY
*/

void
Mcfreestrs(                             /* Free Array Pointers                */
   char        **  str,                 /* given array of pointers            */
   int             nstr)                /* given number of pointers           */
{
   char * const *  endstr = str + nstr; /* ptr to loop end                    */

   if (!str)  return;                   /* return if no array given           */

   for (; str < endstr; str++) {
      free(*str);                       /* free each pointer within the array */
      *str = NULL;
   }
}
/*==========================end Mcfreestrs====================================*/

/*==========================end fs.c==========================================*/
