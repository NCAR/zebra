/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 M0ARG.H 4-Apr-95,10:20:24,`JUDYK' McIDAS Arg-Fetching API Header File   */
/* 2 M0ARG.H 2-May-95,16:48:48,`JUDYK' add STRLEN, chg all fun() to fun(void)*/
/* 3 M0ARG.H 4-May-95,13:42:44,`JUDYK' MALLOC/REALLOC fix for size=0         */
/* 4 M0ARG.H 8-May-95,22:55:02,`JUDYK' add const                             */
/* 5 M0ARG.H 11-May-95,20:08:30,`JUDYK' add const                            */
/* 6 M0ARG.H 6-Jun-95,15:06:22,`USER' Released                               */
/* 7 M0ARG.H 28-Jun-95,17:17:24,`JUDYK' add MCIDAS_SYNTAX_CHARS, etc.        */
/* 8 M0ARG.H 15-Jan-96,14:49:36,`USER' Released                              */
/* 9 M0ARG.H 19-Feb-96,15:57:54,`DWS' reglue: modified file                  */
/* 10 M0ARG.H 20-Feb-96,11:57:32,`USER' Released                             */
/* 11 M0ARG.H 1-Aug-96,16:39:54,`DGLO' Add unbalanced quote error (#6742)    */
/* 12 M0ARG.H 21-Oct-96,16:24:22,`USER' Released                             */
/**** McIDAS Revision History *** */

/*==============================m0arg.h=======================================*/

#ifndef    _M0ARG_H
#define    _M0ARG_H    /* ARGAPI:  McIDAS argument-fetching header file       */

/*----------------------------------------------------------------------------*/
/*
                 List of McIDAS API Argument-Fetching Software            6/5/95
                 ---------------------------------------------

m0arg.h          - McIDAS Argument-Fetching Header File

arghelp.pgm      - Lists on-line help for McIDAS command line syntax.

cmdfetch.c       - McIDAS Command Line Argument-Fetching Functions
   Mccmd         - Build and return the current McIDAS command line.
   mccmd_
   Mccmdnam      - Fetch all keyword names occurring in the command line.
   mccmdnam_
   Mccmdnum      - Return # values associated with given command line keyword
   mccmdnum_
   Mccmdstr      - Fetch a program command line argument in character form.
   mccmdstr_       Prints diagnostics to edest device for standard errors.
   Mccmdquo      - Fetch the quote field string command line argument.
   mccmdquo_       Prints diagnostics to edest device for standard errors.
   Mccmdint      - Fetch a program command line argument in integer type form
   mccmdint_       Prints diagnostics to edest device for standard errors.
   Mccmddbl      - Fetch a program command line argument in double type form.
   mccmddbl_       Prints diagnostics to edest device for standard errors.
   Mccmdiyd      - Fetch a program argument in integer type date form yyyyddd
   mccmdiyd_       Prints diagnostics to edest device for standard errors.
   Mccmdihr      - Fetch a program argument in integer type time form hhmmss.
   mccmdihr_       Prints diagnostics to edest device for standard errors.
   Mccmddhr      - Fetch a program argument in fractional hours form hh.fffff
   mccmddhr_       Prints diagnostics to edest device for standard errors.
   Mccmdill      - Fetch program argument in integer type lat/lon form ddmmss
   mccmdill_       Prints diagnostics to edest device for standard errors.
   Mccmddll      - Fetch program argument in fractional lat/lon form dd.fffff
   mccmddll_       Prints diagnostics to edest device for standard errors.

argfetch.c       - Text Argument-Fetching Functions
   Mcargstr      - Fetch an argument in character form.
   mcargstr_
   Mcargquo      - Fetch the quote field string argument.
   mcargquo_
   Mcargint      - Fetch an argument in integer type format.
   mcargint_
   Mcargdbl      - Fetch an argument in double type format.
   mcargdbl_
   Mcargiyd      - Fetch an argument in integer type date format yyyyddd.
   mcargiyd_
   Mcargihr      - Fetch an argument in integer type time format hhmmss.
   mcargihr_
   Mcargdhr      - Fetch an argument in double fractional hours form hh.fffff
   mcargdhr_
   Mcargill      - Fetch an argument in integer type lat/lon format dddmmss.
   mcargill_
   Mcargdll      - Fetch argument in double fractional lat/lon form ddd.fffff
   mcargdll_

argconv.c        - Argument-Fetching Conversion Functions
   Mcstrtoint    - Convert given numeric token to integer type format.
   mcstrtoint_
   Mcstrtodbl    - Convert given numeric token to double type format.
   mcstrtodbl_
   Mcstrtohex    - Convert given hexadecimal token to integer type format.
   mcstrtohex_
   Mcstrtoiyd    - Convert given date token to integer date format yyyyddd.
   mcstrtoiyd_
   Mcstrtohms    - Convert given time to integer hours, minutes and seconds.
   mcstrtohms_
   Mcstrtoihr    - Convert given time token to integer time format hhmmss.
   mcstrtoihr_
   Mcstrtodhr    - Convert time token to double fractional hours hh.fffff.
   mcstrtodhr_
   Mcstrtoill    - Convert given lat/lon token to integer type format dddmmss
   mcstrtoill_
   Mcstrtodll    - Convert given token to double fractional lat/lon ddd.fffff
   mcstrtodll_
   M0strtoken    - Get ptr to begin, end, and length of first token in string

argerror.c       - Argument-Fetching Error Reporting Functions
   M0argerr      - Print arg-fetching diagnostic messages to edest device,
                   checking the arg-fetching status code for standard errors.
   m0argerr_
   Mcitonth      - Format the given number into string of "nth".

argkey.c         - Argument-Fetching Keyword Validation Functions
   Mccmdkey      - Validate command line keywords, printing errors to edest.
   mccmdkey_
   Mcargkey      - Validate arg-fetching keywords, optionally printing errors
   mcargkey_
   chkcmdkey     - Validate given keywords array with given allowed keywords.
   chkvalkey     - Validate given array of allowed keywords.
   M0uniquekey   - Compare two keyword names to determine if they are unique.
   M0legalkey    - Test given keyword name for legal format. 

argparse.c       - Argument-Fetching Parsing Functions
   M0cmdparse    - Parse the given McIDAS command into arg-fetching structure
   m0cmdparse_
   Mcargparse    - Parse the given text into arg-fetching structure.
   mcargparse_
   M0argtok      - Retrieve next argument token within the given text string.
   m0argtok_
   M0argsyntax   - Update and return the default McIDAS syntax definition.

argcmd.c         - Argument-Fetching Command Line Functions
   M0cmdput      - Set McIDAS command argument-fetching to given arg handle.
   m0cmdput_
   M0cmdget      - Get the current McIDAS command's argument-fetching handle.
   m0cmdget_

argglo.c         - Argument-Fetching Global Keyword Functions
   m0cmdglo_     - Process global keywords and their initializations.
   Mcsetdev      - Update user common with the given DEV= arguments.
   mcsetdev_
   Mcdev2uc      - Convert given DEV= character value to its numeric uc value
   mcdev2uc_
   Mcuc2dev      - Convert given DEV= numeric uc value to its character value
   mcuc2dev_

argutil.c        - Argument-Fetching Structure Maintenance Functions
   M0argalloc    - Allocate a new argument-fetching structure and handle.
   M0argaddkw    - Add new keyword to given argument-fetching handle.
   M0argadd      - Add new keyword argument to given argument-fetching handle
   M0argput      - Put given argument to given arg-fetching keyword position.
   m0argput_
   M0argptr      - Get pointer to arg-fetching structure for given handle.
   M0argget      - Get desired argument for given keyword & position.
   M0arggetkw    - Get given handle's first occurring keyword arg structure.
   M0keycmp      - Compare given string with given "key.str" match string.
   m0keycmp_
   M0argstrtab   - Get given keyword's system string table arg-fetching data.
   M0argstrtabset -Set the arg-fetching string table lookup switch.
   m0argstrtabset_
   Mcargfree     - Free parsed arg-fetching structure for the given handle.
   mcargfree_
   Mcargdump     - Display parsed arg-fetching to McIDAS debug destination.
   mcargdump_
   Mcargcmd      - Build and return a McIDAS command line for given handle.
   mcargcmd_
   Mcargnam      - Fetch all keyword names within parsed arg-fetching text.
   mcargnam_
   Mcargnum      - Return # args for given keyword in parsed arg text.
   mcargnum_

                 McIDAS API Arg-Fetching Compatability Software
                 ----------------------------------------------

argold.c         - Argument-Fetching Compatibility Functions
   kwpkey        - Convert old arg-fetching keyword name to new format,
                   and turn on system string table lookup.
   kwnams_       -> mccmdnam_
   nkwp_         -> mccmdnum_
   cqfld_        -> mcargquo_
   ckwp_         -> mccmdstr_
   cpp_          -> ckwp_
   ikwp_         -> Mcargint or mccmdiyd_ or mccmddhr_
   ipp_          -> ikwp_
   dkwp_         -> Mcargdbl or mccmdiyd_ or mccmddhr_
   dpp_          -> dkwp_
   ikwpyd_       -> mccmdiyd_
   ippyd_        -> ikwpyd_
   ikwphr_       -> mccmdihr_
   ipphr_        -> ikwphr_
   mkwp_         -> mccmdihr_
   mpp_          -> mkwp_
   dkwphr_       -> mccmddhr_
   dpphr_        -> dkwphr_
   ikwpll_       -> mccmdill_
   ippll_        -> ikwpll_
   dkwpll_       -> mccmddll_
   dppll_        -> dkwpll_


                   Argument-Fetching Functions Diagrams                   6/5/95
                   ------------------------------------


================================================================================



Command Line Parsing/Fetching                   Text Parsing/Fetching
-----------------------------                   ---------------------

MAIN1          McIDAS Programs                  User Programs
-----          ---------------                  -------------

mcargfree                                       Mcargparse, Mcargfree
m0cmdparse     Mccmdkey                         Mcargkey
m0cmdput       Mccmdxxx("key.word",             Mcargxxx(arg_handle, "key.word",
m0cmdglo                position, printmsg,              position,
                        def, min, max, value)            def, min, max, value,
                                                         argstr) 
               Mccmdstr, Mccmdquo               Mcargstr, Mcargquo
               Mccmdnam, Mccmdnum               Mcargnam, Mcargnum
               Mccmd                            Mcargcmd





                              McIDAS Programs
                                    |
   _________________________________|______________________________
   |                                |                |            |
   |                                |                |            |
  mccmd_                          mccmdkey_        mccmdnam_    mccmdnum_
   |                                |                |            |
  mcargcmd_    mcargkey_            |              mcargnam_    mcargnum_
___|         ____|         _________|            ____|        ____|
|  |         |   |_________|        |            |   |        |   |
|  |         |   |         |        |            |   |        |   |
| Mcstrtofs  | Mcfstoarr Mcfreearr  |            | Mcarrtofs  | fsalloc
|            |   |         |        |            |   |        |
|            | fsalloc   Mcfreestrs |            | Mcstrtofs  |
|            |                      |            |            |
| Mccmd      |                    Mccmdkey       | Mccmdnam   | Mccmdnum
|___|        |______________________|            |___|        |___|
    |                               |                |            |
  Mcargcmd                        Mcargkey-------->Mcargnam     Mcargnum
    |____         __________________|_____           |            |
    |   |         |           |          |           |          M0arggetkw<----|
    | M0keytxt  Mcfreearr   chkcmdkey  chkvalkey     |   _________|___         |
    |             |           |          |           |  |   |        |         |
    |           Mcfreestrs  M0keycmp,  M0legalkey,   |  | M0keycmp M0argstrtab |
    |                       M0keytxt   M0uniquekey   |  |            |_________|
    |                                                |  |            |          
    |________________________________________________|__|          Mcargparse,
                               |                                   Mcargfree,
                             M0argptr                              M0argptr,
                          _____|_____                              M0argaddkw,
                          |         |                              M0argadd
                        M0cmdget  M0handget                          :




================================================================================

      mcargxxx_  (xxx = str,int,dbl,iyd,ihr,dhr,ill,dll)
________|_________________________
|            |        |          |
|          fsalloc  Mcstrtofs  NUMC2F
|                              (except xxx=str)
|
|                   McIDAS Programs
|                         |
|    _____________________|______________________________________
|    |                    |                                     |
|    | (except xxx=str)   |                                     |
|  mccmdxxx_            mccmdstr_          m0argerr_          mccmdquo_
|    |____________    ____|________________   |____       ______|___________
|    |   |       |    |   |       |       |   |   |       |     |          |
|    | fsalloc NUMC2F | fsalloc Mcstrtofs |   | fsalloc   |   mcargquo_  fsalloc
|    |________________|                   |___|___________|     |
|    |                                        |               mcargstr_
|    |                                        |                 :
|    |                                        |
|  Mccmdxxx                                   |               Mccmdquo
|    |______________________________________  |   ______________|
|    |                                     |  |   |             |
Mcargxxx                                   M0argerr           Mcargquo
  |                                           |____             |
  |                                           |   |             |
  |                                           | Mcitonth      Mcargstr
  |________________________________________   |_________________|
  |                                       |   |
  | (except xxx=str)  (plus xxx=hex,hms)  |   |
Mcstrtoxxx <----mcstrtoxxx_             M0argget
  :          _____|___                    |
  :          |       |                  M0arggetkw <---------------------|
  :        fsalloc NUMC2F   ______________|_____________________         |
  :                         |             |                    |         |
  :        m0keycmp_----> M0keycmp      M0argptr             M0argstrtab |
  :          |                      ______|____          ______|_________|
  :        fsalloc                  |         |          |            |
  :                               M0cmdget  M0handget  Mcargparse,  M0argaddkw,
  :                                                    Mcargfree,   M0argadd
  :                                                    M0argptr       :
  :                                                      :
  :                                                       
  =======================================================================
    |         |    |              |            |           |            |
    |         |    |              |            |           |            |
Mcstrtoint    |  Mcstrtoiyd     Mcstrtoill-->Mcstrtoihr  Mcstrtodll-->Mcstrtodhr
____|         |    |_______________________    |                        |_______
|   |         |    |     |   |   |        |    |                        |      |
|   |         |  mktime time | strftime gmtime |                        |      |
|   |         |              |                 |                        |      |
|   |-------->|------<-------(--------<--------(-----------<------------|      |
|   |         |              | ________________|____________    ________|      |
|   |         |              | |   |           |           |    |       |      |
| strtol    Mcstrtodbl       | | Mcdhrtoihr  Mchmstoihr  Mcstrtohms  MCHMS2DHR |
|     ________|____          | |   |____________          ____|_____           |
|     |   |       |          | |   |           |          |   |    |           |
|     | strtod  Mcstrtohex   | | Mcdhrtohms  Mchmstoihr   | time gmtime        |
|     |       ____|          | |                          |                    |
|     |       |   |          | |                          |                    |
|     |       | strtoul      | |                          |                    |
|_____|_______|______________|_|__________________________|____________________|
                             |
                           M0strtoken

================================================================================
                         (AIX/OS2)
     (AIX) isqx_       mckbdx/kbdctl             isqx_  jsqx_ (OS2)
            |                |                   |_______|
           jsqx_           keyin_ ~~~> ki.pgm        |
            |       _________|          |            |
            |_______(________|__________|            |
                    |        |                       |
                  prescn_  jsq_                    sqtok_
                             |                       |
           sqpgm_<---(AIX)---|---(OS2)-------------->|
             |                                       |
           execvp -------> main <----------------- execp
                             |
                           main1_
_____________________________|____________________________________________
|               |            |                     |                     |
| M0cmdparse  m0cmdparse_  main0_(McIDAS programs) |                     |
|   |           |                                  |                     |
|   |         mcargparse_           m0argtok_  mcargfree_  mcargdump_  m0cmdput_
|   |    _______|__________  ____________|         |           |         |
|   |    |      |         |  |           |         |           |         |
| Mcargparse    |       fsalloc          |     Mcargfree   Mcargdump   M0cmdput
|   |           |__________   ___________|       __|__         |         |
|   |           |         |   |          |       |   |         |         |
|   |           |       Mcfstoarr        |       | Mcfreearr   |         |
|   |           |         |              |       |   |         |         |
|   |           |       fsalloc          |       | Mcfreestrs  |         |
|   |           |__________   ___________|       |____         |         |
|   |           |         |   |          |       |   |         |         |
|   |           |       Mcfreearr        |       | M0handfree  |         |
|   |           |         |              |       |____         |         |
|   |           |       Mcfreestrs       |           |         |         |
|   |           |____            ________|           |         |         |
|   |               |            |       |           |         |         |
|   |             NUMC2F       Mcstrtofs |           |         |         |
|   |___________________________________ |           |         |         |
|   |            |        |   |      | | |           |         |         |
| M0argalloc-->M0argaddkw | M0argadd | M0argtok      |         |         |
|   |            |________(___|______|   |           |         |         |
|   |                     |          | M0argsyntax   |         |         |
|   |____________________ |          |               |         |         |
|   |           |       | |          |               |         |         |
| M0handalloc M0handput Mcargfree    |               |         |         |
|                         |          |               |         |         |
|     m0handfree_   ______|_________ |_______________|_________|_________|
|             |     |     |        | |
|           M0handfree  Mcfreearr  M0argptr
|                         |          |__________
|                         |          |         |
|                       Mcfreestrs M0cmdget  M0handget
|                                                         m0argput_
|                                                        ____|______
m0cmdglo_           mcsetdev_                            |         |
  |____________________  |_______                      M0argput  fsalloc
  |         |         |  |      |            ____________|__________
Mccmdstr  Mccmdint  Mcsetdev  fsalloc        |           |         |
  :         :         |                    M0argaddkw  M0argadd  M0arggetkw <--|
                      |                      |___________|         |           |
mcdev2uc_ --------> Mcdev2uc                 |  ___________________|           |
                                             |  |       |          |           |
mcuc2dev_ --------> Mcuc2dev               M0argptr   M0keycmp   M0argstrtab   |
                                             |__________           |___________|
                                             |         |           :          
                            m0cmdget_ ---> M0cmdget  M0handget

================================================================================

                    Compatible McIDAS Programs:

                            cpp_
                             |
kwnams_       cqfld_        ckwp_                                    nkwp_
  |             |            |____________             ________________|
  |             |            |           |_____________|               |
  |             |            |           |             |               |
mccmdnam_     mcargquo_     mccmdstr_  kwpkey        M0argstrtabset  mccmdnum_
                                         |
                                       M0argstrtabset


  ipp_                           dpp_
   |                              |
  ikwp_                          dkwp_
___|_____                _________|___
|  |    |                |        |  |
|  |  Mcargint         Mcargdbl   |  |
|  |______________________________|  |
|  | or |                         |  |
|  |  mccmddhr_                   |  |
|  |______________________________|  |
|    or |          |         |       |
|     mccmdiyd_  M0argput  M0argget  |
|                                    |
|____________________________________|
                       |
         ______________|__________
         |             |         |
       kwpkey        M0argget  M0argstrtabset <---m0argstrtabset_
         |
       M0argstrtabset


ipphr_     mpp_   dpphr_      ippll_      dppll_      ippyd_
  |         |      |           |           |           |
ikwphr_   mkwp_  dkwphr_     ikwpll_     dkwpll_     ikwpyd_
  |_________|      |______     |______     |______     |________________________
  |         |      |     |     |     |     |     |     |         |        |    |
mccmdihr_   |  mccmddhr_ | mccmdill_ | mccmddll_ | mccmdiyd_ M0argget M0argput |
            |            |           |           |                             |
            |____________|___________|___________|_____________________________|
                    |
          __________|________________
          |         |               |
        kwpkey    M0argstrtabset  abort_ (except mkwp_)
          |
        M0argstrtabset

================================================================================

m0handalloc_  Mcluc
  |            |
M0handalloc   luc_

================================================================================
*/
/*----------------------------------------------------------------------------*/

/*==============================INCLUDES======================================*/
/* Include necessary header files                                             */

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

/*==============================DEFINES=======================================*/
/* Define constant variables and macro definitions.                           */

#define  MCHAND                   0      /* McIDAS command line handle        */
                                 
#define  ARGSTRTAB_LOOKUP         0      /* default for string table lookup   */
                                 
#define  SIZDEST                160      /* size of sdest, etc. print buffers */

/*----------------------------------------------------------------------------*/
/* Define McIDAS command line syntax.  (separators, delimiters, etc.)         */

#define  WHITE_SPACE              " "    /* McIDAS white space characters     */
#define  KEYWORD_SEPARATORS       "=,"   /* McIDAS keyword separators         */
#define  COMMAND_SEPARATORS       ";"    /* McIDAS command separators         */
#define  QFLD_DELIMS_LEFT         "{\""  /* left quote field delimiters       */
#define  QFLD_DELIMS_RIGHT        "}"    /* right quote field delimiters      */
#define  QUOTING_DELIMS_LEFT      "'"    /* McIDAS left quoting char delims   */
#define  QUOTING_DELIMS_RIGHT     "'"    /* McIDAS right quoting char delims  */
#define  QUOTING_ESCAPE           ""     /* use quoting char to escape itself */
#define  MISSING_ARGUMENT         "X"    /* McIDAS missing argument           */
#define  MISSING_ARG_VALUE        "x"    /* McIDAS missing arg replacement    */
#define  MCIDAS_SYNTAX_CHARS      WHITE_SPACE                                  \
                                  KEYWORD_SEPARATORS                           \
                                  COMMAND_SEPARATORS                           \
                                  QFLD_DELIMS_LEFT                             \
                                  QFLD_DELIMS_RIGHT                            \
                                  QUOTING_DELIMS_LEFT                          \
                                  QUOTING_DELIMS_RIGHT                         \
                                  QUOTING_ESCAPE

#define  DATE_SEPARATORS          "/-"   /* syntax to separate date parts     */
#define  TIME_SEPARATORS          ":"    /* syntax to separate time parts     */

#define  HEX                      "$"    /* syntax for hex argument           */

#define  EXPONENTS                "EeDd" /* define all McIDAS exponents       */
#define  NONSTD_EXP               "Dd"   /* non-standard exponent notation    */
#define  STD_EXP                  'e'    /* define standard exponent notation */

#define  POSITIVE                 '+'    /* syntax to indicate positive value */
#define  NEGATIVE                 '-'    /* syntax to indicate negative value */
#define  DECIMAL_POINT            '.'    /* syntax to indicate fraction       */

#define  KEYWORD_MINMAX_SEPARATOR '.'    /* define key.word syntax            */
#define  KEYWORD_MATCH_BEGIN      '*'    /* define key* compatability syntax  */

#define  QFLD_KEYWORD             "\""   /* define quote field keyword name   */

#define  POSITIONAL_KEYWORD(kwname)  (!(kwname) || !strcspn(kwname, " "))

/*----------------------------------------------------------------------------*/
/* Define constant variables for keyword validation.                          */

/* define global keywords                                                     */
#define  DEV_GLOKEY  "DEV.ICE"   /* McIDAS printer output destination keyword */
#define  FON_GLOKEY  "FON.T"     /* non-default font selection for plots      */
#define  TCO_GLOKEY  "TCO.LOR"   /* McIDAS keyword for sendtext color #       */
#define  TWI_GLOKEY  "TWI.NDOW"  /* McIDAS keyword for sendtext window #      */
#define  VIR_GLOKEY  "VIR.TUAL"  /* McIDAS graphics virtual frame # keyword   */
#define  MAX_GLOKEY  5           /* number of McIDAS "global" keywords        */
#define  ALL_GLOKEY  DEV_GLOKEY, FON_GLOKEY, TCO_GLOKEY, TWI_GLOKEY, VIR_GLOKEY

#define  APPEND      "A"         /* "DEV=filename A" to append to end of file */
#define  REPLACE     "R"         /* "DEV=filename R" to replace file          */

/*----------------------------------------------------------------------------*/
/* Define M0argtok's returned statuses.                                       */

#define  TOKARG       -1    /* returned token is a "normal" argument          */
#define  TOKKW        -2    /* returned token is a keyword argument           */
#define  TOKQFLD      -3    /* returned token is a quote field arg            */

/*--------------------------ARG_STAT, ARG_STAT_WHERE, FATAL_STAT--------------*/
/* Argument-Fetching Status Codes
   Define positional status codes for argument-fetching routines, and
   Macros for updating and testing arg-fetching status.  Also see M0argerr(). */

#define  ARG_DEF         0  /* argument comes from default                    */
#define  ARG_KEYIN    1000  /* argument comes from command line               */
#define  ARG_STRTAB   2000  /* argument comes from system string table        */

#define  ARG_STR         0  /* character string argument                      */
#define  ARG_INT       100  /* integer argument                               */
#define  ARG_DBL       200  /* decimal argument (double)                      */
#define  ARG_IYD       300  /* date argument                                  */
#define  ARG_IHR       400  /* integer time argument                          */
#define  ARG_DHR       450  /* double time/latlon argument                    */
#define  ARG_ILL       500  /* integer lat/lon argument                       */
#define  ARG_DLL       550  /* double time/latlon argument                    */
#define  ARG_KW        900  /* keyword status                                 */

#define  ARG_QUO        10  /* quote field string argument                    */
#define  ARG_HEX        10  /* hexadecimal argument                           */
#define  ARG_NOW        10  /* current date/time argument                     */
#define  ARG_YEAR       20  /* year within date argument is illegal           */
#define  ARG_MON        30  /* "mon" month within date argument is illegal    */
#define  ARG_MM         40  /* "mm" month within date argument is illegal     */
#define  ARG_DAY        50  /* day of month within date argument is illegal   */
#define  ARG_DDD        60  /* day of year within date argument is illegal    */
#define  ARG_HOUR       20  /* hours/degrees in int time/latlon is illegal    */
#define  ARG_MIN        30  /* minutes in integer time/latlon arg is illegal  */
#define  ARG_SEC        40  /* seconds in integer time/latlon arg is illegal  */

#define  ARG_INVALID     1  /* illegally formatted (invalid char)             */
#define  ARG_INVDEC      2  /* illegal fractional integer                     */
#define  ARG_ILLEGAL     3  /* exceeds system limits                          */
#define  ARG_LT          4  /* out-of-range argument < given min              */
#define  ARG_GT          5  /* out-of-range argument > given max              */
#define  ARG_NONE        9  /* argument not there                             */

#define  FATAL_STAT(status)  ((status) < -9999)

#define  ARG_STAT(status, update)  ((status)>=0 ? (status)+(update) :          \
                            !FATAL_STAT(status) ? (status)-(update) : (status))

#define  ARG_STAT_WHERE(status, where)                                         \
 ((where)>=0 ? ARG_STAT(status, where) : FATAL_STAT(where) ? (where) : (status))

/*--------------------------ARG_RANGE_TEST------------------------------------*/
/* Test arg-fetching's given range, and return the updated status code.       */

#define  ARG_RANGE_TEST(status, min, max, value)                               \
   (((min) > (max)  ||  (status) < 0) ? (status)             :                 \
    (value) > (max)                   ? -((status) + ARG_GT) :                 \
    (value) < (min)                   ? -((status) + ARG_LT) : (status))

/*------------------------------xxx_RETERR------------------------------------*/

#define  MALLOC_RETERR  -11111             /* default malloc failure status   */

#define  PARM_RETERR    -22222             /* default parm failure status     */

#define  NUMC2F_RETERR  -30000             /* default numeric overflow status */

#define  QUOTE_RETERR  -12345              /* default unbalanced quote status */

/*------------------------------PROG_ERROR------------------------------------*/
/* diagnostics:  print standard program error message                         */

#define  PROG_ERROR(typ, fmt, var)                                             \
   do {                                                                        \
      Mcdprintf(fmt, var);                                                     \
      if (fmt[STRLEN(fmt)-1] != '\n')  Mcdprintf("\n");                        \
      Mcdprintf("(%.*s): %.*s ERROR at line %d\n",                             \
                (50),                                 __FILE__,                \
                (MAX(0, 50 - (int)STRLEN(__FILE__))), #typ,                    \
                                                      __LINE__ );              \
   } while(0)

/*------------------------------MALLOC, REALLOC-------------------------------*/

#define  MALLOC_X(ptr, siz, errexec)                                           \
   if (!((ptr) = (void*)malloc((size_t)(siz)))  &&  (siz) > 0) {               \
      Mceprintf("OUT OF MEMORY\n");                                            \
      PROG_ERROR(malloc, "Failed malloc for size %ld", (long)(siz));           \
      errexec;                                                                 \
   }  else

#define  REALLOC_X(ptr, siz, errexec)                                          \
   if (ptr) {                                                                  \
      if (!((ptr) = (void*)realloc(ptr,(size_t)(siz))) && (siz) > 0) {         \
         Mceprintf("OUT OF MEMORY\n");                                         \
         PROG_ERROR(realloc, "Failed realloc for size %ld", (long)(siz));      \
         errexec;                                                              \
      }                                                                        \
   }                                                                           \
   else  MALLOC_X(ptr, siz, errexec)

#define  MALLOCX(ptr, siz, errexec)                                            \
                         MALLOC_X(ptr,  siz, errexec; return MALLOC_RETERR)

#define  REALLOCX(ptr, siz, errexec)                                           \
                         REALLOC_X(ptr, siz, errexec; return MALLOC_RETERR)

#define  MALLOC(ptr, siz)          MALLOC_X(ptr,  siz, return MALLOC_RETERR)

#define  REALLOC(ptr, siz)         REALLOC_X(ptr, siz, return MALLOC_RETERR)

#define  MALLOC_RC(ptr, siz, rc)   MALLOC_X(ptr,  siz, return (rc)) 

#define  REALLOC_RC(ptr, siz, rc)  REALLOC_X(ptr, siz, return (rc))

/*------------------------------NUMMOV----------------------------------------*/
/* Assign numeric value to a different type, returning 0 on overflow, else 1. */

#define  NUMMOV(to, from)    (((to) = (from)) == (from))

/*------------------------------NUMC2F----------------------------------------*/
/* Assign C type value to fortran type, returning error on overflow, else 0.  */

#define  NUMC2F(fnum, cnum)  (NUMMOV(*fnum, cnum) ? 0 : NUMC2F_RETERR)

/*------------------------------STRLEN----------------------------------------*/
/* Return length of given string, or 0 if NULL.                               */

#define  STRLEN(str)  ((str) ? strlen(str) : 0)

/*==============================TYPEDEFS, STRUCTURES and UNIONS===============*/
/* Define typedefs, structures and unions                                     */

#ifndef    _McArgSyntax
#  define  _McArgSyntax
   typedef struct McArgSyntax {     /* define argument parsing separators
                                                                     McIDAS
                                                                    defaults
                                                                    --------  */
      const char *white_space;          /* white space characters       " "   */
      const char *keyword_separators;   /* keyword separators           "=,"  */
      const char *command_separators;   /* command separators           ";"   */
      const char *qfld_delims_left;     /* left quote field delimiters  "{\"" */
      const char *qfld_delims_right;    /* right quote field delimiters "}"   */
      const char *quoting_delims_left;  /* left quote char delimiters   "'"   */
      const char *quoting_delims_right; /* right quote char delimiters  "'"   */
      const char *quoting_escape;       /* quoting escape char          NULL  */
      const char *missing_argument;     /* missing argument             "X"   */
      const char *missing_arg_value;    /* missing argument replacement " "   */
   }        McArgSyntax;
#  define  nMcArgSyntax  (sizeof(McArgSyntax) / sizeof(char*))  /* # elements */
#endif  /* _McArgSyntax */

/*==============================PROTOTYPES====================================*/
/* Define function prototypes                                                 */

/*******************************************************************
 *              Category: CONVERTER
 *                        (parsing, byte movers, unit converter)
 *
 * Mcargparse()  - Parse the given text into arg-fetching structure.
 *
 */

extern int
Mcargparse(const char *txtstr, const McArgSyntax *given_syntax, int *parsed_len);


/*******************************************************************
 *              Category: USER_INTERFACE
 *                        (menus, messages , strings, command_line)
 *
 * m0cmdglo_()   - Process global keywords and their initializations.
 *
 * M0argerr()    - Print arg-fetching diagnostic messages to the edest device,
 *                 checking the arg -fetching status code for standard errors.
 *
 * M0cmdparse()  - Parse the given McIDAS command into arg-fetching structure.
 *
 */

extern int
m0cmdglo_(void);

extern void
M0argerr(int status, int arg_handle, const char *keyword, int position, const char *printmsg, const void *def, const void *min, const void *max, const char *carg);

extern int
M0cmdparse(const char *cmdlin, int *parsed_len);


/*******************************************************************
 *              Category: UTILITY
 * (functions useful for programming, substringing, byte manipulation, etc)
 *
 * M0argadd()       - Add new keyword argument to given arg-fetching handle.
 *
 * M0argaddkw()     - Add new keyword to given argument-fetching handle.
 *
 * M0argalloc()     - Allocate a new argument-fetching structure and handle.
 *
 * M0argget()       - Get pointer to desired arg for given keyword & position.
 *
 * M0argput()       - Put given argument to given arg-fetching keyword position.
 *
 * M0argstrtabset() - Set the arg-fetching string table lookup switch.
 *
 * M0argsyntax()    - Update and return the default McIDAS syntax definition.
 *
 * M0argtok()       - Retrieve next argument token within the given text string.
 *
 * M0cmdget()       - Get the current McIDAS command's argument-fetching handle.
 *
 * M0cmdput()       - Set command arg-fetching to given parsed arg structure.
 *
 * M0keytxt()    - Reformat the given nth arg-fetching keyword into McIDAS text.
 *
 * M0legalkey()     - Test a defined keyword for legal format. 
 *
 * M0uniquekey()    - Compare two keyword definitions to determine if they 
 *                    are unique, i.e. are not ambiguous.
 *
 */

extern int
M0argadd(int arg_handle, const char *kwname, const char *newarg);

extern int
M0argaddkw(int arg_handle, const char *newkw);

extern int
M0argalloc(void);

extern int
M0argget(int arg_handle, const char *keyword, int position, const char **value);

extern int
M0argput(int arg_handle, const char *keyword, int position, const char *arg);

extern void
M0argstrtabset(int set);

extern McArgSyntax
M0argsyntax(const McArgSyntax *given_syntax);

extern int
M0argtok(const char *txtstr, const McArgSyntax *given_syntax, char **argstr);

extern int
M0cmdget(void);

extern int
M0cmdput(int arg_handle);

extern char *
M0keytxt(int arg_handle, int nthkey);

extern int 
M0legalkey(const char *keyword);

extern int 
M0uniquekey(const char *key1, const char *key2);

/*----------------------------------------------------------------------------*/

#endif  /* _M0ARG_H */

/*==========================end m0arg.h=======================================*/
