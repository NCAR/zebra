/* MTIO.H FOR IBM/AIX */
#ifndef _zeb_zl_mtio_h
#define _zeb_zl_mtio_h

#ifndef AIXV3
# include <sys/mtio.h>
#else
#ifdef  __cplusplus
extern "C" {
#endif

# include <sys/tape.h>

/*
 * Structures and definitions for mag tape io control commands
 */

/* Tape i/o controls */

#define MTWEOF          STWEOF          /* Write end-of-file record     */
#define MTFSF           STFSF           /* Forward space file           */
#define MTBSF           STRSF           /* Backward space file          */
#define MTFSR           STFSR           /* Forward space record         */
#define MTBSR           STRSR           /* Backward space record        */
#define MTREW           STREW           /* Rewind                       */
#define MTOFFL          STOFFL          /* Rewind and unload tape       */
#define MTRETEN         STRETEN         /* Retension command qic like   */
#define MTERASE         STERASE         /* Erase tape command.          */
#define MTONLINE        STINSRT         /* Load a tape opposite of MTOFFL*/
#define MTLOAD          STINSRT         /* Issue a load tape            */
#define MTUNLOAD        STEJECT         /* Issue a unoad tape           */

#define MTIOCTOP	STIOCTOP

/*
 * structure for MTIOCTOP - mag tape op command
 */
struct  mtop    {
        short   mt_op;          /* operations defined below */
        daddr_t mt_count;       /* how many of them */
};

#ifdef  __cplusplus
}
#endif
#endif

#endif  /*_zeb_zl_mtio_h*/

