/*
=================================================================================
=================================================================================
==
==   RCS Filename: $Source: /code/cvs/rdss/zebra/source/src/ingest/uav_ingest/serial_svr.h,v $
==
==   RCS State: $State: Exp $
==
==   RCS Version: $Revision: 1.1 $
==
==   RCS Creation Date: $Date: 1994-04-15 20:14:03 $
==
==   RCS Revision Log:  $Log: not supported by cvs2svn $
 * Revision 1.1  1994/03/05  22:57:09  d3e728
 * Initial revision
 *
==
==  Description:  This file gives the pathnames to the FIFOs or named pipes
==                        used to distribute data received on a serial port. 
=================================================================================
*/

/* named pipes */
#define FIFO_PATH_1  "/apps/development/tmp/fifo.1"
#define FIFO_PATH_2  "/apps/development/tmp/fifo.2"

/* permissions to apply to named pipes */
#define PERMS 0666

/* number of serial packets received before writing one to pipe */
#define FIFO_INTERVAL_1 5
#define FIFO_INTERVAL_2 5

