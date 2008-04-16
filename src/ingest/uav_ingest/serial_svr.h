/*
=================================================================================
=================================================================================
==
==   RCS Filename: $Source: /code/cvs/rdss/zebra/source/src/ingest/uav_ingest/serial_svr.h,v $
==
==   RCS State: $State: Exp $
==
==   RCS Version: $Revision: 1.3 $
==
==   RCS Creation Date: $Date: 2008-04-16 18:26:53 $
==
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

