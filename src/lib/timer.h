/* $Id: timer.h,v 2.8 1999-12-17 17:40:42 granger Exp $ */
/*
 * Timer module protocol requests and responses.
 */
/*		Copyright (C) 1987,88,89,90,91 by UCAR
 *	University Corporation for Atmospheric Research
 *		   All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced
 * or used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular purpose.
 * UCAR does not indemnify any infringement of copyright, patent, or trademark
 * through use or modification of this software.  UCAR does not provide 
 * maintenance or updates for its software.
 */

#ifndef __zeb_timer_h_
#define __zeb_timer_h_

/*
 * The tm_time and tm_status structures are defined with different member
 * names for C++, because "ANSI C++ forbids data members with the same name
 * as the enclosing class".  Any C++ code will have to use the new names,
 * but it should remain byte-compatible with the C-compiled timer library
 * and timer client program.
 */

# ifdef __cplusplus
extern "C"
{
# endif

/*
 * Possible client requests.  */
# define TR_TIME	1	/* What time is it?		*/
# define TR_ABSOLUTE	2	/* Absolute alarm request	*/
# define TR_RELATIVE	3	/* Relative alarm request	*/
# define TR_SET		4	/* Set fake clock timer mode	*/
# define TR_CANCELALL	5	/* Cancel all alarm requests	*/
# define TR_CANCEL	6	/* Cancel one request		*/
# define TR_STATUS	7	/* Get back status		*/
# define TR_PRT		8	/* Go into pseudo real time mode */
# define TR_RT		9	/* Go into real real time mode	*/

/*
 * Incremental times are stored as fractions of seconds -- INCFRAC to be
 * exact.
 */
# define INCFRAC 10

/*
 * The name of the timer process.
 */
# define TIMER_PROC_NAME "Timer"

/*
 * The basic request template.
 */
struct tm_req
{
	int	tr_type;
};


/*
 * The basic time request answer.  All responses follow this template.
 */
# define TRR_TIME	100
# ifdef __cplusplus
struct tm_time
{
	int	tm_type;	/* Answer type			*/
	ZebTime	tm_zt;		/* The current time value	*/
};
# else
struct tm_time
{
	int	tm_type;	/* Answer type			*/
	ZebTime	tm_time;	/* The current time value	*/
};
# endif


/*
 * Alarm requests.
 */
struct tm_abs_alarm_req
{
	int	tr_type;	/* Request type	(== TR_ABSOLUTE)*/
	ZebTime	tr_when;	/* When the alarm happens	*/
	int	tr_inc;		/* Increment to next alarm	*/
	int	tr_align;	/* Align for alarm		*/
	int	tr_param;	/* Param to go back with alarm	*/
};

struct tm_rel_alarm_req
{
	int	tr_type;	/* Request type (== TR_RELATIVE)*/
	int	tr_delay;	/* When the alarm happens	*/
	int	tr_inc;		/* Increment to next alarm	*/
	int	tr_align;	/* Align for alarm		*/
	int	tr_param;	/* Param to go back with alarm	*/
};

/*
 * An alarm response.
 */
# define TRR_ALARM	101
# define TRR_CANCELACK	103	/* Alarm cancel acknowledge	*/
struct tm_alarm
{
	int	tm_type;	/* == TRR_ALARM			*/
	ZebTime	tm_time;	/* The current time		*/
	int	tm_param;	/* Client-supplied time		*/
};

/*
 * The status response.
 */
# define TRR_STATUS	102
# ifdef __cplusplus
struct tm_status
{
	int	tm_type;	/* Answer type			*/
	ZebTime	tm_time;	/* The current time value	*/
	char	tm_stat[1];	/* Actual status -- as long as nec.	*/
};
# else
struct tm_status
{
	int	tm_type;	/* Answer type			*/
	ZebTime	tm_time;	/* The current time value	*/
	char	tm_status[1];	/* Actual status -- as long as nec.	*/
};
# endif


/*
 * A cancel request.
 */
struct tm_cancel
{
	int	tm_type;	/* == TR_CANCEL			*/
	int	tm_param;	/* Param of req to cancel	*/
};


/*
 * The "enter pseudo real time" request.
 */
struct tm_prt
{
	int	tr_type;	/* == TR_PRT				*/
	ZebTime	tr_time;	/* What time to consider it to be	*/
	int	tr_scale;	/* Time scale factor			*/
};

/*
 * When the current notion of the time changes, tell the world.
 */
# define TRR_TCHANGE	104	/* Broadcast on PRT change		*/
struct tm_tchange
{
	int	tm_type;	/* == TRR_TCHANGE			*/
	ZebTime	tm_time;	/* The new time				*/
	zbool	tm_pseudo;	/* Pseudo real time mode?		*/
};


/*
 * Definitions of timer library routines.
 */
void tl_Time FP ((ZebTime *));
void tl_ChangeHandler FP ((void (*func) ()));
void tl_DispatchEvent FP ((struct tm_time *));
void tl_AllCancel FP ((void));
void tl_Cancel FP ((int));
int tl_RelativeReq FP ((void (*func) (), void *, int, int));
int tl_AbsoluteReq FP ((void (*func) (), void *, ZebTime *, int));

/* The following preserve the old interface for compatibility */
void tl_GetTime FP ((UItime *));
int tl_AddRelativeEvent FP ((void (*func) (), void *, int, int));
int tl_AddAbsoluteEvent FP ((void (*func) (), void *, UItime *, int));

# ifdef __cplusplus
}
# endif

#endif /* !__zeb_timer_h_ */
