/* $Id: message.h,v 2.20 1995-05-24 22:30:41 granger Exp $ */
/*
 * Message protocol types.
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
# ifndef _ZEB_MESSAGE_H_
# define _ZEB_MESSAGE_H_

# include <sys/types.h>		/* To get fd_set */

# include "defs.h"		/* For const definition */
# include "config.h"		/* To get CFG_ parameters */

# define MT_MESSAGE	 0	/* Message handler protocol		*/
# define MT_DISPLAYMGR	 1	/* Display manager messages		*/
# define MT_LOG		 2	/* Event logger				*/
# define MT_TIMER	 3	/* Timer				*/
# define MT_ELOG	 4	/* Extended event logger		*/
# define MT_SOUND	 5	/* Sound effects			*/
# define MT_DATASTORE	 6	/* Data store internal protocol		*/
# define MT_IMAGEXFR	 7	/* Image transfer			*/
# define MT_PING	 8	/* Boing...				*/
# define MT_CPING	 9	/* Client ping				*/
# define MT_NETXFR	10	/* Data store network transfer		*/
# define MT_ACINGEST	11	/* Aircraft ingest			*/
# define MT_SLDATA	12	/* Serial line data grabber		*/
# define MT_QUERY	13	/* General status query protocol	*/
# define MT_COMMAND	14	/* Command execution protocol		*/
# define MT_PDMON	15	/* Plot description monitoring		*/
# define MT_PBOUNDS	16	/* Plot bounds exchange protocol	*/
# define MT_MTAP	17	/* Message tap				*/
# define MT_FINISH	18	/* Generic "clean up and exit"		*/

/*
 * Preallocated chunks of protocol numbers for various groups.
 */
# define MT_NEXUS_BASE	30	/* 20 protos for NEXUS			*/
# define MT_ISS_BASE	50	/* and 20 for ISS			*/
	/* Next available: 70 */

# define MT_MAX_PROTO	75	/* Upper limit on number of protocols	*/

/*
 * Return codes for functions called out of msg_Search.  Note that
 * the value for MSG_CONSUMED is something truly funky.  This is because
 * that option was added late, and I don't want to run into a message
 * handler that returns it accidentally.
 */
# define MSG_DONE	0	/* All finished				*/
# define MSG_ENQUEUE	1	/* Enqueue message for later		*/
# define MSG_CONSUMED	-334455	/* Message eaten, but keep searching	*/

/*
 * Value returned by msg_poll() on timeout.
 * -1 for now for backwards compatibility.
 */
# define MSG_TIMEOUT	-1

/*
 * Message handler protocol message types.
 */
# define MH_GREETING 	-1	/* Initial greeting from the server	*/
# define MH_IDENTIFY 	-2	/* Process identification		*/
# define MH_ACK 	-3	/* Server acknowledgement		*/
# define MH_JOIN	-4	/* Join process group			*/
# define MH_CLIENT	-5	/* Client event				*/
# define MH_STATS	-6	/* Message handler stats.		*/
# define MH_NETCLOSE	-7	/* Close network connection		*/
# define MH_PID		-8	/* Report PID				*/
# define MH_CQUERY	-9	/* Does this client exist?		*/
# define MH_CQREPLY	-10	/* Reply to CQUERY			*/
# define MH_QUIT	-11	/* Quit process group			*/
# define MH_DIE 	-99	/* Kill the server -- use with care!	*/
# define MH_SHUTDOWN	-100	/* Server is shutting down		*/

/*
 * Various client-related events, sent to the MSG_CLIENT_EVENTS group.
 */
# define MH_CE_CONNECT		1	/* New client connection	*/
# define MH_CE_DISCONNECT	2	/* Client death			*/
# define MH_CE_JOIN		3	/* New group joined by client	*/
# define MH_CE_QUIT		4	/* Client quit a group		*/

/*
 * Query message types.
 */
# define MHQ_QUERY		1	/* The basic query		*/
# define MHQ_QTEXT		2	/* Query answer text		*/
# define MHQ_QDONE		3	/* Query finished		*/

/*
 * Internet protocol stuff.
 */
# define DEFAULT_PORT	CFG_MSG_DEFAULT_PORT   /* Default tcp port, eg 1500 */
# define SERVICE_NAME	"zeb-msg"

# define MAX_NAME_LEN	CFG_MSGNAME_LEN  /* Maximum length of a name, eg 32 */

/*
 * The type for message handlers
 */
typedef int (*ifptr) ();

/*
 * Structures for message handler protocol messages.
 */
struct mh_template
{
	int	mh_type;	/* The message type		*/
};

/*
 * Boolean reply (i.e. cquery)
 */
struct mh_BoolRepl
{
	int mh_type;
	int mh_reply;
};

/*
 * The greeting structure.
 */
struct mh_greeting
{
	int	mh_type;	/* == MH_GREETING		*/
	char	mh_version[20];	/* Server version number	*/
};

/*
 * The identify and join structure.
 */
struct mh_ident
{
	int	mh_type;	/* == MH_IDENTIFY		*/
	char	mh_name[MAX_NAME_LEN];	/* The name		*/
};

/*
 * IDENTIFY acks.
 */
struct mh_ack
{
	int	mh_type;	/* Message type			*/
	int	mh_number;	/* Sequence number of ack'd msg	*/
	int	mh_atype;	/* Type of ack'd msg		*/
};

/*
 * The client event structure.
 */
struct mh_client
{
	int	mh_type;	/* == MH_CLIENT			*/
	short	mh_evtype;	/* The client event type	*/
	short	mh_inet;	/* This is an internet "client" */
	char	mh_client[MAX_NAME_LEN];/* The client being talked about */
	char	mh_group[MAX_NAME_LEN]; /* Group name, when appl	*/
};

/*
 * For reporting PID's.
 */
struct mh_pid
{
	int	mh_type;	/* == MH_PID			*/
	int	mh_pid;
};

/*
 * For sending stats.
 */
struct mh_stats
{
	int 	mh_type;	/* == MH_STATS				   */
	char	mh_text[1];	/* null-terminated, variable-length string */
};

/*
 * The actual message header structure.
 */
typedef struct message
{
	char	m_from[MAX_NAME_LEN];	/* Who it's from	*/
	char	m_to[MAX_NAME_LEN];	/* Who it is going to	*/
	int	m_proto;		/* Message protocols	*/
	int	m_seq;			/* Sequence number	*/
	short	m_flags;		/* Flag field		*/
	unsigned short	m_len;		/* Message length	*/
	char	*m_data;		/* data pointer (internal
					   use only)	        */
} Message;

/*
 * The extended event log protocol.
 */
struct msg_elog
{
	int	el_flag;		/* Flags -- see below	*/
	char	el_text[1];	/* Message text		*/
};

# define MAXETEXT 	CFG_MSGEVENT_LEN   /* usually 200 */

/*
 * Flags controlling which messages are printed when.
 */
# define EF_EMERGENCY	0x01		/* Print always			*/
# define EF_PROBLEM	0x02		/* Indicates a problem		*/
# define EF_CLIENT	0x04		/* Client events		*/
# define EF_DEBUG	0x08		/* Purely debugging stuff	*/
# define EF_INFO	0x10		/* Informational message	*/
# define EF_DEVELOP 	0x20		/* Development, high-volume, and not
					   meant to be sent to the logger */
# define EF_ALL		0x3f		/* All events mask		*/

# define EF_SETMASK	0x10000000	/* Set the event mask		*/

# define EVENT_LOGGER_NAME	"Event logger"

/*
 * Message Flags.
 */
# define MF_BROADCAST	0x0001	/* Broadcast message	*/

/*
 * The name of the message (unix domain) socket.
 */
# ifndef CFG_MSG_SOCKET_NAME
# define UN_SOCKET_NAME		"/tmp/fcc.socket"
# else
# define UN_SOCKET_NAME		CFG_MSG_SOCKET_NAME
# endif

/*
 * The name of the event manager.
 */
# define MSG_MGR_NAME		"Message manager"
# define MSG_PROTO_VERSION	"V-1.3"

/*
 * Standard group names recognized by the message manager
 */
# define MSG_CLIENT_EVENTS	"Client events"

/*
 * Message tap protocol.
 */
# define MAX_TAP_CLIENT	5
# define MAX_TAP_PROTO 5

struct msg_mtap
{
	int mt_nclient, mt_nproto;
	int mt_protos[MAX_TAP_PROTO];
	char mt_clients[MAX_NAME_LEN][MAX_TAP_CLIENT];
};

/*
 * Message lib routines.
 */
int msg_DispatchQueued FP ((void));
int msg_incoming FP ((int));
int msg_connect FP ((int (*handler) (), char *));
int msg_Connected FP ((void));
void msg_disconnect FP ((void));
void msg_send FP ((char *, int, int, void *, int));
void msg_join FP ((char *));
void msg_quit FP ((char *));
void msg_log FP ((/* char *, ... */));
void msg_ELog FP ((/* int flag, char *s, ... */));
int msg_ELSendMask FP ((int mask));
int msg_ELPrintMask FP ((int mask));
void msg_add_fd FP ((int, int (*handler) ()));
void msg_delete_fd FP ((int));
int msg_get_fd FP ((void));
const char *msg_myname FP((void));
int msg_await FP ((void));
int msg_poll FP ((int timeout));
int msg_PollProto FP ((int timeout, int nproto, int *protolist));
int msg_Search FP ((int proto, int (*func) (), void * param));
void msg_AddProtoHandler FP ((int, int (*) ()));
void msg_Enqueue FP ((Message *msg));
int msg_QueryClient FP ((char *));
void msg_DeathHandler FP ((ifptr f));
/* query protocol */
void msg_SendQuery FP ((char *, int (*) ()));
void msg_AnswerQuery FP ((char *, char *));
void msg_FinishQuery FP ((char *));
void msg_SetQueryHandler FP ((int (*) ()));


/*
 * Network broadcast stuff below.
 */
# define MAXBCAST 	CFG_MSG_MAXBCAST	/* usually 1500 */

void	msg_BCast FP ((int, void *, int));
int	msg_BCSetup FP ((int, int, int (*) ()));
int	msg_PollBCast FP ((int fd));

/*
 * Prototype command proto functions here, since they are also
 * part of the library.
 */
void cp_SetupCmdProto FP ((void));
void cp_Exec FP ((char *process, char *command));

/*
 * For some reason HP-UX uses pointers to int rather than
 * pointers to fd_set in its select prototype.  This typedef is just
 * to get the right cast for the compiler in select calls: pointer
 * to SelectSet.
 */
#ifdef hpux
typedef int SelectSet;
#else
typedef fd_set SelectSet;
#endif

/*
 * The message manager and library reference the netread functions, 
 * but we don't want them to be part of the public interface.
 */
# if defined(MESSAGE_MANAGER) || defined(MESSAGE_LIBRARY)

#define FD_MAP_SIZE 64		/* used for fd map tables */

int msg_XX_netread FP ((int fd, char *dest, int len));
int msg_netread FP ((int fd, char *dest, int len));

# endif /* MESSAGE_MANAGER || MESSAGE_LIBRARY */

# endif /* ! _ZEB_MESSAGE_H_ */
