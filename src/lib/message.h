/* $Id: message.h,v 2.4 1992-02-11 18:24:51 corbet Exp $ */
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

/*
 * Preallocated chunks of protocol numbers for various groups.
 */
# define MT_NEXUS_BASE	30	/* 20 protos for NEXUS			*/
# define MT_ISS_BASE	50	/* and 20 for ISS			*/
	/* Next available: 70 */


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
# define MH_DIE 	-99	/* Kill the server -- use with care!	*/
# define MH_SHUTDOWN	-100	/* Server is shutting down		*/

/*
 * Various client-related events, sent to the "Client events" group.
 */
# define MH_CE_CONNECT		1	/* New client connection	*/
# define MH_CE_DISCONNECT	2	/* Client death			*/
# define MH_CE_JOIN		3	/* New group joined by client	*/

/*
 * Query message types.
 */
# define MHQ_QUERY		1	/* The basic query		*/
# define MHQ_QTEXT		2	/* Query answer text		*/
# define MHQ_QDONE		3	/* Query finished		*/
# define MAX_NAME_LEN	32	/* Maximum length of a name.	*/

/*
 * Internet protocol stuff.
 */
# define DEFAULT_PORT	1500	/* Default tcp port		*/
# define SERVICE_NAME	"zeb-msg"

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
# define MAXETEXT 200
struct msg_elog
{
	int	el_flag;		/* Flags -- see below	*/
	char	el_text[1];	/* Message text		*/
};

/*
 * Flags controlling which messages are printed when.
 */
# define EF_EMERGENCY	0x01		/* Print always			*/
# define EF_PROBLEM	0x02		/* Indicates a problem		*/
# define EF_CLIENT	0x04		/* Client events		*/
# define EF_DEBUG	0x08		/* Purely debugging stuff	*/
# define EF_INFO	0x10		/* Informational message	*/

# define EF_SETMASK	0x10000000	/* Set the event mask		*/


/*
 * Message Flags.
 */
# define MF_BROADCAST	0x0001	/* Broadcast message	*/

/*
 * The name of the message (unix domain) socket.
 */
# define UN_SOCKET_NAME		"/tmp/fcc.socket"

/*
 * The name of the event manager.
 */
# define MSG_MGR_NAME		"Message manager"

/*
 * Message lib routines.
 */
int msg_DispatchQueued FP ((void));
int msg_incoming FP ((int));
int msg_connect FP ((int (*handler) (), char *));
void msg_send FP ((char *, int, int, void *, int));
void msg_join FP ((char *));
void msg_log FP ((/* char *, ... */));
void msg_ELog FP (());
void msg_add_fd FP ((int, int (*handler) ()));
int msg_get_fd FP ((void));
int msg_await FP ((void));
int msg_Search FP ((int proto, int (*func) (), void * param));
void msg_AddProtoHandler FP ((int, int (*) ()));
int msg_QueryClient FP ((char *));
/* query protocol */
void msg_SendQuery FP ((char *, int (*) ()));
void msg_AnswerQuery FP ((char *, char *));
void msg_FinishQuery FP ((char *));
void msg_SetQueryHandler FP ((int (*) ()));


/*
 * Network broadcast stuff below.
 */
# define MAXBCAST 1500

void	msg_BCast FP ((int, void *, int));
int	msg_BCSetup FP ((int, int, int (*) ()));
