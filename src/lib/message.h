/*
 * Message protocol types.
 */
# define MT_MESSAGE	0	/* Message handler protocol		*/
# define MT_DISPLAYMGR	1	/* Display manager messages		*/
# define MT_LOG		2	/* Event logger				*/
/*
 * Message handler protocol message types.
 */
# define MH_GREETING 	-1	/* Initial greeting from the server	*/
# define MH_IDENTIFY 	-2	/* Process identification		*/
# define MH_ACK 	-3	/* Server acknowledgement		*/
# define MH_JOIN	-4	/* Join process group			*/
# define MH_CLIENT	-5	/* Client event				*/
# define MH_DIE 	-99	/* Kill the server -- use with care!	*/
# define MH_SHUTDOWN	-100	/* Server is shutting down		*/
/*
 * Various client-related events, sent to the "Client events" group.
 */
# define MH_CE_CONNECT		1	/* New client connection	*/
# define MH_CE_DISCONNECT	2	/* Client death			*/
# define MH_CE_JOIN		3	/* New group joined by client	*/

# define MAX_NAME_LEN	32	/* Maximum length of a name.	*/

/*
 * Structures for message handler protocol messages.
 */
struct mh_template
{
	int	mh_type;	/* The message type		*/
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
	int	mh_evtype;	/* The client event type	*/
	char	mh_client[MAX_NAME_LEN];/* The client being talked about */
	char	mh_group[MAX_NAME_LEN]; /* Group name, when appl	*/
};



/*
 * The actual message header structure.
 */
struct message
{
	char	m_from[MAX_NAME_LEN];	/* Who it's from	*/
	char	m_to[MAX_NAME_LEN];	/* Who it is going to	*/
	int	m_proto;		/* Message protocols	*/
	int	m_seq;			/* Sequence number	*/
	short	m_flags;		/* Flag field		*/
	short	m_len;			/* Message length	*/
	char	*m_data;		/* data pointer (internal
					   use only)	        */
};

/*
 * Flags.
 */
# define MF_BROADCAST	0x0001	/* Broadcast message	*/

/*
 * The name of the message (unix domain) socket.
 */
# define UN_SOCKET_NAME		"/home/corbet/mocca/message.socket"
/*
 * The name of the event manager.
 */
# define MSG_MGR_NAME		"Message manager"
