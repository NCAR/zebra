/* $Id: NetXfr.h,v 1.2 1991-06-05 16:07:27 corbet Exp $ */
/* 
 * Definitions used for the data store network transfer protocol.
 */

# define OURNAME	"NetXfr"
/*
 * Here are the various message types known to this protocol.
 */
typedef enum
{
	NMT_DataHdr,		/* Begin a data transfer		*/
	NMT_DataContinue,	/* Continuation of data transfer	*/
	NMT_DataDone,		/* End of data transfer			*/
	NMT_DontSend,		/* Stop sending me something		*/
	NMT_DataBCast,		/* Broadcast do_data chunk		*/
	NMT_DataOffsets,	/* Offsets for bcast data		*/
} NetMsgType;

/*
 * The header for incoming data.
 */
# define NAMELEN	60
typedef struct _DataHdr
{
	NetMsgType	dh_MsgType;	/* == NMT_DataHdr		*/
	int		dh_DataSeq;	/* Sequence number		*/
	char		dh_Platform[NAMELEN];	/* The name of this plat */
	DataObject	dh_DObj;	/* The associated data object	*/
	char		dh_BCast;	/* Broadcast will be used	*/
} DataHdr;


/*
 * The various pieces attached to a data object are sent by way of
 * these structures.
 */
typedef struct _DataCont
{
	NetMsgType	dh_MsgType;	/* == NMT_DataContinue		*/
	int		dh_DataSeq;	/* Sequence number		*/
	int		dh_Offset;	/* Offset for this memory	*/
	int		dh_Size;	/* Size of the chunk		*/
	int		dh_Flags;	/* Flags to add to dobj		*/
	char		dh_data[1];	/* The actual data		*/
} DataContinue;

/*
 * When we're done.
 */
typedef struct _DataDone
{
	NetMsgType	dh_MsgType;	/* == NMT_DataDone		*/
	int		dh_DataSeq;	/* Sequence number		*/
} DataDone;


/*
 * Broadcast data chunks look like this:
 */
typedef struct _DataBCChunk
{
	NetMsgType	dh_MsgType;	/* == NMT_DataBCast		*/
	int		dh_DataSeq;	/* Sequence number		*/
	int		dh_Offset;	/* Offset for this memory	*/
	int		dh_Size;	/* Size of the chunk		*/
	int		dh_DataSize;	/* Total data array size	*/
	short		dh_NChunk;	/* Total number of chunks	*/
	short		dh_Chunk;	/* Number of this chunk		*/
	struct _DataBCChunk *dh_Next;	/* For making queues		*/
	char		dh_data[1];	/* The actual data		*/
} DataBCChunk;

/*
 * Array of offsets for bcast, since we don't preserve them separately.
 */
typedef struct _DataOffsets
{
	NetMsgType	dh_MsgType;	/* == NMT_DataOffsets		*/
	int		dh_Offsets[MAXFIELD];	/* The actual offsets	*/
} DataOffsets;

/*
 * Generic template.
 */
typedef struct _DataTemplate
{
	NetMsgType	dh_MsgType;	/* == NMT_DataDone		*/
	int		dh_DataSeq;	/* Sequence number		*/
} DataTemplate;
