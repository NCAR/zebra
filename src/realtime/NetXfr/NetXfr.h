/* $Id: NetXfr.h,v 3.2 1994-10-11 16:26:59 corbet Exp $ */
/* 
 * Definitions used for the data store network transfer protocol.
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

# define OURNAME	"NetXfr"
/*
 * Here are the various message types known to this protocol.
 */
typedef enum
{
	NMT_DataHdr,		/* Begin a data transfer		*/
	NMT_AuxData,		/* Aux data chain entry			*/
	NMT_DataArray,		/* Straight data stuff			*/
	NMT_DataDone,		/* End of data transfer			*/
	NMT_DontSend,		/* Stop sending me something		*/
	NMT_DataBCast,		/* Broadcast do_data chunk		*/
	NMT_DataBRetrans,	/* Data retransmission (bcast)		*/
	NMT_DataOffsets,	/* Offsets for bcast data		*/
	NMT_Retransmit,		/* Please retransmit something		*/
  /* Internal messages */
	NMT_NewPort,		/* Connect to a new port		*/
	NMT_WakeUp		/* Deal with your packets!		*/
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
	DataChunk	dh_DChunk;	/* The associated data object	*/
	char		dh_BCast;	/* Broadcast will be used	*/
	char		dh_BCRLE;	/* Run-length encoded data	*/
	char		dh_NewFile;	/* Start a new file here	*/
} DataHdr;



/*
 * We use these to send the aux data chain.
 */
typedef struct _NxAuxData
{
	NetMsgType	dh_MsgType;	/* == NMT_AuxData		*/
	int		dh_DataSeq;	/* Sequence number		*/
	AuxDataEntry	dh_Ade;		/* Chain entry			*/
	unsigned char	dh_Stuff[1];	/* The aux data itself		*/
} NxAuxData;
	

/*
 * The structure used with non-broadcast data.
 */
# define DTSIZE	1024
typedef struct _DataArray
{
	NetMsgType	dh_MsgType;	/* == NMT_DataArray		*/
	int		dh_DataSeq;	/* The inevitable sequence #	*/
	int		dh_Offset;	/* Where in the array this goes */
	int		dh_Len;		/* How much data here		*/
	unsigned char	dh_Data[DTSIZE]; /* The real stuff		*/
} DataArray;


/*
 * When we're done.
 */
typedef struct _DataDone
{
	NetMsgType	dh_MsgType;	/* == NMT_DataDone		*/
	int		dh_DataSeq;	/* Sequence number		*/
	int		dh_NBSent;	/* How many bcast packets sent	*/
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
	int		dh_ID;		/* Sender ID			*/
	short		dh_NChunk;	/* Total number of chunks	*/
	short		dh_Chunk;	/* Number of this chunk		*/
	struct _DataBCChunk *dh_Next;	/* For making queues		*/
	char		dh_data[1];	/* The actual data		*/
} DataBCChunk;

/*
 * BCast retransmission request.
 */
typedef struct _DataRetransRq
{
	NetMsgType	dh_MsgType;	/* == NMT_Retransmit		*/
	int		dh_DataSeq;	/* Associated sequence		*/
	int		dh_Chunk;	/* Chunk number			*/
	struct _DataRetransRq *dh_Next;	/* For queueing			*/
} DataRetransRq;

/*
 * Array of offsets for bcast, since we don't preserve them separately.
 */
typedef struct _DataOffsets
{
	NetMsgType	dh_MsgType;	/* == NMT_DataOffsets		*/
	int		dh_DataSeq;	/* Sequence number		*/
	int		dh_Offsets[MAXFIELD];	/* The actual offsets	*/
} DataOffsets;



/*
 * The new port request.
 */
typedef struct _NewPort
{
	NetMsgType	dh_MsgType;	/* == NMT_NewPort		*/
	int		dh_Port;	/* The port number		*/
} NewPort;


/*
 * Generic template.
 */
typedef struct _DataTemplate
{
	NetMsgType	dh_MsgType;	/* == NMT_DataDone		*/
	int		dh_DataSeq;	/* Sequence number		*/
} DataTemplate;


/*
 * Global vars.
 */
extern int Seq, Pid;
extern int BCastSave, BCInitialWait, BCRetransWait, BCRetransMax;
extern int Broadcast, BCBurst, BCReceive, Polling;
extern int DbEL;
/*
 * How much data can we put into one UDP packet?  The UDP spec allows
 * us up to around 8K.  But we know that this data is going over an
 * ethernet for the near future, so, since we are fragmenting the data
 * anyway, we might as well avoid further fragmentation at the IP level.
 *
 * IP header = 20 bytes.  UDP = 8 bytes.
 */
# define CBYTES (1500 - 28)	/* Space available to us in packet */
# define MAXDATA (CBYTES - sizeof (DataBCChunk) + 1)	/* Space for data */


/*
 * Global routines.
 */
void BCastSetup FP ((struct ui_command *));
int BCastHandler FP ((int, char *, int));
int DoBCast FP ((PlatformId, DataChunk *));
void ReceiveSetup FP ((int));
void SendOut FP ((PlatformId, void *, int));
void Retransmit FP ((DataRetransRq *));
int ReadBCast FP ((int, char *, int));


/*
 * The Run Length Encoding mechanism:
 *
 * Each "run" is preceeded by a count byte, where 1 <= count <= 127.  A 
 * count of zero is translated to 128.
 *
 * If the uppermost bit of the count byte is set, then the run consists
 * of COUNT bytes, all the same as the byte following COUNT.  Otherwise
 * COUNT distinct bytes follow.
 *
 * Runs do not span packet boundaries, so packets may be decoded 
 * independently, in any order.
 */
