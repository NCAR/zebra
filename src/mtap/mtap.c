/*
 * Tap into the message string.
 */
# include <defs.h>
# include "message.h"
# include <DataStore.h>
# include "../ds/dsPrivate.h"




static char *Std_protos[] =
{
	"message",
	"displaymgr",
	"old event log",
	"timer",
	"event log",
	"sound",
	"data store",
	"ImageXfr",
	"Ping",
	"CPing",
	"NetXfr",
	"ACIngest",
	"SLData",
	"Query",
	"Command",
	"PDMon",
	"PBounds",
	"MTap"
};

# define N_STD_PROTO (sizeof (Std_protos)/sizeof (char *))



int
Handler (msg)
Message *msg;
/*
 * Deal with an incoming message.
 */
{
	struct mh_template *mt;

	switch (msg->m_proto)
	{
	   case MT_MESSAGE:
	   	mt = (struct mh_template *) msg->m_data;
		if (mt->mh_type == MH_SHUTDOWN)
			exit (0);
		break;
	   case MT_MTAP:
	   	MTap (msg->m_data);
		break;
	}
	return (0);
}


main (argc, argv)
int argc;
char **argv;
{
	struct msg_mtap mt;
	extern char *optarg;
	int c;

	msg_connect (Handler, "Eavesdropper");

	mt.mt_nclient = mt.mt_nproto = 0;
	while ((c = getopt (argc, argv, "c:p:")) != -1)
	{
		if (c == 'p')
			mt.mt_protos[mt.mt_nproto++] = atoi (optarg);
		else
			strcpy (mt.mt_clients[mt.mt_nclient++], optarg);
	}
	msg_send (MSG_MGR_NAME, MT_MTAP, FALSE, &mt, sizeof (mt));
	msg_await ();
}




MTap (mtdata)
char *mtdata;
/*
 * Print out mtap data.
 */
{
	Message *msg = (Message *) mtdata;
	msg->m_data = mtdata + sizeof (Message);

	printf ("%-16s > %-16s %4d", msg->m_from, msg->m_to, msg->m_len);
	if (msg->m_proto >= 0 && msg->m_proto < N_STD_PROTO)
		printf (" %s\n", Std_protos[msg->m_proto]);
	else
		printf (" (%d)\n", msg->m_proto);
/*
 * Protocol-specific dumpouts.
 */
	switch (msg->m_proto)
	{
	   case MT_DATASTORE:
	   	DumpDSProto (msg);
		break;
	}
}





DumpDSProto (msg)
Message *msg;
/*
 * Dump out a data store message.
 */
{
	char abuf[80];
	struct dsp_Template *dt = (struct dsp_Template *) msg->m_data;
	struct dsp_GetPlatStruct *dgps;
	struct dsp_PlatStruct *dps;
	struct dsp_CreateFile *dcf;
	struct dsp_R_CreateFile *drcf;
	struct dsp_UpdateFile *duf;
	struct dsp_Notify *dn;
	struct dsp_NotifyRequest *dnr;
	struct dsp_GetFileStruct *dgfs;
	struct dsp_FileStruct *dfs;
	struct dsp_PLock *dp;
	struct dsp_FindDF *dfdf;

	switch (dt->dsp_type)
	{
	   case dpt_NewFileRequest:
		dcf = (struct dsp_CreateFile *) dt;
		printf ("\tNewFileRequest: plat %d f '%s'\n", dcf->dsp_plat,
			dcf->dsp_file);
	   	break;

	   case dpt_R_NewFileSuccess:
		drcf = (struct dsp_R_CreateFile *) dt;
		printf ("\tNewFileSuccess, dfi = %d\n", drcf->dsp_FileIndex);
		break;

	   case dpt_R_NewFileFailure:
	   	printf ("\tNewFileFailure\n"); break;

	   case dpt_UpdateFile:
		duf = (struct dsp_UpdateFile *) dt;
	   	printf ("\tUpdateFile: dfi %d samp %d ow %d last %s\n",
			duf->dsp_FileIndex, duf->dsp_NSamples, 
			duf->dsp_NOverwrite, duf->dsp_Last ? "TRUE" : "FALSE");
		break;
	   case dpt_R_UpdateAck:
	   	printf ("\tUpdateAck\n"); break;

	   case dpt_NotifyRequest:
	   	dnr = (struct dsp_NotifyRequest *) dt;
		printf ("\tNotifyRequest: pid = %d param = %d\n",
			dnr->dsp_pid, dnr->dsp_param);
		break;
	   case dpt_Notify:
	   	dn = (struct dsp_Notify *) dt;
		printf ("\tNotify: pid %d par %d ns %d code %d\n", dn->dsp_pid,
			dn->dsp_param, dn->dsp_nsample, dn->dsp_ucode);
		break;

	   case dpt_GetPlatStruct:
		dgps = (struct dsp_GetPlatStruct *) dt;
		printf ("\tGetPlatStruct: pid = %d\n", dgps->dsp_pid);
		break;
	   case dpt_R_PlatStruct:
		dps = (struct dsp_PlatStruct *) dt;
		printf ("\tPlatStruct (%s)\n", dps->dsp_plat.dp_name);
		break;

	   case dpt_GetFileStruct:
	   	dgfs = (struct dsp_GetFileStruct *) dt;
		printf ("\tGetFileStruct: dfi %d\n", dgfs->dsp_index);
		break;
	   case dpt_R_FileStruct:
	   	dfs = (struct dsp_FileStruct *) dt;
		printf ("\tFileStruct: %s\n", dfs->dsp_file.df_name);
		break;

	   case dpt_PLock:
	   case dpt_R_PLockGranted:
	   	dp = (struct dsp_PLock *) dt;
		printf ("\tPLock%s: id %d\n",
			dt->dsp_type == dpt_PLock ? "" :"Granted",dp->dsp_pid);
		break;
	   case dpt_ReleasePLock:
	   	dp = (struct dsp_PLock *) dt;
		printf ("\tReleasePLock: id %d\n", dp->dsp_pid);
		break;
	   case dpt_Hello:
	   	printf ("\tHello\n");
		break;
	   case dpt_R_ProtoVersion:
	   	printf ("\tProtoVersion = 0x%x\n", 
			((struct dsp_ProtoVersion *) dt)->dsp_version);
		break;
	   case dpt_FindDF:
	   	dfdf = (struct dsp_FindDF *) dt;
		printf ("\tFindDF at %d, pid %d, src %d\n",
			dfdf->dsp_when.zt_Sec,
			dfdf->dsp_pid, dfdf->dsp_src);
		break;
	   case dpt_R_DFIndex:
	   	printf ("\tDFI, index = %d\n",
			((struct dsp_R_DFI *) dt)->dsp_index);
		break;

	   default:
	   	printf ("\tData store proto %d\n", dt->dsp_type);
	}
}
