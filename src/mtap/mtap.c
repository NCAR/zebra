/*
 * Tap into the message string.
 */
# include <stdio.h>
# include <string.h>

# include <defs.h>
# include <message.h>
# include <DataStore.h>
# include <dsPrivate.h>

static void MTap FP ((char *mtdata));

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


#ifdef __STDC__
#define PCASE(proto)	 		\
   case proto:				\
	printf("\t%s\n",#proto); break
#else
#define PCASE(proto)
#endif


void	DumpDSProto FP((Message *msg));
void	DumpLog FP((Message *msg));
void	print_dfe FP((DataFile *dfe));


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
	int ret = 0;

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
	while (ret == 0 || ret == MSG_TIMEOUT)
	{
		ret = msg_poll (5);
		if (ret == MSG_TIMEOUT)  /* flush stdout when we're idle */
			fflush (stdout);
	}
	return (ret);
}




static void
MTap (mtdata)
char *mtdata;
/*
 * Print out mtap data.
 */
{
	Message *msg = (Message *) mtdata;
	msg->m_data = mtdata + sizeof (Message);

	printf ("%-16s > %-16s %4d", msg->m_from, msg->m_to, msg->m_len);
	printf ("  seq=%-8i", msg->m_seq);
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
	   case MT_LOG:
	   case MT_ELOG:
		DumpLog (msg);
		break;
	}
}



void
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
	struct dsp_ClassStruct *dcs;
	struct dsp_CreateFile *dcf;
	struct dsp_R_CreateFile *drcf;
	struct dsp_UpdateFile *duf;
	struct dsp_UpdateAck *dua;
	struct dsp_Notify *dn;
	struct dsp_NotifyRequest *dnr;
	struct dsp_FindDF *dfdf;
	struct dsp_DeleteData *dd;
	struct dsp_FindDFLink *dfl;
	struct dsp_R_SrcInfo *dsi;
	struct dsp_GetPlatDir *dgpd;
	struct dsp_R_PlatDir *drpd;

	switch (dt->dsp_type)
	{
	   case dpt_NewFileRequest:
		dcf = (struct dsp_CreateFile *) dt;
		printf ("\tNewFileRequest: src %d plat %d '%s'\n", 
			dcf->dsp_srcid, dcf->dsp_plat, dcf->dsp_FileName);
	   	break;

	   case dpt_R_NewFileSuccess:
		drcf = (struct dsp_R_CreateFile *) dt;
		printf ("\tNewFileSuccess: %s\n", drcf->dsp_file.df_fullname);
		break;

	   case dpt_R_NewFileFailure:
	   	printf ("\tNewFileFailure\n"); break;

	   case dpt_UpdateFile:
		duf = (struct dsp_UpdateFile *) dt;
	   	printf ("\tUpdateFile: %s samp %d ow %d last %s\n",
			duf->dsp_file.df_fullname, duf->dsp_NSamples, 
			duf->dsp_NOverwrite, duf->dsp_Last ? "TRUE" : "FALSE");
		break;
	   case dpt_R_UpdateAck:
	   	dua = (struct dsp_UpdateAck *) dt;
	   	printf ("\tUpdateAck\n"); 
		print_dfe (&dua->dsp_file);
		break;

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

	   case dpt_DeleteData:
	   case dpt_DeleteObs:
		dd = (struct dsp_DeleteData *) dt;
		TC_EncodeTime (&(dd->dsp_when), TC_Full, abuf);
		printf ("\t%s: pid %d at %s\n", 
			(dd->dsp_type == dpt_DeleteData) ? "DeleteData" :
			"DeleteObs", dd->dsp_plat, abuf);
		break;

	   case dpt_DataGone:
		printf ("\tDataGone: %s\n", 
			((struct dsp_DataGone *)dt)->dsp_file.df_fullname);
		break;

		PCASE(dpt_CancelNotify);
		PCASE(dpt_CancelAck);
		PCASE(dpt_CopyNotifyReq);
		PCASE(dpt_MarkArchived);
		PCASE(dpt_Rescan);
		PCASE(dpt_BCDataGone);

		PCASE(dpt_GetNPlat);
		PCASE(dpt_R_NPlat);

	   case dpt_GetPlatStruct:
		dgps = (struct dsp_GetPlatStruct *) dt;
		printf ("\tGetPlatStruct: ");
		if (dgps->dsp_id == BadPlatform)
		    printf ("%s\n", dgps->dsp_name);
		else
		    printf ("%d\n", dgps->dsp_id);
		break;
	   case dpt_R_PlatStruct:
		dps = (struct dsp_PlatStruct *) dt;
		printf ("\tPlatStruct (%s)\n", dps->dsp_plat.dp_name);
		break;

	   case dpt_GetClassStruct:
		dgps = (struct dsp_GetPlatStruct *) dt;
		printf ("\tGetPlatStruct: ");
		if (dgps->dsp_id == BadClass)
		    printf ("%s\n", dgps->dsp_name);
		else
		    printf ("%d\n", dgps->dsp_id);
		break;
	   case dpt_R_ClassStruct:
		dcs = (struct dsp_ClassStruct *) dt;
		printf ("\tClassStruct (%s)\n", dcs->dsp_class.dpc_name);
		break;
	   case dpt_FindDF:
	   	dfdf = (struct dsp_FindDF *) dt;
		printf ("\tFindDF at %d, pid %d, src %d\n",
			dfdf->dsp_when.zt_Sec,
			dfdf->dsp_pid, dfdf->dsp_srcid);
		break;
	   case dpt_FindDFPrev:
	   	dfl = (struct dsp_FindDFLink *) dt;
		printf ("\tFindDFPrev:");
		print_dfe (&dfl->dsp_file);
		break;
	   case dpt_FindDFNext:
	   	dfl = (struct dsp_FindDFLink *) dt;
		printf ("\tFindDFNext:");
		print_dfe (&dfl->dsp_file);
		break;
	   case dpt_R_DataFile:
	   	printf ("\tDF, %s\n",
			((struct dsp_R_DataFile *) dt)->dsp_file.df_fullname);
		break;
	   case dpt_GetSrcInfo:
	   	printf ("\tGetSourceInfo, %d\n",
			((struct dsp_GetSrcInfo *) dt)->dsp_srcid);
		break;
	   case dpt_R_SrcInfo:
		dsi = (struct dsp_R_SrcInfo*) dt;
		if (dsi->dsp_success)
		    printf ("\tSourceInfo, %s (%d)\n", 
			    dsi->dsp_srcinfo.src_Name, 
			    dsi->dsp_srcinfo.src_Id);
		else
		    printf ("\tSourceInfo, failure\n");
		break;
	   case dpt_GetPlatDir:
		dgpd = (struct dsp_GetPlatDir*) dt;
	   	printf ("\tGetPlatDir, src %d, plat %d\n",
			dgpd->dsp_srcid, dgpd->dsp_pid);
		break;
	   case dpt_R_PlatDir:
		drpd = (struct dsp_R_PlatDir*) dt;
		printf ("\tPlatDir, %s\n", 
			drpd->dsp_success ? drpd->dsp_dir : "(failure)");
		break;
	   case dpt_Hello:
	   	printf ("\tHello\n");
		break;
	   case dpt_R_ProtoVersion:
	   	printf ("\tProtoVersion = 0x%x\n", 
			((struct dsp_ProtoVersion *) dt)->dsp_version);
		break;

		PCASE(dpt_FindAfter);

		PCASE(dpt_PlatformSearch);
		PCASE(dpt_R_PlatformSearch);

	   default:
	   	printf ("\tData store proto %d\n", dt->dsp_type);
	}
}



void
print_dfe(dfe)
DataFile *dfe;
{
	printf ("\tdf '%s', begin: %lu, end: %lu\n",
		dfe->df_fullname,
		dfe->df_core.dfc_begin.zt_Sec,
		dfe->df_core.dfc_end.zt_Sec);
	printf ("\tRev: %li, nsample: %d\n",
		dfe->df_core.dfc_rev,
		dfe->df_core.dfc_nsample);
}


void
DumpLog (msg)
Message *msg;
/*
 * Print the log message
 */
{
	struct msg_elog *el = (struct msg_elog *) msg->m_data;

	if (msg->m_proto == MT_ELOG)
	{
		if ((el->el_flag & EF_SETMASK))
			printf ("\tSET mask: %0#x\n", el->el_flag);
		else if ((el->el_flag & EF_ORMASK))
			printf ("\tOR mask: %0#x\n", el->el_flag);
		else
			printf ("\t'%s'\n", el->el_text);
	}
	else
		printf ("\t'%s'\n", (char *)msg->m_data);
}
