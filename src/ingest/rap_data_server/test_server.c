/******************************************************************************
 *  TEST_SERVER.C Test program for the data servers 
 */

#include <stdio.h>
#include <signal.h>
#include "cdata_server.h"

#define WRITE_BUF_SIZE 1024
#define	READ_BUF_SIZE 262144
#define	DIVISOR 8192
 
/************** GLOBAL DATA *****************************/
char	service_name[64];
char	host_name[64];
int		port,server_id;
int		protofd,sockfd;
int		verbose_mode;

cd_command_t	com;
cd_reply_t		reply;
cd_grid_info_t	info;
 
unsigned char * get_data();
 
typedef struct {
        long    year,month,day,hour,min,sec;
        long    unix_time;
} date_time_t;

/******************************************************************************
 * MAIN :	Open file and send the output to STDOUT
 *
 */
main(argc,argv)
	int	argc;
	char	*argv[];
{
	int i,j;
	int	type,seqno,nbytes;
	int	num_fields;
	unsigned char	*buf,*ptr;	
	char	string[128];
	double	field[6];
	FILE	*infile;

	process_args(argc,argv);	 /* process command line arguments */

	init_sockets();	  /* Set up the TCP/IP socket connections */

	while(1) {

		printf("Enter Field #, Low Height, High Height (km): ");

		fgets(string,128,stdin);
		
		num_fields = freeform(string,128,field);

		if(num_fields != 3) exit(0);

		/* Set up the command request */
		com.primary_com = GET_INFO | GET_MOST_RECENT | GET_DATA;
		com.second_com = GET_XY_PLANE;

		com.min_x = -1000.0;
		com.max_x = 1000.0;
		com.min_y = -1000.0;
		com.max_y = 1000.0;

		com.min_z = field[1];
		com.max_z = field[2];
	
		com.data_field = field[0];
		com.data_type = CHAR;
	
		com.time_min = time(0);
		com.time_max = time(0);

		buf = get_data(&com,&reply,&info,host_name,port);

		print_reply(&reply,stderr);
	
		print_info(&info,stderr);
	
		if(buf != NULL ) {
			if(verbose_mode == 1) {
	
				ptr = buf;
				for(i=0; i < reply.nx; i++ ) {
					printf("\n");
					for(j=0;j < reply.ny; j++) {
						printf("%3u ",(unsigned int) *ptr++);
					}
				}
				printf("\n");
			}
			free(buf);
		} else {
			printf("\n*** No data Returned! ***\n");
		}
	}
}

#define ARG_OPTION_STRING	"p:s:h:v"
/*****************************************************************
 * PROCESS_ARGS: Progess command line arguments. Set option flags
 *	And print usage info if necessary
 */

process_args(argc,argv)
	 int argc;
	 char *argv[];
{
	 int err_flag =0;
	 int  c,i;
	 extern  int optind; /* index to remaining arguments */
	 extern  char *optarg;	/* option argument string */

	if(argc < 3) err_flag++;
 
	 while ((c = getopt(argc, argv,ARG_OPTION_STRING)) != EOF) {
		  switch(c) {
				case 'v' :  /* verbose mode */
					 verbose_mode = 1;
				break;
 
				case 'p' :  /* alternate port */
					 port = atoi(optarg);
				break;
 
				case 's' :  /* alternate service name */
					 strncpy(service_name,optarg,32);
				break;
 
				case 'h' :  /* alternate host name */
					 strncpy(host_name,optarg,32);
				break;
 
				case '?':	/* error in options */
				default:
					 err_flag++;
				break;
		  }
 
	 };
 
	 if(err_flag) {
		  fprintf(stderr,"Usage:test_server -h host_name -p port [-v]\n");
		  fprintf(stderr,"-v: Verbose mode - Prints data plane\n");
		  exit(-1);
	 }
 
}
 
/*****************************************************************
 * INIT_SOCKETS : Init all Socket Connections
 */
 
init_sockets()
{
	int pid;
	void	signal_trap();
	void	sig_io_trap();
 
	signal(SIGINT,signal_trap);
	signal(SIGTERM,signal_trap);
/*	signal(SIGIO,sig_io_trap); /*  */

	pid = getpid();
 
#ifdef SOCK_WORKS
	if((SOCKinit("test_server",pid,-1,-1)) == 0) {
		  fprintf(stderr,"Couldn't Initialize Socket connections\n");
		  exit(-1);
	}

 
	/* Set Up Where Service Should Be Listed */
	if((SOCKsetServiceFile(SOCK_ENV,"ALG_SERVICES")) == 0) {
		  fprintf(stderr,"Couldn't Set Service File\n");
		  exit(-1);
	}
 
	/* Set up the particular service */
	server_id = SOCKclient2way(service_name,WRITE_BUF_SIZE,READ_BUF_SIZE,-1);
 
 
	/* Start */
	if((SOCKbeginOperations()) < 0) {
		  fprintf(stderr,"Couldn't Begin Socket Operations\n");
		  exit(-1);
	}
#endif

}
 
/*****************************************************************
 * SIGNAL_TRAP : Traps Signals so as to die gracefully
 */
void
sig_io_trap()
{
  /*   SOCKexit(); /* Shutdown SOCK process and shared memory */

	fprintf(stderr,"\n############## SIG_IO ##################\n");
	fflush(stderr);

}
 
/*****************************************************************
 * SIGNAL_TRAP : Traps Signals so as to die gracefully
 */
void
signal_trap()
{
  /*   SOCKexit(); /* Shutdown SOCK process and shared memory */

	exit(0);
}



