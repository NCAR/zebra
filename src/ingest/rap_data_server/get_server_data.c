/*************************************************************************
 * GET_SERVER_DATA.C: Routines that and retrieve data from 
 *		Cartesian Data Servers
 *
 * Frank Hage   July 1991 NCAR, Research Applications Program
 */
#define	GET_SERVER_DATA	1

#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>
#include <netinet/in.h>

#include "cdata_server.h"

/**************************************************************************
 *	GET_DATA: Get a Plane of data. Returns a pointer to
 *		The data (NULL if no data available) .
 *      The User must free up the data after use.
 */

unsigned char *
get_cidd_data(command,reply,grid_info,host_name,port)
	cd_command_t	*command;
	cd_reply_t	*reply;	
	cd_grid_info_t	*grid_info;
	char		*host_name;
	int		port;
{
	unsigned char *c_ptr;	/* The pointer to the data */
	int	sockfd;
	 
	cdata_comm_t com;		/* Network structures */
	cdata_reply_t rep;
	cdata_info_t info;

	convert_command(&com,command);

	com.data_type = CHAR;	/* Only type supported so far */
	com.primary_com |= GET_INFO;	/* This routine always gets grid info */

	if((sockfd = open_client_sock(host_name,port)) < 0 ) {
		fprintf (stderr, "get_cidd_data - ");
		switch(sockfd) {
			case -1:
			    fprintf (stderr, "Could not find host name: %s\n",
				host_name);
			    break;
			case -2:
			    fprintf(stderr,"Could not setup socket\n");
			    break;
			case -3:
			    fprintf(stderr,"Could not connect to socket\n");
			    break;
			default:
			    fprintf(stderr,"Unknown Socket connection error\n");
			    break;
		}
		return(NULL);
	}

	
	if(send_message(sockfd,&com,sizeof(com),10) != sizeof(com)) {
		fprintf(stderr,"Data request failed\n");
		close(sockfd);
		return(NULL);
	}
#ifdef JUNK
	if(fcntl(sockfd,F_SETOWN,getpid())<0) {
			fprintf(stderr,"F_SETOWN failed\n");
	}
	if(fcntl(sockfd,F_SETFL,FASYNC)<0) {
		fprintf(stderr,"fcntl fails\n");
	}
#endif

	if(read_message(sockfd,&rep,sizeof(rep),10) != sizeof(rep)) {
		fprintf(stderr,"Read Reply failed\n");
		close(sockfd);
		return(NULL);
	}  

	convert_reply(reply,&rep);

	if(rep.status & INFO_FOLLOWS) {
		if(read_message(sockfd,&info,sizeof(info),20) != sizeof(info)) {
			fprintf(stderr,"Warning: Couldn't read Data Grid Info\n");
			close(sockfd);
			return(NULL);
		}
		convert_info(grid_info,&info);
	}


	if((rep.status & DATA_FOLLOWS) && (rep.n_points > 0)) {
		if((c_ptr = (unsigned char *) malloc(rep.n_points)) == NULL) {
			fprintf(stderr,"Unable to allocate memory for data\n");
			close(sockfd);
			return(NULL);
		}

		if(read_message(sockfd,c_ptr,rep.n_points,20) != rep.n_points) {
			fprintf(stderr,"Couldn't read data\n");
			close(sockfd);
			free(c_ptr);
			return(NULL);
		}
	} else {
		c_ptr = (unsigned char *) NULL;
	}

	close(sockfd);

	return c_ptr;
}
 
 
/******************************************************************************
 * CONVERT_COMMAND: Convert the native floating point format command to the
 *		integer based network command structure
 */

convert_command(com,comm)
	cdata_comm_t *com;
	cd_command_t *comm;
{
	com->primary_com = comm->primary_com;
	com->second_com = comm->second_com;
	com->divisor = (long) CDATA_DIVISOR;
	com->lat_origin = comm->lat_origin * CDATA_DIVISOR;
	com->lon_origin = comm->lon_origin * CDATA_DIVISOR;
	com->ht_origin = comm->ht_origin * CDATA_DIVISOR;
	com->time_min = comm->time_min;
	com->time_max = comm->time_max;
	com->min_x = comm->min_x * CDATA_DIVISOR;
	com->max_x = comm->max_x * CDATA_DIVISOR;
	com->min_y = comm->min_y * CDATA_DIVISOR;
	com->max_y = comm->max_y * CDATA_DIVISOR;
	com->min_z = comm->min_z * CDATA_DIVISOR;
	com->max_z = comm->max_z * CDATA_DIVISOR;
	com->data_field = comm->data_field;
	com->data_type = comm->data_type;
	com->add_data_len = 0;
# ifdef notdef
	to_netl(com,(sizeof (*com)/sizeof(long)));
# endif
}

/******************************************************************************
 * CONVERT_REPLY: Convert the network format reply to native floating point
 * 		format;
 */

convert_reply(reply,rep)
	cd_reply_t	*reply;
	cdata_reply_t *rep;
{
# ifdef notdef
	to_hostl(rep,(sizeof (*rep)/sizeof(long)));
# endif
	reply->status = rep->status;
	reply->orient = rep->orient;
	reply->nx = rep->nx;
	reply->ny = rep->ny;
	reply->nz = rep->nz;
	reply->dx = rep->dx / (double)rep->divisor;
	reply->dy = rep->dy / (double)rep->divisor;
	reply->dz = rep->dz / (double)rep->divisor;
	reply->x1 = rep->x1;
	reply->x2 = rep->x2;
	reply->y1 = rep->y1;
	reply->y2 = rep->y2;
	reply->z1 = rep->z1;
	reply->z2 = rep->z2;
	reply->scale = rep->scale / (double)rep->divisor;
	reply->bias = rep->bias / (double)rep->divisor;
	reply->time_begin = rep->time_begin;
	reply->time_end = rep->time_end;
	reply->time_cent = rep->time_cent;
	reply->bad_data_val = rep->bad_data_val;
	reply->data_type = rep->data_type;
	reply->data_field = rep->data_field;
	reply->n_points = rep->n_points;
	reply->data_length = rep->data_length;
}

/******************************************************************************
 * CONVERT_INFO: Convert the network format reply to native floating point
 * 		format;
 */

convert_info(g_info,info)
	cd_grid_info_t *g_info;
	cdata_info_t *info;
{
	/* 43 longs in current info structure */
# ifdef notdef
	to_hostl(info,43);
# endif

	g_info->order = info->order;
	g_info->data_field = info->data_field;
	g_info->projection = info->projection;
	g_info->lat_origin = info->lat_origin / (double)info->divisor;
	g_info->lon_origin = info->lon_origin / (double)info->divisor;
	g_info->source_x = info->source_x / (double)info->divisor;
	g_info->source_y = info->source_y / (double)info->divisor;
	g_info->source_z = info->source_z / (double)info->divisor;
	g_info->ht_origin = info->ht_origin / (double)info->divisor;
	g_info->nx = info->nx;
	g_info->ny = info->ny;
	g_info->nz = info->nz;
	g_info->dx = info->dx / (double)info->divisor;
	g_info->dy = info->dy / (double)info->divisor;
	g_info->dz = info->dz / (double)info->divisor;
	g_info->min_x = info->min_x / (double)info->divisor;
	g_info->max_x = info->max_x / (double)info->divisor;
	g_info->min_y = info->min_y / (double)info->divisor;
	g_info->max_y = info->max_y / (double)info->divisor;
	g_info->min_z = info->min_z / (double)info->divisor;
	g_info->max_z = info->max_z / (double)info->divisor;
	g_info->north_angle = info->north_angle / (double)info->divisor;
	g_info->gate_spacing = info->gate_spacing / (double)info->divisor;
	g_info->wavelength = info->wavelength / (double)info->divisor;
	g_info->frequency = info->frequency / (double)info->divisor;
	g_info->min_range = info->min_range / (double)info->divisor;
	g_info->max_range = info->max_range / (double)info->divisor;
	g_info->num_gates = info->num_gates;
	g_info->min_gates = info->min_gates;
	g_info->max_gates = info->max_gates;
	g_info->num_tilts = info->num_tilts;
	g_info->min_elev = info->min_elev / (double)info->divisor;
	g_info->max_elev = info->max_elev / (double)info->divisor;
	g_info->radar_const = info->radar_const / (double)info->divisor;
	g_info->nyquist_vel = info->nyquist_vel / (double)info->divisor;
	g_info->delta_azmith = info->delta_azmith / (double)info->divisor;
	g_info->start_azmith = info->start_azmith / (double)info->divisor;
	g_info->beam_width = info->beam_width / (double)info->divisor;
	g_info->pulse_width = info->pulse_width / (double)info->divisor;
	g_info->data_length = info->data_length;
	g_info->noise_thresh = info->noise_thresh / (double)info->divisor;

	strcpy(g_info->units_label_x,info->units_label_x);
	strcpy(g_info->units_label_y,info->units_label_y);
	strcpy(g_info->units_label_z,info->units_label_z);
	strcpy(g_info->field_units,info->field_units);
	strcpy(g_info->field_name,info->field_name);
	strcpy(g_info->source_name,info->source_name);
}

/******************************************************************************
 * TO_NETL Convert a sequence of longs to network byte order
 */

to_netl(lptr,nelem)
	long	*lptr;
	long	nelem;
{
	while(nelem--) {
		*lptr++ = htonl(*lptr);
	};
}

/******************************************************************************
 * TO_HOSTL Convert a sequence of longs to host byte order
 */

to_hostl(lptr,nelem)
	long	*lptr;
	long	nelem;
{
	while(nelem--) {
		*lptr++ = ntohl(*lptr);
	};
}

