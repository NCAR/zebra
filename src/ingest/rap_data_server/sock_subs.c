/*****************************************************************************
 * SOCKSUBS.C : Socket subroutines for applications and servers
 *
 * Written by F. Hage 2/88
 * Latest Update :
 */

#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/un.h>
#include <netdb.h>
extern	int	errno;		/* Unix system call error flag */

/*************************************************************************
 * OPEN_CLIENT_SOCK: Open an Internet socket stream to server on
 *	 hostname at port: port
 *
 *	Returns file discriptor or error;
 *			Errors: -1 = Could not find host name 
 *					-2 = Could not setup socket
 *					-3 = Could not connect to specified port 
 */
  
open_client_sock(hostname,port)
	char	*hostname;		/* name of remote host with server */
	int		port;			/* port number of remote server */
{	
	int	sockfd;				/* socket file descriptor */
	static struct sockaddr_in rem_soc = {AF_INET };
	struct hostent	*hostport;			/* host port info */
	 
	hostport = gethostbyname(hostname);	/* get the remote host info */
	if(!hostport) return(-1);

	/* copy the remote sockets internet address to local hostport struct*/
	bcopy(hostport->h_addr,&rem_soc.sin_addr,sizeof(rem_soc.sin_addr));
	rem_soc.sin_port = port;			/* fill in port number */
	rem_soc.sin_port = htons(rem_soc.sin_port);	

	/*  get a file descriptor for the connection to the remote port */
	if((sockfd = socket(AF_INET,SOCK_STREAM,0)) == -1) {
		return(-2);
	}

	/* Connect the local socket to the remote port ID */
	if(connect(sockfd,&rem_soc,sizeof(rem_soc)) < 0) {
		close(sockfd);
		return(-3);
	}
	return(sockfd);
}


/*****************************************************************************
 * OPEN_SERVER_SOCK : Open up an INternet Stream socket for use as a server
 *			and puts it into listen mode.
 *		Returns proto filedescriptor:  use this in next accept call
 *			OR :  	-1 = Could not open socket
 *					-2 = Could not bind to specified port
 *					-3 = Could not fill in socket name 
 *					-4 = Could not listen 
 *
 * Written by F. Hage 2/88
 * Latest Update :
 */
extern	int	errno;		/* Unix system call error flag */

open_server_sock(port)
	int	port;	/* number of arguments */
{	
	int	protofd;				/* temporary file discriptor */
	static struct sockaddr_in loc_soc;	/* local socket info */
	int		name_len;	

	/*  get a file destriptor for the connection to the remote port */
	if((protofd = socket(AF_INET,SOCK_STREAM,0)) == -1) {
		fprintf(stderr,"Could not set up socket\n");
		return(-1);
	}

	/* set socket options */
	setsockopt(protofd,SOL_SOCKET,~(SO_LINGER),0,0);
	setsockopt(protofd,SOL_SOCKET,SO_REUSEADDR,0,0);
	 
	loc_soc.sin_port = htons(port);
	loc_soc.sin_family = AF_INET;
	loc_soc.sin_addr.s_addr = htonl(INADDR_ANY);

	/* bind to a local port */
	if(bind(protofd,&loc_soc,sizeof(loc_soc)) < 0) {
		fprintf(stderr,"Could'nt bind socket\n");
		close(protofd);
		return(-2);
	}

	/* retrieve the name of (information about) the local socket */
	name_len = sizeof(loc_soc);
	if(getsockname(protofd,&loc_soc,&name_len) < 0) {
		fprintf(stderr,"Getsockname failure\n");
		return(-3);
	}

	if(listen(protofd,5) < 0 ) {		/* Wait for remote connection request */
		fprintf(stderr,"Listening failure \n");
		return(-4);
	}
	return(protofd);
}

/*****************************************************************************
 * GET_NEXT_CLIENT : Gets the next client: Waits until client has connected 
 *
 */

 get_next_client(protofd)
	int	protofd; /* proto filedescriptor of socket in listen mode */
{
	int	name_len;
	int	sockfd;			/* socket file descriptor */

      static union sunion {
         struct sockaddr_in sin;
         struct sockaddr_un sund;
         } sadd;

	name_len = sizeof(struct sockaddr_in);
	if((sockfd = accept(protofd, &sadd, &name_len)) < 0) {
		return(-1);
	}

	return(sockfd);
}


/*****************************************************************************
 * SEND_MESSAGE: Send a message on the file desctiptor- returns bytes written 
 *
 */

send_message(fd,ptr,len,retries)
	int	fd;			/* filedescriptor of socket */
	char	*ptr;	/* source of message */
	int		len;	/* Length of meessage to write */
	int		retries;
{
	int	bytes_written,total,err_count;
	int	target_size = len;
	char	*m_ptr = ptr;

	total = 0;
	err_count = 0;

	while(target_size) {
		bytes_written = write(fd,m_ptr,target_size);
		if(bytes_written <= 0) {
			err_count++;
			if(err_count >= retries) return total;
		} else {
			err_count = 0;
		}
		target_size -= bytes_written;
		m_ptr += bytes_written;
		total += bytes_written;
	}
	 return total;
}

/*****************************************************************************
 * READ_MESSAGE: Get a message from the file desctiptor- returns bytes read 
 *
 */

read_message(fd,ptr,len,retries)
	int	fd;	/* filedescriptor of socket */
	char	*ptr;	/* area to read message into */
	int		len;	/* Length of meessage to read */
	int		retries;
{
	int	bytes_read,total,err_count;
	int	target_size = len;
	char	*m_ptr = ptr;

	total = 0;
	err_count = 0;
	while(target_size) {
		bytes_read = read(fd,m_ptr,target_size);
		if(bytes_read <= 0) {
			err_count++;
			if(err_count >= retries) return total;
		} else {
			err_count = 0;
		}
		target_size -= bytes_read;
		m_ptr += bytes_read;
		total += bytes_read;
	}
	 return total;
}

