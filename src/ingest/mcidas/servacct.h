/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 SERVACCT.H 2-Feb-95,8:27:56,`JMB' move to core                          */
/* 2 SERVACCT.H 31-Mar-95,13:58:32,`USER' Released                           */
/* 3 SERVACCT.H 19-Feb-96,16:12:02,`DWS' reglue: modified file               */
/* 4 SERVACCT.H 20-Feb-96,12:04:54,`USER' Released                           */
/**** McIDAS Revision History *** */

/*
    This defines the servacct data structure; that is, the record which
a server writes out describing its transaction


    j.benson  02/94

*/

#ifndef _SERVACCT_H
#define _SERVACCT_H

struct Servacct {
    unsigned server_address;
    int server_port;
    unsigned int client_address;
    char user[4];			/* mcidas logon initials */
    int project;
    char password[12];
    char transaction[4];
    int input_length;		/* data to server(real) */
    char text[120];

    int reply_length;		/* data from server(real) */
    int cpu;			/* 100 X cpu seconds */
    int returncode;
    char errormsg[72];
    int date;			/* yyddd */
    int start_time;		/* hhmmss */
    int end_time;			/* hhmmss */
} ;

typedef struct Servacct servacct;

#endif  /* _SERVACCT_H  */
