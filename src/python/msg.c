/*
 * Zebra Python module which allows use of the messaging facility.
 */

# include <python2.2/Python.h>
# include <defs.h>
# define MESSAGE_LIBRARY	/* just to get FD_MAP_SIZE */
# include <message.h>

static PyObject *Handler;		/* The message handler callback */
static PyObject *DeathHandler = NULL;	/* Death handler callback	*/
static PyObject *ExcConnection;		/* connection_error exception	*/
static PyObject *ExcError;		/* Generic problem		*/
static PyObject *PQueryHandler = NULL;	/* msg_query handler		*/
static PyObject *PQueryRoutine;

/*
 * The python exception model is just a bit hard for us to implement.
 * Essentially, when an exception happens, everybody is supposed to notice
 * and return NULL values -- none of that setjmp/longjmp business here.
 * What we need to cope is a way to distinguish normal returns through
 * msg_await from exceptions.  This kludge variable does it.
 */
static int ExceptionReturn = 0;

/*
 * Here we keep our own list of protocol handlers (python objects).
 */
static PyObject *PHandlers[MT_MAX_PROTO];
static PyObject *SearchHandler;		/* for msg_search */

/*
 * And added FD's...
 */
static PyObject *FDHandlers[FD_MAP_SIZE];


/*
 * Local forwards.
 */
static PyObject *ZPmsg_add_fd (PyObject *, PyObject *);
static PyObject *ZPmsg_AnswerQuery (PyObject *, PyObject *);
static PyObject *ZPmsg_await (PyObject *, PyObject *);
static PyObject *ZPmsg_connect (PyObject *, PyObject *);
static PyObject *ZPmsg_isConnected (PyObject *, PyObject *);
static PyObject *ZPmsg_delete_fd (PyObject *, PyObject *);
static PyObject *ZPmsg_disconnect (PyObject *, PyObject *);
static PyObject *ZPmsg_join (PyObject *, PyObject *);
static PyObject *ZPmsg_quit (PyObject *, PyObject *);
static PyObject *ZPmsg_get_fd (PyObject *, PyObject *);
static PyObject *ZPmsg_myname (PyObject *, PyObject *);
static PyObject *ZPmsg_deathHandler (PyObject *, PyObject *);
static PyObject *ZPmsg_incoming (PyObject *, PyObject *);
static PyObject *ZPmsg_AddProtoHandler (PyObject *, PyObject *);
static PyObject *ZPmsg_poll (PyObject *, PyObject *);
static PyObject *ZPmsg_PollProto (PyObject *, PyObject *);
static PyObject *ZPmsg_Search (PyObject *, PyObject *);
static PyObject *ZPmsg_send (PyObject *, PyObject *);
static PyObject *ZPmsg_SetQueryHandler (PyObject *, PyObject *);
static PyObject *ZPmsg_FinishQuery (PyObject *, PyObject *);
static PyObject *ZPmsg_SendQuery (PyObject *, PyObject *);
static PyObject *ZPmsg_ELog (PyObject *, PyObject *);

static int ZPmsg_handler (Message *);
static int ProtoHandler (Message *);
static int DispatchMsg (Message *, PyObject *, PyObject *);
static int ConnCheck (void);
static int Ribbit (void);
static int SearchFunc (Message *, PyObject *);
static void InitConstants (PyObject *);
static int FDHandler (int);
static int QueryHandler (char *);
static int QueryRoutine (char *);


/*
 * The method table.
 */
static PyMethodDef Methods[] =
{
	{ "add_fd",		ZPmsg_add_fd, 	1},
	{ "AddProtoHandler",	ZPmsg_AddProtoHandler, 	1},
	{ "AnswerQuery",	ZPmsg_AnswerQuery, 	1},
	{ "await", 		ZPmsg_await, 	1},
	{ "connect", 		ZPmsg_connect, 	1},
	{ "deathHandler",	ZPmsg_deathHandler,	1},
	{ "delete_fd",		ZPmsg_delete_fd, 	1},
	{ "disconnect",		ZPmsg_disconnect,	1},
	{ "ELog",		ZPmsg_ELog,		1},
	{ "FinishQuery",	ZPmsg_FinishQuery,	1},
	{ "get_fd",		ZPmsg_get_fd,	1},
	{ "incoming",		ZPmsg_incoming,	1},
	{ "isConnected",	ZPmsg_isConnected,	1},
	{ "join",		ZPmsg_join,		1},
	{ "myname",		ZPmsg_myname,	1},
	{ "poll",		ZPmsg_poll,		1},
	{ "PollProto",		ZPmsg_PollProto,	1},
	{ "quit",		ZPmsg_quit,		1},
	{ "Search",		ZPmsg_Search,	1},
	{ "send",		ZPmsg_send,		1},
	{ "SendQuery",		ZPmsg_SendQuery,	1},
	{ "SetQueryHandler",	ZPmsg_SetQueryHandler,	1},
	{ NULL, NULL }
};




void
initmsg ()
/*
 * The module initializer called from the python interpreter.
 */
{
	PyObject *module, *dict;
	int i;
/*
 * Get the module initialized.
 */
	module = Py_InitModule ("msg", Methods);
	dict = PyModule_GetDict (module);
/*
 * Set up the exceptions.
 */
	ExcConnection = PyString_FromString ("msg.connection_error");
	PyDict_SetItemString (dict, "connection_error", ExcConnection);
	ExcError = PyString_FromString ("msg.error");
	PyDict_SetItemString (dict, "error", ExcError);
/*
 * Set up all the #%$*%!! constants.
 */
	InitConstants (dict);
/*
 * No protocol handlers yet.
 */
	for (i = 0; i < MT_MAX_PROTO; i++)
		PHandlers[i] = NULL;
	for (i = 0; i < FD_MAP_SIZE; i++)
		FDHandlers[i] = NULL;
}
		



static void
InitConstants (PyObject *dict)
/*
 * Initialize all the message.h constants into the given dictionary.
 *
 * This was a bummer to type.
 */
{
/*
 * Protocol constants.
 */
	PyDict_SetItemString (dict, "MT_MESSAGE", PyInt_FromLong (MT_MESSAGE));
	PyDict_SetItemString (dict, "MT_DISPLAYMGR",
			PyInt_FromLong (MT_DISPLAYMGR));
	PyDict_SetItemString (dict, "MT_LOG", PyInt_FromLong (MT_LOG));
	PyDict_SetItemString (dict, "MT_TIMER", PyInt_FromLong (MT_TIMER));
	PyDict_SetItemString (dict, "MT_ELOG", PyInt_FromLong (MT_ELOG));
	PyDict_SetItemString (dict, "MT_SOUND", PyInt_FromLong (MT_SOUND));
	PyDict_SetItemString (dict, "MT_DATASTORE",
			PyInt_FromLong (MT_DATASTORE));
	PyDict_SetItemString (dict, "MT_IMAGEXFR",
			PyInt_FromLong (MT_IMAGEXFR));
	PyDict_SetItemString (dict, "MT_PING", PyInt_FromLong (MT_PING));
	PyDict_SetItemString (dict, "MT_CPING", PyInt_FromLong (MT_CPING));
	PyDict_SetItemString (dict, "MT_NETXFR", PyInt_FromLong (MT_NETXFR));
	PyDict_SetItemString (dict, "MT_ACINGEST",
			PyInt_FromLong (MT_ACINGEST));
	PyDict_SetItemString (dict, "MT_SLDATA", PyInt_FromLong (MT_SLDATA));
	PyDict_SetItemString (dict, "MT_QUERY", PyInt_FromLong (MT_QUERY));
	PyDict_SetItemString (dict, "MT_COMMAND", PyInt_FromLong (MT_COMMAND));
	PyDict_SetItemString (dict, "MT_PDMON", PyInt_FromLong (MT_PDMON));
	PyDict_SetItemString (dict, "MT_PBOUNDS", PyInt_FromLong (MT_PBOUNDS));
	PyDict_SetItemString (dict, "MT_MTAP", PyInt_FromLong (MT_MTAP));
	PyDict_SetItemString (dict, "MT_FINISH", PyInt_FromLong (MT_FINISH));
	PyDict_SetItemString (dict, "MT_NEXUS_BASE",
			PyInt_FromLong (MT_NEXUS_BASE));
	PyDict_SetItemString (dict, "MT_ISS_BASE",
			PyInt_FromLong (MT_ISS_BASE));
	PyDict_SetItemString (dict, "MT_MAX_PROTO",
			PyInt_FromLong (MT_MAX_PROTO));
/*
 * Stuff for msg_Search.
 */
	PyDict_SetItemString (dict, "MSG_DONE", PyInt_FromLong (MSG_DONE));
	PyDict_SetItemString (dict, "MSG_ENQUEUE",
			PyInt_FromLong (MSG_ENQUEUE));
	PyDict_SetItemString (dict, "MSG_CONSUMED",
			PyInt_FromLong (MSG_CONSUMED));
/*
 * Others.
 */
	PyDict_SetItemString (dict, "MSG_TIMEOUT",
			PyInt_FromLong (MSG_TIMEOUT));
/*
 * Event logger.
 */
	PyDict_SetItemString (dict, "EF_EMERGENCY",
			PyInt_FromLong (EF_EMERGENCY));
	PyDict_SetItemString (dict, "EF_PROBLEM", PyInt_FromLong (EF_PROBLEM));
	PyDict_SetItemString (dict, "EF_CLIENT", PyInt_FromLong (EF_CLIENT));
	PyDict_SetItemString (dict, "EF_DEBUG", PyInt_FromLong (EF_DEBUG));
	PyDict_SetItemString (dict, "EF_INFO", PyInt_FromLong (EF_INFO));
	PyDict_SetItemString (dict, "EF_DEVELOP", PyInt_FromLong (EF_DEVELOP));
	PyDict_SetItemString (dict, "EF_ALL", PyInt_FromLong (EF_ALL));
	PyDict_SetItemString (dict, "EF_SETMASK", PyInt_FromLong (EF_SETMASK));
	PyDict_SetItemString (dict, "EF_ORMASK", PyInt_FromLong (EF_ORMASK));
/*
 * Group and process names.
 */
	PyDict_SetItemString (dict, "MSG_EVERYBODY",
			PyString_FromString (MSG_EVERYBODY));
	PyDict_SetItemString (dict, "MSG_CLIENT_EVENTS",
			PyString_FromString (MSG_CLIENT_EVENTS));
	PyDict_SetItemString (dict, "EVENT_LOGGER_GROUP",
			PyString_FromString (EVENT_LOGGER_GROUP));
	PyDict_SetItemString (dict, "MSG_MGR_NAME",
			PyString_FromString (MSG_MGR_NAME));
/*
 * Internal message protocols.
 */
	PyDict_SetItemString (dict, "MH_GREETING",
			PyInt_FromLong (MH_GREETING));
	PyDict_SetItemString (dict, "MH_IDENTIFY",
			PyInt_FromLong (MH_IDENTIFY));
	PyDict_SetItemString (dict, "MH_ACK", PyInt_FromLong (MH_ACK));
	PyDict_SetItemString (dict, "MH_JOIN", PyInt_FromLong (MH_JOIN));
	PyDict_SetItemString (dict, "MH_CLIENT", PyInt_FromLong (MH_CLIENT));
	PyDict_SetItemString (dict, "MH_STATS", PyInt_FromLong (MH_STATS));
	PyDict_SetItemString (dict, "MH_NETCLOSE",
			PyInt_FromLong (MH_NETCLOSE));
	PyDict_SetItemString (dict, "MH_PID", PyInt_FromLong (MH_PID));
	PyDict_SetItemString (dict, "MH_CQUERY", PyInt_FromLong (MH_CQUERY));
	PyDict_SetItemString (dict, "MH_CQREPLY",
			PyInt_FromLong (MH_CQREPLY));
	PyDict_SetItemString (dict, "MH_QUIT", PyInt_FromLong (MH_QUIT));
	PyDict_SetItemString (dict, "MH_LISTGROUP",
			PyInt_FromLong (MH_LISTGROUP));
	PyDict_SetItemString (dict, "MH_GROUP", PyInt_FromLong (MH_GROUP));
	PyDict_SetItemString (dict, "MH_NOTFOUND",
			PyInt_FromLong (MH_NOTFOUND));
	PyDict_SetItemString (dict, "MH_DIE", PyInt_FromLong (MH_DIE));
	PyDict_SetItemString (dict, "MH_SHUTDOWN",
			PyInt_FromLong (MH_SHUTDOWN));
/*
 * Event flags.
 */
	PyDict_SetItemString (dict, "EF_EMERGENCY",
			PyInt_FromLong (EF_EMERGENCY));
	PyDict_SetItemString (dict, "EF_PROBLEM", PyInt_FromLong (EF_PROBLEM));
	PyDict_SetItemString (dict, "EF_CLIENT", PyInt_FromLong (EF_CLIENT));
	PyDict_SetItemString (dict, "EF_DEBUG", PyInt_FromLong (EF_DEBUG));
	PyDict_SetItemString (dict, "EF_INFO", PyInt_FromLong (EF_INFO));
	PyDict_SetItemString (dict, "EF_DEVELOP", PyInt_FromLong (EF_DEVELOP));
	PyDict_SetItemString (dict, "EF_ALL", PyInt_FromLong (EF_ALL));
	PyDict_SetItemString (dict, "EF_SETMASK", PyInt_FromLong (EF_SETMASK));
	PyDict_SetItemString (dict, "EF_ORMASK", PyInt_FromLong (EF_ORMASK));

/*	PyDict_SetItemString (dict, "MH_@", PyInt_FromLong (MH_@)); */
}





static PyObject *
ZPmsg_connect (self, args)
PyObject *self, *args;
/*
 * msg.connect (handler, ident)
 */
{
	char *ident;
/*
 * Parse out the arguments.
 */
	if (! PyArg_ParseTuple (args, "Os", &Handler, &ident))
		return NULL;
	
	if (! PyFunction_Check (Handler) && ! PyMethod_Check (Handler))
	{
	    PyErr_SetString (ExcConnection, "connect: bad function passed");
	    return 0;
	}

	
	Py_INCREF (Handler);
/*
 * Try to do the connection.
 */
	if (! msg_connect (ZPmsg_handler, ident))
	{
		PyErr_SetString (ExcConnection,
				"Unable to connect to message handler");
		Py_DECREF (Handler);
		return (0);
	}
	msg_DeathHandler (Ribbit);
/*
 * Looks like we're set.
 */
	Py_INCREF (Py_None);
	return (Py_None);
}



static int
ZPmsg_handler (Message *msg)
/*
 * Deal with an incoming message.
 */
{
	return (DispatchMsg (msg, Handler, NULL));
}




static int
DispatchMsg (Message *msg, PyObject *recip, PyObject *param)
/*
 * Send a message through to the designated recipient.  Param is an
 * optional parameter to pass to the function; pass as NULL if your
 * function is not expecting it.
 */
{
	PyObject *args, *ret;
	int iret;
/*
 * Build up the arguments out of the message.  Then pass them through
 * to the handler function.
 */
	args = Py_BuildValue ((param == NULL) ? "(sis#)" : "(sis#O)",
			msg->m_from, msg->m_proto, msg->m_data, msg->m_len,
			param);
	ret = PyEval_CallObject (recip, args);
	Py_DECREF (args);
/*
 * See if the called guy died with an exception.  If so, we have
 * to pass it on through.
 */
	if (ret == NULL)
	{
		ExceptionReturn = TRUE;
		return (1);
	}
/*
 * Nope, normal return.
 */
	iret = PyInt_AsLong (ret);
	Py_XDECREF (ret);
	return (iret);
}




static PyObject *
ZPmsg_isConnected (PyObject *self, PyObject *args)
/*
 * Return true iff we are connected.
 */
{
	return (Py_BuildValue ("i", msg_Connected ()));
}




static int
ConnCheck ()
/*
 * Are we connected?  If not, set up an exception before returning.
 */
{
	if (msg_Connected ())
		return (1);
	PyErr_SetString (ExcError, "No connection active");
	return (0);
}





static PyObject *
ZPmsg_disconnect (PyObject *self, PyObject *args)
/*
 * Disconnect from the message handler.
 */
{
/*
 * If we're not connected, gripe.
 */
	if (! ConnCheck ())
		return NULL;
/*
 * Do the disconnect call, free up the handler, and we're done.
 */
	msg_disconnect ();
	Py_XDECREF (Handler);
	Handler = NULL;
	Py_INCREF (Py_None);
	return (Py_None);
}




static PyObject *
ZPmsg_join (PyObject *self, PyObject *args)
/*
 * Join a message group.
 */
{
	char *group;
/*
 * Are we connected?
 */
	if (! ConnCheck ())
		return (NULL);
/*
 * Pull out the argument.
 */
	if (! PyArg_ParseTuple (args, "s", &group))
		return (NULL);
/*
 * Ship it off.
 */
	msg_join (group);
	Py_INCREF (Py_None);
	return (Py_None);
}




static PyObject *
ZPmsg_quit (PyObject *self, PyObject *args)
/*
 * Quit a message group.
 */
{
	char *group;
/*
 * Are we connected?
 */
	if (! ConnCheck ())
		return (NULL);
/*
 * Pull out the argument.
 */
	if (! PyArg_ParseTuple (args, "s", &group))
		return (NULL);
/*
 * Ship it off.
 */
	msg_quit (group);
	Py_INCREF (Py_None);
	return (Py_None);
}





static PyObject *
ZPmsg_await (PyObject *self, PyObject *args)
/*
 * Go into the wait/dispatch loop.
 */
{
/*
 * Are we connected?
 */
	if (! ConnCheck ())
		return (NULL);
/*
 * Just do the wait call.
 */
	ExceptionReturn = 0;
	msg_await (); /* No return anytime soon. */
/*
 * Is this an exception return?
 */
	if (ExceptionReturn)
	{
		ExceptionReturn = 0;
		return NULL;
	}
/*
 * Nope, regular return.
 */
	Py_INCREF (Py_None);
	return (Py_None);
}




static PyObject *
ZPmsg_get_fd (PyObject *self, PyObject *args)
/*
 * Get the socket file descriptor, for an external select or some such.
 */
{
/*
 * Are we connected?
 */
	if (! ConnCheck ())
		return (NULL);
/*
 * Get the file descriptor, package it up and return it.
 */
	return (Py_BuildValue ("i", msg_get_fd ()));
}


static PyObject *
ZPmsg_myname (PyObject *self, PyObject *args)
{
	const char *name;
/*
 * Are we connected?
 */
	if (! ConnCheck ())
		return (NULL);
/*
 * Get the name, package it up, and return it.
 */
	name = msg_myname ();
	return (Py_BuildValue ("s", name));
}


static PyObject *
ZPmsg_deathHandler (PyObject *self, PyObject *args)
/*
 * Set up a death handler.
 */
{
	PyObject *func;
/*
 * Are we connected?
 */
	if (! ConnCheck ())
		return (NULL);
/*
 * Pull out the function to call.
 */
	if (! PyArg_ParseTuple (args, "O", &func))
		return (NULL);

	if (! PyFunction_Check (func) && ! PyMethod_Check (func))
	{
	    PyErr_SetString (ExcError, "deathHandler: bad function passed");
	    return 0;
	}
/*
 * Tweak refcounts and set the new handler.
 */
	Py_XDECREF (DeathHandler);
	Py_INCREF (func);
	DeathHandler = func;
/*
 * Done.
 */
	Py_INCREF (Py_None);
	return (Py_None);
}



static int Ribbit ()
/*
 * The message system is going down.
 */
{
	PyObject *ret;
	if (DeathHandler != NULL)
	{
		PyObject *empty = Py_BuildValue ("()");
		ret = PyEval_CallObject (DeathHandler, empty);
		if (ret == NULL)
		    ExceptionReturn = TRUE;
		Py_DECREF (empty);
		Py_XDECREF (ret);
	}

	return (0);
}


static PyObject *
ZPmsg_incoming (PyObject *self, PyObject *args)
/*
 * Dispatch one incoming message.
 */
{
	int ret;
/*
 * Do the call.
 */
	ExceptionReturn = FALSE;
	ret = msg_incoming (msg_get_fd ());
	if (ExceptionReturn)
	{
		ExceptionReturn = FALSE;
		return (NULL);
	}
	return (Py_BuildValue ("i", ret));
}




static PyObject *
ZPmsg_AddProtoHandler (PyObject *self, PyObject *args)
/*
 * AddProtoHandler (proto, handler)
 */
{
	int proto;
	PyObject *handler;
/*
 * Pull out the args.
 */
	if (! PyArg_ParseTuple (args, "iO", &proto, &handler))
		return (NULL);


	if (! PyFunction_Check (handler) && ! PyMethod_Check (handler))
	{
	    PyErr_SetString (ExcError, "AddProtoHandler: bad function passed");
	    return 0;
	}
/*
 * Make sure that the protocol value is in range.
 */
	if (proto < 0 || proto >= MT_MAX_PROTO)
	{
		PyErr_SetString (ExcError, "AddProtoHandler on bad proto");
		return (NULL);
	}
/*
 * Establish our internal handler as the real one, and remember the python
 * handler.
 */
	msg_AddProtoHandler (proto, ProtoHandler);
	Py_XINCREF (handler);
	PHandlers[proto] = handler;
/*
 * Done.
 */
	Py_INCREF (Py_None);
	return (Py_None);
}




static int
ProtoHandler (Message *msg)
/*
 * Dispatch a message through to a designated protocol handler.
 */
{
	return (DispatchMsg (msg, PHandlers[msg->m_proto], NULL));
}



/*
 * Template to save some typing.  Maybe.
 */
static PyObject *
ZPmsg_poll (PyObject *self, PyObject *args)
{
	int timeout, ret;
/*
 * Are we connected?
 */
	if (! ConnCheck ())
		return (NULL);
/*
 * Get the timeout and do the call.
 */
	if (! PyArg_ParseTuple (args, "i", &timeout))
		return (NULL);
	ExceptionReturn = FALSE;
	ret = msg_poll (timeout);
/*
 * Fix up the return value and pass it on.
 */
	if (ExceptionReturn)
	{
		ExceptionReturn = FALSE;
		return NULL;
	}
	return (Py_BuildValue ("i", ret));
}





static PyObject *
ZPmsg_PollProto (PyObject *self, PyObject *args)
/*
 * Poll one or more specific protocols.  This call has to be a bit
 * different; the python equivalent is:
 *
 *	msg.PollProto (timeout, p1, p2...)
 *
 * Up to a maximum of ten protocols can be handled here.
 */
{
	int nproto, plist[10], timeout, ret;
/*
 * Are we connected?
 */
	if (! ConnCheck ())
		return (NULL);
/*
 * Initialize the protocol list.
 */
	for (nproto = 0; nproto < 10; nproto++)
		plist[nproto] = -999999;
/*
 * Parse out the args and do the call.
 */
	if (! PyArg_ParseTuple (args, "i|iiiiiiiiii", &timeout, plist,
			plist + 1, plist + 2, plist + 3, plist + 4, plist + 5,
			plist + 6, plist + 7, plist + 8, plist + 9))
		return (NULL);
	for (nproto = 0; nproto < 10; nproto++)
		if (plist[nproto] == -999999)
			break;
	ExceptionReturn = FALSE;
	ret = msg_PollProto (timeout, nproto, plist);
/*
 * Fix up the return value and pass it on.
 */
	if (ExceptionReturn)
	{
		ExceptionReturn = FALSE;
		return NULL;
	}
	return (Py_BuildValue ("i", ret));
}



static PyObject *
ZPmsg_Search (PyObject *self, PyObject *args)
/*
 * Implement msg_search.
 */
{
	PyObject *func, *param;
	int proto, ret;
/*
 * Are we connected?
 */
	if (! ConnCheck ())
		return (NULL);
/*
 * Parse out the args.
 */
	if (! PyArg_ParseTuple (args, "iOO", &proto, &func, &param))
		return (NULL);

	if (! PyFunction_Check (func) && ! PyMethod_Check (func))
	{
	    PyErr_SetString (ExcError, "Search: bad function passed");
	    return 0;
	}

	Py_XINCREF (func);
	Py_XINCREF (param);
	SearchHandler = func;
/*
 * Do the search.
 */
	ExceptionReturn = FALSE;
	ret = msg_Search (proto, SearchFunc, param);
/*
 * Deref our stuff and check for exceptions.
 */
	Py_XDECREF (func);
	Py_XDECREF (param);
	if (ExceptionReturn)
		return (NULL);
	return (Py_BuildValue ("i", ret));
}




static int
SearchFunc (Message *msg, PyObject *param)
/*
 * The internal search handler.  Pass things through to the python
 * handler and see what happens.
 */
{
	int ret;
/*
 * We can essentially use DispatchMsg here, but we do have to check for
 * exceptions and remap the return value to pass them through.
 */
	ret = DispatchMsg (msg, SearchHandler, param);
	return (ExceptionReturn ? 0 : ret);
}





static PyObject *
ZPmsg_send (PyObject *self, PyObject *args)
/*
 * Send a message.
 *
 * msg.send (to, proto, broadcast, data)
 */
{
	char *to, *data;
	int proto, broadcast, dlen;
/*
 * Are we connected?
 */
	if (! ConnCheck ())
		return (NULL);
/*
 * Get the args, and simply do it.
 */
	if (! PyArg_ParseTuple (args, "siis#", &to, &proto, &broadcast,
			&data, &dlen))
		return (NULL);
	msg_send (to, proto, broadcast, data, dlen);
/*
 * Done.
 */
	Py_INCREF (Py_None);
	return (Py_None);
}


static PyObject *
ZPmsg_add_fd (PyObject *self, PyObject *args)
{
	int fd;
	PyObject *handler;
/*
 * Are we connected?
 */
	if (! ConnCheck ())
		return (NULL);
/*
 * Get the args.
 */
	if (!PyArg_ParseTuple (args, "iO", &fd, &handler))
		return (NULL);

	if (fd < 0 || fd >= FD_MAP_SIZE)
	{
		PyErr_SetString (ExcError, "Bad file descriptor");
		return (NULL);
	}

	if (! PyFunction_Check (handler) && ! PyMethod_Check (handler))
	{
	    PyErr_SetString (ExcError, "add_fd: bad function passed");
	    return 0;
	}
/*
 * Do the add.
 */
	Py_XINCREF (handler);
	if (FDHandlers[fd])
		Py_DECREF (FDHandlers[fd]);
	FDHandlers[fd] = handler;
	msg_add_fd (fd, FDHandler);
/*
 * Done.
 */
	Py_INCREF (Py_None);
	return (Py_None);
}



static int
FDHandler (int fd)
/*
 * Dispatch out a readable FD.
 */
{
	PyObject *arg, *ret;
	
	if (! FDHandlers[fd])
		return (0);		/* Should never happen, of course */
/*
 * Do the call.
 */
	arg = Py_BuildValue ("(i)", fd);
	ret = PyEval_CallObject (FDHandlers[fd], arg);
	Py_DECREF (arg);
	Py_XDECREF (ret);
/*
 * If we have an exception here, try to print something now because it's
 * not at all clear that we'll be able to pass it all the way back.
 */
	if (ret == NULL)
		PyRun_SimpleString("import traceback; traceback.print_last()");

	return (0);
}



static PyObject *
ZPmsg_delete_fd (PyObject *self, PyObject *args)
{
	int fd;
/*
 * Are we connected?
 */
	if (! ConnCheck ())
		return (NULL);
/*
 * Get the fd and check it.
 */
	if (! PyArg_ParseTuple (args, "i", &fd))
		return (NULL);
	if (fd < 0 || fd >= FD_MAP_SIZE)
	{
		PyErr_SetString (ExcError, "Bad fd in delete_fd");
		return (NULL);
	}
/*
 * Clean it out.
 */
	Py_XDECREF (FDHandlers[fd]);
	FDHandlers[fd] = NULL;
/*
 * Done.
 */
	Py_INCREF (Py_None);
	return (Py_None);
}





static PyObject *
ZPmsg_SetQueryHandler (PyObject *self, PyObject *args)
{
	PyObject *handler;
/*
 * Are we connected?
 */
	if (! ConnCheck ())
		return (NULL);
/*
 * Pull it out and set it.
 */
	if (! PyArg_ParseTuple (args, "O", &handler))
		return (NULL);

	if (! PyFunction_Check (handler) && ! PyMethod_Check (handler))
	{
	    PyErr_SetString (ExcError, "SetQueryHandler: bad function passed");
	    return 0;
	}

	Py_XDECREF (PQueryHandler);
	Py_INCREF (handler);
	PQueryHandler = handler;
	msg_SetQueryHandler (QueryHandler);
/*
 * Done.
 */
	Py_INCREF (Py_None);
	return (Py_None);
}




static int
QueryHandler (char *from)
/*
 * Deal with an incoming query.
 */
{
	PyObject *arg, *ret;
/*
 * Call the python query handler.
 */
	arg = Py_BuildValue ("(s)", from);
	ExceptionReturn = FALSE;
	ret = PyEval_CallObject (PQueryHandler, arg);
	Py_DECREF (arg);
	Py_XDECREF (ret);
/*
 * Check for bummers.
 */
	if (ret == NULL)
		ExceptionReturn = TRUE;
	return (0);
}





static PyObject *
ZPmsg_AnswerQuery (PyObject *self, PyObject *args)
/*
 * Answer a query.
 */
{
	char *to, *text;
/*
 * Are we connected?
 */
	if (! ConnCheck ())
		return (NULL);
/*
 * Get the args and ship it back.
 */
	if (! PyArg_ParseTuple (args, "ss", &to, &text))
		return (NULL);
	msg_AnswerQuery (to, text);
/*
 * Done.
 */
	Py_INCREF (Py_None);
	return (Py_None);
}



static PyObject *
ZPmsg_FinishQuery (PyObject *self, PyObject *args)
/*
 * We're done answering this query.
 */
{
	char *to;
/*
 * Are we connected?
 */
	if (! ConnCheck ())
		return (NULL);
/*
 * Do it.
 */
	if (! PyArg_ParseTuple (args, "s", &to))
		return (NULL);
	msg_FinishQuery (to);
/*
 * Done.
 */
	Py_INCREF (Py_None);
	return (Py_None);
}




static PyObject *
ZPmsg_SendQuery (PyObject *self, PyObject *args)
/*
 * Send a query to another process.
 */
{
	char *whom;
	PyObject *routine;
/*
 * Are we connected?
 */
	if (! ConnCheck ())
		return (NULL);
/*
 * Get the args out.
 */
	if (! PyArg_ParseTuple (args, "sO", &whom, &routine))
		return (NULL);

	if (! PyFunction_Check (routine) && ! PyMethod_Check (routine))
	{
	    PyErr_SetString (ExcError, "SendQuery: bad function passed");
	    return 0;
	}

	PQueryRoutine = routine;
	Py_INCREF (PQueryRoutine);
/*
 * Do the query and go on with life.
 */
	msg_SendQuery (whom, QueryRoutine);
/*
 * Done.
 */
	Py_INCREF (Py_None);
	return (Py_None);
}




static int
QueryRoutine (char *s)
/*
 * Deal with a response from a query.
 */
{
	PyObject *arg, *ret;
/*
 * Build up the argument.  A null string means an end to the responses;
 * in that case pass through None.
 */
	if (s)
		arg = Py_BuildValue ("(s)", s);
	else
		arg = Py_BuildValue ("(O)", Py_None);
/*
 * Ship it through.
 */
	ExceptionReturn = FALSE;
	ret = PyEval_CallObject (PQueryRoutine, arg);
	Py_DECREF (arg);
	Py_XDECREF (ret);
/*
 * Check for exceptions and return.
 */
	if (ret == NULL)
		ExceptionReturn = TRUE;
	return (0);
}



static PyObject *
ZPmsg_ELog (PyObject *self, PyObject *args)
/*
 * The event logger interface:
 *
 * 	msg.ELog (type, string)
 *
 * The string is passed through unchanged -- use the python "%" substitution
 * mechanism if need be.
 */
{
	int type;
	char *s;
/*
 * Are we connected?
 */
	if (! ConnCheck ())
		return (NULL);
/*
 * Just send it through.  Indirect it via "%s" so that percent signs will
 * not get messed with.
 */
	if (! PyArg_ParseTuple (args, "is", &type, &s))
		return (NULL);
	msg_ELog (type, "%s", s);
/*
 * Done.
 */
	Py_INCREF (Py_None);
	return (Py_None);
}
