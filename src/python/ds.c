/*
 * ds: Python module to provide Zebra data store services 
 */
# if __cplusplus
extern "C"
{
# endif
    
# include <python2.2/Python.h>
# include <time.h>

# include <message.h>
# include <defs.h>
# include <DataStore.h>


/*
 * If the Python side requests new data notifications, we need a function
 * to call 
 */
static PyObject* Update;

/*
 * Our very own "FatalError" exception type
 */
static PyObject* ExcFatal;

/*
 * Our prototypes
 */
static PyObject* ZPds_RequestUpdates (PyObject*, PyObject*);
static void ZPds_NewData (PlatformId pid, int param, ZebraTime *t);
static int ZPds_DSCheck (void);

/*
 * The method table.
 */
static PyMethodDef Methods[] =
{
	{ "RequestUpdates", ZPds_RequestUpdates, 1},
	{ NULL, NULL }
};




void
initds ()
/*
 * The module initializer called from the python interpreter.
 */
{
    PyObject *module, *dict;
    int i;
/*
 * Get the module initialized
 */
    module = Py_InitModule ("ds", Methods);
    dict = PyModule_GetDict (module);
/*
 * Set up our exception
 */
    ExcFatal = PyString_FromString ("FatalError");
    PyDict_SetItemString (dict, "FatalError", ExcFatal);
}
		


static PyObject *
ZPds_RequestUpdates (PyObject *self, PyObject *args)
/*
 * 
 */
{
    PlatformId pid;
    char *platname;
/*
 * Make sure we've initialized the DS
 */
    if (! ZPds_DSCheck())
	return 0;
/*
 * Parse out the platform and the function to call when new data are available
 */
    if (Update)
	Py_XDECREF (Update);
    
    if (! PyArg_ParseTuple (args, "sO", &platname, &Update))
	return 0;
/*
 * Make sure the handler is kosher and increment its reference count
 */
    if (! PyFunction_Check (Update) && ! PyMethod_Check (Update))
	return 0;
	
    Py_INCREF (Update);
/*
 * Get PlatformId
 */
    if ((pid = ds_LookupPlatform (platname)) == BadPlatform)
    {
	msg_ELog (EF_INFO, "Bad platform '%s'", platname);
	Py_INCREF (Py_None);
	return (Py_None);
    }
/*
 * Cancel any old notification request
 */
    ds_CancelNotify();
/*
 * Ask for notification request
 */
    ds_RequestNotify( pid, 0, ZPds_NewData );

    Py_INCREF (Py_None);
    return (Py_None);
}



static int
ZPds_DSCheck( void )
{
    static initialized = 0;
    
    if (! initialized)
    {
	if (! ds_Initialize())
	{
	    PyErr_SetString (ExcFatal, "Error initializing datastore");
	    return 0;
	}
	initialized = 1;
    }
    return 1;
}



static void
ZPds_NewData (PlatformId pid, int param, ZebraTime *t)
{
    PyObject *args;
    double dtime = t->zt_Sec + 1.0e-6 * t->zt_MicroSec;

    args = Py_BuildValue ("(sid)", ds_PlatformName (pid), param, dtime);

    PyEval_CallObject (Update, args);

    Py_XDECREF (args);
}


# if __cplusplus
} // end extern "C"
# endif
