/*
 * zebra: Python wrapper module for other Zebra modules.  It is necessary
 * for all of the modules to be in the same shared library so that they have
 * the same name space for the msg_lib stuff.
 */
# if __cplusplus
extern "C"
{
# endif
    
# include <python2.2/Python.h>

/*
 * The (empty) method table.
 */
static PyMethodDef Methods[] =
{
    { NULL, NULL }
};



void
initzebra ()
/*
 * Just import our member modules
 */
{
/*
 * Initialize the "zebra" parent module
 */
    PyObject* zmodule = Py_InitModule ("zebra", Methods);
    PyObject* zdict = PyModule_GetDict (zmodule);
/*
 * The ADD_SUBMODULE macro just calls the init<submodule>() function, 
 * gets a pointer to the submodule using PyImport_AddModule(), then adds 
 * the submodule to the "zebra" module's dictionary.
 */
# define ADD_SUBMODULE(_submod_) \
    { \
	extern void init##_submod_(); \
	char submodname[] = #_submod_; \
	PyObject* submodule; \
	\
	init##_submod_(); \
	submodule = PyImport_AddModule (submodname); \
	PyDict_SetItemString (zdict, submodname, submodule); \
    }
/*
 * Add the submodules
 */
    ADD_SUBMODULE (msg);
    ADD_SUBMODULE (ds);
}


# if __cplusplus
} // end of extern "C"
# endif
