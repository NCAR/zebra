/* Python module interface to the UNIDATA netCDF library       */
/* copywrite 1996 Bill Noon, Northeast Regional Climate Center */
/* Version dated 9/3/96                                        */
#include "Python.h"

#include <string.h>
#include "netcdf.h"
#include "udunits.h"

#define PyInit_NC initnc

#define NEW_VARNUM 10
#define INC_VARNUM 5
#define MAX_VARS 128
#define MAX_DIMS 10

static PyObject *PyNC_Error;

typedef struct {
	int id;
	long len;
	struct PyNCvar_Object *varobj;
} reclist;

typedef struct {
	PyObject_HEAD
	char *filename;
	int ncid, rwmode, defmode, opened;
	int ndims, nvars, ngatts, recdim;
	int nrecvar;
	int have_time;
	int debug;
	utUnit timebase;
	reclist recs[MAX_VARS];
} PyNCfile_Object;

typedef struct {
	int id;
	int len;
	int size;
	int start;
	int end;
} dimlist;

typedef struct {
	PyObject_HEAD
	PyNCfile_Object *ncfile;
	char name[MAX_NC_NAME];
	int varid;
	nc_type vartype;
	int natts;
	int ndims;
	int do_unit;
	int debug;
	utUnit db_unit, out_unit;
	double db_slope, db_intr, out_slope, out_intr;
	dimlist dims[MAX_DIMS];
	int (* assign)();
} PyNCvar_Object;

typedef int (* assign_func_type) ();

staticforward PyTypeObject PyNCfile_Type;
staticforward PyTypeObject PyNCvar_Type;
staticforward PyObject *PyNC_get_var(PyNCfile_Object *self, PyObject *args);

static size_t ncSizeOf[10];

static PyObject *
nc_err_(message)
	char *message;
{
	PyErr_SetString(PyNC_Error, message);
	return NULL;
}

static PyObject *
nc_err_d(message, num)
	char *message;
	int num;
{
	char errbuf[100];
	sprintf(errbuf, message, num);
	PyErr_SetString(PyNC_Error, errbuf);
	return NULL;
}

static PyObject *
nc_err_sd(message, str, num)
	char *message, *str;
	int num;
{
	char errbuf[100];
	sprintf(errbuf, message, str, num);
	PyErr_SetString(PyNC_Error, errbuf);
	return NULL;
}

static PyObject *
nc_err_s(message, str)
	char *message, *str;
{
	char errbuf[100];
	sprintf(errbuf, message, str);
	PyErr_SetString(PyNC_Error, errbuf);
	return NULL;
}

/* Don't use self -- only a place holder for the info when needed for the    */
/* udunits conversions.  May be NULL when these routines used for attributes */
/* assign_1str doesn't use self, assign_str does */
   
static int
assign_1str(self, obj, tptr, len, count)
	PyNCvar_Object *self;
	PyObject *obj;
	char **tptr;
	int len, *count;
{
	if PyString_Check(obj) {
		*count += len;
		if (*count > len) return -1;
		strncpy(*tptr, PyString_AsString(obj), len);
		*tptr += (size_t)len;
		return 0;
	}
	return -2;
}

static int
assign_str(self, obj, tptr, len, count)
	PyNCvar_Object *self;
	PyObject *obj;
	char **tptr;
	int len, *count;
{
	int t_len;
	if PyString_Check(obj) {
		t_len = self->dims[self->ndims -1].len;
		*count += t_len;
		if (*count > len) return -1;
		strncpy(*tptr, PyString_AsString(obj), t_len);
		*tptr += (size_t)t_len;
		return 0;
	}
	return -2;
}

static int
assign_c(self, obj, tptr, len, count)
	PyNCvar_Object *self;
	PyObject *obj;
	char **tptr;
	int len, *count;
{
	if PyString_Check(obj) {
		strncpy(*tptr, PyString_AsString(obj), 1);
		(*tptr)++; (*count)++;
		return 0;
	}
	return -2;
}

static int
assign_b(self, obj, tptr, len, count)
	PyNCvar_Object *self;
	PyObject *obj;
	char **tptr;
	int len, *count;
{
	if PyInt_Check(obj) {
		**tptr = (char) PyInt_AsLong(obj);
		(*tptr)++; (*count)++;
		return 0;
	}
	if PyFloat_Check(obj) {
		**tptr = (char) PyFloat_AsDouble(obj);
		(*tptr)++; (*count)++;
		return 0;
	}
	return -2;
}

static int
assign_s(self, obj, tptr, len, count)
	PyNCvar_Object *self;
	PyObject *obj;
	short **tptr;
	int len, *count;
{
	if PyInt_Check(obj) {
		**tptr = (short) PyInt_AsLong(obj);
		(*tptr)++; (*count)++;
		return 0;
	}
	if PyFloat_Check(obj) {
		**tptr = (short) PyFloat_AsDouble(obj);
		(*tptr)++; (*count)++;
		return 0;
	}
	return -2;
}

static int
assign_l(self, obj, tptr, len, count)
	PyNCvar_Object *self;
	PyObject *obj;
	nclong **tptr;
	int len, *count;
{
	if PyInt_Check(obj) {
		**tptr = (nclong) PyInt_AsLong(obj);
		(*tptr)++; (*count)++;
		return 0;
	}
	if PyFloat_Check(obj) {
		**tptr = (nclong) PyFloat_AsDouble(obj);
		(*tptr)++; (*count)++;
		return 0;
	}
	return -2;
}

static int
assign_f(self, obj, tptr, len, count)
	PyNCvar_Object *self;
	PyObject *obj;
	float **tptr;
	int len, *count;
{
	if PyInt_Check(obj) {
		**tptr = (float) PyInt_AsLong(obj);
		(*tptr)++; (*count)++;
		return 0;
	}
	if PyFloat_Check(obj) {
		**tptr = (float) PyFloat_AsDouble(obj);
		(*tptr)++; (*count)++;
		return 0;
	}
	return -2;
}

static int
assign_d(self, obj, tptr, len, count)
	PyNCvar_Object *self;
	PyObject *obj;
	double **tptr;
	int len, *count;
{
	if PyInt_Check(obj) {
		**tptr = (double) PyInt_AsLong(obj);
		(*tptr)++; (*count)++;
		return 0;
	}
	if PyFloat_Check(obj) {
		**tptr = (double) PyFloat_AsDouble(obj);
		(*tptr)++; (*count)++;
		return 0;
	}
	return -2;
}

static assign_func_type assign_func[] = {
	0,
	(assign_func_type) assign_b,
	(assign_func_type) assign_c,
	(assign_func_type) assign_s,
	(assign_func_type) assign_l,
	(assign_func_type) assign_f,
	(assign_func_type) assign_d
	};
	
static int
assign_ub(self, obj, tptr, len, count)
	PyNCvar_Object *self;
	PyObject *obj;
	char **tptr;
	int len, *count;
{
	if PyInt_Check(obj) {
		**tptr = (char) (self->db_slope *  PyInt_AsLong(obj) + self->db_intr);
		(*tptr)++; (*count)++;
		return 0;
	}
	if PyFloat_Check(obj) {
		**tptr = (char) (self->db_slope *  PyFloat_AsDouble(obj) + self->db_intr);
		(*tptr)++; (*count)++;
		return 0;
	}
	return -2;
}

static int
assign_us(self, obj, tptr, len, count)
	PyNCvar_Object *self;
	PyObject *obj;
	short **tptr;
	int len, *count;
{
	if PyInt_Check(obj) {
		**tptr = (short) (self->db_slope *  PyInt_AsLong(obj) + self->db_intr);
		(*tptr)++; (*count)++;
		return 0;
	}
	if PyFloat_Check(obj) {
		**tptr = (short) (self->db_slope *  PyFloat_AsDouble(obj) + self->db_intr);
		(*tptr)++; (*count)++;
		return 0;
	}
	return -2;
}

static int
assign_ul(self, obj, tptr, len, count)
	PyNCvar_Object *self;
	PyObject *obj;
	nclong **tptr;
	int len, *count;
{
	if PyInt_Check(obj) {
		**tptr = (nclong) (self->db_slope *  PyInt_AsLong(obj) + self->db_intr);
		(*tptr)++; (*count)++;
		return 0;
	}
	if PyFloat_Check(obj) {
		**tptr = (nclong) (self->db_slope *  PyFloat_AsDouble(obj) + self->db_intr);
		(*tptr)++; (*count)++;
		return 0;
	}
	return -2;
}

static int
assign_uf(self, obj, tptr, len, count)
	PyNCvar_Object *self;
	PyObject *obj;
	float **tptr;
	int len, *count;
{
	if PyInt_Check(obj) {
		**tptr = (float) (self->db_slope *  PyInt_AsLong(obj) + self->db_intr);
		(*tptr)++; (*count)++;
		return 0;
	}
	if PyFloat_Check(obj) {
		**tptr = (float) (self->db_slope *  PyFloat_AsDouble(obj) + self->db_intr);
		(*tptr)++; (*count)++;
		return 0;
	}
	return -2;
}

static int
assign_ud(self, obj, tptr, len, count)
	PyNCvar_Object *self;
	PyObject *obj;
	double **tptr;
	int len, *count;
{
	if PyInt_Check(obj) {
		**tptr = self->db_slope * (double) PyInt_AsLong(obj) + self->db_intr;
		(*tptr)++; (*count)++;
		return 0;
	}
	if PyFloat_Check(obj) {
		**tptr = self->db_slope * (double) PyFloat_AsDouble(obj) + self->db_intr;
		(*tptr)++; (*count)++;
		return 0;
	}
	return -2;
}

static assign_func_type assign_ufunc[] = {
	0,
	(assign_func_type) assign_ub,
	0,
	(assign_func_type) assign_us,
	(assign_func_type) assign_ul,
	(assign_func_type) assign_uf,
	(assign_func_type) assign_ud
	};
	
static int
assign_mem(self, func, obj, tptr, len, count)
	PyNCvar_Object *self;
	assign_func_type *func;
	PyObject *obj;
	nclong **tptr;
	int len, *count;
{
	int i, number, ret = 0;
	
	if PyTuple_Check(obj) {
		number = PyTuple_Size(obj);
		for (i=0; i < number; i++) {
			if (*count >= len) return -1;
			if (ret = assign_mem(self,func, PyTuple_GetItem(obj,i),tptr,len,count))
				return ret;
		}
		return 0;
	}
	if PyList_Check(obj) {
		number = PyList_Size(obj);
		for (i=0; i < number; i++) {
			if (*count >= len) return -1;
			if (ret = assign_mem(self,func, PyList_GetItem(obj,i),tptr,len,count))
				return ret;
		}
		return 0;
	}
	return (*func)(self,obj, tptr, len, count);
}

static PyObject *
get_var(self, varid)
	PyNCfile_Object *self;
	int varid;
{
	PyNCvar_Object *the_var;
	char name[MAX_NC_NAME];
	int i, ncid, ndims, natts, dimids[MAX_DIMS];
	long length;

	ncid = self->ncid;
	the_var = PyObject_NEW(PyNCvar_Object, &PyNCvar_Type);
	if (the_var == NULL)
		return NULL;
	the_var->varid = varid;
	the_var->ncfile = self;
	the_var->debug = 0;
	the_var->do_unit = 0;
	ncvarinq(ncid,varid, name, &the_var->vartype, 
		&the_var->ndims, dimids, &the_var->natts);
	(void) strcpy(the_var->name, name);
	for (i=the_var->ndims-1;i>=0;i--) {
		the_var->dims[i].id = dimids[i];
		ncdiminq(ncid, dimids[i], (char *)0, &length);
		the_var->dims[i].len = length;
	}
	the_var->dims[the_var->ndims-1].size = ncSizeOf[the_var->vartype];
	for (i=the_var->ndims-1;i>0;i--) {
		the_var->dims[i-1].size = the_var->dims[i].len * the_var->dims[i].size;
	}
	if (the_var->vartype == NC_CHAR && the_var->ndims != 0) 
		the_var->assign = (assign_func_type) assign_str;
	else 
		the_var->assign = assign_func[the_var->vartype];
	return (PyObject *)the_var;
}

static PyObject *
sync_recvar(self)
	PyNCfile_Object *self;
{
	reclist *recs = self->recs;
	void **old;
	int i, nRecVar, old_nRecVar;
	int *recids;
	long *reclens, t_len;
	
	nRecVar = self->nrecvar;
	old_nRecVar = nRecVar;
	if (old_nRecVar > 0) {
		old = malloc(sizeof(PyNCvar_Object *)*nRecVar);
		for (i=0; i<nRecVar; i++)
			old[i] = (void *)recs[i].varobj;
	}
	else 
		old = malloc(1); /* just to make free easier */
	
	if (ncrecinq(self->ncid, &nRecVar, (void *)0, (void *)0) == -1) {
		free(old);
		return nc_err_d("Problem getting record variable information <%d>",ncerr);
	}
	self->nrecvar = nRecVar;
	if (nRecVar > 0) {
		recids = malloc(sizeof(int)*nRecVar);
		reclens = malloc(sizeof(long)*nRecVar);
		if (ncrecinq(self->ncid, &nRecVar, recids, reclens) == -1) {
			free(old);
			free(recids);
			free(reclens);
			return nc_err_d("Problem getting record variable information <%d>",ncerr);
		}
		
		for (i=0; i<nRecVar; i++) {
			recs[i].id = recids[i];
			recs[i].len = (((reclens[i] +3)/4L)*4L); /* needed to fix unaligned access on alpha */
			recs[i].varobj = (void *) get_var(self, recids[i]);
			/* may need to do a Py_INCREF here */
		}
		free(recids);
		free(reclens);
	}
	
	for (i=0; i<old_nRecVar; i++)
		Py_DECREF((PyObject *)old[i]);
		
	free(old);
	return Py_None;
}

static void
PyNCfile_Dealloc(fileobject)
	PyNCfile_Object *fileobject;
{
	int i;
	
	if (fileobject->opened) {
		if (ncabort(fileobject->ncid) == -1) {
			PyErr_SetString(PyNC_Error, "Problem closing netCDF file");
		}
	}

	if (fileobject->debug) {
		printf("Deallocating ncfile: %s\n",fileobject->filename);
	}
	free(fileobject->filename);
	if (fileobject->nrecvar > 0)
		for (i=0; i<fileobject->nrecvar; i++)
			Py_DECREF((PyObject *)(fileobject->recs[i].varobj));
	PyMem_DEL(fileobject);
}

void sync_ncfile(self)
	PyNCfile_Object *self;
{
	int ndims, nvars, ngatts, recdim;

	(void) ncinquire(self->ncid, &ndims, &nvars, &ngatts, &recdim);
	self->ndims = ndims;
	self->nvars = nvars;
	self->ngatts = ngatts;
	self->recdim = recdim;
}

static PyObject *
PyNC_create(self, args)
	PyObject *self;
	PyObject *args;
{
	PyNCfile_Object *ncfile;
	char *name;
	int mode = NC_NOCLOBBER, ncid, i;
	if (!PyArg_ParseTuple(args, "s|i", &name, &mode))
		return NULL;

	if ( ((mode & 0x0f) != NC_NOCLOBBER) && ((mode & 0x0f) != NC_CLOBBER))
		return nc_err_d("mode must be CLOBBER or NOCLOBBER (%d)",mode);
	
	ncid = nccreate(name,mode);
	if (ncid == -1)
		return nc_err_sd("Problem creating %s netCDF file (%d)",name,ncerr);

	ncfile = PyObject_NEW(PyNCfile_Object, &PyNCfile_Type);
	if (ncfile == NULL)
		return NULL;
	ncfile->filename = malloc(strlen(name)+1);
	(void) strcpy(ncfile->filename, name);
	ncfile->rwmode = 1;
	ncfile->defmode = 1;
	ncfile->ncid = ncid;
	ncfile->opened = 1;
	ncfile->debug = 0;
	ncfile->nrecvar = -1; /* set recvar to not inited */
	if (utIsTime(&ncfile->timebase) == UT_ENOINIT) 
		if (utInit("") != 0) 
			Py_FatalError("Can't initialize the udunits package");
	ncfile->have_time = 0;
	sync_ncfile(ncfile);

	return (PyObject *)ncfile;
}

static PyObject *
PyNC_open(self, args)
	PyObject *self;
	PyObject *args;
{
	PyNCfile_Object *ncfile;
	char *name;
	int mode = NC_NOWRITE,ncid, i;

	if (!PyArg_ParseTuple(args, "s|i", &name, &mode))
		return NULL;

	if ( (mode != NC_NOWRITE) && (mode != NC_WRITE))
		return nc_err_d("mode must be WRITE or NOWRITE (%d)",mode);
	
	ncid = ncopen(name,mode);
	if (ncid == -1)
		return nc_err_sd("Problem opening %s netCDF file (%d)",name,ncerr);

	ncfile = PyObject_NEW(PyNCfile_Object, &PyNCfile_Type);
	if (ncfile == NULL)
		return NULL;
	ncfile->filename = malloc(strlen(name)+1);
	(void) strcpy(ncfile->filename, name);
	ncfile->rwmode = mode;
	ncfile->defmode = 0;
	ncfile->ncid = ncid;
	ncfile->opened = 1;
	ncfile->debug = 0;
	ncfile->nrecvar = -1; /* set recvar to not inited */
	if (ncfile->debug)
		printf("opened file %s ncid: %d\n",name,ncid);
	if (ncattinq(ncid,NC_GLOBAL,"_basetime",(nc_type *)0,&i)!=-1) {
		char *time;
		time = malloc(i+1);
		if (utIsTime(&ncfile->timebase) == UT_ENOINIT)
			if (utInit("") != 0) 
			Py_FatalError("Can't initialize the udunits package");
		ncattget(ncid,NC_GLOBAL,"_basetime",time);
		time[i] = '\000';
		if ((utScan(time,&ncfile->timebase) == 0) && 
			(utIsTime(&ncfile->timebase)))
			ncfile->have_time = 1;
		else ncfile->have_time = 0;
		free(time);
	}
	else ncfile->have_time = 0;
	sync_ncfile(ncfile);
	
	return (PyObject *)ncfile;
}

static PyObject *
PyNC_close(self, args)
	PyNCfile_Object *self;
	PyObject *args;
{
	int i;
	if (ncclose(self->ncid) == -1)
		return nc_err_d("Problem closing the netCDF file (%d)",ncerr);

	self->opened = 0;

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
PyNC_abort(self, args)
	PyNCfile_Object *self;
	PyObject *args;
{
	int i;
	if (ncabort(self->ncid) == -1)
		return nc_err_d("Problem reverting the netCDF file (%d)",ncerr);

	self->opened = 0;

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
PyNC_sync(self, args)
	PyNCfile_Object *self;
	PyObject *args;
{
	if (ncsync(self->ncid) == -1)
		return nc_err_d("Problem syncing the netCDF file (%d)",ncerr);

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
PyNC_endef(self, args)
	PyNCfile_Object *self;
	PyObject *args;
{
	if (ncendef(self->ncid) == -1)
		return nc_err_d("Problem leaving define mode (%d)",ncerr);

	self->defmode = 0;
	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
PyNC_redef(self, args)
	PyNCfile_Object *self;
	PyObject *args;
{
	if (ncredef(self->ncid) == -1)
		return nc_err_d("Problem reentering define mode (%d)",ncerr);

	self->defmode = 1;
	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
PyNC_setfill(self, args)
	PyNCfile_Object *self;
	PyObject *args;
{
	if (ncsetfill(self->ncid, NC_FILL) == -1)
		return nc_err_d("Problem setting fill mode (%d)",ncerr);
	
	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
PyNC_setnofill(self, args)
	PyNCfile_Object *self;
	PyObject *args;
{
	if (ncsetfill(self->ncid, NC_NOFILL) == -1)
		return nc_err_d("Problem setting nofill mode (%d)",ncerr);
	
	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
PyNC_def_dim(self, args)
	PyNCfile_Object *self;
	PyObject *args;
{
	char *name;
	int length;

	if (self->defmode == 0)
		return nc_err_("Must enter define mode to define dimensions");
	if (self->rwmode == NC_NOWRITE)
		return nc_err_("File must be opened in WRITE mode to define dimensions");
	
	if (!PyArg_ParseTuple(args, "si", &name, &length))
		return NULL;
	if (length == NC_UNLIMITED && self->recdim != -1)
		return nc_err_("Record dimension already defined");
	if (ncdimdef(self->ncid, name, length) == -1)
		return nc_err_sd("Problem defining new dimension <%s> (%d)",name,ncerr);
	
	sync_ncfile(self);
	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
PyNC_def_att(self, args)
	PyNCfile_Object *self;
	PyObject *args;
{
	assign_func_type func = (assign_func_type) assign_1str;
	char errbuf[100], *name, *var_name;
	int nctype, var_id, length = -1, dim_array[2], ret, count;
	size_t size;
	PyObject *data;
	char *t_data, *t_ptr;

	if (!PyArg_ParseTuple(args,"ssiO|i",&name,&var_name,&nctype,&data,&length))
		return NULL;
	if (nctype < NC_BYTE || nctype > NC_DOUBLE)
		return nc_err_d("nc variable type is invalid: %d",nctype);
	
	if (strlen(var_name) == 0) var_id = NC_GLOBAL;
	else {
		var_id = ncvarid(self->ncid, var_name);
		if (var_id == -1)
			return nc_err_sd("Variable <%s> does not exist (%d)",var_name,ncerr);
	}
	if (PyList_Check(data))
		data = PyList_AsTuple(data);
	if (length == -1)
		if (PyString_Check(data))
			length = PyString_Size(data);
		else if (PyTuple_Check(data))
			length = PyTuple_Size(data);
	else length = 1;

	count = 0;
	size = ncSizeOf[nctype] * (length + 1);
	t_ptr = t_data = malloc(size);
	if (PyString_Check(data)) 
		ret = assign_mem(NULL, &func, data, &t_ptr, length, &count);
	else
		ret = assign_mem(NULL, &assign_func[nctype], data, &t_ptr, length, &count);
	if (ret) {
		free(t_data);
		if (ret == -2)
			return nc_err_d("Data doesn't match type <%d>",nctype);
		if (ret == -1)
			return nc_err_("Inconsistent number of values");		
	}

	if (ncattput(self->ncid,var_id,name,nctype,length,t_data)==-1) {
		free(t_data);
		sprintf(errbuf,"Problem putting attribute <%s> in variable <%s> (%d)",
			name, var_name, ncerr);
		PyErr_SetString(PyNC_Error, errbuf);
		return NULL;
	}

	if ((var_id == NC_GLOBAL) && (strcmp(name,"_basetime")==0)) {
		if ((utScan(t_data,&self->timebase) == 0) && 
			(utIsTime(&self->timebase)))
			self->have_time = 1;
		else self->have_time = 0;
	}
	free(t_data);
	sync_ncfile(self);
	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
PyNC_def_var(self, args)
	PyNCfile_Object *self;
	PyObject *args;
{
	char *name, *dim_name;
	PyObject *obj=NULL, *dim_obj;
	int nctype, n_dim, dims[MAX_DIMS], i;

	if (!PyArg_ParseTuple(args, "si|O", &name, &nctype, &obj))
		return NULL;
	if (nctype < NC_BYTE || nctype > NC_DOUBLE)
		return nc_err_d("nc variable type is invalid: %d",nctype);
	
	if (obj == NULL) {
		n_dim = 0;
		dims[0] = -1;
	}
	else if (PyString_Check(obj)) {
		n_dim = 1;
		dim_name = PyString_AsString(obj);
		dims[0] = ncdimid(self->ncid, dim_name);
		if (dims[0] == -1)
			return nc_err_sd("Dimension <%s> isn't valid (%d)",dim_name,ncerr);
	}
	else if (PyTuple_Check(obj)) {
		n_dim = PyTuple_Size(obj);
		for (i=0;i<n_dim;i++) {
			dim_obj = PyTuple_GetItem(obj, i);
			if (!PyString_Check(dim_obj)) 
				return nc_err_("Dimension names must be strings");
			dim_name = PyString_AsString(dim_obj);
			dims[i] = ncdimid(self->ncid, dim_name);
			if (dims[i] == -1)
				return nc_err_sd("Dimension <%s> isn't valid (%d)",dim_name,ncerr);
			if (i != 0 && dims[i] == self->recdim)
				return nc_err_s("Record dimension <%s> must be first",dim_name);
		}
	}

	else 
		return nc_err_("Third argument must be a tuple of dimension names");

	if (ncvardef(self->ncid, name, (nc_type)nctype, n_dim, dims) == -1)
		return nc_err_sd("Unable to create variable <%s> (%d)",name,ncerr);
	
	sync_ncfile(self);

	Py_INCREF(Py_None);
	return Py_None;
	}

static PyObject *
PyNC_list_dim(self, args)
	PyNCfile_Object *self;
	PyObject *args;
{
	int i;
	long length;
	char *name;
	char dim_name[MAX_NC_NAME];

	if (PyArg_ParseTuple(args, "")) {
		PyObject *list = PyList_New(self->ndims);
		for (i=0;i<self->ndims;i++) {
			ncdiminq(self->ncid, i, dim_name, &length);
			PyList_SetItem(list, i, Py_BuildValue("(sl)",dim_name,length));
		}
		return list;
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject * make_var_tuple(ncid,varid)
	int ncid, varid;
{
	int i;
	int n_dims, n_atts, dimids[MAX_DIMS];
	nc_type datatype;
	char var_name[MAX_NC_NAME];
	PyObject *dimlist, *tlist;

	ncvarinq(ncid, varid, var_name, &datatype, &n_dims,
	dimids, &n_atts);
	dimlist = PyList_New(n_dims);
	for (i=0;i<n_dims;i++)
		PyList_SetItem(dimlist, i, PyInt_FromLong((long)dimids[i]));
	tlist = Py_BuildValue("(siiiO)",var_name,(int)datatype,n_atts,
		n_dims,dimlist);
	Py_DECREF(dimlist);
	return tlist;
}

static PyObject *
PyNC_list_var(self, args)
	PyNCfile_Object *self;
	PyObject *args;
{
	int i, varid;
	char *name;
	char errbuf[100];

	if (PyArg_ParseTuple(args, "")) {
	PyObject *list = PyList_New(self->nvars);
	for (i=0;i<self->nvars;i++) {
		PyList_SetItem(list, i, make_var_tuple(self->ncid,i));
		}
	return list;
	}

	if (PyArg_ParseTuple(args, "s", &name)) {
	varid = ncvarid(self->ncid, name);
	if (varid == -1)
		return nc_err_sd("Invalid variable name <%s> (%d)",name,ncerr);
	return make_var_tuple(self->ncid,varid);
	}

	if (PyArg_ParseTuple(args, "i", &varid)) {
	i = ncvarinq(self->ncid, varid, (char *)0, (nc_type *)0,
			(int *)0, (int *)0, (int *)0);
	if (i == -1) {
		sprintf(errbuf,"Invalid variable id <%d> (%d)",varid,ncerr);
		PyErr_SetString(PyNC_Error, errbuf);
		return NULL;
		}
	return make_var_tuple(self->ncid,varid);
	}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
fill_att_value(datatype, data_ptr, length)
	int datatype;
	void *data_ptr;
	int length;
{
	if (datatype == NC_CHAR) 
		return PyString_FromStringAndSize((char*)data_ptr, length);
	if (length == 1)
		switch (datatype) {
			case NC_BYTE: 
				return PyInt_FromLong((long) *((char*)data_ptr));
			case NC_SHORT:
				return PyInt_FromLong((long) *((short*)data_ptr));
			case NC_LONG:
				return PyInt_FromLong((long) *((nclong*)data_ptr));
			case NC_FLOAT:
				return PyFloat_FromDouble((double) *((float*)data_ptr));
			case NC_DOUBLE:
				return PyFloat_FromDouble(*((float*)data_ptr)++);
		}
	else {
		PyObject *t_list;
		int i, size = ncSizeOf[datatype];
		
		t_list = PyList_New(length);
		for (i=0; i<length; i++)
			PyList_SetItem(t_list, i, fill_att_value(datatype, 
				(void*)((size_t)data_ptr+(i*size)), 1));
		return t_list;
	}
}				
				
static PyObject *
make_att_value(ncid,varid,name)
	int ncid, varid;
	char *name;
{
	int length;
	nc_type datatype;
	void *t_ptr, *t_data;
	PyObject *ret;

	ncattinq(ncid, varid, name, &datatype, &length);
	t_ptr = t_data = malloc(ncSizeOf[datatype] * length);
	ncattget(ncid, varid, name, t_ptr);
	ret = fill_att_value(datatype, t_ptr, length);
	free(t_ptr);
	return ret;
}

static PyObject *
build_att_dict(ncid, varid, natts)
	int ncid, varid, natts;
{
	int attid;
	char name[MAX_NC_NAME], errbuf[100];
	PyObject *dict, *value, *att;

	dict = PyDict_New();
	for (attid=0; attid < natts; attid++) {
		if (ncattname(ncid, varid, attid, name) == -1) {
			sprintf(errbuf,
				"Problem getting attribute name <%d> <%d> (%d)",attid,varid,ncerr);
			PyErr_SetString(PyNC_Error, errbuf);
			Py_DECREF(dict);
			return NULL;
			}
		att = PyString_FromString(name);
		value = make_att_value(ncid,varid,name);
		PyDict_SetItem(dict, att, value);
		Py_DECREF(att);
		Py_DECREF(value);
		}
	return dict;
}

static PyObject *
PyNC_list_att(self, args)
	PyNCfile_Object *self;
	PyObject *args;
{
	int i, natts, attid, varid;
	char name[MAX_NC_NAME], *vname=NULL;

	if (PyArg_ParseTuple(args, "|s", &vname)) {
		if (vname == NULL) {
			varid = NC_GLOBAL;
			natts = self->ngatts;
			}
		else {
			varid = ncvarid(self->ncid, vname);
			if (varid == -1)
				return nc_err_sd("Invalid variable name <%s> (%d)",vname,ncerr);
			if (ncvarinq(self->ncid, varid, (char *)0, (nc_type *)0,
				(int *)0, (int *)0, &natts) == -1)
				return nc_err_sd("Problem getting attribute number for variable name <%s> (%d)", vname, ncerr);
			}
		return build_att_dict(self->ncid, varid, natts);
		}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject * 
PyNC_date2rec(self, args)
	PyNCfile_Object *self;
	PyObject *args;
{
	int utError;
	int year, month=1, day=1, hour=0, minute=0;
	float second=0.0;
	double value;

	if (!self->have_time) {
		return nc_err_("Basetime attribute not properly defined");
		}
	if (!PyArg_ParseTuple(args, "i|iiiif", &year, &month, &day, &hour, 
		&minute, &second))
		return NULL;
	utError = utInvCalendar(year,month,day,hour,minute,second,&self->timebase,&value);
	if (utError != 0)
		return nc_err_d("Error in utInvCalendar <%d>",utError);
	return PyInt_FromLong((long)value);
}

static PyObject * 
PyNC_rec2date(self, args)
	PyNCfile_Object *self;
	PyObject *args;
{
	int utError;
	int year, month=1, day=1, hour=0, minute=0;
	float second=0.0;
	double value;

	if (!self->have_time) {
		return nc_err_("Basetime attribute not properly defined");
		}
	if (!PyArg_ParseTuple(args, "d", &value))
		return NULL;
	utError = utCalendar(value,&self->timebase,&year,&month,&day,&hour,&minute,&second);
	if (utError != 0)
		return nc_err_d("Error in utCalendar <%d>",utError);
	return Py_BuildValue("(iiiiif)",year,month,day,hour,minute,second);
}

static PyObject *
PyNC_var_unit(self, args)
	PyNCvar_Object *self;
	PyObject *args;
{
	int utError, att_len;
	int ncid, varid;
	char *out_units, *db_units = NULL;
	nc_type att_type;
	
	if (self->vartype == NC_CHAR)
		return nc_err_("Variable must be a numberic type.");
		
	if (!PyArg_ParseTuple(args, "s|s", &out_units, &db_units))
		return NULL;
	
	if (strlen(out_units) == 0) { /* cancel using unit conversions */
		self->do_unit = 0;
		self->assign = assign_func[self->vartype];
		return Py_None;
	}
	
	if ((utError = utScan(out_units, &self->out_unit)) != 0)
		return nc_err_sd("problem converting <%s> to units. <%d>",out_units, utError);
	
	ncid = self->ncfile->ncid;
	varid = self->varid;
	if (db_units == NULL) { /* use the _units attribute if it exists */
		if ((ncattinq(ncid, varid, "_units", &att_type, &att_len) == -1)
				|| (att_type != NC_CHAR)) /* does not exist */
			return nc_err_("Must have some unit defined");
		/* use the _units attribute */
		db_units = (char *) malloc(att_len+1);
		if (ncattget(ncid, varid, "_units", db_units) == -1) {
			free(db_units);
			return nc_err_sd("Problem getting _units attribute. <%d>",ncerr);
		}
		
		db_units[att_len] = '\0'; /* terminate the string */
		utError = utScan(db_units, &self->db_unit);
		free(db_units);
	}
	else /* use the specified db_units */
		utError = utScan(db_units, &self->db_unit);
		
	if (utError != 0) 
		return nc_err_d("problem converting database to units. <%d>", utError);
			
	utError = utConvert(&self->out_unit,&self->db_unit,&self->db_slope,&self->db_intr);
	if (utError != 0) 
		return nc_err_sd("Problem converting from <%s>: <%d>",out_units,utError);
	
	utError = utConvert(&self->db_unit,&self->out_unit,&self->out_slope,&self->out_intr);
	if (utError != 0) 
		return nc_err_sd("Problem converting to <%s>: <%d>",out_units,utError);
	
	self->do_unit = 1;
	self->assign = assign_ufunc[self->vartype];
	
	return Py_None;
}

static PyObject *
PyNC_var_cvtdb(self, args)
	PyNCvar_Object *self;
	PyObject *args;
{
	PyObject *value;
	double t_value;
	
	if (self->do_unit == 0)
		return nc_err_("Must set the units to use");
	if (!PyArg_ParseTuple(args, "O", &value))
		return NULL;
	if (PyInt_Check(value))
		t_value = (double) PyInt_AsLong(value);
	else if (PyFloat_Check(value))
		t_value = PyFloat_AsDouble(value);
	else
		return nc_err_("Must be integer or float");
	return PyFloat_FromDouble(t_value * self->db_slope + self->db_intr);

}

static PyObject *
PyNC_var_cvtout(self, args)
	PyNCvar_Object *self;
	PyObject *args;
{
	PyObject *value;
	double t_value;
	
	if (self->do_unit == 0)
		return nc_err_("Must set the units to use");
	if (!PyArg_ParseTuple(args, "O", &value))
		return NULL;
	if (PyInt_Check(value))
		t_value = (double) PyInt_AsLong(value);
	else if (PyFloat_Check(value))
		t_value = PyFloat_AsDouble(value);
	else
		return nc_err_("Must be integer or float");
	return PyFloat_FromDouble(t_value * self->out_slope + self->out_intr);

}

static void
PyNCvar_Dealloc(var_object)
	PyNCvar_Object *var_object;
{
	if (var_object->debug) {
		printf("Deallocating ncvariable: %s\n",var_object->name);
		}
	PyMem_DEL(var_object);
}

static PyObject *
PyNC_var_dim(self, args)
	PyNCvar_Object *self;
	PyObject *args;
{
	int i;

	if (PyArg_ParseTuple(args, "")) {
		PyObject *list = PyList_New(self->ndims+1);
		for (i=0;i<=self->ndims;i++) {
			PyList_SetItem(list, i, Py_BuildValue("(iii)",
			self->dims[i].id, self->dims[i].len, self->dims[i].size));
			}
		return list;
		}

	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *
PyNC_var_att(self, args)
	PyNCvar_Object *self;
	PyObject *args;
{
	if (self->natts > 0)
		return build_att_dict(self->ncfile->ncid, self->varid, self->natts);
	Py_INCREF(Py_None);
	return Py_None;
}

static int
var_length(self)
	PyNCvar_Object *self;
{
	long dim_len;

	if (self->ndims == 0)
		return 1;
	return self->dims[0].len;
}

static PyObject *
make_var_list(self, data_ptr, dim)
	PyNCvar_Object *self;
	void *data_ptr;
	int dim;
{
	if (self->vartype == NC_CHAR && dim == self->ndims - 1) {
		int size;
		PyObject *t_obj;
		char *t_string;

		size = self->dims[dim].len;
		t_string = malloc(size+1);
		t_string[size] = '\000';
		(void) strncpy(t_string, data_ptr, size);
		t_obj = PyString_FromString(t_string);
		free(t_string);
		return t_obj;
	}
	
	if (dim == self->ndims) {
		if (self->do_unit == 0)
			switch (self->vartype) {
				case NC_CHAR:
					return PyString_FromStringAndSize( data_ptr, 1);
				case NC_BYTE:
					return PyInt_FromLong((long) *((char*)data_ptr));
				case NC_SHORT:
					return PyInt_FromLong((long) *((short*)data_ptr));
				case NC_LONG:
					return PyInt_FromLong((long) *((nclong*)data_ptr));
				case NC_FLOAT:
					return PyFloat_FromDouble((double) *((float*)data_ptr));
				case NC_DOUBLE:
					return PyFloat_FromDouble(*((double*)data_ptr));
			}
		else {
			double t_value;
			switch (self->vartype) {
				case NC_BYTE:
					t_value = (double) *((char*)data_ptr);
					break;
				case NC_SHORT:
					t_value = (double) *((short*)data_ptr);
					break;
				case NC_LONG:
					t_value = (double) *((nclong*)data_ptr);
					break;
				case NC_FLOAT:
					t_value = (double) *((float*)data_ptr);
					break;
				case NC_DOUBLE:
					t_value = *((double*)data_ptr);
					break;
			}
			t_value = t_value * self->out_slope + self->out_intr;
			return PyFloat_FromDouble(t_value);
		}
	}
	else {
		PyObject *t_list;
		int i, size, n_elements;

		size = self->dims[dim].size;
		n_elements = self->dims[dim].len;
		t_list = PyList_New(n_elements);
		for (i=0;i<n_elements;i++)
			PyList_SetItem(t_list, i,
				make_var_list(self, (void *)((size_t)data_ptr+(i*size)),dim+1));
		return t_list;
	}
	return nc_err_("Error making var list. Shouldn't get here");
}

static PyObject *
PyNCvar_slice(self, ilow, ihigh)
	PyNCvar_Object *self;
	int ilow, ihigh;
{
	PyObject *tList;
	static long start[MAX_DIMS], count[MAX_DIMS];
	void * t_data;
	int i, size, n_elements;

	if ((self->ndims == 0) && (ilow != 0 || ihigh != 1))
		return nc_err_s("Variable <%s> is a scalar, only arrays can be sliced",
			self->name);

	start[0]=ilow;
	count[0]=ihigh-ilow;
	if (self->ndims != 0) {
		for (i=1;i<self->ndims;i++) {
			start[i] = 0;
			count[i] = self->dims[i].len;
		}
		size = self->dims[0].size;
	} else size = ncSizeOf[self->vartype];
	
	t_data = malloc(count[0] * size);
	
	if (ncvarget(self->ncfile->ncid, self->varid, start, count, t_data) == -1)
		return nc_err_sd("Problem getting data for variable <%s> (%d)",
			self->name, ncerr);

	n_elements = ihigh-ilow;
	if (n_elements == 1 && self->ndims == 0)
		tList = make_var_list(self, t_data,0);
	else if (n_elements == 1)
		tList = make_var_list(self, t_data,1);
	else {
		tList = PyList_New(n_elements);
		for (i=0;i<n_elements;i++)
			PyList_SetItem(tList, i,
				make_var_list(self, (void *)((size_t)t_data + (i*size)),1));
	}

	free(t_data);
	return tList;
}

static PyObject *
PyNCvar_item(self, i)
	PyNCvar_Object *self;
	int i;
{
	return PyNCvar_slice(self, i, i+1);
}

static int
PyNCvar_ass_slice(self, ilow, ihigh, obj)
	PyNCvar_Object *self;
	int ilow, ihigh;
	PyObject *obj;
{
	int len, i, ret, t_len = 0, ndims = self->ndims;
	void *tptr, *ptr;
	char errbuf[100];
	long *start, *count, length;
	
	if (ilow < 0 || ilow >= ihigh) {
		PyErr_SetString(PyNC_Error, "Invalid assignment index");
		return -1;
	}
	
	len = ihigh - ilow;
	if (self->vartype != NC_CHAR) {
		if (ndims > 0) {
			if (self->ncfile->recdim != self->dims[0].id && ihigh > self->dims[0].len) {
				sprintf(errbuf,"Unable to extend non-record dimension: (%d) out of bounds",
					ihigh);
				PyErr_SetString(PyNC_Error, errbuf);
				return -1;
			}
			for (i=1; i < ndims; i++)
				len *= self->dims[i].len;
		}
	}
	else {
		if (ndims == 0) { /* scalar character */
			if (len != 1) {
				PyErr_SetString(PyNC_Error, "Cannot assign an array to a scalar");
				return -1;
			}
		}
		else if (ndims == 1) { /* simple string */
			if ((ilow == 0 && ihigh == 1) || (ilow == 0 && ihigh == self->dims[0].len)) {
				len = self->dims[0].len;
			}
			else { /* sub string */
				PyErr_SetString(PyNC_Error, "Not handled yet");
				return -1;
			}
		}
		else { /* array of strings */
			for (i=1; i < ndims -1; i++)
				len *= self->dims[i].len;
			len *= self->dims[ndims-1].len;
		}
	}
	tptr = ptr = malloc(len * ncSizeOf[self->vartype]);
	if (ret = assign_mem(self, &(self->assign), obj, &tptr, len, &t_len)) {
		free(ptr);
		if (ret == -2) {
			PyErr_SetString(PyNC_Error, "Incorrect data type");
			return -1;
		}
		if (ret == -1) {
			PyErr_SetString(PyNC_Error, "Incorrect number of data items");
			return -1;
		}
		return ret;  /* just in case */
	}
	/* make sure exact number of values */
	if (t_len != len) {
		free(ptr);
		PyErr_SetString(PyNC_Error, "Incorrect number of data items");
		return -1;
	}

	if (ndims == 0) {
		start = malloc(sizeof(long));
		count = malloc(sizeof(long));
	} else {
		start = malloc(ndims * sizeof(long));
		count = malloc(ndims * sizeof(long));
	}
	start[0] = ilow;
	count[0] = ihigh - ilow;
	for (i=1;i<ndims;i++) {
		start[i] = 0;
		count[i] = self->dims[i].len;
	}
	ret = ncvarput(self->ncfile->ncid,self->varid,start,count,ptr);
	free(ptr);
	free(start);
	free(count);
	if (ret == -1) {
		sprintf(errbuf,"Error in putting data into variable (%d)",ncerr);
		PyErr_SetString(PyNC_Error, errbuf);
		return -1;
	}
	if (ihigh > self->dims[0].len) {
		ncdiminq(self->ncfile->ncid,self->dims[0].id,(char *)0, &length);
		self->dims[0].len = length;
	}
	return 0;
}

static int
PyNCvar_ass_item(self, i, obj)
	PyNCvar_Object *self;
	int i;
	PyObject *obj;
{
	return PyNCvar_ass_slice(self, i, i+1, obj);
}

static PyObject *
PyNC_get_rec(self, args)
	PyNCfile_Object *self;
	PyObject *args;
{
	PyObject *var_tuple = NULL, *temp, *name, *dict;
	long recnum;
	size_t size;
	void **data, *t_ptr, *data_ptr;
	int i, nRecVar;
	reclist *recs = self->recs;
	
	if (!PyArg_ParseTuple(args, "l|O", &recnum, &var_tuple))
		return NULL;
	if (var_tuple == NULL) {
		if (self->recdim == -1)
			return nc_err_("There is no record dimension");
		if (self->nrecvar == -1) {
			temp = sync_recvar(self);
			if (temp == NULL)
				return temp;
		}
		nRecVar = self->nrecvar;
		if (nRecVar == 0)
			return nc_err_("There are no record variables defined");
		size = 0;
		for (i=0; i<nRecVar; i++)
			size += (size_t) recs[i].len;
		t_ptr = data_ptr = malloc(size);
		data = malloc(sizeof(size_t) * nRecVar);
		for (i=0; i<nRecVar; i++) {
			data[i] = t_ptr;
			t_ptr = (void *)((size_t)t_ptr + (size_t) recs[i].len);
		}
		if (ncrecget(self->ncid, recnum, data) == -1) {
			free(data);
			free(data_ptr);
			return nc_err_d("Problem with ncrecget <%d>", ncerr);
		}
		dict = PyDict_New();
		if (dict == NULL) {
			free(data);
			free(data_ptr);
			return NULL;
		}
		for (i=0; i<nRecVar; i++) {
			if (((PyNCvar_Object *)recs[i].varobj)->ndims == 0) 
				temp = make_var_list(recs[i].varobj, data[i], 0);
			else
				temp = make_var_list(recs[i].varobj, data[i], 1);
			name = PyString_FromString(((PyNCvar_Object *)recs[i].varobj)->name);
			if ((temp == NULL) || (name == NULL)) {
				free(data);
				free(data_ptr);
				Py_DECREF(dict);
				return NULL;
			}
			PyDict_SetItem(dict, name, temp);
			Py_DECREF(temp);
			Py_DECREF(name);
		}
		free(data);
		free(data_ptr);
		return dict;
	}
	Py_INCREF(Py_None);
	return Py_None;
}

static PyMethodDef PyNCvar_methods[] =
{
	{"list_dim", (PyCFunction)PyNC_var_dim, 1},
	{"list_att", (PyCFunction)PyNC_var_att, 1},
	{"use_unit", (PyCFunction)PyNC_var_unit, 1},
	{"cvt_to_nc", (PyCFunction)PyNC_var_cvtdb, 1},
	{"cvt_from_nc", (PyCFunction)PyNC_var_cvtout, 1},
	{NULL, NULL}
};

static PyObject *
PyNCvar_getattr(self, name)
	PyNCvar_Object *self;
	char *name;
{
	PyObject *method;

	method = Py_FindMethod(PyNCvar_methods, (PyObject *)self, name);
	if (method != NULL)
		return method;
	PyErr_Clear();

	if (strcmp(name, "name") == 0)
		return PyString_FromString((char *)self->name);
	if (strcmp(name, "ndims") == 0)
		return PyInt_FromLong((long)self->ndims);
	if (strcmp(name, "natts") == 0)
		return PyInt_FromLong((long)self->natts);
	if (strcmp(name, "ncfile") == 0) {
		Py_INCREF(self->ncfile);
		return (PyObject *)self->ncfile;
	}
	{
		int ncid = self->ncfile->ncid, varid = self->varid, attid;
		nc_type t_type;
		int t_len;
		PyObject *t_obj;
		
		if (ncattinq(ncid, varid, name, &t_type, &t_len) != -1) {
			t_obj = make_att_value(ncid, varid, name);
			return t_obj;
		}
	}
	PyErr_SetString(PyExc_AttributeError, name);
	return NULL;
}

static PySequenceMethods PyNCvar_as_sequence = {
	(inquiry)var_length,				/*sq_length*/
	(binaryfunc)0,						/*sq_concat*/
	(intargfunc)0,						/*sq_repeat*/
	(intargfunc)PyNCvar_item,			/*sq_item*/
	(intintargfunc)PyNCvar_slice,		/*sq_slice*/
	(intobjargproc)PyNCvar_ass_item,	/*sq_ass_item*/
	(intintobjargproc)PyNCvar_ass_slice,/*sq_ass_slice*/
};

static PyTypeObject PyNCvar_Type = {
	PyObject_HEAD_INIT(&PyType_Type)
	0,								/* ob_size */
	"netCDF variable",				/* tp_name */
	sizeof(PyNCvar_Object),			/* tp_basicsize */
	0,								/* tp_itemsize */
	/* methods */
	(destructor)PyNCvar_Dealloc,	/* tp_dealloc */
	0,								/* tp_print */
	(getattrfunc)PyNCvar_getattr,	/* tp_getattr */
	(setattrfunc)0,					/* tp_setattr */
	0,								/* tp_compare */
	0,								/* tp_repr */
	0,								/* tp_as_number */
	&PyNCvar_as_sequence,			/* tp_as_sequence */
	0,								/* tp_as_mapping */
};

static PyObject *
PyNC_get_var(self, args)
	PyNCfile_Object *self;
	PyObject *args;
{
	char *name;
	int varid;
	
	if (!PyArg_ParseTuple(args, "s", &name))
		return NULL;
	
	varid = ncvarid(self->ncid, name);
	if (self->debug) 
		printf("inited variable %s varid: %d\n",name,varid);
	if (varid == -1)
		return nc_err_sd("Variable <%s> does not exist (%d)",name,ncerr);
	
	return (PyObject *) get_var(self, varid);
}

static PyMethodDef PyNCfile_methods[] =
{
	{"create", PyNC_create, 1},
	{"open", PyNC_open, 1},
	{"close", (PyCFunction)PyNC_close, 1},
	{"abort", (PyCFunction)PyNC_abort, 1},
	{"sync", (PyCFunction)PyNC_sync, 1},
	{"endef", (PyCFunction)PyNC_endef, 1},
	{"redef", (PyCFunction)PyNC_redef, 1},
	{"setfill", (PyCFunction)PyNC_setfill, 1},
	{"setnofill", (PyCFunction)PyNC_setnofill, 1},
	{"def_dim", (PyCFunction)PyNC_def_dim, 1},
	{"def_att", (PyCFunction)PyNC_def_att, 1},
	{"def_var", (PyCFunction)PyNC_def_var, 1},
	{"var", (PyCFunction)PyNC_get_var, 1},
	{"list_dim", (PyCFunction)PyNC_list_dim, 1},
	{"list_var", (PyCFunction)PyNC_list_var, 1},
	{"list_att", (PyCFunction)PyNC_list_att, 1},
	{"get_rec", (PyCFunction)PyNC_get_rec, 1},
	{"date2rec", (PyCFunction)PyNC_date2rec, 1},
	{"rec2date", (PyCFunction)PyNC_rec2date, 1},
	{NULL, NULL, NULL}
};

static PyObject *
PyNCfile_getattr(self, name)
	PyNCfile_Object *self;
	char *name;
{
	PyObject *method;
	
	if (!self->opened)
		return nc_err_("NC file is closed.");
	method = Py_FindMethod(PyNCfile_methods, (PyObject *)self, name);
	if (method != NULL)
		return method;
	PyErr_Clear();
	
	if (strcmp(name, "filename") == 0) {
		return PyString_FromString((char *)self->filename);
	}
	if (strcmp(name, "ndims") == 0) {
		return PyInt_FromLong((long)self->ndims);
	}
	if (strcmp(name, "nvars") == 0) {
		return PyInt_FromLong((long)self->nvars);
	}
	if (strcmp(name, "natts") == 0) {
		return PyInt_FromLong((long)self->ngatts);
	}
	if (strcmp(name, "gatts") == 0) {
		int i;
		char att_name[30];
		PyObject *att;
		PyObject *list = PyList_New(self->ngatts);
		for (i=0;i<self->ngatts;i++) {
			ncattname(self->ncid, NC_GLOBAL, i, att_name);
			att = PyString_FromString(att_name);
			PyList_SetItem(list, i, att);
		}
		return list;
	}
	{
		int ncid = self->ncid, varid = -1, attid;
		char a_name[MAX_NC_NAME];
		
		for (attid = 0; attid < self->ngatts; attid++) {
			if (ncattname(ncid, varid, attid, a_name) == -1)
				nc_err_sd("Problem getting attribute name <%s> <%d>", name, ncerr);
			if (strncmp(name, a_name, MAX_NC_NAME) == 0)
				return make_att_value(ncid, varid, a_name);
		}
	}

	PyErr_SetString(PyExc_AttributeError, name);
	return NULL;
}

static PyTypeObject PyNCfile_Type = {
	PyObject_HEAD_INIT(&PyType_Type)
	0,					/* ob_size */
	"netCDF file",			/* tp_name */
	sizeof(PyNCfile_Object),		/* tp_basicsize */
	0,					/* tp_itemsize */
	/* methods */
	(destructor)PyNCfile_Dealloc,	/* tp_dealloc */
	0,					/* tp_print */
	(getattrfunc)PyNCfile_getattr,	/* tp_getattr */
	(setattrfunc)0,			/* tp_setattr */
	0,					/* tp_compare */
	0,					/* tp_repr */
	0,					/* tp_as_number */
	0,					/* tp_as_sequence */
	0,					/* tp_as_mapping */
	0,					/* tp_hash */
};

void
PyInit_NC()
{
	PyObject *m, *d;

	ncopts = 0;
	m = Py_InitModule("nc", PyNCfile_methods);

	d = PyModule_GetDict(m);
	PyNC_Error = Py_BuildValue("s", "nc.error");
	PyDict_SetItemString(d,"error", PyNC_Error);
	PyDict_SetItemString(d,"NOWRITE", PyInt_FromLong((long) NC_NOWRITE));
	PyDict_SetItemString(d,"WRITE", PyInt_FromLong((long) NC_WRITE));
	PyDict_SetItemString(d,"CLOBBER", PyInt_FromLong((long) NC_CLOBBER));
	PyDict_SetItemString(d,"NOCLOBBER", PyInt_FromLong((long)NC_NOCLOBBER));
	PyDict_SetItemString(d,"UNLIMITED", PyInt_FromLong((long)NC_UNLIMITED));
	PyDict_SetItemString(d,"CHAR", PyInt_FromLong((long)(int)NC_CHAR));
	PyDict_SetItemString(d,"BYTE", PyInt_FromLong((long)(int)NC_BYTE));
	PyDict_SetItemString(d,"SHORT", PyInt_FromLong((long)(int)NC_SHORT));
	PyDict_SetItemString(d,"LONG", PyInt_FromLong((long)(int)NC_LONG));
	PyDict_SetItemString(d,"FLOAT", PyInt_FromLong((long)(int)NC_FLOAT));
	PyDict_SetItemString(d,"DOUBLE", PyInt_FromLong((long)(int)NC_DOUBLE));
#ifdef NC_COMPRESS
	PyDict_SetItemString(d,"COMPRESS", PyInt_FromLong((long)NC_COMPRESS));
#endif

	if (utInit("") != 0) 
	Py_FatalError("Can't initialize the udunits package");
	ncSizeOf[NC_BYTE] = sizeof(char);
	ncSizeOf[NC_CHAR] = sizeof(char);
	ncSizeOf[NC_SHORT] = sizeof(short);
	ncSizeOf[NC_LONG] = sizeof(nclong);
	ncSizeOf[NC_FLOAT] = sizeof(float);
	ncSizeOf[NC_DOUBLE] = sizeof(double);
	/* assign_func[NC_CHAR] = &assign_c;
	assign_func[NC_BYTE] = &assign_b;
	assign_func[NC_SHORT] = &assign_s;
	assign_func[NC_LONG] = &assign_l;
	assign_func[NC_FLOAT] = &assign_f;
	assign_func[NC_DOUBLE] = &assign_d; */
	if (PyErr_Occurred())
		Py_FatalError("can't initialize module nc");
}
