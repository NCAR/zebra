//
// C++ interface to UI symbol tables.
//

typedef int STTraverseProc (const char *, int, const SValue *, long);

class STable
{
	stbl	table;		// The actual symbol table
public:
	STable () { table = 0; }		// Defer init
	STable (const char *name) { table = usy_c_stbl (name); }
	~STable () { usy_z_stbl (table); }
	void init (const char *name) { table = usy_c_stbl (name); }
//
// Symbol creation.
//
	void set (const char *, int);
	void set (const char *, float);
	void set (const char *, const char *);
	void set (const char *, const void *);
	void set (const char *, const date &);
//
// Retrieval.
//
	int get (const char *, int &) const;
	int get (const char *, float &) const;
	int get (const char *, char *&) const;
	int get (const char *, void *&) const;
	int get (const char *, date &) const;
	int get (const char *, int &, SValue &) const;
	int traverse (STTraverseProc *, long) const;
//
// Deletion.
//
	void zap (const char *sym) { usy_z_symbol (table, sym); }
};





//
// Symbol definition routines.
//

inline void
STable::set (const char *sym, int value)
{
	SValue v;
	v.us_v_int = value;
	usy_s_symbol (table, sym, SYMT_INT, &v);
}


inline void
STable::set (const char *sym, float value)
{
	SValue v;
	v.us_v_float = value;
	usy_s_symbol (table, sym, SYMT_FLOAT, &v);
}


inline void STable::set (const char *sym, const char *value)
{
	SValue v;
	v.us_v_ptr = (char *) value;
	usy_s_symbol (table, sym, SYMT_STRING, &v);
}


inline void
STable::set (const char *sym, const void *value)
{
	SValue v;
	v.us_v_ptr = (char *) value;
	usy_s_symbol (table, sym, SYMT_POINTER, &v);
}


inline void
STable::set (const char *sym, const date &value)
{
	SValue v;
	v.us_v_date = value;
	usy_s_symbol (table, sym, SYMT_DATE, &v);
}



//
// Symbol retrieval.
//
inline int
STable::get (const char *sym, int &value) const
{
	int type;
	SValue v;
	if (! usy_g_symbol (table, sym, &type, &v) || type != SYMT_INT)
		return (0);
	value = v.us_v_int;
	return (1);
}


inline int
STable::get (const char *sym, float &value) const
{
	int type;
	SValue v;
	if (! usy_g_symbol (table, sym, &type, &v) || type != SYMT_FLOAT)
		return (0);
	value = v.us_v_float;
	return (1);
}


inline int
STable::get (const char *sym, char * &value) const
{
	int type;
	SValue v;
	if (! usy_g_symbol (table, sym, &type, &v) || type != SYMT_STRING)
		return (0);
	value = v.us_v_ptr;
	return (1);
}


inline int
STable::get (const char *sym, void * &value) const
{
	int type;
	SValue v;
	if (! usy_g_symbol (table, sym, &type, &v) || type != SYMT_POINTER)
		return (0);
	value = v.us_v_ptr;
	return (1);
}


inline int
STable::get (const char *sym, date &value) const
{
	int type;
	SValue v;
	if (! usy_g_symbol (table, sym, &type, &v) || type != SYMT_DATE)
		return (0);
	value = v.us_v_date;
	return (1);
}

//
// Catchall get function.
//
inline int
STable::get (const char *sym, int &type, SValue &v) const
{
	return (usy_g_symbol (table, sym, &type, &v));
}


//
// Traversal.
//
inline int
STable::traverse (STTraverseProc *proc, long param) const
{
	return (usy_traverse (table, proc, param, TRUE));
}


//
// A couple of template functions to avoid the need to go through
// the "pointer" interface.  We can only hope that people do not
// get their types confused here.
//
template <class C> inline void
StoreSym (STable &st, const char *name, const C &value)
{
	st.set (name, (const void *) &value);
}

template <class C> inline int
GetSym (const STable &st, const char *name, C &value)
{
	void *v;
	if (! st.get (name, v))
		return (0);
	value = * (C *) v;
	return (1);
}
