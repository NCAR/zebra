/* -*- C++ -*-
 *
 * $Id: FieldClass.h,v 2.1 1999-07-10 01:15:36 granger Exp $
 *
 * Interface for the classes in the Field class hierarchy.
 */
#ifndef _FieldClass_h_
#define _FieldClass_h_

#include <iostream>
#include "Field.h"

/*
 * An EField is implemented as a Field, but it enforces the name, type, and
 * units parameters, so as to make an "E"xplicit "Field".
 */
class EField : public Field
{
public:
	EField (const char *name_, 
		const char *type_,
		const char *units_,
		const char *title_ = 0) : 
		Field (name_, type_, units_, title_)
	{ }
};


/*
 * A ZField is a Zebra field, meaning it is all an EField is but it
 * also has a FieldId assigned to itself at construction.
 */
class ZField : public EField
{
public:
	ZField (const char *name_, 
		const char *type_,
		const char *units_,
		const char *title_ = 0) : 
		EField (name_, type_, units_, title_)
	{
		// Note the order for the F_Field call is different
		// from the usual order for all of the Field class
		// constructors.
		fid = F_Field (name_, type_, title_, units_);
	}
	
	inline FieldId fieldId () const
	{
		return fid;
	}

protected:
	FieldId fid;

};


/* So now we try to define a mechanism for defining fields as user types.
 * The "class information", or the field information, is a ZField singleton
 * referenced by the static class methods.  In most other aspects a field
 * instance behaves just like a regular storage type.
 *
 * FieldClassImpl is the state and behavior for a singleton field class,
 * and FieldClass is the class which actually defines the type-related
 * state and behavior and from which fields instances are actually
 * declared.  The FieldClass inherits the class methods of the state
 * singleton from FieldClassImpl. */

template <class F>
class FieldClassImpl
{
protected:
	typedef F field_type;

	FieldClassImpl () {}

	static inline const ZField &field()
	{
		if (!info)
			info = field_type::newZField();
		return *info;
	}

public:

	// The encapsulation of the field type access as class members
	// through our singleton object implementation
	static inline const char* Name()
	{ return (field().Name()); }
	static inline const char* TypeName()
	{ return (field().TypeName()); }
	static inline const char* Desc()
	{ return (field().Desc()); }
	static inline const char* Units()
	{ return (field().Units()); }
	static const char* FullName()
	{ return (field().FullName()); }
	static inline FieldId fieldId ()
	{ return field().fieldId(); }

	static inline const ZField &zField () { return field(); }

	// We can look like a ZField by returning our singleton when cast
	operator const ZField & ()
	{
		return field();
	}

private:
	static ZField *info;
};



template <class F, class T>
class FieldClass : public FieldClassImpl<F>
{
public:
	typedef T value_type;

	FieldClass (T v = T()) : m_value(v)
	{ }

	inline const T &value () const { return m_value; }
	inline T &value () { return m_value; }

	inline operator T () const { return m_value; }
	inline operator T& () { return m_value; }

	bool set (T value_, const char *units_)
	{
		// convert units
		m_value = value_;
	}

protected:
	T m_value;
};


#define DefineTypedFieldClass(FC,TYPE,NAME,TYPENAME,UNITS,DESC) \
struct FC##Impl { \
	static inline ZField *newZField () \
 { return new ZField (NAME, TYPENAME, UNITS, DESC); } }; \
typedef FieldClass<FC##Impl,TYPE> FC; \
ZField *FieldClassImpl<FC##Impl>::info = 0;

#define DefineField(FC,NAME,TYPENAME,UNITS,DESC) \
DefineTypedFieldClass(FC,float,NAME,TYPENAME,UNITS,DESC)


/*
 * Provide for subclasses of a field class type, where the type name and
 * member value type are the same, but the field name, units, and description
 * may differ.
 */

template <class F, class SF, class ST>
class SubFieldClass : public FieldClass<SF,ST>
{
public:
	SubFieldClass (value_type v = value_type())
		: FieldClass<SF,ST>(v)
	{ }
};

#define DefineFieldSubclass(FC,SC) \
class FC##Type {}; \
typedef SubFieldClass<FC##Type, SC::field_type, SC::value_type> FC



#endif /* _FieldClass_h_ */
