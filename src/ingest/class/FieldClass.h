/* -*- mode: c++; c-basic-offset: 8; -*-
 *
 * $Id: FieldClass.h,v 2.3 2002-10-21 23:10:03 granger Exp $
 *
 * Interface for the classes in the Field class hierarchy.
 */
#ifndef _FieldClass_h_
#define _FieldClass_h_

#include <iostream>
#include "Field.h"
#include <DataStore.h>

// Given a compiler type, define a template function to return the
// corresponding DCT element type.  The specializations below supply
// the correct enum for each template type.
template <class T> inline DC_ElemType dctype() { return DCT_Unknown; }

#define TypeElementPair(c,d) \
template <> inline DC_ElemType dctype<c>() { return d; }

TypeElementPair(unsigned char,DCT_UnsignedChar)
TypeElementPair(short,DCT_ShortInt)
TypeElementPair(int,DCT_Integer)
TypeElementPair(long,DCT_LongInt)
TypeElementPair(float,DCT_Float)
TypeElementPair(double,DCT_Double)

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
 * A ZField is a Zebra field, meaning it is all an EField is but it also
 * has a FieldId assigned to itself at construction.  Creating the ID
 * represents the registration of a "class" of fields with the DataStore.
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


/*
 * A FieldClass is a ZField which can also return its element type.
 * This is the class meant to implement a Field as a Type, specifying
 * the interface to the information shared by a "class" of fields.
 */
class FieldClass : public ZField
{
public:
    FieldClass (const char *name_, 
		const char *type_,
		const char *units_,
		const char *title_ = 0) : 
	ZField (name_, type_, units_, title_)
    { }

    virtual DC_ElemType elementType () const = 0;

    virtual ~FieldClass () { }
};

// Now implement the elementType() method in template subclasses.
template <class T>
class FieldClassT : public FieldClass
{
public:
    FieldClassT (const char *name_, 
		 const char *type_,
		 const char *units_,
		 const char *title_ = 0) : 
	FieldClass (name_, type_, units_, title_)
    { }

    virtual DC_ElemType elementType () const
    {
	return dctype<T>();
    }
};


// Now use the FieldClassT as the information "implementation" behind
// a full-fledged class corresponding to a ZField.

/* So now we try to define a mechanism for defining fields as user types.
 * The "class information", or the field information, is a FieldClassT
 * singleton referenced by the static class methods.  In most other aspects
 * a field instance behaves just like a regular storage type.
 *
 * FieldClassImpl is the state and behavior for a singleton field class,
 * and FieldImpl is the class which actually defines the type-related
 * state and behavior and from which fields instances are actually
 * declared.  The FieldImpl inherits the class methods of the state
 * singleton from FieldClassImpl.
 *
 * Because the correct class methods are discriminated by the Type of a
 * specialized template, subclasses of the field class type do not inherit
 * any space overhead.  The subclasses implement the storage type and only
 * require space for the data value storage, while still looking like a
 * complete class and allowing each instance of the field class to share
 * the same "class" information through the static info pointer. 
 */

// At least under gcc, empty (size 0) objects are not allowed, so inheriting
// this class automatically adds to the size of the subclass.
// So we implement the FieldClassImpl superclass as a macro which 
// all fields include into their own definition.
// 
#define FIELDCLASSIMPL(F) \
protected: \
	typedef F field_type; \
 \
	static inline const FieldClass &field() \
	{ \
		if (!info) \
			info = field_type::newFieldClass(); \
		return *info; \
	} \
 \
public: \
 \
	static inline const char* Name() \
	{ return (field().Name()); } \
	static inline const char* TypeName() \
	{ return (field().TypeName()); } \
	static inline const char* Desc() \
	{ return (field().Desc()); } \
	static inline const char* Units() \
	{ return (field().Units()); } \
	static const char* FullName() \
	{ return (field().FullName()); } \
	static inline FieldId fieldId () \
	{ return field().fieldId(); } \
 \
    static inline DC_ElemType elementType() \
    { return field().elementType(); } \
 \
	static inline const FieldClass &fieldClass () { return field(); } \
 \
	operator const FieldClass & () \
	{ \
		return field(); \
	} \
 \
private: \
	static FieldClass *info



// This is the class for data instances of a Field.  "Class" methods
// are inherited from the FieldClassImpl template type, so that the
// field data instantiations do not need any extra space to point to
// the class implementation.  The implementation here is specific to
// data values.
//
template <class F, class T>
class FieldType
{
	FIELDCLASSIMPL(F);
public:
	typedef T datum_type;

	FieldType (T v = T()) : m_value(v)
	{ }

	inline const T &value () const { return m_value; }
	inline T &value () { return m_value; }

	//inline operator T () const { return m_value; }
	inline operator T& () { return m_value; }

	bool set (T value_, const char *units_)
	{
		// convert units
		m_value = value_;
	}

protected:
	T m_value;
};


#ifdef FC_DEFINE_FIELDS
#define DefineTypedFieldClass(FC,TYPE,NAME,TYPENAME,UNITS,DESC) \
struct FC##Impl { \
	static inline FieldClass *newFieldClass () \
 { return new FieldClassT<TYPE> (NAME, TYPENAME, UNITS, DESC); } }; \
typedef FieldType<FC##Impl,TYPE> FC; \
FieldClass *FC::info = 0;
#else
#define DefineTypedFieldClass(FC,TYPE,NAME,TYPENAME,UNITS,DESC) \
struct FC##Impl { \
	static inline FieldClass *newFieldClass () \
 { return new FieldClassT<TYPE> (NAME, TYPENAME, UNITS, DESC); } }; \
typedef FieldType<FC##Impl,TYPE> FC;
#endif

#define DefineField(FC,NAME,TYPENAME,UNITS,DESC) \
DefineTypedFieldClass(FC,float,NAME,TYPENAME,UNITS,DESC)
#define DefineTypedField(FC,TYPE,NAME,TYPENAME,UNITS,DESC) \
DefineTypedFieldClass(FC,TYPE,NAME,TYPENAME,UNITS,DESC)


/*
 * Provide for subclasses of a field class type, where the type name and
 * member value type are the same, but the field name, units, and description
 * may differ.
 */

template <class F, class SF, class ST>
class SubFieldClass : public FieldType<SF,ST>
{
public:
	// Note the datum_type type must be qualified as is for GCC 3.2
	// to grok it without complaining about an implicit typename.
	SubFieldClass (typename FieldType<SF,ST>::datum_type v =
		       datum_type())
		: FieldType<SF,ST>(v)
	{ }
};

#define DefineFieldSubclass(FC,SC) \
class FC##Type {}; \
typedef SubFieldClass<FC##Type, SC::field_type, SC::datum_type> FC



#endif /* _FieldClass_h_ */
