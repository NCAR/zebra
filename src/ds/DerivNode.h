//
// DerivNode class: nodes of a derivation tree.  Subclasses include ConstDNode,
// OpDNode, RawFldDNode, and FuncDNode.
// $Id: DerivNode.h,v 3.5 2002-09-17 20:00:18 granger Exp $
//
//
//		Copyright (C) 1998 by UCAR
//	University Corporation for Atmospheric Research
//		   All rights reserved
//
// No part of this work covered by the copyrights herein may be reproduced
// or used in any form or by any means -- graphic, electronic, or mechanical,
// including photocopying, recording, taping, or information storage and
// retrieval systems -- without permission of the copyright owner.
// 
// This software and any accompanying written materials are provided "as is"
// without warranty of any kind.  UCAR expressly disclaims all warranties of
// any kind, either express or implied, including but not limited to the
// implied warranties of merchantibility and fitness for a particular purpose.
// UCAR does not indemnify any infringement of copyright, patent, or trademark
// through use or modification of this software.  UCAR does not provide 
// maintenance or updates for its software.
//
# ifndef __zebra_DerivNode_h
# define __zebra_DerivNode_h

# include <string.h>
# include "Field.h"
# include "DataStore.h"

class DerivTable;
class ResultCache;	// defined in and used in DerivNode.cc

//
// Abstract class DerivNode, of which there are four instantiable types:
// ConstDNode, OpDNode, RawFldDNode, and FuncDNode.
//
// DerivNode public methods:
//
//	const char* ClassId( void ) const
//
//		Return a const string naming the class of the object.
//
//
//	ostream& PutTo( ostream& s ) const
//
//		Write a representation of yourself to ostream s.  This 
//		function will be called when the "<<" operator is used.
//
//
//	DerivNode* Copy( void ) const
//
//		Return a pointer to a new DerivNode which is a copy of 
//		yourself.  It is the caller's responsibility to delete
//		the returned pointer.
//
//
//	Field* FieldList( int* nflds ) const
//
//		Return the list of fields required for this derivation and
//		the length of the returned list.  Each required field will be
//		included in the list exactly once.  It is the responsibility of
//		the caller to delete[] the returned list.  An NULL pointer will
//		be returned if the list is empty.
//
//
//	DerivNode* MetaEval( const DerivTable* tables[], const int ntables,
//			     const Field* avail, const int navail, 
//			     const Field* cantuse = 0, 
//			     const int ncantuse = 0 ) const
//
//		Return a pointer to a new DerivNode equivalent to yourself,
//		but having only RawFldDNodes of fields from the "avail"
//		list and ConstDNodes as leaf nodes.  I.e., return a
//		DerivNode which can be successfully Eval'ed if given the
//		listed available fields.  If this isn't possible, return
//		0. It is the caller's responsibility to delete the returned
//		pointer.  Derivations from the given tables may be used,
//		and the tables will be searched in the order given.  
//		Derivations using fields in the cantuse list will be 
//		disregarded.
//
//
//	void Eval( const Field flds[], const int nflds, const int ndata, 
//		   const double* dataptrs[], double* out_data, 
//		   double badval, ResultCache *rcache = 0 ) const
//
//		Perform the derivation: flds and nflds are a list and count
//		of raw fields; ndata is the number of data points; dataptrs
//		are pointers to data arrays for each of the raw fields;
//		out_data is the array to which the results are to be
//		written; badval is the bad data flag, both for the incoming
//		data and to be used for the results; rcache (optional) is a
//		cache of intermediate results, which if non-zero is
//		searched to see if an equivalent of this node has already
//		been calculated, and to which our results are added if we
//		have to calculate them ourselves.
//
//
//	int operator ==( const DerivNode& d )
//
//		Return non-zero iff DerivNode d is equivalent to this node.
//
//	
// ConstDNode public methods:
//
//	ConstDNode( double v )
//
//		Create a ConstDNode (constant DerivNode) representing the 
//		constant value v.
//
//	double Value( void )
//
//		Return the node's value.
//
//
// OpDNode public methods:
//
//	OpDNode( const char* op,  DerivNode* l, DerivNode* r )
//
//		Create an OpDNode (operator DerivNode): op is an operator
//		from the list "+", "-", "*", "/", "%"; l and r are pointers
//		to DerivNodes for the left and right operands, and BECOME
//		THE PROPERTY OF THE CREATED OPDNODE, hence should not be
//		deleted.
//
//	const char* Oper( void ) const
//
//		Return the operator string.
//
//
//	const DerivNode& Left( void ) const
//	const DerivNode& Right( void ) const
//
//		Return a reference to the left or right operand DerivNode,
//		respectively.
//
//
// RawFldDNode public methods:
//
//	RawFldDNode( Field* f )
//	RawFldDNode( const Field& f )
//	RawFldDNode( const RawFldDNode& n )
//
//		Create a RawFldDNode, with f as the field needed (or copying
//		the field from RawFldDNode n, in the last interface).
//
//
//	const Field& Fld( void ) const
//
//		Return a reference to the node's field.
//
//
// FuncDNode public methods:
//
//	FuncDNode( const char* funcname, DerivNode* a0 = 0, DerivNode* a1 = 0, 
//		   DerivNode* a2 = 0, DerivNode* a3 = 0 )
//
//		Create a FuncDNode: funcname is the name of the function to
//		be called; a0-a3 are up to four DerivNode arguments to be
//		passed to the function.
//
//	FuncDNode( const FuncDNode& n )
//
//		Create a FuncDNode which is a copy of n.
//
//	const char* FuncName( void ) const
//
//		Return the name of the node's function.
//
//	const DerivNode& Arg( int which ) const
//
//		Return a reference to the node's which'th function argument.
//
//
// DerivNode private methods:
//
//		
//	void Calculate( const Field flds[], const int nflds, const int ncalc, 
//			const double* dataptrs[], double* out_data, 
//			const double badval, ResultCache *rcache ) const
//
//		Private function to perform the guts of evaluation specific to
//		a derived class.
//

class DerivNode;
std::ostream& operator <<( std::ostream& s, const DerivNode& dnode );



class DerivNode
{
public:
    virtual ~DerivNode( void ) { }
    virtual const char* ClassId( void ) const = 0;
    virtual std::ostream& PutTo( std::ostream& s ) const = 0;
    virtual DerivNode* Copy( void ) const = 0;
    virtual Field* FieldList( int* nflds ) const = 0;
    virtual DerivNode* MetaEval( const DerivTable* dtables[], 
				 const int ndtables,
				 const Field* avail, const int navail, 
				 const Field* cantuse = 0, 
				 const int ncantuse = 0 ) const = 0;
    void Eval( const Field flds[], const int nflds, const int ndata, 
	       const double* dataptrs[], double* out_data, const double badval,
	       ResultCache *rcache = 0 ) const;
    virtual int operator ==( const DerivNode& d ) const = 0;
private:
    virtual void Calculate( const Field flds[], const int nflds, 
			    const int ndata, const double* dataptrs[], 
			    double* out_data, const double badval, 
			    ResultCache *rcache ) const = 0;
};

//
// ConstDNode - a numeric constant (leaf node)
//
class ConstDNode : public DerivNode
{
public:
    ConstDNode( double v ) { val = v; }
    ~ConstDNode( void ) {}
    inline const char* ClassId( void ) const { return clid; }
    inline double Value( void ) const { return val; }
    std::ostream& PutTo( std::ostream& s ) const;
    DerivNode* Copy ( void ) const { return new ConstDNode( val ); }
    Field* FieldList( int* nflds ) const { *nflds = 0; return 0; };
    DerivNode* MetaEval( const DerivTable* dtables[], const int ndtables,
			 const Field *avail, const int navail, 
			 const Field *cantuse = 0, 
			 const int ncantuse = 0 ) const;
    void Calculate( const Field flds[], const int nflds, const int ndata, 
		    const double* dataptrs[], double* out_data, 
		    const double badval, ResultCache *rcache ) const;
    int operator ==( const DerivNode& d ) const;
private:
    static const char *clid;
    double	val;
};




//
// OpDNode - an operator and two nodes as operands
//
class OpDNode : public DerivNode
{
public:
// l and r become the responsibility of the DerivNode (i.e., don't
// delete them yourself!)
    OpDNode( const char* op,  DerivNode* l, DerivNode* r );
    OpDNode( const OpDNode& n );
    ~OpDNode( void );
    inline const char* ClassId( void ) const { return clid; }
    const char* Oper( void ) const { return oper; }
    const DerivNode& Left( void ) const { return *left; }
    const DerivNode& Right( void ) const { return *right; }
    std::ostream& PutTo( std::ostream& s ) const;
    DerivNode* Copy( void ) const { return new OpDNode( *this ); }
    Field* FieldList( int* nflds ) const;
    DerivNode* MetaEval( const DerivTable* dtables[], const int ndtables, 
			 const Field *avail, const int navail, 
			 const Field *cantuse = 0, 
			 const int ncantuse = 0 ) const;
    void Calculate( const Field flds[], const int nflds, const int ndata, 
		    const double* dataptrs[], double* out_data, 
		    const double badval, ResultCache *rcache ) const;
    int operator ==( const DerivNode& d ) const;
private:
    static const char *clid;
    char*	oper;
    DerivNode*	left;
    DerivNode*	right;
};



//
// RawFldDNode - leaf node that just contains a field identifier
//
class RawFldDNode : public DerivNode
{
public:
// If you use the RawFldDNode( Field* f ) constuctor, the field pointer
// becomes the responsibility of the RawFldDNode.  Don't delete the pointer
// yourself!
    RawFldDNode( Field* f ) { fld = f; }
    RawFldDNode( const Field& f ) { fld = new Field( f ); }
    RawFldDNode( const RawFldDNode& n ) { fld = new Field( *(n.fld) ); }
    ~RawFldDNode( void ) { delete fld; }
    inline const char* ClassId( void ) const { return clid; }
    const Field& Fld( void ) const { return *fld; }
    std::ostream& PutTo( std::ostream& s ) const;
    DerivNode* Copy( void ) const { return new RawFldDNode( *this ); }
    Field* FieldList( int* nflds ) const;
    DerivNode* MetaEval( const DerivTable* dtables[], const int ndtables,
			 const Field *avail, const int navail, 
			 const Field *cantuse = 0, 
			 const int ncantuse = 0 ) const;
    void Calculate( const Field flds[], const int nflds, const int ndata, 
		    const double* dataptrs[], double* out_data, 
		    const double badval, ResultCache *rcache ) const;
    int operator ==( const DerivNode& d ) const;
private:
    static const char *clid;
    Field*	fld;
};



//
// FuncDNode - a function with 0-4 nodes as arguments
//
struct FDNFuncInfo;
const int FuncDNodeMaxArgs = 4;

class FuncDNode : public DerivNode
{
public:
// any arg nodes passed will become the responsibility of the
// FuncDNode (i.e., don't delete them yourself!)
    FuncDNode( const char* funcname, DerivNode* a0 = 0, DerivNode* a1 = 0, 
	       DerivNode* a2 = 0, DerivNode* a3 = 0 );
    FuncDNode( const FuncDNode& n );
    ~FuncDNode( void );
    inline const char* ClassId( void ) const { return clid; }
    const char* FuncName( void ) const;
    const DerivNode& Arg( int which ) const { return *arg[which]; }
    std::ostream& PutTo( std::ostream& s ) const;
    DerivNode* Copy( void ) const { return new FuncDNode( *this ); }
    Field* FieldList( int* nflds ) const;
    DerivNode* MetaEval( const DerivTable* dtables[], const int ndtables,
			 const Field *avail, const int navail, 
			 const Field *cantuse = 0, 
			 const int ncantuse = 0 ) const;
    void Calculate( const Field flds[], const int nflds, const int ndata, 
		    const double* dataptrs[], double* out_data, 
		    const double badval, ResultCache *rcache ) const;
    int operator ==( const DerivNode& d ) const;
private:
    static const char *clid;
    FDNFuncInfo*	funcinfo;
    DerivNode*	arg[FuncDNodeMaxArgs];
};

//
// Definition of FD_Function, the type of function to be called from 
// FuncDNodes, and a public interface to add a function to the FuncDNode 
// classwide table of allowed functions
//
typedef void (*FDFunction)( double *result, const double badval, 
			    const int ndata, ... );
void FDN_AddFunction( const char* fname, const int nargs,  
		      const FDFunction func);

# endif // __zebra_DerivNode_h_
