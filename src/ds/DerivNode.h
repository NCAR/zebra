# ifndef __zebra_DerivNode_h
# define __zebra_DerivNode_h

# include "Field.h"
# include <string.h>

class DerivNode;
class ConstNode;
class OpNode;
class RawFldNode;
class FuncNode;

//
// Abstract class DerivNode, of which there are four instantiable types:
// ConstNode, OpNode, RawFldNode, and FuncNode.
//
// Public member functions:
//
//	ostream& PutTo( ostream& s ) const
//		Write a representation of yourself to ostream s.  This 
//		function will be called when the "<<" operator is used.
//
//	DerivNode* Copy( void ) const
//		Return a pointer to a new DerivNode which is a copy of 
//		yourself.
//
//	DerivNode* MetaEval( const Field *avail, int navail, 
//			     const Field *cantuse = 0, int ncantuse = 0 ) const
//		Return a pointer to a new DerivNode equivalent to yourself, 
//		but having only fields from the "avail" list and constants 
//		as leaf nodes.  If this isn't possible, return 0.
//
class DerivNode
{
    public:
	virtual ostream& PutTo( ostream& s ) const = 0;
	virtual DerivNode* Copy( void ) const = 0;
	virtual DerivNode* MetaEval( const Field *avail, int navail, 
				     const Field *cantuse = 0, 
				     int ncantuse = 0 ) const = 0;
};

ostream& operator <<( ostream& s, const DerivNode& dnode );



//
// ConstNode - a numeric constant (leaf node)
//
class ConstNode : public DerivNode
{
    public:
	ConstNode( double v ) { val = v; }
	inline double Value( void ) const { return val; }
	ostream& PutTo( ostream& s ) const { return( s << val ); }
	DerivNode* Copy ( void ) const { return new ConstNode( val ); }
	DerivNode* MetaEval( const Field *avail, int navail, 
			     const Field *cantuse = 0, 
			     int ncantuse = 0 ) const;
    private:
	double	val;
};




//
// OpNode - an operator and two nodes as operands
//
class OpNode : public DerivNode
{
    public:
	// l and r become the responsibility of the DerivNode (i.e., don't
	// delete them yourself!)
	OpNode( const char* op,  DerivNode* l, DerivNode* r );
	OpNode( const OpNode& n );
	~OpNode( void );
	const char* Oper( void ) const { return oper; }
	const DerivNode& Left( void ) const { return *left; }
	const DerivNode& Right( void ) const { return *right; }
	ostream& PutTo( ostream& s ) const;
	DerivNode* Copy( void ) const { return new OpNode( *this ); }
	DerivNode* MetaEval( const Field *avail, int navail, 
			     const Field *cantuse = 0, 
			     int ncantuse = 0 ) const;
    private:
	char*	oper;
	DerivNode*	left;
	DerivNode*	right;
};



//
// RawFldNode - leaf node that just contains a field identifier
//
class RawFldNode : public DerivNode
{
    public:
	// the Field pointer passed to the constructor becomes the property
	// of the created RawFldNode
	inline RawFldNode( Field* f ) { fld = f; }
	RawFldNode( const RawFldNode& n ) { fld = new Field( *(n.fld) ); }
	inline ~RawFldNode( void ) { delete fld; }
	const Field& Fld( void ) const { return *fld; }
//	ostream& PutTo( ostream& s ) const { return( s << *fld ); }
	ostream& PutTo( ostream& s ) const { return( s << fld->Name() ); }
	DerivNode* Copy( void ) const { return new RawFldNode( *this ); }
	DerivNode* MetaEval( const Field *avail, int navail, 
			     const Field *cantuse = 0, 
			     int ncantuse = 0 ) const;
    private:
	Field*	fld;
};



//
// FuncNode - a function with 0-4 nodes as arguments
//
class FuncNode : public DerivNode
{
    public:
	// any arg nodes passed will become the responsibility of the
	// FuncNode (i.e., don't delete them yourself!)
	FuncNode( const char* funcname, DerivNode* a0 = 0, DerivNode* a1 = 0, 
		  DerivNode* a2 = 0, DerivNode* a3 = 0 );
	FuncNode( const FuncNode& n );
	~FuncNode( void );
	const char* FuncName( void ) const { return name; }
	const DerivNode& Arg( int which ) const { return *arg[which]; }
	ostream& PutTo( ostream& s ) const;
	DerivNode* Copy( void ) const { return new FuncNode( *this ); }
	DerivNode* MetaEval( const Field *avail, int navail, 
			     const Field *cantuse = 0, 
			     int ncantuse = 0 ) const;
    private:
	char*	name;
	DerivNode*	arg[4];
};

# endif // __zebra_DerivNode_h_
