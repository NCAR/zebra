//
// DerivNode class: nodes of a derivation tree.  Subclasses include ConstDNode,
// OpDNode, RawFldDNode, and FuncDNode.
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
# include <math.h>
# include <stdio.h>
# include "DerivNode.h"
# include "DerivTable.h"

extern "C"
{
# include <met_formulas.h>
}

RCSID ("$Id")

//
// ResultCache class to hold intermediate results while a DerivNode is being
// evaluated.
//
class ResultCache
{
public:
    ResultCache( void );
    ~ResultCache( void );
    void Add( const DerivNode& dnode, const double *data, const int ndata );
    double *Find( const DerivNode& dnode, int *ndata = 0 );
private:
    int	maxcachelen;
    int	cachelen;
    DerivNode **nodes;
    double **results;
    int *lengths;
};



std::ostream& 
operator <<( std::ostream& s, const DerivNode& dnode )
{
    return (dnode.PutTo(s));
}




void
DerivNode::Eval( const Field flds[], const int nflds, const int ndata, 
		 const double* dataptrs[], double* out_data, 
		 const double badval, ResultCache *rcache ) const
//
// Perform the derivation: flds and nflds are a list and count of raw
// fields; ndata is the number of data points; dataptrs are pointers to
// data arrays for each of the raw fields; out_data is the array to which
// the results are to be written; badval is the bad data flag, both for the
// incoming data and to be used for the results; rcache is a cache of
// intermediate results, which if non-zero is searched to see if an
// equivalent of this node has already been calculated, and to which our
// results are added if we have to calculate them ourselves.
//
{
//
// Do we need to create our our result cache?
//
    int make_rcache = (rcache == 0);
//
// Perform the derivation in manageable chunks to limit our intermediate
// memory usage
//
    const int chunklen = 50000;

    for (int offset = 0; offset < ndata; offset += chunklen)
    {
    //
    // How many elements are we doing this time?
    //
	int ncalc = (ndata - offset) < chunklen ? (ndata - offset) : chunklen;
    //
    // Destination location, with offset applied
    //
	double *destptr = out_data + offset;
    //
    // Simple if we find a copy of ourselves in the result cache
    //
	double *results;
	if (rcache && (results = rcache->Find( *this )) != 0)
	{
	    for (int i = 0; i < ncalc; i++)
		destptr[i] = results[i];

	    continue;
	}
    //
    // Make our own result cache if necessary
    //
	if (make_rcache)
	    rcache = new ResultCache;
    //
    // New source data pointers, with offset applied
    //
	const double **srcptrs = new const double*[nflds];
	
	for (int f = 0; f < nflds; f++)
	    srcptrs[f] = dataptrs[f] + offset;
    //
    // Perform the calculation
    //
	Calculate( flds, nflds, ncalc, srcptrs, destptr, badval, rcache );
	delete[] srcptrs;
    //
    // Add our results to the cache if it was given to us.  Delete the cache
    // if we created it ourselves.
    //
	if (make_rcache)
	{
	    delete rcache;
	    rcache = 0;
	}
	else
	    rcache->Add( *this, destptr, ncalc );
    }

    return;
}


//
// ConstDNode member functions
//
const char *ConstDNode::clid = "ConstDNode";


std::ostream& 
ConstDNode::PutTo( std::ostream& s ) const 
{
    return( s << val );
}


DerivNode*
ConstDNode::MetaEval( const DerivTable* dtables[], const int ndtables,
		      const Field *avail, const int navail, 
		      const Field *cantuse, const int ncantuse ) const
{
    return ( Copy() );
}



void
ConstDNode::Calculate( const Field flds[], const int nflds, const int ncalc, 
		       const double* dataptrs[], double* out_data, 
		       const double badval, ResultCache *rcache ) const
{
    for (int i = 0; i < ncalc; i++)
	out_data[i] = val;
}



int
ConstDNode::operator ==( const DerivNode& d ) const
{
    if (ClassId() != d.ClassId())
	return 0;

    ConstDNode *cd = (ConstDNode*)&d;
    return (val == cd->val); 
}


//
// OpDNode member functions
//
const char *OpDNode::clid = "OpDNode";

OpDNode::OpDNode( const char* op,  DerivNode* l, DerivNode* r ) 
{
    oper = new char[strlen( op ) + 1];
    strcpy( oper, op );
// we become the owner of l and r
    left = l;
    right = r;
}


OpDNode::OpDNode( const OpDNode& n )
{
    oper = new char[strlen( n.oper ) + 1];
    strcpy( oper, n.oper );
    left = n.left->Copy();
    right = n.right->Copy();
}


OpDNode::~OpDNode( void ) 
{ 
    delete[] oper; 
    delete left; 
    delete right; 
}
	

std::ostream&
OpDNode::PutTo( std::ostream& s ) const 
{ 
    return( s << "(" << *left << " " << oper << " " << *right << ")"); 
}


Field*
OpDNode::FieldList( int* nflds ) const
{
    int i, nleft, nright;
//
// Get the field lists from our right an left nodes
//
    Field* leftlist = left->FieldList( &nleft );
    Field* rightlist = right->FieldList( &nright );
//
// Allocate retlist big enough to hold both the right and left lists, and
// initialize it from the left list.
//
    Field *retlist = new Field[nleft + nright];
    int retlistlen = 0;
    for (i = 0; i < nleft; i++)
	retlist[retlistlen++] = leftlist[i];
//
// Now merge in the right list, looking out for duplicate fields
//
    for (i = 0; i < nright; i++)
    {
	int	f;
	
	for (f = 0; f < retlistlen; f++)
	    if (rightlist[i] == retlist[f])
		break;

	if (f == retlistlen)
	    retlist[retlistlen++] = rightlist[i];
    }

    delete[] leftlist;
    delete[] rightlist;

    *nflds = retlistlen;
    return retlist;
}
	

DerivNode*
OpDNode::MetaEval( const DerivTable* dtables[], const int ndtables, 
		   const Field *avail, const int navail, 
		   const Field *cantuse, const int ncantuse ) const
{
    DerivNode	*l = 0, *r = 0;

    if ((l = left->MetaEval( dtables, ndtables, avail, navail, 
			     cantuse, ncantuse )) != 0 &&
	(r = right->MetaEval( dtables, ndtables, avail, navail, 
			      cantuse, ncantuse )) != 0)
	return( new OpDNode( oper, l, r ) );
    else
    {
	delete l;
	delete r;
	return( 0 );
    }
}


void
OpDNode::Calculate( const Field flds[], const int nflds, const int ncalc, 
		    const double* dataptrs[], double* out_data, 
		    const double badval, ResultCache *rcache ) const
{
    double* leftdp = new double[ncalc];
    double* rightdp = new double[ncalc];
//
// First evaluate the left and right operands
//
    left->Eval( flds, nflds, ncalc, dataptrs, leftdp, badval, rcache );
    right->Eval( flds, nflds, ncalc, dataptrs, rightdp, badval, rcache );
//
// Operate!
//
    for (int i = 0; i < ncalc; i++)
    {
	double	lval = leftdp[i];
	double	rval = rightdp[i];
	
	if (lval == badval || rval == badval)
	    out_data[i] = badval;
	else
	{
	    if (! strcmp (oper, "+"))
		out_data[i] = lval + rval; 
	    else if (! strcmp (oper, "-"))
		out_data[i] = lval - rval;
	    else if (! strcmp (oper, "*"))
		out_data[i] = lval * rval;
	    else if (! strcmp (oper, "/"))
		out_data[i] = lval / rval;
	    else if (! strcmp (oper, "%"))
		out_data[i] = fmod( lval, rval );
	    else
	    {
		fprintf( stderr, 
			 "Unknown operator '%s' in OpDNode::Calculate()\n",
			 oper );
		out_data[i] = badval;
	    }
	}
    }
//
// Delete the arrays from the evaluated left and right operands
//
    delete[] leftdp;
    delete[] rightdp;
}


int
OpDNode::operator ==( const DerivNode& d ) const
{
    if (ClassId() != d.ClassId())
	return 0;

    OpDNode *od = (OpDNode*)&d;

    if (strcmp (oper, od->oper))
	return 0;

    return (*left == *(od->left) && *right == *(od->right));
}



//
// RawFldDNode member functions
//
const char *RawFldDNode::clid = "RawFldDNode";


std::ostream& 
RawFldDNode::PutTo( std::ostream& s ) const 
{ 
    return( s << fld->FullName() ); 
}


Field* 
RawFldDNode::FieldList( int* nflds ) const
{
    *nflds = 1;
// We have to use "new Field[]" to keep in line with the interface spec, which
// says that the user will be able to use "delete[]" on what we return.
    Field *flist = new Field[1];
    flist[0] = *fld;
    return flist;
}


DerivNode*
RawFldDNode::MetaEval( const DerivTable* dtables[], const int ndtables, 
		       const Field *avail, const int navail, 
		       const Field *cantuse, const int ncantuse ) const
{
    DerivNode	*dtree = 0;
    double	slope, intercept;
    int	i;
//
// First look for this field among the available fields
//
    for (i = 0; i < navail; i++)
    {
	if (! avail[i].CanYield( *fld, &slope, &intercept ))
	    continue;
    //
    // Found it!
    //
	dtree = new RawFldDNode( avail[i] );
    //
    // Put in the slope and intercept for units conversion (if any)
    //	
	if (dtree && slope != 1.0)
	    dtree = new OpDNode( "*", new ConstDNode( slope ), 
				 dtree );

	if (dtree && intercept != 0.0)
	    dtree = new OpDNode( "+", new ConstDNode( intercept ), 
				 dtree );

	return( dtree );
    }
//
// If the wanted field is in the list of fields we can't use, return 0
//
    for (i = 0; i < ncantuse; i++)
	if (cantuse[i] == *fld)
	    return 0;
//
// We didn't find it among the available fields, so see if we can derive it
//
// First, add our field to the list of fields we can't use.  *We're* trying to 
// derive it, and we don't want some deeper recursion into MetaEval() to 
// try also.
//
    Field	*newcantuse = new Field[ncantuse + 1];

    for (i = 0; i < ncantuse; i++)
	newcantuse[i] = cantuse[i];

    newcantuse[ncantuse] = *fld;
//
// For each derivation list...
//
    DerivNode	*deriv;
	
    for (int tbl = 0; tbl < ndtables; tbl++)
    {
    //
    // For each derivation in the table that matches our field, try to MetaEval
    // the derivation.  If the MetaEval succeeds, we have a good derivation!
    //
	for (i = 0; (deriv = dtables[tbl]->NthDerivation( *fld, i )) != 0; i++)
	{
	    dtree = deriv->MetaEval( dtables, ndtables, avail, navail, 
				     newcantuse, ncantuse + 1 );
	    delete deriv;
	
	    if (dtree)
		break;
	}

	if (dtree)
	    break;
    }

    delete[] newcantuse;
    return (dtree);
}


void
RawFldDNode::Calculate( const Field flds[], const int nflds, const int ncalc, 
			const double* dataptrs[], double* out_data, 
			const double badval, ResultCache *rcache ) const
{
//
// If our field is in the list, just copy over the data
//	
    for (int f = 0; f < nflds; f++)
    {
	if (flds[f] != *fld)
	    continue;
	
	memmove (out_data, dataptrs[f], ncalc * sizeof (double));
	return;
    }
//
// Not there, so fill with badval
//
    for (int i = 0; i < ncalc; i++)
	out_data[i] = badval;
}


int
RawFldDNode::operator ==( const DerivNode& d ) const
{ 
    if (ClassId() != d.ClassId())
	return 0;
    
    RawFldDNode	*rd = (RawFldDNode*)&d;
    return (*fld == *(rd->fld));
}


//
// FuncDNode stuff starts here...
//

//
// The predefined functions that can be used by FuncDNodes
//
static void FDN_BadFill( double *result, const double badval, 
			 const int ndata );
static void FDN_ln( double *result, const double badval, const int ndata,
		    const double *operand );
static void FDN_exp( double *result, const double badval, const int ndata,
		     const double *operand );
static void FDN_sqrt( double *result, const double badval, const int ndata, 
		      const double *operand );
static void FDN_sin( double *result, const double badval, const int ndata,
		     const double *operand );
static void FDN_cos( double *result, const double badval, const int ndata,
		     const double *operand );
static void FDN_atan2( double *result, const double badval, const int ndata,
		       const double *y, const double *x );
static void FDN_pow( double *result, const double badval, const int ndata,
		     const double *mant, const double *exp );
static void FDN_t_wet( double *result, const double badval, const int ndata,
		       const double *t, const double *p, const double *rh );
static void FDN_e_sw( double *result, const double badval, const int ndata,
		      const double *t );

//
// Class-wide table of functions that can be used by FuncDNodes
//

struct FDNFuncInfo
{
    char*	name;
    int		nargs;
    FDFunction	func;
} *FDN_FTable = 0, FDN_BadFuncInfo = { "badfill", 0, (FDFunction)FDN_BadFill };

int	FDN_TableLen = 0;
int	FDN_MaxTableLen = 0;

static void	FDN_InitFTable( void );


//
// FuncDNode member functions.  We become the owner of the DerivNodes passed
// to us as args at instantiation.
//
const char *FuncDNode::clid = "FuncDNode";

FuncDNode::FuncDNode( const char* funcname, DerivNode* a0, DerivNode* a1, 
		      DerivNode* a2, DerivNode* a3 )
{
    int	f;
//
// Make sure the class-wide table of allowed functions has been built
//
    if (! FDN_FTable)
	FDN_InitFTable ();
//
// Linear search to find the named function in our table
//
    for (f = 0; f < FDN_TableLen; f++)
	if (! strcmp (funcname, FDN_FTable[f].name))
	    break;
//
// Use the function info we found, or FDN_BadFuncInfo.
//
    if (f == FDN_TableLen)
    {
	fprintf( stderr, "FuncDNode::FuncDNode(): Unknown function '%s()'\n",
		 funcname );
	fprintf( stderr, 
		 "A function returning badvalue will be used instead.\n" );

	funcinfo = &FDN_BadFuncInfo;
	return;
    }

    funcinfo = FDN_FTable + f;
//
// Verify the arg count
//    
    int	nargs = a3 ? 4 : a2 ? 3 : a1 ? 2 : a0 ? 1 : 0;
    if (nargs != funcinfo->nargs)
    {
	fprintf( stderr, "FuncDNode::FuncDNode: %s takes %d args, not %d.\n",
		 funcinfo->name, funcinfo->nargs, nargs );
	fprintf( stderr, 
		 "A function returning badvalue will be used instead.\n" );

	funcinfo = &FDN_BadFuncInfo;
    }
//
// Save the args we were given
//    
    arg[0] = a0;
    arg[1] = a1;
    arg[2] = a2;
    arg[3] = a3;
}


FuncDNode::FuncDNode( const FuncDNode& n ) 
{
    funcinfo = n.funcinfo;
    for (int i = 0; i < FuncDNodeMaxArgs; i++)
    {
	if (n.arg[i])
	    arg[i] = n.arg[i]->Copy();
	else
	    arg[i] = 0;
    }
};


FuncDNode::~FuncDNode( void ) 
{ 
    for (int i = 0; i < funcinfo->nargs; i++)
	if (arg[i])
	    delete arg[i];
};


std::ostream&
FuncDNode::PutTo( std::ostream& s ) const 
{
    s << funcinfo->name << "(";
    for (int i = 0; i < funcinfo->nargs; i++)
    {
	if (i > 0)
	    s << ", ";

	s << *arg[i];
    }

	
    s << ")";
	
    return( s ); 
}


const char* 
FuncDNode::FuncName( void ) const 
{
    return funcinfo->name;
}


Field* 
FuncDNode::FieldList( int* nflds ) const
{
    int fcount[FuncDNodeMaxArgs], fcountsum, a;
    Field*	flist[FuncDNodeMaxArgs];
//
// Get the field lists from our argument nodes
//
    fcountsum = 0;
    
    for (a = 0; a < funcinfo->nargs; a++)
    {
	flist[a] = arg[a]->FieldList( &fcount[a] );
	fcountsum += fcount[a];
    }
//
// Allocate retlist big enough to hold both the lists from all our arg nodes
//
    Field* retlist = new Field[fcountsum];
    int retlistlen = 0;
//
// Now merge together the arg nodes' lists, looking out for duplicate fields
//
    for (a = 0; a < funcinfo->nargs; a++)
    {
	for (int i = 0; i < fcount[a]; i++)
	{
	    int f;

	    for (f = 0; f < retlistlen; f++)
		if (flist[a][i] == retlist[f])
		    break;
	    if (f == retlistlen)
		retlist[retlistlen++] = flist[a][i];
	}
    }
//
// Delete the lists we got from our arg nodes
//
    for (a = 0; a < funcinfo->nargs; a++)
	delete[] flist[a];

    *nflds = retlistlen;
    return retlist;
}


DerivNode*
FuncDNode::MetaEval( const DerivTable* dtables[], const int ndtables, 
		     const Field *avail, const int navail, 
		     const Field *cantuse, const int ncantuse ) const
{
    DerivNode	*a[4];

    a[0] = a[1] = a[2] = a[3] = 0;
//
// Loop through the non-NULL args
//
    for (int i = 0; i < funcinfo->nargs; i++)
    {
    //
    // Get the meta-eval of each arg.  If any of the meta-eval's fails,
    // delete the saved stuff and return NULL.
    //
	if (! (a[i] = arg[i]->MetaEval( dtables, ndtables, avail, navail, 
					cantuse, ncantuse )))
	{
	    for (int k = 0; k < i; k++)
		delete a[k];
	    return 0;
	}
    }
//
// We have successfully meta-eval'ed our args, so everything's cool
//
    return new FuncDNode( funcinfo->name, a[0], a[1], a[2], a[3] );
}


void
FuncDNode::Calculate( const Field flds[], const int nflds, const int ncalc, 
		      const double* dataptrs[], double* out_data, 
		      const double badval, ResultCache *rcache ) const
{
    double* arg_dp[FuncDNodeMaxArgs];
    int a;
//
// First evaluate each of our args
//
    for (a = 0; a < funcinfo->nargs; a++)
    {
	arg_dp[a] = new double[ncalc];
	arg[a]->Eval( flds, nflds, ncalc, dataptrs, arg_dp[a], badval, 
		      rcache );
    }
//
// Now call our function
//
    switch (funcinfo->nargs)
    {
      case 0:
	funcinfo->func( out_data, badval, ncalc );
	break;
      case 1:
	funcinfo->func( out_data, badval, ncalc, arg_dp[0] );
	break;
      case 2:
	funcinfo->func( out_data, badval, ncalc, arg_dp[0], arg_dp[1] );
	break;
      case 3:
	funcinfo->func( out_data, badval, ncalc, arg_dp[0], arg_dp[1], 
			arg_dp[2] );
	break;
      case 4:
	funcinfo->func( out_data, badval, ncalc, arg_dp[0], arg_dp[1], 
			arg_dp[2], arg_dp[3] );
	break;
    }
//
// Free the arrays from our evaluated args
//
    for (a = 0; a < funcinfo->nargs; a++)
	delete[] arg_dp[a];
}


int
FuncDNode::operator ==( const DerivNode& d ) const
{
    if (ClassId() != d.ClassId())
	return 0;

    FuncDNode *fd = (FuncDNode*)&d;

    if (funcinfo != fd->funcinfo)
	return 0;

    for (int i = 0; i < funcinfo->nargs; i++)
	if (! (*arg[i] == *(fd->arg[i])))
	    return 0;

    return 1;
}


static void
FDN_InitFTable ( void )
{
    FDN_MaxTableLen = 20;
    FDN_FTable = new struct FDNFuncInfo[FDN_MaxTableLen];
    
    FDN_AddFunction( "ln", 1, (FDFunction) FDN_ln );
    FDN_AddFunction( "exp", 1, (FDFunction) FDN_exp );
    FDN_AddFunction( "sqrt", 1, (FDFunction) FDN_sqrt );
    FDN_AddFunction( "sin", 1, (FDFunction) FDN_sin );
    FDN_AddFunction( "cos", 1, (FDFunction) FDN_cos );
    FDN_AddFunction( "atan2", 2, (FDFunction) FDN_atan2 );
    FDN_AddFunction( "pow", 2, (FDFunction) FDN_pow );
    FDN_AddFunction( "t_wet", 3, (FDFunction) FDN_t_wet );
    FDN_AddFunction( "e_sw", 1, (FDFunction) FDN_e_sw );
}

void
FDN_AddFunction( const char *fname, const int nargs, const FDFunction func )
{
//
// Get a bigger function list if necessary
//
    if (FDN_TableLen == FDN_MaxTableLen)
    {
	FDN_MaxTableLen += 20;
	struct FDNFuncInfo *newlist = new struct FDNFuncInfo[FDN_MaxTableLen];
	for (int i = 0; i < FDN_TableLen; i++)
	    newlist[i] = FDN_FTable[i];
	delete[] FDN_FTable;
	FDN_FTable = newlist;
    }
//
// Add the function to our list
//
    struct FDNFuncInfo* fentry = &(FDN_FTable[FDN_TableLen]);
    fentry->name = new char[strlen (fname) + 1];
    strcpy( fentry->name, fname);
    fentry->nargs = nargs;
    fentry->func = func;
    FDN_TableLen++;
}

//
// The predefined FuncDNode-callable functions
//

static void
FDN_BadFill( double *result, const double badval, const int ndata )
{
    for (int i = 0; i < ndata; i++)
	result[i] = badval;
}


static void
FDN_ln( double *result, const double badval, const int ndata,
	const double *operand )
{
    for (int i = 0; i < ndata; i++)
    {
	if ( operand[i] == badval || operand[i] <= 0 )
	    result[i] = badval;
	else
	    result[i] = log( operand[i] );
    }
}


static void
FDN_exp( double *result, const double badval, const int ndata,
	 const double *operand )
{
    for (int i = 0; i < ndata; i++)
    {
	if ( operand[i] == badval )
	    result[i] = badval;
	else
	    result[i] = exp( operand[i] );
    }
}


static void
FDN_sqrt( double *result, const double badval, const int ndata, 
	  const double *operand )
{
    for (int i = 0; i < ndata; i++)
    {
	if ( operand[i] == badval || operand[i] < 0 )
	    result[i] = badval;
	else
	    result[i] = sqrt( operand[i] );
    }
}


static void
FDN_sin( double *result, const double badval, const int ndata,
	 const double *operand )
{
    for (int i = 0; i < ndata; i++)
    {
	if ( operand[i] == badval )
	    result[i] = badval;
	else
	    result[i] = sin( operand[i] );
    }
}


static void
FDN_cos( double *result, const double badval, const int ndata,
	 const double *operand )
{
    for (int i = 0; i < ndata; i++)
    {
	if ( operand[i] == badval || operand[i] < 0 )
	    result[i] = badval;
	else
	    result[i] = sqrt( operand[i] );
    }
}


static void
FDN_atan2( double *result, const double badval, const int ndata,
	   const double *y, const double *x )
{
    for (int i = 0; i < ndata; i++)
    {
	if (x[i] == badval || y[i] == badval)
	    result[i] = badval;
	else if (x[i] == 0.0 && y[i] == 0.0)
	    result[i] = badval;
	else
	    result[i] = atan2( y[i], x[i] );
    }
}


static void
FDN_pow( double *result, const double badval, const int ndata,
	 const double *mant, const double *exp )
{
    for (int i = 0; i < ndata; i++)
    {
	if (mant[i] == badval || exp[i] == badval)
	    result[i] = badval;
	else
	    result[i] = pow( mant[i], exp[i] );
    }
}


static void
FDN_t_wet( double *result, const double badval, const int ndata,
	   const double *t, const double *p, const double *rh )
{
    for (int i = 0; i < ndata; i++)
    {
	if ( t[i] == badval || p[i] == badval || rh[i] == badval )
	    result[i] = badval;
	else
	    result[i] = t_wet( t[i], p[i], rh[i] );
    }
}


static void
FDN_e_sw( double *result, const double badval, const int ndata,
	  const double *t )
{
    for (int i = 0; i < ndata; i++)
    {
	if ( t[i] == badval )
	    result[i] = badval;
	else
	    result[i] = e_sw( t[i] );
    }
}


//
// ResultCache stuff
//

ResultCache::ResultCache( void )
{
    maxcachelen = 0;
    cachelen = 0;
    nodes = 0;
    results = 0;
    lengths = 0;
}


void
ResultCache::Add( const DerivNode& dnode, const double *data, const int ndata )
{
//
// Make sure we have enough space
//
    if (cachelen == maxcachelen)
    {
	maxcachelen += 20;

	DerivNode **newnodes = new DerivNode*[maxcachelen];
	double **newresults = new double*[maxcachelen];
	int *newlengths = new int[maxcachelen];

	for (int i = 0; i < cachelen; i++)
	{
	    newresults[i] = results[i];
	    newnodes[i] = nodes[i];
	    newlengths[i] = lengths[i];
	}

	delete[] nodes;
	delete[] results;
	delete[] lengths;

	nodes = newnodes;
	results = newresults;
	lengths = newlengths;
    }
//
// Add the new node and results
//
    nodes[cachelen] = dnode.Copy();
    lengths[cachelen] = ndata;
    results[cachelen] = new double[ndata];

    double *dest = results[cachelen];
    for (int i = 0; i < ndata; i++)
	dest[i] = data[i];

    cachelen++;
}


ResultCache::~ResultCache( void )
{
    for (int i = 0; i < cachelen; i++)
    {
	delete nodes[i];
	delete[] results[i];
    }
    
    delete[] nodes;
    delete[] results;
    delete[] lengths;
}


double*
ResultCache::Find( const DerivNode& dnode, int* ndata )
{
//
// Look for a match
//  
    int	n;
    for (n = 0; n < cachelen; n++)
	if (dnode == *(nodes[n]))
	    break;

    if (n == cachelen)
	return 0;
//
// We have a match!
//
    if (ndata)
	*ndata = lengths[n];
    return (results[n]);
}

    

    
    
