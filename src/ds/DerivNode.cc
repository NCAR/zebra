# include "DerivNode.h"

ostream& 
operator <<( ostream& s, const DerivNode& dnode )
{
	return (dnode.PutTo(s));
}

//
// ConstNode member functions
//
DerivNode*
ConstNode::MetaEval( const Field *avail, int navail, const Field *cantuse, 
		     int ncantuse ) const
{
	return ( Copy() );
}


//
// OpNode member functions
//
OpNode::OpNode( const char* op,  DerivNode* l, DerivNode* r ) 
{
	oper = new char[strlen( op ) + 1];
	strcpy( oper, op );
	left = l;
	right = r;
}


OpNode::OpNode( const OpNode& n )
{
	oper = new char[strlen( n.oper ) + 1];
	strcpy( oper, n.oper );
	left = n.left->Copy();
	right = n.right->Copy();
}


OpNode::~OpNode( void ) 
{ 
	delete oper; 
	delete left; 
	delete right; 
}
	

ostream& 
OpNode::PutTo( ostream& s ) const 
{ 
	return( s << "(" << *left << " " << oper << " " << *right << ")"); 
}


DerivNode*
OpNode::MetaEval( const Field *avail, int navail, const Field *cantuse, 
		  int ncantuse ) const
{
	DerivNode	*l, *r;

	if ((l = left->MetaEval( avail, navail, cantuse, ncantuse )) != 0 && 
	    (r = right->MetaEval( avail, navail, cantuse, ncantuse )) != 0)
		return( new OpNode( oper, l, r ) );
	else
		return( 0 );
	
		
		
}

//
// RawFldNode member functions
//
DerivNode*
RawFldNode::MetaEval( const Field *avail, int navail, const Field *cantuse, 
		      int ncantuse ) const
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
		dtree = new RawFldNode( new Field( avail[i] ) );
	//
	// Put in the slope and intercept for units conversion (if any)
	//	
		if (dtree && slope != 1.0)
			dtree = new FuncNode( "*", new ConstNode( slope ), 
					      dtree );

		if (dtree && intercept != 0.0)
			dtree = new FuncNode( "+", new ConstNode( intercept ), 
					      dtree );

		return( dtree );
	}
//
// If the wanted field is in the list of fields we can't use, return 0
//
	for (i = 0; i < ncantuse; i++)
		if (cantuse[i].CanYield( *fld ))
			return 0;
//
// We didn't find it among the available fields, so see if we can derive it
//
// First, add our field to the list of fields we can't use.  *We're* trying to 
// derive it, and we don't want some deeper recursion into MetaEval() to 
// try also.
//
	Field	*newcantuse = new Field[ncantuse + 1];
	if (ncantuse > 0)
		memcpy( newcantuse, cantuse, ncantuse * sizeof( Field ) );
	newcantuse[ncantuse] = *fld;
//
// Loop through the possible derivations
//
	for (i = 0; (dtree = DerivList.NthDerivation( fld, i )) != 0; i++)
	{
		if ((dtree = dtree->MetaEval( avail, navail, newcantuse, 
					      ncantuse + 1 )) != 0)
			break;
	}
	
	delete newcantuse;
	return (dtree);
}


//
// FuncNode member functions
//
FuncNode::FuncNode( const char* funcname, DerivNode* a0, DerivNode* a1, 
		    DerivNode* a2, DerivNode* a3 )
{
	name = new char[strlen( funcname ) + 1];
	strcpy( name, funcname );
	arg[0] = a0;
	arg[1] = a1;
	arg[2] = a2;
	arg[3] = a3;
}


FuncNode::FuncNode( const FuncNode& n ) 
{
	name = new char[strlen( n.name ) + 1];
	strcpy( name, n.name );
	for (int i = 0; i < 4; i++)
		arg[i] = n.arg[i] ? n.arg[i]->Copy() : 0;
};


FuncNode::~FuncNode( void ) 
{ 
	delete name;
	for (int i = 0; i < 4; i++)
		delete arg[i];
};


ostream&
FuncNode::PutTo( ostream& s ) const 
{
	s << name << "(" << *arg[0];
	if (arg[1])
		s << ", " << *arg[1];
	if (arg[2])
		s << ", " << *arg[2];
	if (arg[3])
		s << ", " << *arg[3];
	
	s << ")";
	
	return( s ); 
}


DerivNode*
FuncNode::MetaEval( const Field *avail, int navail, const Field *cantuse, 
		    int ncantuse ) const
{
	DerivNode	*a[4];

	a[0] = a[1] = a[2] = a[3] = 0;
//
// Loop through the non-NULL args
//
	for (int i = 0; (i < 4) && arg[i]; i++)
	{
	//
	// Get the meta-eval of each arg.  If any of the meta-eval's fails,
	// delete the saved stuff and return NULL.
	//
		if (! (a[i] = arg[i]->MetaEval( avail, navail, cantuse, 
					        ncantuse )))
		{
			for (int k = 0; k < i; k++)
				delete a[k];
			return 0;
		}
	}
//
// We have successfully meta-eval'ed our args, so everything's cool
//
	return new FuncNode( name, a[0], a[1], a[2], a[3] );
}


