//
// Exception classes.
//

class dsmException
{
// Yawn
};



//
// For load exceptions.
//
class LoadException : public dsmException { };
class LoadNoPlats : public LoadException {} ;
