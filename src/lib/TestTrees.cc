/*
 * Instantiate BTree templates and code for ZTime keys.
 */


#include "SerialZTime.hh"

#include "BTreeFile.cc"

template class BTree<ZTime,ZTime>;
template class BTreeFile<ZTime,ZTime>;

template class BTree<string,string>;
template class BTreeFile<string,string>;

template class BTree<long,long>;
template class BTreeFile<long,long>;
