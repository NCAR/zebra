/*
 * Instantiate BTree templates and code for ZTime keys.
 */


#include "T_BTree.hh"
#include "BTreeFile.cc"

using namespace std;

template class BTree<ZTime,DataFileCore>;
template class BTreeFile<ZTime,DataFileCore>;

template class BTree<ZTime,ZTime>;
template class BTreeFile<ZTime,ZTime>;

template class BTree<string,string>;
template class BTreeFile<string,string>;

template class BTree<long,long>;
template class BTreeFile<long,long>;
