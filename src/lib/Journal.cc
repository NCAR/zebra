/*
 * BlockFile journal
 */

#include <stdio.h>
#include <errno.h>
#include <iostream>
#include <iomanip>

//#include <defs.h>
//RCSID ("$Id: Journal.cc,v 1.12 2002-09-17 20:00:19 granger Exp $");

#include "BlockFile.hh"		// Our interface definition
#include "BlockFileP.hh"	// For the private header structure and stuff
#include "AuxBlock.hh"
#include "Logger.hh"
#include "Format.hh"


const long Journal::MaxEntries = 256;

const Journal::ChangeType Journal::BeginTransaction = 0;
const Journal::ChangeType Journal::BlockRemoved = 1;
const Journal::ChangeType Journal::BlockAdded = 2;
const Journal::ChangeType Journal::BlockChanged = 3;
const Journal::ChangeType Journal::EndTransaction = 4;

static const char *change_names[] =
{
	"Begin", "Removed", "Added", "Changed", "End"
};


// Static methods

const char *
Journal::ChangeName (ChangeType c)
{
	int i = (int) c;
	return ((i >= 0 && i <= 4) ? change_names[i] : "Illegal");
}


// Constructor

Journal::Journal (BlockFile &bf_, Block &b, SyncBlock *parent_) :
	SyncBlock (bf_, b), RefBlock (b, parent_)
{
	// cout << "Constructing Journal" << endl;
	max = MaxEntries;
	first = 0;
	last = 0;
	serialSize = 0;
	entries = new JournalEntry[max];
}



Journal::~Journal ()
{
	if (entries)
		delete[] entries;
}



int
Journal::Changed (BlkVersion rev, BlkOffset offset, BlkSize length)
{
	readSync ();

	int diff = 1;
	if (rev >= (unsigned long)bf->header->revision || first == last)
	{
		diff = 0;
	}
	else if (rev < entries[(last+max-1)%max].block.revision)
	{
		// The oldest entry is after the rev, so consider it changed
		diff = 1;
	}
	else
	{
		// Search the journal for a change to this region
		// *more recent* than the given revision: the block
		// is already in sync *at* the given revision.  In other
		// words, don't make blocks read sync with changes
		// they just wrote.
		// 
		diff = 0;
		int i = first;
		while (i != last && entries[i].block.revision > rev)
		{
			Block *b = &entries[i].block;
			if (entries[i].change == BlockChanged &&
			    b->offset + b->length > offset &&
			    offset + length > b->offset)
			{
				diff = 1;
				break;
			}
			i = (i + 1) % max;
		}
	}
	return diff;
}



void
Journal::Record (Journal::ChangeType change, BlkOffset offset, BlkSize length)
{
	// Insert new records (latest rev) at the beginning of the queue
	readSync ();
	first = (max + first - 1) % max;
	Block *b = &entries[first].block;
	b->offset = offset;
	b->length = length;
	b->revision = bf->header->revision;
	entries[first].change = change;
	if (first == last)	// queue has come full circle
	{
		last = (max + last - 1) % max;
	}
	mark ();
}




void
Journal::Show (std::ostream &out)
{
	readSync ();

	int i = first;
	Format f("   Block (%7u, %7u, %7u) : %-20s");
	while (i != last)
	{
		Block *b = &entries[i].block;
		out << f % b->offset % b->length % b->revision
			% ChangeName (entries[i].change) 
		    << std::endl;
		i = (i + 1) % max;
	}
}




int
Journal::encode (SerialBuffer &sbuf)
{
	sbuf << max << first << last;
	for (int i = 0; i < max; ++i)
	{
		sbuf << (const JournalEntry)entries[i];
	}
	return (0);
}



int
Journal::decode (SerialBuffer &sbuf)
{
	sbuf >> max >> first >> last;
	for (int i = 0; i < max; ++i)
	{
		sbuf >> entries[i];
	}
	return 0;
}



long
Journal::encodedSize (SerialBuffer &sbuf)
{

	if (! serialSize)
	{
		long s = serialCount (sbuf, max);
		s += serialCount (sbuf, first);
		s += serialCount (sbuf, last);
		s += max * serialCount (sbuf, JournalEntry());
		serialSize = s;
	}
	return (serialSize);
}

