/*
 * BlockFile journal
 */

#include <stdio.h>
#include <errno.h>
#include <iostream.h>
#include <iomanip.h>

#include <defs.h>

RCSID ("$Id: Journal.cc,v 1.4 1997-12-17 03:51:15 granger Exp $");

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


// Constructor

Journal::Journal (BlockFile &bf, Block &b, SyncBlock *parent) :
	SyncBlock (bf, b), RefBlock (b, parent)
{
	cout << "Constructing Journal" << endl;
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

	int changed;
	if (rev >= (unsigned long)bf->header->revision || first == last)
	{
		changed = 0;
	}
	else if (rev < entries[first].block.revision)
	{
		// The oldest entry is after the rev, so consider it changed
		changed = 1;
	}
	else
	{
		// Search the journal for a change to this region
		// more recent than the given revision
		changed = 0;
		int i = first;
		while (i != last && entries[i].block.revision >= rev)
		{
			Block *b = &entries[i].block;
			if (entries[i].change == BlockChanged &&
			    b->offset + b->length >= offset &&
			    offset + length >= b->offset)
			{
				changed = 1;
				break;
			}
			i = (i + 1) % max;
		}
	}
	return changed;
}



void
Journal::Record (Journal::ChangeType change, BlkOffset offset, BlkSize length)
{
	// Insert new records (lastest rev) at the beginning of the queue
	readSync ();
	first = (max + first - 1) % max;
	Block *b = &entries[first].block;
	b->offset = offset;
	b->length = length;
	b->revision = bf->header->revision;
	entries[first].change = change;
	if (first == last)
	{
		last = (max + last - 1) % max;
	}
	mark ();
}



int
Journal::encode (SerialBuffer &sbuf)
{
	sbuf << max << first << last;
	for (int i = 0; i < max; ++i)
	{
		sbuf << entries[i];
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
Journal::size (SerialBuffer &sbuf)
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

