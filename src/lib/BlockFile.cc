/*
 * The "BlockFile" interface to random-access, block-structured files.
 */

#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <iostream.h>
#include <iomanip.h>

//#include <defs.h>
//RCSID ("$Id: BlockFile.cc,v 1.9 1998-06-02 23:23:50 granger Exp $");

#include "BlockFile.hh"		// Our interface definition
#include "BlockFileP.hh"
#include "AuxBlock.hh"
#include "Logger.hh"
#include "Format.hh"

static const unsigned long BF_BLOCK_MAGIC = BLOCK_FILE_MAGIC+1;


/* ================================================================
 * The C++ BlockFile class interface
 */


/* --------------------
 * Convenience routines
 */

inline void BlockFile::seek (BlkOffset offset)
{
	fseek (fp, offset, SEEK_SET);
}


inline long BlockFile::seek_end ()
{
	fseek (fp, 0, SEEK_END);
	return (ftell(fp));
}


void BlockFile::init ()
{
	fp = 0;
	status = NOT_OPEN;
	errno = 0;
	path = 0;
	lock = 0;
	writelock = 0;
	header = 0;
	freelist = 0;
	journal = 0;
	log = Logger::For ("BlockFile");
	rbuf = new SerialBuffer;
	wbuf = new SerialBuffer;

	memset (&(this->stats), 0, sizeof(this->stats));

	log->Debug ("Initialized");
}


/* ----------------
 * Public interface
 */

/*
 * The default constructor and intializer
 */
BlockFile::BlockFile ()
{
	init ();
}



BlockFile::BlockFile (const char *path, int app_magic = 0, 
		      int flags = BF_NONE)
{
	init ();
	Open (path, app_magic, flags);
}


/*
 * Open the given file path, creating the file if not there else overwriting
 * any existing file.
 */
int
BlockFile::Create (const char *path, unsigned long app_magic = 0, 
		   int flags = BF_NONE)
{
	return (Open (path, app_magic, flags | BF_CREATE));
}



/*
 * Open the given file path, creating the file if necessary.
 */
int
BlockFile::Open (const char *path, unsigned long app_magic = 0, 
		 int flags = BF_NONE)
{
	if (fp)			// Close any currently open file
		Close();

	int create = 0;		// non-zero when we're creating a new file
	status = COULD_NOT_OPEN;

	log->Debug (Printf("%s file: %s", (flags & BF_CREATE) ? "Creating" :
			   "Opening", path));

	// Initialize path
	this->path = (char *) malloc (strlen(path)+1);
	strcpy (this->path, path);

	// Create (truncate an existing) file iff explicitly requested
	// or the file entry does not already exist.
	struct stat st;
	create = (flags & BF_CREATE) || 
		(stat (path, &st) < 0 && ::errno == ENOENT);

	if (create)
		WriteLock ();
	else
		ReadLock ();

	// Try to open a file pointer on the given path
	fp = fopen (path, create ? "w+" : "r+");
	if (! fp)
	{
		this->errno = ::errno;
		Unlock ();
		::free (this->path);
		this->path = 0;
		status = COULD_NOT_OPEN;
		return (status);
	}
	status = OK;

	// Allocate space for the header
	header = new BlockFileHeader (*this, app_magic);

	// Unless the file length is 0, assume we're opening an existing file
	if (seek_end() > 0)
	{
		// Sync with an existing header
		create = 0;
		header->readSync ();
		if (app_magic != 0 && app_magic != header->app_magic)
			status = WRONG_APP_MAGIC;
		// But leave the file open...
	}
	else
	{
		// Creating a new header
		create = 1;
		// Pad the header block first to the correct size
		long len = header->bf_length;
		char pad[ len ];
		memset (pad, 0, len);
		write (0, pad, len);
		header->mark();
	}

	// Once we have a header, we can associate our auxillary blocks
	freelist = new FreeList (*this, header->freelist, header);
	journal = new Journal (*this, header->journal, header);

	if (create)
	{
		// Force allocation of auxiliary blocks near beginning of file
		WriteSync (1);
	}
	Unlock ();
	return (status);
}



const char *
BlockFile::Path ()
{
	return (this->path);
}



/*
 * Set the application's header block.
 */
int
BlockFile::setHeader (Block &b, unsigned long app_magic)
{
	WriteLock ();
	header->app_header = b;
	if (app_magic)
		header->app_magic = app_magic;
	header->mark();
	Unlock ();
	return (0);
}





/*
 * Get the application's header block.
 */
int
BlockFile::getHeader (Block *b, unsigned long *app_magic)
{
	ReadLock ();
	if (b)
		*b = header->app_header;
	if (app_magic)
		*app_magic = header->app_magic;
	Unlock ();
	return (0);
}




/*
 * Close the file if open, and release any allocated memory.  Upon
 * return we should be in the exact same state as after the default
 * constructor with no arguments, ready for a call to Open() or Create().
 */
int
BlockFile::Close ()
{
	// Release file-specific resources
	if (fp)
	{
		log->Debug (Printf("Closing '%s'", path ? path : "<no file>"));
		WriteSync ();	// should be no-op unless we're exclusive
		fclose (fp);
		fp = 0;
	}
	if (freelist)
	{
		delete freelist;
		freelist = 0;
	}
	if (journal)
	{
		delete journal;
		journal = 0;
	}
	if (header)
	{
		delete header;
		header = 0;
	}
	if (path)
	{
		::free (path);
		path = 0;
	}
	status = NOT_OPEN;
	return (status);
}



void
BlockFile::ReadLock ()
{
	if (lock++ == 0)
	{
		log->Debug (Printf("Read locking '%s'", path));
		if (header)
			header->readSync ();
	}
}



void
BlockFile::WriteLock ()
{
	if (lock++ == 0)
	{
		log->Debug (Printf("Write locking '%s'", path));
		if (header)
			header->readSync ();
	}
	writelock = 1;
}



void
BlockFile::Unlock ()
{
	if (lock == 1)
	{
		if (writelock && header)
			WriteSync ();
		writelock = 0;
		log->Debug (Printf("Unlocked '%s'", path));
	}
	--lock;
}



/*
 * Sync all in-memory structures to disk.
 */
void
BlockFile::WriteSync (int force)
{
	WriteLock ();
	log->Debug (Printf("WriteSync '%s'", path));
	journal->writeSync (force);
	freelist->writeSync (force);
	header->writeSync (force);
	Unlock ();		// No need to do another write sync!
}



/*
 * Make sure in-memory structures are up-to-date with the disk
 */
void
BlockFile::ReadSync ()
{
	log->Debug (Printf("ReadSync '%s'", path));
	ReadLock ();
	freelist->readSync ();
	journal->readSync ();
	Unlock ();
}
	


/* Destructor:
 *
 * Call the Close method.
 */
BlockFile::~BlockFile ()
{
	log->Debug ("Destructor()");
	Close();
	if (log)
	{
		delete log;
		log = 0;
	}
	if (rbuf)
	{
		delete rbuf;
		rbuf = 0;
	}
	if (wbuf)
	{
		delete wbuf;
		wbuf = 0;
	}
}



ostream&
BlockFile::DumpHeader (ostream& out) 
{
	if (! header)
	{
		out << "File not open." << endl;
		return out;
	}

	ReadSync();
	BlockFileHeader *h = header;
	Block *b;
	out << (header->dirty() ? "*Dirty*" : "Clean") << endl;
	out << "File: " << path << endl;
	out << "Status(" << status << "), Errno(" << this->errno << ")\n";
	out << setiosflags(ios::showbase);
	out << "Block magic = " << hex << h->bf_magic << endl;
	out << "App magic   = " << hex << h->app_magic << endl;
	out << dec;
	out << "Header size = " << h->header_size << endl;
	out << "Blk version = " << hex << h->bf_version << dec << endl;
	out << "Revision    = " << h->revision << endl;
	out << "Length      = " << h->bf_length << endl;
	b = &(h->app_header);
	out << "App header  = " << b->length << " bytes @ " <<
		b->offset << " rev " << b->revision << endl;
	b = &(h->journal);
	out << "Journal     = " << b->length << " bytes @ " <<
		b->offset << " rev " << b->revision << endl;
	journal->Show (out);
	b = &(h->freelist);
	out << "Free list   = " << b->length << " bytes @ " <<
		b->offset << " rev " << b->revision << endl;
	freelist->Show (out);
	return (out);
}



// Reads the block into the serial buffer and returns the buffer
SerialBuffer *
BlockFile::readBuffer (BlkOffset addr, BlkSize length)
{
	ReadLock ();
	rbuf->Seek (0);
	rbuf->Need (length);
	read (rbuf->getBuffer(), addr, length);
	Unlock ();
	return (rbuf);
}


// Returns a serial buffer with enough space for the given length
SerialBuffer *
BlockFile::writeBuffer (BlkSize length)
{
	wbuf->Seek (0);
	wbuf->Need (length);
	return (wbuf);
}


// Write the current contents of the serial buffer to an offset
int
BlockFile::Write (BlkOffset addr, SerialBuffer *sbuf)
{
	return Write (addr, sbuf->getBuffer(), sbuf->Position());
}


/* ------------------------------------------------------------------
 */

BlkOffset 
BlockFile::Alloc (BlkSize size, BlkSize *actual)
/*
 * Find a block of space to hold the requested size, including
 * the block prefix.
 */
{
	WriteLock();
	freelist->readSync ();
	BlkOffset addr = alloc (size, actual);
	Unlock ();
	return (addr);
}



void
BlockFile::Free (BlkOffset addr, BlkSize len)
{
	WriteLock ();
	free (addr, len);
	journal->Record (Journal::BlockRemoved, addr, len);
	Unlock ();
}



/*
 * Read some bytes into memory.
 */
void *
BlockFile::Read (void *buf, BlkOffset block, BlkSize len)
{
	ReadLock ();
	read (buf, block, len);
	Unlock ();
	return (status == OK ? buf : NULL);
}



int
BlockFile::Write (BlkOffset block, void *buf, BlkSize len)
{
	WriteLock ();

	// Make sure the revision number has been advanced
	header->mark();

	// Only record changes from outside our implementation
	journal->Record (Journal::BlockChanged, block, len);
	write (block, buf, len);
	Unlock ();
	return (status == OK ? 0 : -1);
}
	


BlkVersion
BlockFile::Revision ()
{
	ReadLock ();		// Syncs the header for us
	Unlock ();
	return (header->revision);
}



int
BlockFile::Changed (BlkVersion rev, BlkOffset offset, BlkSize length)
{
	ReadLock ();
	int changed = journal->Changed (rev, offset, length);
	Unlock ();
	return (changed);
}




/* ----------------
 * Internals
 *
 * The actual implementations of the public interface, but they assume
 * the necessary entrance requirements are already met, such as the file
 * is locked and sync'ed.
 * ---------------- */


BlkOffset 
BlockFile::alloc (BlkSize size, BlkSize *actual)
/*
 * Find a block of space to hold the requested size, including
 * the block prefix.
 */
{
	BlkOffset addr;
	BlkSize actual_len;

	// Query the free list
	addr = freelist->Request (size, &actual_len);

	// Finally return the address
	if (actual)
		*actual = actual_len; // - header->prefix_length;
	log->Debug (Printf("Allocated %d bytes @ offset %d for request of %d", 
			   actual_len, addr, size));
	return (addr);
}




void
BlockFile::free (BlkOffset addr, BlkSize len)
{
	log->Debug (Printf("Free block @ %d of size %d", addr, len));

	// Add the block to the free list.
	freelist->Free (addr, len);
}



/*
 * Reserve a block at the end of the file, extending the file size.  Return
 * the offset of the block.
 */
BlkOffset
BlockFile::append (BlkSize size)
{
	BlkOffset off = header->bf_length;
	header->bf_length += size;
	header->mark ();
	// seek (header->bf_length);
	log->Debug (Printf("Appending block %d bytes @ %d (eof)", size, off));
	return (off);
}


void
BlockFile::recover (BlkOffset off)
{
	header->bf_length = off;
	header->mark();
	ftruncate (fileno (fp), header->bf_length);
	log->Debug (Printf("Truncated to %d", off));
}



int
BlockFile::write (BlkOffset block, void *buf, BlkSize len)
{	
	status = OK;
	seek (block);
	if (fwrite ((char *)buf, (size_t)len, 1, fp) != 1)
	{
		this->errno = ::errno;
		this->status = WRITE_FAILED;
	}
	return (status);
}



int
BlockFile::write (BlkOffset addr, SerialBuffer *sbuf)
{
	return write (addr, sbuf->getBuffer(), sbuf->Position());
}



int
BlockFile::read (void *buf, BlkOffset block, BlkSize size)
{	
	status = OK;
	seek (block);
	if (fread ((char *)buf, (size_t)size, 1, fp) != 1)
	{
		this->errno = ::errno;
		this->status = READ_FAILED;
	}
	return (status);
}



/* ================================================================
 * The C BlockFile interface
 */

#ifdef notdef

extern "C"
{

#include "BlockFile.h"


BlockFileHandle
BlockOpenFile (const char *path, int app_magic, int *app_version, 
	       int flags);

/*
 * Allocate a block of space in the file of the given size and return
 * the offset of that block.  Return BadBlkOffset on error.
 */
BlkOffset
BlockAdd (BlockFileHandle *, BlkSize size);


/*
 * Write an array of bytes into the given block.
 * Return zero on success, less than zero on error.
 */
int
BlockWrite (BlockFileHandle *, BlkOffset, BlkSize, void *bytes);


/*
 * Read an array of bytes from the given block into an array of bytes.
 * Return zero on success, less than zero on failure.
 */
int
BlockRead (BlockFileHandle *, BlkOffset, BlkSize, void *bytes);


/*
 * Free up a region of the file for re-use.
 */
void
BlockRemove (BlkOffset offset, BlkSize size);






}


#endif /* notdef */

