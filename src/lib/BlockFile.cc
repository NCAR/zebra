/*
 * The "BlockFile" interface to random-access, block-structured files.
 */

#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <iostream>
#include <iomanip>

//#include <defs.h>
//RCSID ("$Id: BlockFile.cc,v 1.20 2002-09-17 20:00:19 granger Exp $");

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

/*
 * Return true on success, false otherwise.
 */
inline bool BlockFile::seek (BlkOffset offset)
{
	int s = fseek (fp, offset, SEEK_SET);
	if (s != 0)
	{
		this->errnum = errno;
		this->status = ERROR;
		log.System (Format("seek to %lu") % offset);
	}
	return (s == 0);
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
	errnum = 0;
	path = 0;
	lock = 0;
	writelock = 0;
	flags = 0;
	header = 0;
	freelist = 0;
	journal = 0;
	//log = Logger::For ("BlockFile");
	rbuf = new SerialBuffer;
	wbuf = new SerialBuffer;

	memset (&(this->stats), 0, sizeof(this->stats));

	log.Debug ("Initialized");
}


void
BlockFile::Stats::reset()
{
	num_reads = 0;
	bytes_read = 0;
	num_writes = 0;
	bytes_writ = 0;
}


/* ----------------
 * Public interface
 */

/*
 * The default constructor and intializer
 */
BlockFile::BlockFile () : log("BlockFile")
{
	init ();
}



BlockFile::BlockFile (const char *path_, 
		      int app_magic /* = 0 */, 
		      int flags_ /* = BF_NONE */) : log("BlockFile")
{
	init ();
	Open (path_, app_magic, flags_);
}


/*
 * Open the given file path, creating the file if not there else overwriting
 * any existing file.
 */
int
BlockFile::Create (const char *path_, unsigned long app_magic /* = 0 */, 
		   int flags_ /* = BF_NONE */)
{
	return (Open (path_, app_magic, flags_ | BF_CREATE));
}



/*
 * Open the given file path, creating the file if necessary.
 */
int
BlockFile::Open (const char *path_, unsigned long app_magic /* = 0 */, 
		 int flags_ /* = BF_NONE */)
{
	Format fmt("Open(%s,%0x,%i)");
	EnterBlock eb(log, fmt % path_ % app_magic % flags_);
	if (fp)			// Close any currently open file
		Close();

	int create = 0;		// non-zero when we're creating a new file
	status = COULD_NOT_OPEN;

	log && log.Debug (Format("%s file: %s") %
			  ((flags_ & BF_CREATE) ? "Creating" : "Opening") %
			  path_);

	// Initialize path
	this->path = (char *) malloc (strlen(path_)+1);
	strcpy (this->path, path_);

	// Create (truncate an existing) file iff explicitly requested
	// or the file entry does not already exist.
	struct stat st;
	create = (flags_ & BF_CREATE) || 
		(stat (path, &st) < 0 && errno == ENOENT);
	int readonly = flags_ & BF_READONLY;

	if (! readonly && create)
		WriteLock ();
	else
		ReadLock ();

	// Try to open a file pointer on the given path
	fp = fopen (path, readonly ? "r" : (create ? "w+" : "r+"));
	if (! fp)
	{
		this->errnum = errno;
		log.System (Format("opening %s") % path);
		Unlock ();
		::free (this->path);
		this->path = 0;
		status = COULD_NOT_OPEN;
		return (status);
	}
	status = OK;
	log.Extend (path);

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
		char *pad = new char[ len ];
		memset (pad, 0, len);
		write (0, pad, len);
		header->mark();
		delete[] pad;
	}

	// Once we have a header, we can associate our auxillary blocks
	freelist = new FreeList (*this, header->freelist, header);
	journal = new Journal (*this, header->journal, header);
	flags = flags_;

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
BlockFile::setHeader (const Block &b, unsigned long app_magic)
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
	EnterBlock eb(log,"Close()");
	// Release file-specific resources
	if (fp)
	{
		// log.Info (Format("Closing '%s'") % path);
		/*
		 * Force writesync if we have a lock and may have changed,
		 * or if we've had exclusive access.
		 */
		if ((lock > 0 && writelock) || Exclusive())
			WriteSync (1);
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
	log.Declare ("BlockFile");
	status = NOT_OPEN;
	lock = 0;
	writelock = 0;
	flags = 0;
	return (status);
}



void
BlockFile::ReadLock ()
{
	if (lock++ == 0)
	{
		log.Debug ("read lock");
		if (header && !Exclusive())
			header->readSync ();
	}
}



void
BlockFile::WriteLock ()
{
	if (lock++ == 0)
	{
		log.Debug ("write lock");
		if (header && !Exclusive())
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
		log.Debug ("unlock");
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
	log.Debug ("WriteSync");
	journal->writeSync (force);
	freelist->writeSync (force);
	header->writeSync (force);
	fflush (fp);
	Unlock ();		// No need to do another write sync!
}



/*
 * Make sure in-memory structures are up-to-date with the disk
 */
void
BlockFile::ReadSync ()
{
	log.Debug ("ReadSync");
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
	log.Debug ("Destructor()");
	Close();
#if 0
	if (log)
	{
		delete log;
		log = 0;
	}
#endif
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



std::ostream&
BlockFile::DumpHeader (std::ostream& out) 
{
        using std::endl;
	using std::hex;
	using std::dec;

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
	out << "Status(" << status << "), Errno(" << this->errnum << ")\n";
	out << setiosflags(std::ios::showbase);
	out << "Block magic = " << std::hex << h->bf_magic << endl;
	out << "App magic   = " << std::hex << h->app_magic << endl;
	out << std::dec;
	out << "Header size = " << h->header_size << endl;
	out << "Blk version = " << std::hex << h->bf_version << dec << endl;
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



// Reads the block into the serial buffer and returns the buffer.
// The length can be zero, in which case the current read buffer
// is just returned.
SerialBuffer *
BlockFile::readBuffer (BlkOffset addr, BlkSize length)
{
	if (length > 0)
	{
		ReadLock ();
		rbuf->Seek (0);
		rbuf->Need (length);
		read (rbuf->getBuffer(), addr, length);
		Unlock ();
	}
	return (rbuf);
}


// Returns a serial buffer with enough space for the given length.
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
	/*
	 * If this block was the application header, that header
	 * is no longer valid.
	 */
	if (addr == header->app_header.offset)
	{
		log.Info ("App header freed; resetting BlockFile header");
		setHeader (Block());
	}
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
 * Find a block of space to hold the requested size.
 */
{
	BlkOffset addr;
	BlkSize actual_len;

	// Query the free list
	addr = freelist->Request (size, &actual_len);

	// Finally return the address
	if (actual)
		*actual = actual_len; // - header->prefix_length;
	log.Debug (Format("allocated %lu @ %lu for request of %lu")
		   % actual_len % addr % size);
	return (addr);
}




void
BlockFile::free (BlkOffset addr, BlkSize len)
{
	log.Debug (Format("free block @ %lu of size %lu") % addr % len);

	// Add the block to the free list.
	freelist->Free (addr, len);
}



/*
 * Reserve a block at the end of the file, extending the file size.  Return
 * the offset of the block.  We need to actually write zeros at the end of
 * the file so that subsequent reads of this block will not read past eof.
 */
BlkOffset
BlockFile::append (BlkSize size)
{
	static const unsigned long zero = 0;
	static const size_t len = sizeof(zero);
	BlkOffset off = header->bf_length;
	header->bf_length += size;
	header->mark ();
	log.Debug (Format("append block %lu bytes @ %lu (eof)") % size % off);
	if (write (header->bf_length - len, &zero, len) != OK)
		log.Problem ("extend end of file failed");
	return (off);
}


void
BlockFile::recover (BlkOffset off)
{
	header->bf_length = off;
	header->mark();
	ftruncate (fileno (fp), header->bf_length);
	log.Debug (Format("truncated to %lu") % off);
}



int
BlockFile::write (BlkOffset block, const void *buf, BlkSize len)
{	
	status = OK;
	++stats.num_writes;
	stats.bytes_writ += len;
	if (! seek (block) || fwrite ((char *)buf, (size_t)len, 1, fp) != 1)
	{
		this->errnum = errno;
		this->status = WRITE_FAILED;
		log.System (Format("write len %lu at %lu") % len % block);
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
	++stats.num_reads;
	stats.bytes_read += size;
	if (! seek(block) || fread((char *)buf, (size_t)size, 1, fp) != 1)
	{
		this->errnum = errno;
		this->status = READ_FAILED;
		log.System (Format("read len %lu at %lu") % size % block);
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

