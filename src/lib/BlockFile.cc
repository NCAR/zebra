/*
 * The "BlockFile" interface to random-access, block-structured files.
 */

#include <stdio.h>
#include <errno.h>
#include <iostream.h>
#include <iomanip.h>

#include <defs.h>

RCSID ("$Id: BlockFile.cc,v 1.2 1997-11-24 18:15:07 granger Exp $");

#include "BlockFile.hh"		// Our interface definition
#include "BlockFileP.hh"	// For the private header structure and stuff

static const unsigned long DEFAULT_APP_MAGIC = 0x01020304;
static const unsigned long BF_BLOCK_MAGIC = BLOCK_FILE_MAGIC+1;

/*
 * Private forward declarations.
 */
static void InitHeader (BlockFileHeader *h, int app_magic = 0);


#ifdef notdef
//
// Compact free list memory management into a convenient structure
//
class FreeList
{
public:
	FreeList (BlockFile &bf);
	~FreeList (void);

	void WriteSync ();	/* Send changes to disk */
	void ReadSync ();	/* Read changes from disk */

	/*
	 * Add a freed block to the free block list.
	 */
	void FreeBlock (BlkOffset offset, BlkSize length);

	/*
	 * Find a block for this request and take it off the list.
	 * Return non-zero on success, zero otherwise.
	 */
	int RequestBlock (BlkSize length, BF_FreeBlock &ret);
	BlkOffset RequestBlock (BlkSize length, BlkSize *ret_length);

	friend BlockFile;

private:
	int ncache;	/* Number of free blocks allocated space in array */
	int n;		/* Actual number of free blocks in use in array */
	BlkVersion rev;	/* Free list block revision we last sync'ed to */
	BF_FreeBlock *blocks; /* The actual array of free blocks */
	BlockFile *bf;

	void Remove (int);
	void Add (BlkSize length, BlkSize length);

};
#endif


/* ================================================================
 * The C++ BlockFile class interface
 */


#ifdef notdef
// XDR stream operators

XDR_ADDTYPE(BlockFileHeader)
XDR_ADDTYPE(BF_Prefix)
#endif


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
	fp = NULL;
	status = NOT_OPEN;
	errno = 0;
	path = NULL;
	header = NULL;
	freelist = new FreeList (*this);
	log = new Logger("BlockFile");

	memset (&(this->stats), 0, sizeof(this->stats));

	encode = NULL;
	decode = NULL;
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
BlockFile::Create (const char *path, int app_magic = 0, int flags = BF_NONE)
{
	return (Open (path, app_magic, flags | BF_CREATE));
}



/*
 * Open the given file path, creating the file if necessary.
 */
int
BlockFile::Open (const char *path, int app_magic = 0, int flags = BF_NONE)
{
	status = COULD_NOT_OPEN;

	log->Debug ("%s file: %s", (flags & BF_CREATE) ? "Creating" :
		    "Opening", path);

	// First try to open a file pointer on the given path
	fp = fopen (path, (flags & BF_CREATE) ? "w+" : "r+");
	if (! fp)
	{
		sys_errno = errno;
		return (status);
	}

	// Initialize
	status = OK;
	this->path = (char *) malloc (strlen(path)+1);
	strcpy (this->path, path);

	// Create our encode and decode streams on the file
	decode = new ReadXDR(fp);
	encode = new WriteXDR(fp);
	if (! encode || ! encode->success() || 
	    ! decode || ! decode->success())
	{
		Close ();
		return (XDR_ERROR);
	}

	// Allocate space for the header
	header = (BlockFileHeader *) malloc (sizeof (BlockFileHeader));

	// If the file length is 0, assume we have to initialize it to
	// a new block file first.
	if (seek_end() == 0)
	{
		InitHeader (header, app_magic);
		header->file_length = header->header_size;
		WriteHeader ();
	}
	else	// Sync with an existing header
	{
		ReadHeader ();
		if (app_magic != 0 && app_magic != header->app_magic)
			status = WRONG_APP_MAGIC;
		// But leave the file open...
	}
	return (status);
}



void
BlockFile::SetAppVersion (BlkVersion version, const char *description = NULL)
{
	header->app_version = version;
	if (description)
	{
		strncpy (header->description, description, 
			 sizeof(header->description) - 1);
		header->description[sizeof(header->description) - 1] = '\0';
	}
	/* WriteHeader (); */
}


/*
 * Close the file if open, and release any allocated memory.  Upon
 * return we should be in the exact same state as after the default
 * constructor with no arguments, ready for a call to Open() or Create().
 */
int
BlockFile::Close ()
{
	log->Debug ("Closing '%s'", path ? path : "<no file>");
	WriteSync ();		// Sync all overhead info as necessary
	if (fp)
	{
		fclose (fp);
		fp = NULL;
	}
	if (freelist)
	{
		delete freelist;
		freelist = NULL;
	}
	if (header)
	{
		free (header);
		header = NULL;
	}
	if (encode)
	{
		delete encode;
		encode = NULL;
	}
	if (decode)
	{
		delete decode;
		decode = NULL;
	}
	if (path)
	{
		free (path);
		path = NULL;
	}
	status = NOT_OPEN;
	return (status);
}



/*
 * Write all in-memory structures to disk.
 */
void
BlockFile::WriteSync ()
{
	log->Debug ("WriteSync '%s'", path);
	freelist->WriteSync ();
	WriteHeader ();
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
		log = NULL;
	}
}



ostream&
BlockFile::DumpHeader (ostream& out) const
{
	BlockFileHeader *h = header;
	BF_Block *b;

	/* ReadHeader (); */
	out << "File: " << path << endl;
	out << "Status(" << status << "), Errno(" << sys_errno << ")\n";
	out << setiosflags(ios::showbase);
	out << "Block magic = " << hex << h->block_magic << endl;
	out << "App magic   = " << hex << h->app_magic << endl;
	out << dec;
	out << "Header size = " << h->header_size << endl;
	out << "Description = " << h->description << endl;
	out << "App version = " << h->app_version << endl;
	out << "Blk version = " << hex << h->block_version << dec << endl;
	out << "Revision    = " << h->revision << endl;
	out << "Length      = " << h->file_length << endl;
	out << "Prefix len  = " << h->prefix_length << endl;
	b = &(h->app_header);
	out << "App header  = " << b->length << " bytes @ " <<
		b->offset << " rev " << b->revision << endl;
	b = &(h->freelist);
	out << "Free list   = " << b->length << " bytes @ " <<
		b->offset << " rev " << b->revision << endl;
#ifdef notdef
	out << "Bytes Req   = " << h->requested << endl;
	out << "  Granted   = " << h->granted << endl;
	out << "Free blocks = " << h->nfree << endl;
	out << "  Bytes     = " << h->spacefree << endl;
#endif
	return (out);
}



BlkVersion
BlockFile::AppVersion ()
{
	/* ReadHeader (); */
	return (header->app_version);
}


/* ------------------------------------------------------------------
 * The BlockStore interface implementation
 */

BlkOffset 
BlockFile::Alloc (BlkSize size, BlkSize *actual)
/*
 * Find a block of space to hold the requested size, including
 * the block prefix.
 */
{
	BlkOffset addr;
	BlkSize actual_len;
	BF_Prefix bp;

	bp.block_magic = BF_BLOCK_MAGIC;
	bp.magic = header->app_magic;
	bp.alloc = 0;

	// The size we actually need includes the prefix
	size += header->prefix_length;

	// Query the free list
	addr = freelist->RequestBlock (size, &actual_len);
	if (! addr)
	{
		addr = AppendBlock (size, &actual_len);
	}

	// Now write the prefix
	bp.alloc = actual_len;
	XDRStream *xdrs = encodeStream (addr);
	xdrs << bp;
	addr += header->prefix_length;

	// Finally return the address
	if (actual)
		*actual = actual_len - header->prefix_length;
	log->Debug ("Allocated %d bytes @ offset %d for request of %d", 
		    actual_len, addr, size - header->prefix_length);
	return (addr);
}



#ifdef notdef
BlkOffset 
BlockFile::Realloc (BlkOffset addr, BlkSize size)
/*
 * First check this block for the amount originally allocated, and if 
 * the new size fits, return the same block.
 *
 * Otherwise, free the block and put it on the free list, get
 * a new block to hold the requested size, and copy the old block into
 * the new block.
 *
 * At present we don't do anything about making blocks smaller, i.e.,
 * splitting a reduced block to free up another block.
 */
{
	BF_Prefix bp;

	// Read the prefix for this block
	XDRStream *xdrs = decodeStream (addr - header->prefix_length);
	xdrs >> bp;

	// If the allocated size will hold the request, we're dandy
	if (bp.alloc >= size + header->prefix_length)
		return (addr);

	// Free the block and get a new one, which might result in a
	// concatenation and give us the same block address back.
	addr -= header->prefix_length;
	freelist->FreeBlock (addr, bp.alloc);

	// Query the free list
	size += header->prefix_length;
	BlkSize alloc;
	BlkOffset next = freelist->RequestBlock (size, &alloc);
	if (! next)
	{
		next = AppendBlock (size, &alloc);
	}

	// See if we need to copy data.
	if (next != addr)
	{
		int n = bp.alloc;
		BlkOffset start = (next > addr) ? addr : addr + n - 1;
	}

	// Now write the prefix
	bp.alloc = alloc;
	XDRStream *xdrs = encodeStream (addr);
	xdrs << bp;
	addr += header->prefix_length;

	// Finally return the address
	return (addr);


}
#endif

void
BlockFile::Free (BlkOffset addr)
{
	BF_Prefix bp;

	// Read the prefix for this block
	addr -= header->prefix_length;
	XDRStream *xdrs = decodeStream (addr);
	xdrs >> bp;
	if (bp.block_magic != BF_BLOCK_MAGIC)
	{
		log->Problem ("Corrupt block prefix @ %lu, wrong magic %lu",
			      addr, bp.block_magic);
	}

	log->Debug ("Free block @ %d of size %d", addr, bp.alloc);

	// Add the block to the free list.
	freelist->FreeBlock (addr, bp.alloc);
}



/*
 * Read some bytes into memory.
 */
void *
BlockFile::Read (void *buf, BlkOffset block, BlkSize len)
{
	status = OK;
	seek (block);
	read (buf, len);
	return (status == OK ? buf : NULL);
}



/*
 * Decode bytes from file into memory using given decoder.
 */
void *
BlockFile::Read (void *buf, BlkOffset block, BlkSize len, XDRDecoder xp)
{
	XDRStream *xdrs = decodeStream (block);
	xdrs->translate (buf, xp);
	return (buf);
}



int
BlockFile::Write (BlkOffset block, void *buf, BlkSize len)
{
	status = OK;
	seek (block);
	write (buf, len);
	return (status == OK ? 0 : -1);
}
	


int
BlockFile::Write (BlkOffset block, void *buf, BlkSize len, XDREncoder xp)
{
	XDRStream *xdrs = encodeStream (block);
	xdrs->translate (buf, xp);
	return (0);
}



/* ----------------
 * Internals
 * ---------------- */

#ifdef notdef
/*
 * Add a free block to the free block list.
 */
void
BlockFile::AddFreeBlock (BlkOffset addr, BlkSize size)
{
	// Find our free block list and begin searching it for a block
	// which borders this one.  If we can't find one, then this block
	// gets added to the end.
	BlkOffset off;

	if (! freelist)		// need to create one or read one
	{
		if (! header->freelist.offset)
		{
			// create one
			header->freelist.length = INITIAL_FREE_BLOCK_SIZE;
			header->freelist.offset = 
				AppendBlock (header->freelist.length);
		}
		// Allocate in-memory storage for the block list
		freelist = (BF_BlockList *) malloc (header->freelist.length);

		if (header->freelist.offset)
		{
			// read one
			read (
	}
	++nfree;
	spacefree += size;
}
#endif


/*
 * Reserve a block at the end of the file, extending the file size.  Return
 * the offset of the block.
 *
 * For now the block allocation policy is to allocate in 256-byte chunks.
 * We'll see how that works for a while...
 */
BlkOffset
BlockFile::AppendBlock (BlkSize size, BlkSize *actual = NULL)
{
	BlkOffset off = header->file_length;
	size = (((size-1) >> 8) + 1) << 8;
	header->file_length += size;
	mark (HEADER);
	if (actual)
		*actual = size;
	log->Debug ("Appending block %d bytes @ %d (eof)", size, off);
	return (off);
}



static void InitBlock (BF_Block &b)
{
	b.offset = 0;
	b.length = 0;
	b.revision = 0;
	b.count = 0;
}



/*
 * Initialize a new header.
 */
static void InitHeader (BlockFileHeader *h, int app_magic = 0)
{
	h->block_magic = BLOCK_FILE_MAGIC;
	h->app_magic = (app_magic == 0) ? DEFAULT_APP_MAGIC : app_magic;
	h->header_size = sizeof (BlockFileHeader);
	strcpy (h->description, "No description.");
	h->app_version = 0;
	h->block_version = BLOCK_FILE_VERSION;
	h->revision = 0;
	// Prefix length requires encoded size of block prefix
	BF_Prefix bp = { 0, 0, 0 };
	h->prefix_length = XDRStream::Length (xdr_BF_Prefix, &bp);
	h->file_length = h->header_size;
	InitBlock (h->app_header);
	InitBlock (h->freelist);
#ifdef notdef
	InitBlock (h->userlist);
	InitBlock (h->blocklist);
	InitBlock (h->locks);
	InitBlock (h->journal);
#endif
#ifdef notdef
	h->requested = 0;
	h->granted = 0;
	h->spacefree = 0;
	h->nfree = 0;
#endif
}



/*
 * Write the header to the file.
 */
void
BlockFile::WriteHeader (void)
{
	encode->setpos(0);
	encode << *header;
}



/*
 * Read the header from the file.
 */
void
BlockFile::ReadHeader (void)
{
	decode->setpos(0);
	decode >> *header;
}




int
BlockFile::write (void *buf, long size)
{	
	if (fwrite ((char *)buf, (size_t)size, 1, fp) != 1)
	{
		this->sys_errno = errno;
		this->status = WRITE_FAILED;
	}
	return (status);
}



int
BlockFile::read (void *buf, long size)
{	
	if (fread ((char *)buf, (size_t)size, 1, fp) != 1)
	{
		this->sys_errno = errno;
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

