//
// The file index class.
//

//
// The description of a single file.
//
class IndexFile
{
	IndexFile	*if_next;		// Next file in chain
	char *		if_name;		// Name of the file
	char *		if_plat;		// Platform name
	int		if_size;		// How big it is.
	ZebTime		if_begin;		// When it begins.
	ZebTime		if_end;			// When it ends.
	int		if_fileno;		// (tape) file number
	int		if_marked;
public:
	IndexFile (const char *plat, const char *name, int size, int filenum,
			const ZebTime *begin, const ZebTime *end);
	IndexFile (const IndexFile &);		// Initialization
	~IndexFile () { delete[] if_plat; delete[] if_name; }
//
// Basic info.
//
	const char * name () const { return if_name; }	// Get name of file
	const char * plat () const { return if_plat; }	// Get name of plat
	const ZebTime &begin () const { return if_begin; } // begin time
	const ZebTime &end () const { return if_end; };	// End time
	const int size () const { return if_size; } // how big?
	int filenum () const { return if_fileno; }	// Tape file number
	IndexFile *next () const { return if_next; }
	void link (IndexFile &next) { if_next = &next; }; // Link into chain
	int& isMarked () { return if_marked; }
};



//
// Platform index type.
//
class PlatformIndex
{
	friend STTraverseProc ZapPlat;
	STable table;			// table containing platforms.
	struct PlatInfo
	{
		ZebTime pi_begin, pi_end;
		IndexFile *pi_files;
		int pi_marked;
	};
	struct PlatInfo *findPlat (const char *name) const;
	static int TNum;		// To make unique stbl names
public:
	PlatformIndex ();
	PlatformIndex (const char *);		// Load from file.
	~PlatformIndex ();
	void add (const char *, IndexFile &);
	void add (const char *);
	int save (const char *) const;		// Save to file
	int coverage (const char *, ZebTime &, ZebTime &) const;
	int& isMarked (const char *);
	IndexFile *files (const char *) const;
};
