/* memline draws lines of continents on the screen */
#define NROWS	1100
#define NCOLS	1100
memline(npoints, Line, Element)
int 
	npoints,
	Line[],
	Element[];
	{
/* variable file is allocated, file is opened and closed in
makeimage2.c */
	extern int file;
	int i;
	if( npoints <= 1) return(0);
#ifdef graphics
	mgipln(0xfff);
	mgihue(255);
	mgils(npoints, Element, Line); 
	mgipln(0xff);
#endif
	write(file, &npoints, sizeof(int));
	write(file, Line, npoints * sizeof(int));
	write(file, Element, npoints * sizeof(int));
	return(0);
	}
