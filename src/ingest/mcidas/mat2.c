/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 MAT2.C 27-Feb-96,13:15:18,`ROBERTM' initial checkin of DMSP nav         */
/* 2 MAT2.C 17-Apr-96,14:52:36,`USER' Released                               */
/* 3 MAT2.C 27-Sep-96,16:29:36,`ROBERTM' Include mcidasp.h; Improve doc block*/
/* 4 MAT2.C 22-Oct-96,19:42:08,`USER' Released                               */
/**** McIDAS Revision History *** */

/*****************************************
 * INCLUDES
 ****************************************/

#include "mcidasp.h"
#include "m0frame.h"
#include <string.h>


/*****************************************
 * PROTOTYPES (local functions)
 ****************************************/

#if defined(M0DEBUG)

static void
MTshred(M0MT *);

#endif

/* IMPLEMENTATION NOTES:
 * The purpose of this data structure is to allow a two-dimensional
 * matrix of 'double' that 
 *   1) be accessed by simple subscripting, i.e.
 *        some_value = pMT->val[3][8]
 *      fetches the value from row 3 column 8 (zero-based), 
 *   2) can be of arbitrary size, and
 *   3) is fully self-describing, i.e. can be passed to functions
 *      and be fully useable within the function without any
 *	prior or ancillary knowledge of its size
 *
 * The way this is done is to allocate a single block of memory
 * at least large enough to accommodate the entire matrix. This
 * is the 'physical size.' It is then conceptually broken up into
 * physical 'rows' starting every 'mxcol' words, and the addresses
 * of the start of each physical row captured in an array of
 * pointers. This array of pointers, 'val,' is the means of accessing
 * the 2-d array. Subscripting it once, pMT->val[3], gives the
 * address in memory of the start of row 3. Subscripting again,
 * pMT->val[3][8] gives the 8th element of row 3 as desired.
 *
 * Note that the 'logical' size of the matrix can be less than
 * the physical size, i.e. there can be unused elements at the
 * end of each row, and unused rows at the end of the block. The
 * M0MT object keeps track of both its logical and physical size.
 * When compiled with M0DEBUG defined, it can use its knowledge of
 * its logical size to destroy the contents of the physical matrix
 * that are outside the logical dimensions, thereby making array
 * bounds errors obvious. */



/*****************************************
 * FUNCTION DEFINITIONS
 ****************************************/

/*
*| Name:
*|      M0MTnew - Creates a two-dimensional matrix object.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	int
*|	M0MTnew(M0MT **ppMT, int nrows, int ncols);
*|
*| Input:
*|      ppMT            - Address of pointer to two-dimensional
*|                        matrix.
*|      nrows           - Number of rows.
*|      ncols           - Number of columns.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|       0      - Successful.
*|      -1      - Memory allocation for control struct failed.
*|      -1      - Memory allocation for row pointers failed.
*|      -3      - Memory allocation for matrix failed.
*|
*| Remarks:
*|	It is assumed that *ppMT is null and nrows and ncols are 
*|      nonzero.  This is validated with assertions when compiled with
*|      M0DEBUG defined.  Memory leaks, or worse, may result if these
*|	assumptions are violated.
*|
*| Categories: 
*|	navigation 
*|      utility
*/

int
M0MTnew(M0MT **ppMT, int nrows, int ncols)
{
	double	*pBlock;	/* pointer to block of 
				 * allocated memory for
				 * matrix elements	  */
	int	row;		/* current row		  */
	M0MT	*pMT;		/* local pointer to M0MT  *
				 * (just for clarity)	  */

	M0ASSERT(*ppMT==NULL);	/* matrix already allocated!	*/
	M0ASSERT(nrows>0);	/* non-positive row count!	*/
	M0ASSERT(ncols>0);	/* non-positive column count!	*/


	/* Allocate 2-d matrix control struct and set dimensions */

	if ( !M0newMemory((void **)&pMT, sizeof(M0MT)) )
	    return -1;

	pMT->mxrow	= nrows;
	pMT->nrow	= nrows;
	pMT->mxcol	= ncols;
	pMT->ncol	= ncols;


	/* Allocate row pointers */

	if ( !M0newMemory((void **)&(pMT->val),
	  nrows*sizeof(double *)) ) {
		M0freeMemory((void **)&pMT);
		return -2;
	}


	/* Allocate memory for matrix storage and capture offsets into
	 * it for use as row pointers */

	if ( !M0newMemory((void **)&pBlock,
	  nrows*ncols*sizeof(double)) ) {
		M0freeMemory((void **)&pMT->val);
		M0freeMemory((void **)&pMT);
		return -3;
	}
	for ( row=0; row<nrows; row++ ) {
		pMT->val[row] = &pBlock[row*ncols];
	}


	/* transcribe temporary M0MT pointer back into argument */

	*ppMT = pMT;

	return 0;
}



/*
*| Name:
*|      M0MTresize - Resizes a two-dimensional matrix object.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	int
*|	M0MTresize(M0MT **ppMT, int nrows, int ncols);
*|
*| Input:
*|      ppMT       - Address of pointer to two-dimensional
*|                   Matrix.
*|      nrows      - New number of nrows.
*|      ncols      - New number of columns.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|       0         - Successful.
*|      -1         - Could not resize block.
*|      -2         - Could not resize row pointer array.
*|
*| Remarks:
*|      It is assumed that *ppMT is non-null and *ppMT->val[0]
*|	is non-null, i.e. that the matrix already has some allocated
*|      space.  These are validated with assertions when compiled with
*|	M0DEBUG defined.  The matrix allocation does not actually
*|      shrink, only the logical size.  With M0DEBUG defined, space
*|	outside the logical bounds of the array is filled with
*|	garbage after a shrink, and newly allocated space is filled
*|      with garbage after an expand.  This is designed to trap
*|      inappropriate use by the caller - referencing beyond array
*|	bounds or using uninitialized elements, respectively.
*|	The original contents, including logical sizes, are un-
*|	changed in the event of a memory allocation failure.
*|
*| Categories: 
*|      navigation
*|      utility
*/

int
M0MTresize(M0MT **ppMT, int new_nrow, int new_ncol)
{
	double	*pBlock;	/* pointer to allocated block	*/

	int	new_mxcol;	/* new allocated column size	*/
	int	new_mxrow;	/* new allocated row size	*/
	int	new_size;	/* new allocated # elements	*/
	int	old_mxrow;	/* prior allocated row
				 * size of matrix object	*/
	int	old_mxcol;	/* prior allocated column
				 * size of matrix object	*/
	int	old_ncol;	/* initial logical column size	*/
	int	old_nrow;	/* initial logical row size	*/
	int	old_size;	/* initial allocated size,
				 * total number of elements	*/
	int	row;		/* row index			*/

	M0flag	data_move;	/* do matrix contents need
				 * to be moved in memory?	*/
	M0MT	*pMT;		/* local pointer to matrix
				 * object*/

	/* Validate arguments */

	M0ASSERT(new_nrow>0);	/* non-positive row count!	*/
	M0ASSERT(new_ncol>0);	/* non-positive column count!	*/

	/* Initialize variables	*/
	
	pMT		= *ppMT;
	old_mxrow	= pMT->mxrow;		
	old_mxcol	= pMT->mxcol;
	old_nrow	= pMT->nrow;
	old_ncol	= pMT->ncol;
	old_size	= old_mxrow*old_mxcol;
	data_move	= M0FALSE;


	/* Validate that matrix header (M0MT) is allocated;
	 * capture starting location of matrix memory block
	 * and validate that there is one allocated */

	M0ASSERT(pMT != NULL);
	pBlock = pMT->val[0];
	M0ASSERT(pBlock != NULL);


	/****************************************************
	 * Determine new physical (allocated) dimensions
	 * and total size. Note if the physical row length
	 * (mxrow) is increasing; if so, a data move will
	 * also be necessary
	 ***************************************************/

	/* Neither physical dimension is ever reduced in this
	 * implementation. This may waste some space (having a 2x10
	 * and resizing to 10x2 will result in a physical size of
	 * 10x10) but is simpler. */
	
	if ( new_nrow > old_mxrow ) {
		new_mxrow = new_nrow;
	}
	else {
		new_mxrow = old_mxrow;
	}

	/* Data are laid out row-wise in the memory block, so if
	 * the length physically available for each row changes,
	 * the starting points of the rows change and the data 
	 * have to be moved */

	if ( new_ncol > old_mxcol ) {
		data_move = M0TRUE;
		new_mxcol = new_ncol;
	}
	else {
		new_mxcol = old_mxcol;
	}
	new_size  = new_mxrow*new_mxcol;

	/****************************************************
	 * If more physical space is needed, request it.
	 * If the number of rows has increased, enlarge the
	 * array of row pointers.
	 * If the physical row length has changed, reset
	 * the row pointers to point to the start of the
	 * enlarged rows within the memory block.
	 ***************************************************/

	if ( new_size > old_size ) {

		/* expand the memory block for matrix storage */

		if( !M0resizeMemory((void **)&pBlock,
		  new_size*sizeof(double)) ) {
			return -1;
		}
	}


	if( new_mxrow > old_mxrow ) {

		/* Enlarge the row pointer array */

		if( !M0resizeMemory((void **)&(pMT->val),
          	  new_mxrow*sizeof(double *)) ) {
			return -2;
		}
	}

	if( (new_mxcol != old_mxcol) || (new_mxrow !=old_mxrow) ) {

		/* If the physical row length or number of
		 * rows have changed, reset the row pointers
		 * to account for the new starting
		 * locations of rows in the memory block */

		for ( row=0; row<new_mxrow; row++ ) {
			pMT->val[row] = pBlock+row*new_mxcol;
		}
	}

	/****************************************************
	 * If the physical row length has changed, the
	 * existing data have to be moved.
	 ***************************************************/

	if( data_move ) {

		/* Elements of each row are contiguous and can
		 * thus be moved intact, but the rows must be
		 * moved in the right order to avoid over-writing
		 * any locations that are yet to move. 
		 * In general:
		 *   If the rows are getting longer, they must be
		 *      moved from last to first.
		 *   If shorter, move from first to last.
		 * In the present implementation, the first case
		 * is the only one as physical dimensions are
		 * never reduced. */

		for ( row=old_nrow-1; row>0; row-- ) {
	        	memmove( (void *)(pBlock+row*new_mxcol),
			  (void *)(pBlock+row*old_mxcol),
			  old_ncol*sizeof(double) );
		}
	}

	/****************************************************
	 * Adjust the physical bounds of the array.
	 ***************************************************/

        pMT->mxrow = new_mxrow;
        pMT->mxcol = new_mxcol;

#if defined(M0DEBUG)

	/* The 'shred' will destroy all elements within the 
	 * physical bounds of the array but outside the logical
	 * bounds. */

	MTshred(pMT);
#endif

	/****************************************************
	 * Adjust the logical bounds of the array.
	 ***************************************************/

	pMT->nrow	= new_nrow;
	pMT->ncol	= new_ncol;

#if defined(M0DEBUG)

	/* if the array shrunk in either dimension, this 
	 * will destroy those elements that are still in
	 * storage but are no longer 'logically' accessible.
	 * This makes accidental access painfully evident
	 * during development with M0DEBUG defined */
	
	MTshred(pMT);
#endif
	return 0;
}



#ifdef M0DEBUG

/*
*| Name:
*|      MTshred - Destroys element values outside logical bounds
*|                of matrix.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	void
*|	MTshred(M0MT *pMT);
*|
*| Input:
*|      pMT     - Pointer to two-dimensional matrix.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	none
*|
*| Remarks:
*|	This routine is available only when compiled with the M0DEBUG
*|	option and only within this module.
*|
*| Categories: 
*|      utility
*/

static void
MTshred(M0MT *pMT)
{
	int	row;		/* row index			*/
	int	nrow;		/* logical row length		*/
	int	ncol;		/* logical column length	*/
	int	mxrow;		/* physical row length		*/
	int	mxcol;		/* physical column length	*/

	/* Capture current physical and logical dimensions of
	 * matrix in local variables -- this is for clarity
	 * only */

	nrow	= pMT->nrow;
	ncol	= pMT->ncol;
	mxrow	= pMT->mxrow;
	mxcol	= pMT->mxcol;

	/* Shred portions of physical rows that are beyond
	 * logical row end */

	for ( row=0; row < nrow; row++ ) {
		memset((void *)(pMT->val[row]+ncol),
		  M0GARBAGE, (mxcol-ncol)*sizeof(double));
	}

	/* Shred all elements of physical rows that are
	 * not logically accessible */

	for ( row=nrow; row<mxrow; row++ ) {
		memset((void *)(pMT->val[row]),
		  M0GARBAGE, mxcol*sizeof(double));
	}

	return;
}

#endif	/* #ifdef M0DEBUG */



/*
*| Name:
*|      M0MTdel - Destroys a two-dimensional matrix object.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	void
*|	M0MTdel(M0MT **ppMT);
*|
*| Input:
*|      ppMT    - Address of pointer to two-dimensional matrix.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	none
*|
*| Remarks:
*|	All of the dynamic components of the 2-d matrix are freed.
*|	The pointer is set to null to prevent accidental re-use.
*|
*| Categories: 
*|      utility
*/

void
M0MTdel(M0MT **ppMT) 
{
	M0MT	*pMT = *ppMT;		/* local copy of pointer, just
					 * for clarity */

	/* Free the matrix data block, the array of row pointers,
	 * the control structure M0MT */

	M0freeMemory((void **)&(pMT->val[0]) );
	M0freeMemory((void **)&(pMT->val   ) );
	M0freeMemory((void **)&pMT);	

	/* null the pointer to the control structure to 
	 * prevent later accidental use */

	*ppMT = NULL;

	return;
}


#if defined(M0DEBUG)

/*
*| Name:
*|      M0MTvalid - Validates an array address in a 2-d object.
*|
*| Interface:
*|	#include "m0frame.h"
*|
*|	M0flag
*|	M0MTvalid(M0MT *pMT, int row, int col);
*|
*| Input:
*|      pMT       - Pointer to two-dimensional matrix.
*|      row       - Row number of matrix element address to
*|                  validate.
*|      col       - Column number of matrix element address to
*|                  validate.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|      M0TRUE    - address [row][col] is within logical
*|                  bounds of array.
*|      M0FALSE   - Address [row][col] is not within logical
*|                  bounds of array.
*|
*| Remarks:
*|	This routine is available only when mat2.c is compiled with
*|	M0DEBUG defined. If an address outside the physical bounds
*|	of allocated memory is requested, an assertion will be
*|	generated here. The return code of M0FALSE applies to those
*|	addresses that are within allocated memory but not the logical
*|	size of the array and they should be caught with an assertion
*|	in the caller.
*|
*| Categories: 
*|      utility
*/

M0flag
M0MTvalid(M0MT *pMT, int row, int col)
{
	M0ASSERT(pMT != NULL);

	M0ASSERT(row>=0);	/* negative row index!	*/
	M0ASSERT(col>=0);	/* negative row index!	*/

	/* validate the row pointer, then the address, to lie within
	 * allocated memory */

	M0ASSERT(M0validPointer((void *)pMT->val[row],
	  sizeof(double *)) );
	M0ASSERT(M0validPointer((void *)&pMT->val[row][col],
	  sizeof(double)) );

	/* now validate logical address */

	if( row >= pMT->nrow || col >= pMT->ncol )
	    return(M0FALSE);
	else
	    return(M0TRUE);

}


void
M0MTmemchk( M0MT *pMT )
{
	/* mark as referenced the matrix data block, the
	 * vector of row pointers, and the control struct */

	M0noteMemRef( pMT->val[0] );
	M0noteMemRef( pMT->val );
	M0noteMemRef( pMT );

	return;
}


#endif

#if defined(UNIT_TEST)

/**********************************
 * HEADER FILES
 *********************************/

#include <stdio.h>
#include <string.h>

/**********************************
 * private functions
 *********************************/

void print_table(M0MT *pTable)
{
	int 		mxrow;	/* physical row dimension	*/
	int		mxcol;	/* physical column dimension	*/
	int		col;	/* column index			*/
	int		row;	/* row index			*/
	double		miss;	/* uninitialized data point	*/
	double		value;	/* table element		*/

	memset(&miss, M0GARBAGE, sizeof(double));

	mxrow	= pTable->mxrow;
	mxcol	= pTable->mxcol;

        printf("\nTable contents\n        ");

        for ( col=0; col<mxcol; col++) {
	    printf("%5d", col);
	}
	printf("\n----- | ");
	for ( col=0; col<mxcol; col++) {
	    printf("%s", "-----");
	}
	printf("\n");

	for ( row=0; row<mxrow; row++) {
	    printf("%5d | ", row);
	    for ( col=0; col<mxcol; col++ ) {
		value = pTable->val[row][col];
		if ( value == miss )
		    printf("  NaN");
		else
		    printf("%5.0f", pTable->val[row][col]);
	    }
	    printf("\n");
	} 
	printf("\n");


	return;
}


/**********************************
 * UNIT TEST main() DEFINITION
 *********************************/

int main ( int argc, char *argv[] )
{
	M0MT	*pTable;	/* 2-d matrix object to test	*/

	int	col;		/* column index			*/
	int	nrow;		/* logical size, rows		*/
	int	ncol;		/* logical size, columns	*/
	int	rc;		/* function return code		*/
	int	row;		/* row index			*/

	int	col_chg;	/* number of columns to add
				 * and remove */
	int	row_chg;	/* number of rows to add and
				 * remove */



	/* Argument fetching: get initial dimensions and amounts
	 * to change them by*/

	if ( argc < 5 ) {
		printf("\nusage: %s <nrow> <ncol> "
		  "<row_chg> <col_chg>\n\n", argv[0] );
		printf("\n%s tabulates the function 2*row+col+1\n",
		  argv[0]);
		printf("\nThe table will be created with nrow rows\n");
		printf("and ncol columns. It will then be shrunk\n");
		printf("by row_chg and col_chg respectively, and\n");
		printf("then enlarged by twice that\n\n");
		return(-1);
	}

	if(sscanf(argv[1],"%u", &nrow) != 1) {
		return(-2);
	}
	if(sscanf(argv[2],"%u", &ncol) != 1) {
		return(-2);
	}
	if(sscanf(argv[3],"%d", &row_chg) != 1) {
		return(-2);
	}
	if(sscanf(argv[4],"%d", &col_chg) != 1) {
		return(-2);
	}




	/* Allocate the initial matrix and fill it in */

	printf("allocating initial matrix nrow=%u ncol=%u\n",
	  nrow, ncol);

	rc = M0MTnew(&pTable,nrow,ncol);
	if( rc != 0 ) { 
		fprintf(stderr,"\nM0MTnew() failed, rc=%d\n", rc);
		return(-1);
	}

	for ( row=0; row<nrow; row++ ) {
		for ( col=0; col<ncol; col++) {

#if defined(M0DEBUG)
			if ( M0MTvalid(pTable,row,col))  {
#endif
				pTable->val[row][col]
				  = 2.*row+col+1;
#if defined(M0DEBUG)
			}
			else {
				printf("row=%d col=%d is "
				  "illegal address\n", row, col);
			}
#endif
		}
	}


	print_table(pTable);

	while( !yes("Ready to shrink?") ) {
	};


	/* Shrink the allocated space for the table */

	printf("Resize to %d rows and %d columns\n",
	  nrow-row_chg, ncol-col_chg);

	rc = M0MTresize(&pTable, nrow-row_chg, ncol-col_chg);
	if ( rc != 0 )  {
	    fprintf(stderr,"\nM0MTresize() failed, rc=%d\n", rc);
	    return(-1);
	}
	else {
	    printf("Removed %d rows and %d columns from table\n",
	      row_chg, col_chg);
	}
	print_table(pTable);




	/* Expand the allocated space for the table */

	while( !yes("Ready to expand?") ) {
	};

	printf("Resize to %d rows and %d columns\n",
	  nrow+row_chg, ncol+col_chg);

	rc = M0MTresize(&pTable, nrow+row_chg, ncol+col_chg);
	if( rc != 0 ) {
	    fprintf(stderr,"\nM0MTresize() failed, rc=%d\n", rc);
	    return(-1);
	}
	else {
	    printf("Added %d rows and %d columns to original table\n",
	      row_chg, col_chg);
	}
	print_table(pTable);



	/* now throw the matrix object away */

	M0MTdel(&pTable);

#ifdef M0DEBUG
	M0dumpMemInfo(stderr);
#endif
	printf("\nTable deleted, all done\n");

	return(0);
}


#endif 
