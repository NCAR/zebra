/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 CNAV.C 27-Feb-96,13:13:28,`ROBERTM' initial checkin of DMSP nav         */
/* 2 CNAV.C 17-Apr-96,14:47:38,`USER' Released                               */
/* 3 CNAV.C 9-Sep-96,10:32:12,`BILLL' Added programmer documentation (6926). */
/* 4 CNAV.C 18-Sep-96,14:07:36,`DAVEST' Added mcidasp.h for nav work         */
/* 5 CNAV.C 23-Sep-96,12:34:32,`USER' Released                               */
/* 6 CNAV.C 27-Sep-96,16:29:44,`ROBERTM' Improve documentation block style   */
/* 7 CNAV.C 22-Oct-96,19:40:58,`USER' Released                               */
/**** McIDAS Revision History *** */

#include <string.h>
#include "mcidas.h"
#include "mcidasp.h"
#include "m0frame.h"
#include "m0gpnav.h"


/* Here is the complete declaration of M0ND, declared
 * as an incomplete type in m0gpnav.h */

struct M0ND_ {
	void	**apObject;	/* Array of object pointers	*/
	size_t	obj_size;	/* size of each object		*/
	int	nInstances;	/* Number of allocated instances*/
};



/*
*| Name:
*|      M0NDnew - Initializes a navigation data collection.
*|
*| Interface:
*|      #include "mcidas.h"
*|      #include "m0cnav.h"
*|	
*|      int
*|      M0NDnew(size_t size, M0ND **phND)
*|
*| Input:
*|      size        - Size of objects that will be stored.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      *phND       - Pointer to nav data collection object.
*|
*| Return values:
*|      0  - success
*|     -1  - memory allocation failure
*|
*| Remarks:
*|      M0NDnew() allocates memory.
*|      M0ND is an incomplete type; users of the sub
*|      system can only define pointers to it.  Such pointers
*|      must be initialized by calling e.g.
*|		M0NDnew(sizeof(my_data_type), &hMylist);
*|      'hMylist' is assumed NULL. This assumption is validated
*|      with an assertion when compiled with M0DEBUG defined.
*|
*| Categories: 
*|      navigation
*/

int
M0NDnew(size_t size, M0ND **phND)
{
	/* Make sure the collection is not already allocated. If not,
	 * allocate and populate it	*/

	M0ASSERT( *phND == NULL ); 

	if( ! M0newMemory( (void **)phND, sizeof(M0ND) ) ) {
		return -1;
	}

	M0ASSERT ( M0validPointer(*phND, sizeof(M0ND) ) );

	(*phND)->apObject	= NULL;
	(*phND)->obj_size	= size;
	(*phND)->nInstances	= 0;

	return 0;
}

/*
*| Name:
*|      M0NDdel - Deallocates a navigation data collection.
*|
*| Interface:
*|      #include "mcidas.h"
*|      #include "m0cnav.h"
*|	
*|      void
*|      M0NDdel(M0ND **phND)
*|
*| Input:
*|      phND    - Address of handle to nav data collection.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|      none
*|
*| Remarks:
*|      M0NDdel() frees collections created with M0NDnew() and
*|      populated with M0NDadd().
*|
*|      M0ND is an incomplete type; users of the sub
*|      system can only define pointers to it.  Such a pointer
*|      must be initialized by calling e.g.
*|      M0NDnew(sizeof(my_data_type), &hMylist);
*|
*|      'hMylist' is assumed NULL.  This assumption is validated
*|      with an assertion when compiled with M0DEBUG defined.
*|
*| Categories: 
*|      navigation
*/

void
M0NDdel(M0ND **phND)
{
	int		i;		/* instance index	*/	

	/* Verify that the collection exists */

	M0ASSERT( *phND != NULL );

	/* If any objects are allocated, traverse the list of 	*
	 * pointers to them and free them. Recall that indexing	*
	 * is 1-based. Then free the pointer list... */

	if( (*phND)->apObject != NULL ) {
		for( i = 1; i <= (*phND)->nInstances; i++ ) {
	
			void	*pObject;

			pObject = (*phND)->apObject[i-1];
			if( pObject != NULL ) {
				M0freeMemory( &pObject );
			}
		}

		/* ... the array of object pointers ...		*/

		M0freeMemory( (void **)&((*phND)->apObject) );
	}
	
	/* ... and the control structure itself.		*/

	M0freeMemory( (void **)phND);
	*phND = NULL;

	return;
}

/*
*| Name:
*|      M0NDadd - Adds an instance of an object to collection.
*|
*| Interface:
*|      #include "mcidas.h"
*|      #include "m0cnav.h"
*|	
*|      int
*|      M0NDadd(M0ND *hND, int instance,
*|      void *pObject, size_t size)
*|
*| Input:
*|      hND         - Handle to collection.
*|      instance    - Number of instance (1-based).
*|      pObject     - Pointer to object.
*|      size        - Size (bytes) of object.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|      0           - Success.
*|     -1           - Memory allocation failure.
*|
*| Remarks:
*|	    M0NDadd() allocates memory.
*|      A copy of *pObject will be added to the collection
*|      for the specified instance.  The object previously
*|      associated with this instance (if any) will be deleted.
*|      Note that if pObject points to a dynamically allocated
*|      object (and not a locally defined one in the caller),
*|      the caller is responsible for freeing it.  M0NDadd() saves
*|      a copy, not the original.
*|
*| Categories: 
*|      navigation
*/

int
M0NDadd(M0ND *pND, int instance, void *pObject, size_t size)
{
	void		*pNewObj;	/* new object pointer	*/

	/* Validate a positive instance			*/

	M0ASSERT(instance >= 1);

	/* Validate that the collection is initialized	*/

	M0ASSERT( pND!= NULL);	/* collection not initialized! */

	/* Validate the size of the object; it must exactly
	 * match the size specified for this particular collection */

	M0ASSERT( size == pND->obj_size);
						
	/* Enlarge the array of object pointers, if necessary,
	 * and set them to NULL (at present they are not
	 * initialized) */


	if(instance > pND->nInstances) {

		int	i;	/* loop index	*/
		M0flag	ok;	/* memory allocation OK ? */

		if( pND->nInstances == 0 ) {
			ok = M0newMemory( (void **)&(pND->apObject),
			  instance*sizeof(void *));
		}
		else {
			ok = M0resizeMemory((void **)&(pND->apObject),
			  instance*sizeof(void *));
		}
		if( !ok ) {
			return -1;
		}

		for( i= pND->nInstances; i < instance; i++) {
			pND->apObject[i] = NULL;
		}

		pND->nInstances	= instance;
	}

	/* Make a copy of the new object. If there is an old
	 * object stored for this instance, free it before
	 * storing the address of the new object */

	if( ! M0newMemory( (void **)&pNewObj, size) ) {
		return -1;
	}
	memcpy(pNewObj, pObject, size);

	if( pND->apObject[instance-1] != NULL ) {
		M0freeMemory( (void **)&(pND->apObject[instance-1]));
	}
	M0ASSERT( M0validPointer( pND->apObject,
	  sizeof(void *)*(pND->nInstances) ) );
	pND->apObject[instance-1] = pNewObj;


	return 0;
}

/*
*| Name:
*|      M0NDget - Gets an instance of an object from collection.
*|
*| Interface:
*|      #include "mcidas.h"
*|      #include "m0cnav.h"
*|	
*|      int
*|      M0NDget(M0ND *hND, int instance, size_t size,
*|              void *pObject)
*|
*| Input:
*|      hND         - Handle to collection.
*|      instance    - Number of instance (1-based).
*|      size        - Size (bytes) of object.
*|
*| Input and Output:
*|      *pObject    - Pointer to block where a copu of the object will be
*|                    placed.  The block will contain at least ;size'
*|                    bytes.
*|
*| Output:
*|      none
*|
*| Return values:
*|       0  - Success.
*|      -1  - requested object does not exist.
*|
*| Remarks:
*|       M0NDget() does not allocate any memory.
*|       It is the caller's responsibility to see that *pObject
*|       references a block that is large enough to receive the
*|       object.  This can be a pointer to a local variable or to
*|       an allocated block; the caller receives a copy of the
*|       stored object at that location.
*|
*| Categories: 
*|       navigation
*/

int
M0NDget(M0ND *hND, int instance, size_t size, void *pObject)
{
	/* Validate that the instance number is positive, the	*
	 * collection is initialized, and the requested size	*
	 * is consistent with that provided to M0NDnew() when	*
	 * the collection was created */

	M0ASSERT(instance >= 1);
	M0ASSERT(hND != NULL);
	M0ASSERT(size == hND->obj_size);


	/* The instance does not exist if it is either larger	*
	 * than the number stored or has a null object pointer	*/

	if( instance > hND->nInstances ||
	    hND->apObject[instance-1] == NULL ) {
		return -1;
	}

	/* If a non-null pointer exists, it is assumed to	*
	 * reference known memory. If so, copy the object	*/

	M0ASSERT ( M0validPointer(hND->apObject[instance-1],
	  size));
	memcpy(pObject, hND->apObject[instance-1], size);

	return 0;
}

#ifdef M0DEBUG

/*
*| Name:
*|      M0NDmemchk  - Validates memory allocation for
*|                    navigation data collection.
*|
*| Interface:
*|      #include "mcidas.h"
*|      #include "m0frame.h"
*|      #include "m0gpnav.h"
*|
*|      void
*|      M0NDmemchk(void);
*|
*| Input:
*|      none
*|
*|      Input and Output:
*|      none
*|
*| Output:
*|      none
*|
*| Return values:
*|      none
*|
*| Remarks:
*|	    This function is used in conjunction with the
*|      'safe' memory manager to validate allocated memory
*|      blocks for bad pointers and leaks. To use it, call
*|      M0clearMemRefs(), then the validation function for
*|      each subsystem component (M0NDmemchk() is in this
*|      category), then M0checkMemRefs().  Leaks will result
*|      in an assertion.
*|
*|      Assigned handles are stored in a doubly-linked
*|      list, in reverse order assigned (head is most recent).
*|
*| Categories: 
*|      navigation
*/
void
M0NDmemchk(M0ND *pND)
{
	M0flag	ok;	/* Boolean return code	*/
	
	if(pND!= NULL) {

	    /* if pND non-null, validate that it points
	     * to a block of the correct size and note it
	     * as referenced */

	    ok = M0validPointer( pND, sizeof(M0ND) );
	    M0ASSERT(ok);		/* list control struct
					 * pointer is bad	*/
	    M0noteMemRef(pND);

	    if( pND->nInstances != 0) {

	        int	i;	/* current instance	*/
		void	*pObj;	/* current object	*/

	        /* make sure size is set, too */

	        M0ASSERT(pND->obj_size > 0);

	        /* If there are any instances stored, validate
	         * the array of pointers to them. Make sure that
	         * the pointer array is large enough to hold the
	         * number of instances claimed, and mark it as
	         * referenced */

	        ok = M0validPointer(pND->apObject,
	          sizeof(void *) * pND->nInstances);
	        M0ASSERT(ok);	/* Object pointer stack
				 * is bad	*/
	        M0noteMemRef(pND->apObject);

		for(i=0; i<pND->nInstances; i++ ) {

	            /* For each object pointer, verify that
	             * it indeed points to an object of the correct
	             * size, and then mark it as referenced. Skip
		     * null pointers; they are for 'handles' that
		     * were never used */

	            pObj = (pND->apObject)[i];
		    if(pObj != NULL ) {
	            	ok = M0validPointer(pObj, pND->obj_size);
	            	M0ASSERT(ok);	/* object pointer bad	*/
	            	M0noteMemRef(pObj);
		    }
	        }
	    }
	}
	return;
}


#endif	/* #ifdef M0DEBUG	*/
