/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 NCHDL.C 27-Feb-96,13:15:34,`ROBERTM' initial checkin of DMSP nav        */
/* 2 NCHDL.C 17-Apr-96,14:53:16,`USER' Released                              */
/* 3 NCHDL.C 27-Sep-96,16:29:48,`ROBERTM' Include mcidasp.h; improve doc bloc*/
/* 4 NCHDL.C 22-Oct-96,19:42:38,`USER' Released                              */
/**** McIDAS Revision History *** */

#include "mcidas.h"
#include "mcidasp.h"
#include "m0frame.h"
#include "m0gpnav.h"

/* Implementation note:
 *
 * The list of handle addresses and values is maintained as a
 * doubly-linked list. I have chosen a d.l.l. because it
 * is easier to re-order if, for performance reasons, I want
 * to be sure that the most recently accessed node is always
 * at the front. At present this is not done, though.
 *
 * Here is the linked-list node structure:
 */

struct hdl_node {
	struct hdl_node	*prev;	/* previous node	*/
	struct hdl_node	*next;	/* next     node	*/
	Fint		*p_hdl;	/* handle address	*
				 * from application	*/
	Fint		hdl;	/* handle value		*/
};

/* And here are the head and tail pointers for the list and
 * the next handle value to be assigned:
 */

static struct hdl_node	*Head    = NULL;   /* list head         */
static struct hdl_node	*Tail    = NULL;   /* list tail         */
static Fint		Next_hdl = 1;	   /* next handle value
					    * to assign */



/*
*| Name:
*|      m0nchdl - Retrieves the nav/cal handle for a given slot.
*|
*| Interface:
*|	integer function
*|	m0nchdl(integer handle)
*|
*| Input:
*|	none
*|
*| Input and Output:
*|	none
*|
*| Output:
*|      handle  - Handle to this nav/cal instance.
*|
*| Return values:
*|       1      - Success, handle added to list
*|       0      - Success, previously assigned handle recognized
*|	-1	- Memory allocation failure 
*|
*| Remarks:
*|      M0nchdl() allocates memory.
*|
*|      The address of 'handle' should be unique for each
*|	nav/cal 'slot' (instance) in use by an APPLICATION.
*|	This can conveniently be done by passing a variable
*|	in named common from the nav/cal API routine
*|	nvxini() and then using that variable as the 'handle'
*|	argument to all re-entrant C nav/cal calls (navigation
*|	examples <type>ini(), <type>fwd(), <type>inv(),
*|	and <type>opt() where <type> is the name of the 
*|	navigation type).
*|
*|      Generated handles are positive integers.
*|
*| Categories: 
*|	navigation  
*/

extern Fint 
m0nchdl_(Fint *handle)
{

	/* Local variables	*/

	struct hdl_node	*current;	/* pointer for traversing
					 * list of assigned
					 * handles */


	/* Compare the incoming address of 'handle' to the
	 * known handles registered so far by this application.
	 * If the handle address is recognized, return the
	 * value. If not, allocate a new handle instance
	 * and generate a new, unique (to this application)
	 * handle value and return that. */

	if(Head != NULL) {

		M0ASSERT(Tail != NULL);	/* If list head is non-null,
					 * so must tail be */

		current = Head;

		while ( current != NULL ) {
	
			if(current->p_hdl == handle) {
			
				*handle = current->hdl;
				return 0;
			}
			current = current->next;
		}
	}

	/* Handle address 'handle' could not be found, so allocate
	 * a new 'hdl_node' instance, assign a handle
	 * value, and link it onto the front of the list */

	if( ! M0newMemory( (void **)&current,
	  sizeof(struct hdl_node) ) ) {
		return -1;
	}
	current->p_hdl	= handle;
	current->hdl	= Next_hdl;
	Next_hdl++;

	current->prev	= NULL;
	current->next	= Head;
	Head		= current;
	if( Tail == NULL ) {
		Tail = current;
	}

	*handle		= current->hdl;
	return 1;
}

#ifdef M0DEBUG


/*
*| Name:
*|      M0NCmemchk      - Validates nav/cal handle list
*|                        memory allocations.
*|
*| Interface:
*|	#include "mcidas.h"
*|	#include "m0cnav.h"
*|	#include "m0frame.h"
*|
*|	void
*|	M0NCmemchk(void);
*|
*| Input:
*|	none
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
*|      This function is used in conjunction with the
*|	'safe' memory manager to validate allocated memory
*|      blocks for bad pointers and leaks.  To use it, call
*|	M0clearMemRefs(), then the validation function for
*|	each subsystem component that uses dynamic memory
*|	(M0NCmemchk() is in this category), then
*|      M0checkMemRefs().  Leaks will result in an assertion.
*|
*| Categories: 
*|	navigation  
*/

void
M0NCmemchk( void )
{

	if( Head == NULL ) {
		M0ASSERT(Tail == NULL);	/* if one is null, the
					 * other must be, too! */
	}
	else {
		struct hdl_node	*current	 = Head;
		while(current != NULL) {
			M0ASSERT( M0validPointer(
			  (void *)current,
			  sizeof(struct hdl_node) ) );
			M0noteMemRef((void *)current);
			current = current->next;
		}
	}
	return;
}

#endif	/* #ifdef M0DEBUG	*/
