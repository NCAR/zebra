/* 5/87 jc */
/*
 * This file describes the format of an operations list.  Said lists are
 * an array of structures, each of which describes one operation.  Simple
 * operations may be completely described by the structure, while more
 * complex ones may require the allocation of additional storage.
 */

# define N_OP_STRUCT	50	/* Number of OP structs in each group */
struct oplist
{
	struct oplist	*opc_chain;	/* Next group of ops		*/
	struct operation *opc_data;	/* Actual ops to perform	*/
	int	opc_nops;		/* Number of opts in this list	*/
};


# define EXTRADATA	5	/* Size of extra data array	*/
struct operation
{
	int	op_opcode;		/* The code of the op to perform */
	int	op_color;		/* Color value to use		*/
	int	op_npt;			/* Number of data points	*/
	int	op_extra[EXTRADATA];	/* Assorted other info		*/
	char	*op_data;		/* Additional information	*/
};

/*
 * Opcodes, and extra data words.
 */
# define GOP_POLYLINE	0	/* Draw a polyline			*/
# define  GOP_PL_LTYPE	 0		/* Vector line type		*/
# define GOP_TEXT	1	/* Write text				*/
# define  GOP_T_X	 0		/* X position			*/
# define  GOP_T_Y	 1		/* Y Position			*/
# define  GOP_T_FONT	 2		/* Font number			*/
# define  GOP_T_SCALE	 3		/* Scale, *100			*/
# define  GOP_T_ROT	 4		/* Rotation in degrees (*100)	*/
# define GOP_SETHCW	2	/* Set hardware clip window		*/
# define  GOP_W_X0	 0		/* x0				*/
# define  GOP_W_Y0	 1		/* y0				*/
# define  GOP_W_X1	 2		/* x1				*/
# define  GOP_W_Y1	 3		/* y1				*/
