/*
 * LUN types
 */
# define	LUN_FREE	0
# define	LUN_LOCAL	1
# define	LUN_NTDSK_BIO	2
# define	LUN_NTDSK_DISK	3
/*
 * 'bio.c' operations
 */
# define	OP_BIO_OPEN	0
# define	OP_BIO_VIEW	1
# define	OP_BIO_CREATE	2
# define	OP_BIO_TEMP	3
# define	OP_BIO_CLOSE	4
# define	OP_BIO_READ	5
# define	OP_BIO_WRITE	6
/*
 * 'disk.c' operations
 */
# define	OP_DCREATE	7
# define	OP_DOPEN	8
# define	OP_DVIEW	9
# define	OP_DAPPEND	10
# define	OP_DPUT		11
# define	OP_DGET		12
# define	OP_DRFA		13
# define	OP_DAGAIN	14
# define	OP_DFIND	15
# define	OP_DCLOSE	16
# define	OP_DREWIND	17
