/*
 * Useful definitions.
 */
/* $Id: defs.h,v 2.0 1991-07-18 23:07:02 corbet Exp $ */
# ifndef _DEFS_H_
# define _DEFS_H_

# include <ui.h>
# include <memory.h>

/*
 * FCC-specific defined types.
 */
typedef struct date_st time;	/* Different from UI "date" so we can 
				   change it. */
enum pmode { NoMode, History, RealTime };

/*
 * Locations.
 */
typedef struct s_Location
{
	float	l_lat;
	float	l_lon;
	float	l_alt;
} Location;


/*
 * Functions.
 */
# ifdef __STDC__
	char *malloc (unsigned size);
	char *realloc (void *ptr, unsigned size);
	void tw_DefTimeWidget (int (*callback) (), char *title);
	void tw_DialAdjust (int, int);
	int InterpDTime (char *);
	void TC_SysToFcc (long, time *);
	long TC_FccToSys (time *);
	void	RL_Encode (unsigned char *, unsigned char *, int, int, 
			int *, int *);
	void 	RL_Decode (unsigned char *, unsigned char *const, int);
	int	CommaParse (char *, char **);
# else
	char *malloc ();
	char *realloc ();
	void tw_DefTimeWidget ();
	void tw_DialAdjust ();
	int InterpDTime ();
	void TC_SysToFcc ();
	long TC_FccToSys ();
	void	RL_Encode ();
	void 	RL_Decode ();
	int	CommaParse ();
# endif


/*
 * Macros
 */
# define ALLOC(type) ((type *) malloc (sizeof (type)))
# define ODD(v) ((v) & 0x1)
# define EVEN(v) (((v) & 0x1) == 0)

# define DLT(d1,d2) ((d1).ds_yymmdd < (d2).ds_yymmdd || \
	((d1).ds_yymmdd == (d2).ds_yymmdd && (d1).ds_hhmmss < (d2).ds_hhmmss))

# define DLE(d1,d2) ((d1).ds_yymmdd < (d2).ds_yymmdd || \
	((d1).ds_yymmdd == (d2).ds_yymmdd && (d1).ds_hhmmss <= (d2).ds_hhmmss))
/*
 * Set up inline so that we can use it.
 */
# ifndef __GNUC__
# define inline
# endif


# endif /* _DEFS_H_ */
