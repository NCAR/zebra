/*
 * Event queue definitions.
 */

/*
 * Priorities.
 */
typedef enum EqPriority {
	PUrgent = 0,
	PDisplay = 1,
	PWhenever = 2
} EQpriority;

/*
 * Overrides
 */
typedef enum {
	Override,
	Augment,
	Bounce
} EQoverride;


# ifdef __STDC__
void Eq_AddEvent (EQpriority pri, void (*proc) (), void *data, int len,
	EQoverride override);
void Eq_ZapProc (EQpriority pri, void (*proc) ());

# else
void Eq_AddEvent ();
void Eq_ZapProc ();

# endif
