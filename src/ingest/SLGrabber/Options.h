

#ifndef _Options_h_
#define _Options_h_

#define OptionArgument &Option_Flag
extern char Option_Flag;

void OptionSetup (int *argc, char *argv[], const char *options[]);
int OptionNext ();
char *OptionArg ();

#endif /* _Options_h_ */
