/* 10/88 jc */
# include <string.h>
# include <stdlib.h>

void
fixdir (env, def, file, dest)
char *env, *def, *file, *dest;
/*
 * Translate an environment variable, and qualify the
 * given FILE by the result.  If there is no translation for
 * ENV, the DEF name, if non-null, will be used.
 * The result is put into DEST.
 */
{
	char *trans, *temp;
	
/*
 * First of all, look at the file name.  If it starts with a slash,
 * we simply take it as it is.
 */
	if (file[0] == '/')
	{
		strcpy (dest, file);
		return;
	}
/*
 * If the environment variable translates, use it.
 */
 	if ((trans = getenv (env)))
	{
		strcpy (dest, trans);
/*
 * Check to see if the last character in the directory name indicates
 * that the file is on a VMS machine.  If so, don't add a '/'.
 */
		temp = &dest[strlen(dest) - 1];
		if (strcspn (temp, ":]>")) strcat (dest, "/");
		strcat (dest, file);
	}
/*
 * Failing that, copy the def if it exists, then put in the file.
 */
 	else if (def)
	{
		strcpy (dest, def);
		temp = &dest[strlen(dest) - 1];
		if (strcspn (temp, ":]>")) strcat (dest, "/");
		strcat (dest, file);
	}
	else
		strcpy (dest, file);
}



void
fixdir_t (env, def, file, dest, type)
char *env, *def, *file, *dest, *type;
/*
 * Translate an environment variable, and qualify the
 * given FILE by the result.  If there is no translation for
 * ENV, the DEF name, if non-null, will be used.
 * The result is put into DEST.
 * Also, if a file lacks a type string, add it.
 */
{
	char *trans, *slash;
/*
 * First of all, look at the file name.  If it starts with a slash,
 * we simply take it as it is.
 */
	if (file[0] == '/')
		strcpy (dest, file);
/*
 * If the environment variable translates, use it.
 */
 	else if ((trans = getenv (env)))
	{
		strcpy (dest, trans);
		strcat (dest, "/");
		strcat (dest, file);
	}
/*
 * Failing that, copy the def if it exists, then put in the file.
 */
 	else if (def)
	{
		strcpy (dest, def);
		strcat (dest, "/");
		strcat (dest, file);
	}
	else
		strcpy (dest, file);
/*
 * Look for an extension, in a rather simple sort of way.
 */
 	if ((slash = strrchr (dest, '/')) == 0)
		slash = dest;
	if (! strchr (slash, '.'))
		strcat (dest, type);
}

