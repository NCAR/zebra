void
strcpyUC (dest, src)
char *dest, *src;
/*
 * Copy src to dest, converting to upper case.
 */
{
	while (*src)
		if (*src >= 'a' && *src <= 'z')
			*dest++ = *src++ + 'A' - 'a';
		else
			*dest++ = *src++;
	*dest = 0;
}

