int
swap4 (v)
int v;
{
	char *cv = (char *) &v, c;

	c = cv[0]; cv[0] = cv[3]; cv[3] = c;
	c = cv[1]; cv[1] = cv[2]; cv[2] = c;
	return (v);
}

