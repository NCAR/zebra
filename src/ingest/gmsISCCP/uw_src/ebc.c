#include <stdio.h>
#include <fcntl.h>
main()
	{
	int i;
	unsigned char a[256];
	int file; 

	for(i = 0; i < 256; i++)
		a[i] = i;
	file = open("ebcdic.dat", O_WRONLY, 0777);
	if( file < 1)
		{ perror("Not open"); exit(0);}
	write(file, a, sizeof(a));
	close(file);
	exit(0);
	}
