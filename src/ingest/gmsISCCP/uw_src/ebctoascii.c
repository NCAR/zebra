/* ebctoascii converts n bytes of data in an array
from ebcdic to ascii.  The input array "data" 
gets changed.  Returns 0 is all is ok.  Returns
1 if the data file cannot be found.
*/
#include <stdio.h>
#include <fcntl.h>
int ebcboot = 1;

ebctoascii(data, n)
unsigned char data[];
int n;
	{
	int i, file;
	unsigned char temp;
	static unsigned char map[256];
	if( ebcboot)
		{
		file = open("ascii.dat", O_RDONLY);
		if( file < 1)
			{
			perror("Ascii.dat not open");
			return(1);
			}
		read(file, map, 256);
		close(file);
		ebcboot = 0;
		}
	for( i = 0; i < n; i++)
		{
		temp = data[i];
		data[i] = map[temp];
		}
	return(0);
	}
