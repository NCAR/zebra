#include <stdio.h>

main()
{
short int	buf[220];
int		*ibuf;

ibuf = (int *)buf;

while(fread((char *)buf, sizeof(buf),1,stdin))
  {
	if (!buf[7]%5)
	printf("%d %.2f %.2f %.1f 0 0 0\n",
	buf[5]*3600+buf[6]*60+buf[7],
	ibuf[28]*3.352761e-7,ibuf[29]*3.352761e-7,ibuf[27]*4.650878e-4,
	ibuf[30]*7.849819e-07,ibuf[31]*7.849819e-07,ibuf[32]*5.813594e-07);

}
exit(1);
}
