main(argc, argv)
int argc;
char *argv[];
	{
	int low, high;
	int color, value;
	int colors[256], i;
	int index;
	float scale;

	low = atoi(argv[1]);
	high = atoi(argv[2]);
	scale = 256. / (float) (high - low);
	mgiasngp(0,0);
	for( i = 0; i < low; i++)
		colors[i] = 0;
	for( i = high +1; i < 256; i++)
		colors[i] = 0;

	for(i = low; i <= high; i++)
		{
		index = 255 - (i - low) * scale;
		colors[i] = (index << 16) | (index << 8) | index;
		}
	mgicms(0, 256, colors);
	mgideagp();
	exit(0);
	}

		
