main()
	{
	static float data[5] = { 1., 2., 3., 4., 5};
	static float sum[5];

	sumit(sum, data);
	sumit(sum, data);
	exit(0);
	}

sumit(sum, data)
float *sum;
float *data;
	{
	int nwords = 5;

	while( nwords--)
		*sum++ += *data++;
	return(0);

	}
