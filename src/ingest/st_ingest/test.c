main ()
{
	float	t = 30.0, dp = 26.0, p = 1000.0, w;
	extern float	bolton(), escalc(), raf_thetae();

	printf ("Bolton: %.2f\n", bolton (p, t, dp));
	printf ("RAF: %.2f\n", raf_thetae (p, t, dp, 0.0));
}
