# include <math.h>

main ()
{
	float	lat, lon;

	cvt_Origin (-12.4572222, 130.9252778);
	
	cvt_ToLatLon (81.4126, -50.8722, &lat, &lon);
	printf ("{\"Anna\", \"ann\", %.4f, %.4f},\n", lat, lon);

	cvt_ToLatLon (10.4811, -66.1751, &lat, &lon);
	printf ("{\"Batc\", \"bat\", %.4f, %.4f},\n", lat, lon);

	cvt_ToLatLon (-33.7591, 75.8243, &lat, &lon);
	printf ("{\"Bath\", \"bah\", %.4f, %.4f},\n", lat, lon);

	cvt_ToLatLon (-4.7319, -33.6691, &lat, &lon);
	printf ("{\"Bell\", \"bel\", %.4f, %.4f},\n", lat, lon);

	cvt_ToLatLon (0.0000, 0.0000, &lat, &lon);
	printf ("{\"Berr\", \"ber\", %.4f, %.4f},\n", lat, lon);

	cvt_ToLatLon (-87.6911, -78.9574, &lat, &lon);
	printf ("{\"Chan\", \"chp\", %.4f, %.4f},\n", lat, lon);

	cvt_ToLatLon (-32.6788, 4.5927, &lat, &lon);
	printf ("{\"Char\", \"car\", %.4f, %.4f},\n", lat, lon);

	cvt_ToLatLon (-59.5677, -20.5108, &lat, &lon);
	printf ("{\"Dum \", \"dum\", %.4f, %.4f},\n", lat, lon);

	cvt_ToLatLon (-54.9404, 117.8200, &lat, &lon);
	printf ("{\"Gard\", \"gap\", %.4f, %.4f},\n", lat, lon);

	cvt_ToLatLon (48.5000, -84.0045, &lat, &lon);
	printf ("{\"Good\", \"gom\", %.4f, %.4f},\n", lat, lon);

	cvt_ToLatLon (12.6757, 23.8396, &lat, &lon);
	printf ("{\"Gunn\", \"gup\", %.4f, %.4f},\n", lat, lon);

	cvt_ToLatLon (39.5817, -16.8014, &lat, &lon);
	printf ("{\"Hump\", \"hum\", %.4f, %.4f},\n", lat, lon);

	cvt_ToLatLon (27.0459, 7.2469, &lat, &lon);
	printf ("{\"Kool\", \"kol\", %.4f, %.4f},\n", lat, lon);

	cvt_ToLatLon (-47.3836, -72.9643, &lat, &lon);
	printf ("{\"La B\", \"lab\", %.4f, %.4f},\n", lat, lon);

	cvt_ToLatLon (-48.4017, -108.7119, &lat, &lon);
	printf ("{\"Litc\", \"lit\", %.4f, %.4f},\n", lat, lon);

	cvt_ToLatLon (-17.9315, 1.5688, &lat, &lon);
	printf ("{\"Mand\", \"man\", %.4f, %.4f},\n", lat, lon);

	cvt_ToLatLon (16.6178, -9.2114, &lat, &lon);
	printf ("{\"McMi\", \"mil\", %.4f, %.4f},\n", lat, lon);

	cvt_ToLatLon (23.0349, -85.9674, &lat, &lon);
	printf ("{\"Mt. \", \"mbu\", %.4f, %.4f},\n", lat, lon);

	cvt_ToLatLon (96.2770, 11.8213, &lat, &lon);
	printf ("{\"Old \", \"ops\", %.4f, %.4f},\n", lat, lon);

	cvt_ToLatLon (-5.3712, 76.8214, &lat, &lon);
	printf ("{\"Pick\", \"pic\", %.4f, %.4f},\n", lat, lon);

	cvt_ToLatLon (89.8796, -14.2355, &lat, &lon);
	printf ("{\"Poin\", \"pst\", %.4f, %.4f},\n", lat, lon);

	cvt_ToLatLon (-28.5468, 114.4949, &lat, &lon);
	printf ("{\"Snak\", \"sbo\", %.4f, %.4f},\n", lat, lon);

	cvt_ToLatLon (58.2736, 9.2296, &lat, &lon);
	printf ("{\"Wool\", \"wol\", %.4f, %.4f},\n", lat, lon);
}
