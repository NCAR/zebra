/*
 * draw_text definitions
 */
# define JustifyCenter		0
# define JustifyBottom		1
# define JustifyTop		2
# define JustifyBaseline	3
# define JustifyLeft		4
# define JustifyRight		5

# ifdef __STDC__
	void	DrawText (Widget, Drawable, GC, int, int, char *, double, 
			double, int, int);
	void	DT_StrokeText (Widget, Drawable, GC, int, int, char *, 
			double, double, int, int);
	void	DT_TextBox (Widget, Drawable, int, int, char *, double, 
			double, int, int, int *, int *, int *, int *);
# else
	void	DrawText (), DT_StrokeText (), DT_TextBox ();
# endif
