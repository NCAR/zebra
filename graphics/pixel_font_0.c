/*  7/87 jc */
/*
 * This file is a hardwired, minimum-resolution pixel font.
 */
# include "param.h"

/*
 * The same old font directory structure.
 */
struct font_dir
{
	short	fd_pheight;	/* Character pixel height	*/
	short	fd_pwidth;	/* Character pixel width	*/
	short	fd_ref_y;	/* Reference point Y coordinate */
	short	fd_ref_x;	/* Reference point X coordinate */
	int	fd_rastor;	/* Offset to raster info	*/
	int	fd_tfm_width;	/* The TFM width of a character */
};


/*
 * The individual rastor info.  Yes, believe it or not, I actually typed this
 * whole damn thing in.  My fingers hurt.
 */
int Pf0_rasters[] = {
/* "A" -- 5 x 7 */
# define A_RAST 0
		        0x20,
			0x50,
			0x88,
			0x88,
			0xf8,
			0x88,
			0x88,

/* "B" -- 5 x 7 */
# define B_RAST A_RAST + 7
		        0xf0,
			0x48,
			0x48,
			0x70,
			0x48,
			0x48,
			0xf0,

/* "C" -- 4 x 7 */
# define C_RAST B_RAST + 7
		        0x30,
			0x48,
			0x40,
			0x40,
			0x40,
			0x48,
			0x30,

/* "D" -- 5 x 7 */
# define D_RAST C_RAST + 7
		        0xf0,
			0x48,
			0x48,
			0x48,
			0x48,
			0x48,
			0xf0,

/* "E" -- 4 x 7 */
# define E_RAST D_RAST + 7
		        0xf0,
			0x80,
			0x80,
			0xe0,
			0x80,
			0x80,
			0xf0,

/* "F" -- 4 x 7 */
# define F_RAST E_RAST + 7
		        0xf0,
			0x80,
			0x80,
			0xe0,
			0x80,
			0x80,
			0x80,

/* "G" -- 5 x 7 */
# define G_RAST F_RAST + 7
		        0x30,
			0x48,
			0x80,
			0x98,
			0x88,
			0x48,
			0x30,

/* "H" -- 5 x 7 */
# define H_RAST G_RAST + 7
		        0x88,
			0x88,
			0x88,
			0xf8,
			0x88,
			0x88,
			0x88,

/* "I" -- 3 x 7 */
# define I_RAST H_RAST + 7
		        0xe0,
			0x40,
			0x40,
			0x40,
			0x40,
			0x40,
			0xe0,

/* "J" -- 5 x 7 */
# define J_RAST I_RAST + 7
		        0x38,
			0x10,
			0x10,
			0x10,
			0x10,
			0x90,
			0x60,

/* "K" -- 5 x 7 */
# define K_RAST J_RAST + 7
		        0x88,
			0x90,
			0xa0,
			0xc0,
			0xa0,
			0x90,
			0x88,

/* "L" -- 5 x 7 */
# define L_RAST K_RAST + 7
		        0x80,
			0x80,
			0x80,
			0x80,
			0x80,
			0x80,
			0xf8,

/* "M" -- 7 x 7 */
# define M_RAST L_RAST + 7
		        0x82,
			0xc6,
			0xaa,
			0x92,
			0x82,
			0x82,
			0x82,

/* "N" -- 6 x 7 */
# define N_RAST M_RAST + 7
		        0x84,
			0xc4,
			0xa4,
			0x94,
			0x8c,
			0x84,
			0x84,

/* "O" -- 5 x 7 */
# define O_RAST N_RAST + 7
		        0x70,
			0x88,
			0x88,
			0x88,
			0x88,
			0x88,
			0x70,

/* "P" -- 5 x 7 */
# define P_RAST O_RAST + 7
		        0xf0,
			0x88,
			0x88,
			0xf0,
			0x80,
			0x80,
			0x80,

/* "Q" -- 6 x 7 */
# define Q_RAST P_RAST + 7
		        0x78,
			0x84,
			0x84,
			0x84,
			0x84,
			0x88,
			0x74,

/* "R" -- 5 x 7 */
# define R_RAST Q_RAST + 7
		        0xf0,
			0x88,
			0x88,
			0xf0,
			0xa0,
			0x90,
			0x88,

/* "S" -- 5 x 7 */
# define S_RAST R_RAST + 7
		        0x70,
			0x88,
			0x80,
			0x70,
			0x08,
			0x88,
			0x70,

/* "T" -- 5 x 7 */
# define T_RAST S_RAST + 7
		        0xf8,
			0x20,
			0x20,
			0x20,
			0x20,
			0x20,
			0x20,

/* "U" -- 5 x 7 */
# define U_RAST T_RAST + 7
		        0x88,
			0x88,
			0x88,
			0x88,
			0x88,
			0x88,
			0x70,

/* "V" -- 5 x 7 */
# define V_RAST U_RAST + 7
		        0x88,
			0x88,
			0x88,
			0x88,
			0x88,
			0x50,
			0x20,

/* "W" -- 7 x 7 */
# define W_RAST V_RAST + 7
		        0x82,
			0x82,
			0x82,
			0x92,
			0x92,
			0xaa,
			0x44,

/* "X" -- 7 x 7 */
# define X_RAST W_RAST + 7
		        0x82,
			0x44,
			0x28,
			0x10,
			0x28,
			0x44,
			0x82,

/* "Y" -- 7 x 7 */
# define Y_RAST X_RAST + 7
		        0x82,
			0x44,
			0x28,
			0x10,
			0x10,
			0x10,
			0x10,

/* "Z" == 5 x 7 */
/* # define Z_RAST Y_RAST + 7 */
# define Z_RAST 175
		        0xf8,
			0x08,
			0x10,
			0x20,
			0x40,
			0x80,
			0xf8,

/* "!" -- 1 x 7 */
# define EXP_RAST Z_RAST + 7
			0x80,
			0x80,
			0x80,
			0x80,
			0x80,
			0x00,
			0x80,

/* """ -- 3 x 2 */
# define DQ_RAST EXP_RAST + 7
			0xa0,
			0xa0,

/* "#" -- 6 x 7 */
# define SHARP_RAST DQ_RAST + 2
			0x48,
			0x48,
			0xfc,
			0x48,
			0xfc,
			0x48,
			0x48,

/* "$" -- 5 x 7 */
# define DOL_RAST SHARP_RAST + 7
			0x20,
			0x70,
			0xa0,
			0x70,
			0x28,
			0x70,
			0x20,

/* "%" -- 7 x 7 */
# define PCT_RAST DOL_RAST + 7
			0x42,
			0xa4,
			0x48,
			0x10,
			0x24,
			0x4a,
			0x84,

/* "&" -- 6 x 7 */
# define AMP_RAST PCT_RAST + 7
			0x60,
			0x90,
			0x90,
			0x60,
			0x94,
			0x88,
			0x74,

/* "'" -- 1 x 2	*/
# define SQ_RAST AMP_RAST + 7
			0x80,
			0x80,

/* "(" -- 3 x 7 */
# define OP_RAST SQ_RAST + 2
			0x20,
			0x40,
			0x80,
			0x80,
			0x80,
			0x40,
			0x20,

/* ")" -- 3 x 7 */
# define CP_RAST OP_RAST + 7
			0x80,
			0x40,
			0x20,
			0x20,
			0x20,
			0x40,
			0x80,

/* "*" -- 6 x 5 */
# define STAR_RAST CP_RAST + 7
			0x48,
			0x30,
			0xfc,
			0x30,
			0x48,

/* "+" 5 x 5 */
# define PLUS_RAST STAR_RAST + 5
			0x20,
			0x20,
			0xf8,
			0x20,
			0x20,

/* "," -- 2 x 2 x -2 */
# define COMMA_RAST PLUS_RAST + 5
			0xc0,
			0xc0,
			0x40,
			0x80,

/* "-" -- 5 x 1 */
# define DASH_RAST COMMA_RAST + 4
			0xf8,

/* "." -- 2 x 2 */
# define DOT_RAST DASH_RAST + 1
			0xc0,
			0xc0,

/* "/" -- 7 x 7 */
# define SLASH_RAST DOT_RAST + 2
			0x02,
			0x04,
			0x08,
			0x10,
			0x20,
			0x40,
			0x80,

/* "0" -- 5 x 7 */
# define ZERO_RAST SLASH_RAST + 7
			0x70,
			0x88,
			0x98,
			0xa8,
			0xc8,
			0x88,
			0x70,

/* "1" -- 3 x 7 */
# define ONE_RAST ZERO_RAST + 7
			0x40,
			0xc0,
			0x40,
			0x40,
			0x40,
			0x40,
			0xe0,

/* "2" -- 5 x 7 */
# define TWO_RAST ONE_RAST + 7
			0x70,
			0x88,
			0x08,
			0x70,
			0x80,
			0x80,
			0xf8,

/* "3" -- 5 x 7 */
# define THREE_RAST TWO_RAST + 7
			0x70,
			0x88,
			0x08,
			0x70,
			0x08,
			0x88,
			0x70,

/* "4" -- 5 x 7 */
# define FOUR_RAST THREE_RAST + 7
			0x10,
			0x30,
			0x50,
			0x90,
			0xf8,
			0x10,
			0x10,

/* "5" -- 5 x 7 */
# define FIVE_RAST FOUR_RAST + 7
			0xf8,
			0x80,
			0xf0,
			0x08,
			0x08,
			0x88,
			0x70,

/* "6" -- 5 x 7 */
# define SIX_RAST FIVE_RAST + 7
			0x30,
			0x40,
			0x80,
			0xf0,
			0x88,
			0x88,
			0x70,

/* "7" -- 5 x 7 */
# define SEVEN_RAST SIX_RAST + 7
			0xf8,
			0x88,
			0x10,
			0x20,
			0x40,
			0x40,
			0x40,

/* "8" -- 5 x 7 */
# define EIGHT_RAST SEVEN_RAST + 7
			0x70,
			0x88,
			0x88,
			0x70,
			0x88,
			0x88,
			0x70,

/* "9" -- 5 x 7 */
# define NINE_RAST EIGHT_RAST + 7
			0x70,
			0x88,
			0x88,
			0x78,
			0x08,
			0x08,
			0x70,

/* ":" -- 2 x 6 */
# define COLON_RAST NINE_RAST + 7
			0xc0,
			0xc0,
			0x00,
			0x00,
			0xc0,
			0xc0,

/* ";" -- 2 x 6 x -2 */
# define SEMI_RAST COLON_RAST + 6
			0xc0,
			0xc0,
			0x00,
			0x00,
			0xc0,
			0xc0,
			0x40,
			0x80,

/* "<" -- 4 x 7 */
# define LT_RAST SEMI_RAST + 8
			0x10,
			0x20,
			0x40,
			0x80,
			0x40,
			0x20,
			0x10,

/* "=" -- 6 x 3 */
# define EQUAL_RAST LT_RAST + 7
			0xfc,
			0x00,
			0xfc,

/* ">" -- 4 x 7 */
# define GT_RAST EQUAL_RAST + 3
			0x80,
			0x40,
			0x20,
			0x10,
			0x20,
			0x40,
			0x80,

/* "?" 5 x 7 x -1 */
# define QUEST_RAST GT_RAST + 7
			0x70,
			0x88,
			0x08,
			0x30,
			0x20,
			0x20,
			0x00,
			0x20,

/* "@" -- 8 x 7 */
# define AT_RAST QUEST_RAST + 8
			0x38,
			0x45,
			0x9d,
			0xa5,
			0x9d,
			0x42,
			0x3c,

/* "[" -- 3 x 7 */
# define OSB_RAST AT_RAST + 7
			0xe0,
			0x80,
			0x80,
			0x80,
			0x80,
			0x80,
			0xe0,

/* "\" -- 7 x 7 */
# define BS_RAST OSB_RAST + 7
			0x80,
			0x40,
			0x20,
			0x10,
			0x08,
			0x04,
			0x02,

/* "]" -- 3 x 7 */
# define CSB_RAST BS_RAST + 7
			0xe0,
			0x20,
			0x20,
			0x20,
			0x20,
			0x20,
			0xe0,

/* "^" -- 5 x 3 */
# define CARAT_RAST CSB_RAST + 7
			0x20,
			0x50,
			0x88,

/* "_" -- 6 x 1 */
# define US_RAST CARAT_RAST + 3
			0xfc,

/* "`" -- 3 x 3 */
# define BPRIME_RAST US_RAST + 1
			0x80,
			0x40,
			0x20,

/* "a" -- 5 x 5 */
# define a_RAST BPRIME_RAST + 3
			0x70,
			0x08,
			0x78,
			0x88,
			0x78,

/* "b" -- 5 x 7 */
/* # define b_RAST a_RAST + 5 */
# define b_RAST 408
			0x80,
			0x80,
			0xb0,
			0xc8,
			0x88,
			0xc8,
			0xb0,

/* "c" -- 4 x 5 */
# define c_RAST b_RAST + 7
			0x70,
			0x80,
			0x80,
			0x80,
			0x70,

/* "d" -- 5 x 7 */
# define d_RAST c_RAST + 5
			0x08,
			0x08,
			0x68,
			0x98,
			0x88,
			0x98,
			0x68,

/* "e" -- 5 x 5 */
# define e_RAST d_RAST + 7
			0x70,
			0x88,
			0xf0,
			0x80,
			0x70,

/* "f" -- 4 x 7 */
# define f_RAST e_RAST + 5
			0x20,
			0x50,
			0x40,
			0xe0,
			0x40,
			0x40,
			0x40,

/* "g" -- 5 x 5 x -1 */
# define g_RAST f_RAST + 7
			0x70,
			0x88,
			0x88,
			0x78,
			0x08,
			0x78,

/* "h" -- 5 x 7 */
# define h_RAST g_RAST + 6
			0x80,
			0x80,
			0xb0,
			0xc8,
			0x88,
			0x88,
			0x88,

/* "i" -- 3 x 7 */
# define i_RAST h_RAST + 7
			0x40,
			0x00,
			0xc0,
			0x40,
			0x40,
			0x40,
			0xe0,

/* "j" -- 4 x 9 */
# define j_RAST i_RAST + 7
			0x10,
			0x00,
			0x10,
			0x10,
			0x10,
			0x10,
			0x10,
			0x90,
			0x60,

/* "k" -- 5 x 7 */
# define k_RAST j_RAST + 9
			0x80,
			0x80,
			0x88,
			0x90,
			0xa0,
			0xd0,
			0x88,

/* "l" -- 3 x 7 */
# define l_RAST k_RAST + 7
			0x80,
			0x40,
			0x40,
			0x40,
			0x40,
			0x40,
			0x20,

/* "m" -- 5 x 5 */
# define m_RAST l_RAST + 7
			0xd0,
			0xa8,
			0xa8,
			0xa8,
			0x88,

/* "n" -- 5 x 5 */
# define n_RAST m_RAST + 5
			0xb0,
			0xc8,
			0x88,
			0x88,
			0x88,

/* "o" -- 5 x 5 */
# define o_RAST n_RAST + 5
			0x70,
			0x88,
			0x88,
			0x88,
			0x70,

/* "p" -- 5 x 4 x -3 */
# define p_RAST o_RAST + 5
			0xf0,
			0x88,
			0x88,
			0xf0,
			0x80,
			0x80,
			0x80,

/* "q" -- 5 x 4 x -3 */
# define q_RAST p_RAST + 7
			0x78,
			0x88,
			0x88,
			0x78,
			0x08,
			0x08,
			0x08,

/* "r" -- 5 x 5 */
# define r_RAST q_RAST + 7
			0xb0,
			0xc8,
			0x80,
			0x80,
			0x80,

/* "s" -- 5 x 5 */
# define s_RAST r_RAST + 5
			0x78,
			0x80,
			0x70,
			0x08,
			0xf0,

/* "t" -- 5 x 7 */
# define t_RAST s_RAST + 5
			0x40,
			0x40,
			0xe0,
			0x40,
			0x40,
			0x48,
			0x30,

/* "u" -- 5 x 5 */
# define u_RAST t_RAST + 7
			0x88,
			0x88,
			0x88,
			0x98,
			0x68,

/* "v" -- 5 x 5 */
# define v_RAST u_RAST + 5
			0x88,
			0x88,
			0x88,
			0x50,
			0x20,

/* "w" -- 7 x 5 */
# define w_RAST v_RAST + 5
			0x82,
			0x82,
			0x92,
			0x92,
			0x6c,

/* "x" -- 5 x 5 */
# define x_RAST w_RAST + 5
			0x88,
			0x50,
			0x20,
			0x50,
			0x88,

/* "y" -- 5 x 4 x -3 */
# define y_RAST x_RAST + 5
			0x88,
			0x88,
			0x88,
			0x78,
			0x08,
			0x88,
			0x70,

/* "z" -- 5 x 5 */
# define z_RAST y_RAST + 7
			0xf8,
			0x10,
			0x20,
			0x40,
			0xf8,

/* "{" -- 4 x 7 */
# define OB_RAST z_RAST + 5
			0x03,
			0x40,
			0x40,
			0x80,
			0x40,
			0x40,
			0x03,

/* "|" -- 1 x 7 */
# define VB_RAST OB_RAST + 7
			0x80,
			0x80,
			0x80,
			0x80,
			0x80,
			0x80,
			0x80,

/* "}" -- 4 x 7 */
# define CB_RAST VB_RAST + 7
			0xc0,
			0x20,
			0x20,
			0x10,
			0x20,
			0x20,
			0xc0,

/* "~" -- 7 x 3 */
# define TILDE_RAST CB_RAST + 7
			0x60,
			0x92,
			0x0c
};



/*
 * Here is the actual font directory.
 */
struct font_dir Pf0_dir[128] =
{
/*	 hgt	width	refy	refx	    rastor	junk		*/
	{ ___,	___,	___,	___,		___,	___	}, /* NULL */
	{ ___,	___,	___,	___,		___,	___	}, /* ^A */
	{ ___,	___,	___,	___,		___,	___	}, /* ^B */
	{ ___,	___,	___,	___,		___,	___	}, /* ^C */
	{ ___,	___,	___,	___,		___,	___	}, /* ^D */
	{ ___,	___,	___,	___,		___,	___	}, /* ^E */
	{ ___,	___,	___,	___,		___,	___	}, /* ^F */
	{ ___,	___,	___,	___,		___,	___	}, /* ^G */
	{ ___,	___,	___,	___,		___,	___	}, /* ^H */
	{ ___,	___,	___,	___,		___,	___	}, /* ^I */
	{ ___,	___,	___,	___,		___,	___	}, /* ^J */
	{ ___,	___,	___,	___,		___,	___	}, /* ^K */
	{ ___,	___,	___,	___,		___,	___	}, /* ^L */
	{ ___,	___,	___,	___,		___,	___	}, /* ^M */
	{ ___,	___,	___,	___,		___,	___	}, /* ^N */
	{ ___,	___,	___,	___,		___,	___	}, /* ^O */
	{ ___,	___,	___,	___,		___,	___	}, /* ^P */
	{ ___,	___,	___,	___,		___,	___	}, /* ^Q */
	{ ___,	___,	___,	___,		___,	___	}, /* ^R */
	{ ___,	___,	___,	___,		___,	___	}, /* ^S */
	{ ___,	___,	___,	___,		___,	___	}, /* ^T */
	{ ___,	___,	___,	___,		___,	___	}, /* ^U */
	{ ___,	___,	___,	___,		___,	___	}, /* ^V */
	{ ___,	___,	___,	___,		___,	___	}, /* ^W */
	{ ___,	___,	___,	___,		___,	___	}, /* ^X */
	{ ___,	___,	___,	___,		___,	___	}, /* ^Y */
	{ ___,	___,	___,	___,		___,	___	}, /* ^Z */
	{ ___,	___,	___,	___,		___,	___	}, /* ESC */
	{ ___,	___,	___,	___,		___,	___	}, /* FS */
	{ ___,	___,	___,	___,		___,	___	}, /* GS */
	{ ___,	___,	___,	___,		___,	___	}, /* RS */
	{ ___,	___,	___,	___,		___,	___	}, /* US */
	{ ___,	___,	___,	___,		___,	___	}, /* SPC */
	{   7,	  1,	  6,	 -1,	   EXP_RAST,	___	}, /* ! */
	{   2,	  3,	  6,	 -1,	    DQ_RAST,	___	}, /* "" */
	{   7,	  6,	  6,	 -1,	 SHARP_RAST,	___	}, /* # */
	{   7,	  5,	  6,	 -1,	   DOL_RAST,	___	}, /* $ */
	{   7,	  7,	  6,	 -1,	   PCT_RAST,	___	}, /* % */
	{   7,	  6,	  6,	 -1,	   AMP_RAST,	___	}, /* & */
	{   2,	  1,	  6,	 -1,	    SQ_RAST,	___	}, /* ' */
	{   7,	  3,	  6,	 -1,	    OP_RAST,	___	}, /* ( */
	{   7,	  3,	  6,	 -1,	    CP_RAST,	___	}, /* ) */
	{   5,	  6,	  5,	 -1,	  STAR_RAST,	___	}, /* * */
	{   5,	  5,	  5,	 -1,	  PLUS_RAST,	___	}, /* + */
	{   4,	  2,	  1,	 -1,	 COMMA_RAST,	___	}, /* , */
	{   1,	  5,	  3,	 -1,	  DASH_RAST,	___	}, /* - */
	{   2,	  2,	  1,	 -1,	   DOT_RAST,	___	}, /* . */
	{   7,	  7,	  6,	 -1,	 SLASH_RAST,	___	}, /* / */
	{   7,	  5,	  6,	 -1,	  ZERO_RAST,	___	}, /* 0 */
	{   7,	  3,	  6,	 -1,	   ONE_RAST,	___	}, /* 1 */
	{   7,	  5,	  6,	 -1,	   TWO_RAST,	___	}, /* 2 */
	{   7,	  5,	  6,	 -1,	 THREE_RAST,	___	}, /* 3 */
	{   7,	  5,	  6,	 -1,	  FOUR_RAST,	___	}, /* 4 */
	{   7,	  5,	  6,	 -1,	  FIVE_RAST,	___	}, /* 5 */
	{   7,	  5,	  6,	 -1,	   SIX_RAST,	___	}, /* 6 */
	{   7,	  5,	  6,	 -1,	 SEVEN_RAST,	___	}, /* 7 */
	{   7,	  5,	  6,	 -1,	 EIGHT_RAST,	___	}, /* 8 */
	{   7,	  5,	  6,	 -1,	  NINE_RAST,	___	}, /* 9 */
	{   6,	  2,	  5,	 -1,	 COLON_RAST,	___	}, /* : */
	{   8,	  2,	  5,	 -1,	  SEMI_RAST,	___	}, /* ; */
	{   7,	  4,	  6,	 -1,	    LT_RAST,	___	}, /* < */
	{   3,	  6,	  4,	 -1,	 EQUAL_RAST,	___	}, /* = */
	{   7,	  4,	  6,	 -1,	    GT_RAST,	___	}, /* > */
	{   8,	  5,	  6,	 -1,	 QUEST_RAST,	___	}, /* ? */
	{   7,	  8,	  6,	 -1,	    AT_RAST,	___	}, /* SPC */
	{   7,    5,      6,     -1,	     A_RAST,    ___	}, /* A */
	{   7,    5,      6,     -1,	     B_RAST,    ___	}, /* B */
	{   7,    5,      6,     -1,	     C_RAST,    ___	}, /* C */
	{   7,    5,      6,     -1,	     D_RAST,    ___	}, /* D */
	{   7,    4,      6,     -1,	     E_RAST,    ___	}, /* E */
	{   7,    4,      6,     -1,	     F_RAST,    ___	}, /* F */
	{   7,    5,      6,     -1,	     G_RAST,    ___	}, /* G */
	{   7,    5,      6,     -1,	     H_RAST,    ___	}, /* H */
	{   7,    3,      6,     -1,	     I_RAST,    ___	}, /* I */
	{   7,    5,      6,     -1,	     J_RAST,    ___	}, /* J */
	{   7,    5,      6,     -1,	     K_RAST,    ___	}, /* K */
	{   7,    5,      6,     -1,	     L_RAST,    ___	}, /* L */
	{   7,    7,      6,     -1,	     M_RAST,    ___	}, /* M */
	{   7,    6,      6,     -1,	     N_RAST,    ___	}, /* N */
	{   7,    5,      6,     -1,	     O_RAST,    ___	}, /* O */
	{   7,    5,      6,     -1,	     P_RAST,    ___	}, /* P */
	{   7,    6,      6,     -1,	     Q_RAST,    ___	}, /* Q */
	{   7,    5,      6,     -1,	     R_RAST,    ___	}, /* R */
	{   7,    5,      6,     -1,	     S_RAST,    ___	}, /* S */
	{   7,    5,      6,     -1,	     T_RAST,    ___	}, /* T */
	{   7,    5,      6,     -1,	     U_RAST,    ___	}, /* U */
	{   7,    5,      6,     -1,	     V_RAST,    ___	}, /* V */
	{   7,    7,      6,     -1,	     W_RAST,    ___	}, /* W */
	{   7,    7,      6,     -1,	     X_RAST,    ___	}, /* X */
	{   7,    7,      6,     -1,	     Y_RAST,    ___	}, /* Y */
	{   7,    5,      6,     -1,	     Z_RAST,    ___	}, /* Z */
	{   7,    3,      6,     -1,	   OSB_RAST,    ___	}, /* [ */
	{   7,    7,      6,     -1,	    BS_RAST,    ___	}, /* \ */
	{   7,    3,      6,     -1,	   CSB_RAST,    ___	}, /* ] */
	{   3,    5,      6,     -1,	 CARAT_RAST,    ___	}, /* ^ */
	{   1,    6,      0,     -1,	    US_RAST,    ___	}, /* _ */
	{   3,    3,      6,     -1,	BPRIME_RAST,    ___	}, /* ` */
	{   5,    5,      4,     -1,	     a_RAST,    ___	}, /* a */
	{   7,    5,      6,     -1,	     b_RAST,    ___	}, /* b */
	{   5,    5,      4,     -1,	     c_RAST,    ___	}, /* c */
	{   7,    5,      6,     -1,	     d_RAST,    ___	}, /* d */
	{   5,    5,      4,     -1,	     e_RAST,    ___	}, /* e */
	{   7,    4,      6,     -1,	     f_RAST,    ___	}, /* f */
	{   6,    5,      4,     -1,	     g_RAST,    ___	}, /* g */
	{   7,    5,      6,     -1,	     h_RAST,    ___	}, /* h */
	{   7,    3,      6,     -1,	     i_RAST,    ___	}, /* i */
	{   9,    4,      6,     -1,	     j_RAST,    ___	}, /* j */
	{   7,    5,      6,     -1,	     k_RAST,    ___	}, /* k */
	{   7,    3,      6,     -1,	     l_RAST,    ___	}, /* l */
	{   5,    5,      4,     -1,	     m_RAST,    ___	}, /* m */
	{   5,    5,      4,     -1,	     n_RAST,    ___	}, /* n */
	{   5,    5,      4,     -1,	     o_RAST,    ___	}, /* o */
	{   7,    5,      4,     -1,	     p_RAST,    ___	}, /* p */
	{   7,    5,      4,     -1,	     q_RAST,    ___	}, /* q */
	{   5,    5,      4,     -1,	     r_RAST,    ___	}, /* r */
	{   5,    5,      4,     -1,	     s_RAST,    ___	}, /* s */
	{   7,    5,      6,     -1,	     t_RAST,    ___	}, /* t */
	{   5,    5,      4,     -1,	     u_RAST,    ___	}, /* u */
	{   5,    5,      4,     -1,	     v_RAST,    ___	}, /* v */
	{   5,    7,      4,     -1,	     w_RAST,    ___	}, /* w */
	{   5,    5,      4,     -1,	     x_RAST,    ___	}, /* x */
	{   7,    5,      4,     -1,	     y_RAST,    ___	}, /* y */
	{   5,    5,      4,     -1,	     z_RAST,    ___	}, /* z */
	{   7,    4,      6,     -1,	    OB_RAST,    ___	}, /* { */
	{   7,    1,      6,     -1,	    VB_RAST,    ___	}, /* | */
	{   7,    4,      6,     -1,	    CB_RAST,    ___	}, /* } */
	{   3,    7,      6,     -1,	 TILDE_RAST,    ___	}  /* ~ */
};
