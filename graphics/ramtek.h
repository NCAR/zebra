/*	10/84 jc	*/
/*
 * RAMTEK instruction definitions.
 */
# define	RM_I_ADLR	0x4700 /* Add display list registers */
# define	RM_I_ALCON	0x5c00 /* Allocate context */
# define	RM_I_ALDL	0x3b00 /* Allocate display list */
# define	RM_I_ALPF	0x4c00 /* Allocate programmable font */
# define	RM_I_ANDDLR	0x8700 /* AND display list registers */
# define	RM_I_ATTPF	0x4e00 /* Attach programmable font */
# define	RM_I_ARC1	0x6e00 /* Write arc type 1 */
# define	RM_I_ARC2	0x6f00 /* Write arc type 2 */
# define	RM_I_ARC3	0x7000 /* Write arc type 3 */

# define	RM_I_BERS	0x2b00 /* Bulk erase */

# define	RM_I_CCM	0x1d00 /* Call control memory */
# define	RM_I_CDL	0x4100 /* Call display list */
# define	RM_I_CIRC	0x6d00 /* Write circle */
# define	RM_I_CKES	0x7700 /* Clear keyboard echo state */
# define	RM_I_CLCF	0x7500 /* Clear local cursor function */
# define	RM_I_CLKF	0x7300 /* Clear local keyboard function */
# define	RM_I_COMBI	0x6b00 /* Combine image */
# define	RM_I_COMBT	0x6c00 /* Combine image triggered */
# define	RM_I_COPY	0x6900 /* Copy image */
# define	RM_I_COPYT	0x6a00 /* Copy image triggered */
# define	RM_I_CWC	0x3200 /* Center window at cursor */

# define	RM_I_DCVC	0x3400 /* Define cursor/video configuration */
# define	RM_I_DD		0x6300 /* Disable detect */
# define	RM_I_DDCC	0x3300 /* Define device/cursor configuration */
# define	RM_I_DDLR	0x4400 /* Decrement display list register */
# define	RM_I_DECON	0x5d00 /* Deallocate context */
# define	RM_I_DEDL	0x3c00 /* Deallocate display list */
# define	RM_I_DEPF	0x4d00 /* Deallocate programmable font */
# define	RM_I_DIVDLR	0x8500 /* Divide display list registers */

# define	RM_I_ED		0x6200 /* Enable detect */
# define	RM_I_ERS	0x0900 /* Erase */

# define	RM_I_FILL	0x3a00 /* Fill */

# define	RM_I_IDLR	0x4500 /* Increment display list register */
# define	RM_I_IM		0x5700 /* Initialize matrix */
# define	RM_I_INIT	0x0600 /* Initialize */
# define	RM_I_INOP	0x0700 /* No operation */

# define	RM_I_JDLR	0x4b00 /* Jump conditional upon display list register */

# define	RM_I_LAM	0x0300 /* Load auxiliary memory */
# define	RM_I_LCF	0x3000 /* Load cursor font */
# define	RM_I_LCCIO	0x3100 /* Load cursor to COP/index/origin */
# define	RM_I_LDL	0x1b00 /* Load display list */
# define	RM_I_LDLR	0x4800 /* Load display list register */
# define	RM_I_LDLRP	0x1f00 /* Load display list reverse packing */
# define	RM_I_LM		0x5300 /* Load current matrix */
# define	RM_I_LMPF	0x4f00 /* Load multiple programmable fonts */
# define	RM_I_LOAD	0x0100 /* Load hard register */
# define	RM_I_LOADSO	0x2800 /* Load subchannel origins */
# define	RM_I_LPF	0x1500 /* Load programmable font */
# define	RM_I_LPFRP	0x2100 /* Load programmable font reverse packing */

# define	RM_I_MM		0x5500 /* Multiply matrices */
# define	RM_I_MMI	0x5600 /* Multiply matrices immediate */
# define	RM_I_MULDLR	0x8600 /* Multiply display list registers */

# define	RM_I_ORDLR	0x8800 /* OR display list registers */

# define	RM_I_PLDLR	0x4a00 /* Parameter load display list registers */
# define	RM_I_POPE	0x1400 /* Restore environment */
# define	RM_I_POPM	0x5100 /* Pop matrix */
# define	RM_I_PUSHE	0x1300 /* Save environment */
# define	RM_I_PUSHM	0x5000 /* Push matrix */

# define	RM_I_RAM	0x0400 /* Read auxiliary memory */
# define	RM_I_RCON	0x6000 /* Read context associations */
# define	RM_I_RCSG	0x2f00 /* Read cursor status global */
# define	RM_I_RCSL	0x8300 /* Read cursor status local */
# define	RM_I_RCSP	0x2e00 /* Read cursor status pixel */
# define	RM_I_RCSS	0x1700 /* Read cursor status screen */
# define	RM_I_RD		0x6500 /* Resume detect */
# define	RM_I_RDB	0x6800 /* Read back detect buffer */
# define	RM_I_RDL	0x1c00 /* Read display list */
# define	RM_I_RDLRP	0x2000 /* Read display list reverse packing */
# define	RM_I_READ	0x0200 /* Read soft register */
# define	RM_I_READAS	0x5f00 /* Read allocation state */
# define	RM_I_READM	0x5b00 /* Read matrix */
# define	RM_I_READP	0x2400 /* Read normal parameters */
# define	RM_I_RERR	0x2500 /* Read error status */
# define	RM_I_RESCON	0x3d00 /* Reset context */
# define	RM_I_RETDL	0x4200 /* Return from display list */
# define	RM_I_RI		0x0b00 /* Read image */
# define	RM_I_RKB	0x1900 /* Read keyboard */
# define	RM_I_ROTAT	0x5a00 /* Rotate matrix */
# define	RM_I_RSET	0x0500 /* Reset */
# define	RM_I_RTC	0x4000 /* Read tablet coordinates */
# define	RM_I_RTCS	0x8c00 /* Read tablet coordinates and status */

# define	RM_I_SCALE	0x5800 /* Scale matrix */
# define	RM_I_SCON	0x5e00 /* Select context */
# define	RM_I_SCRX	0x1100 /* Scroll X */
# define	RM_I_SCRY	0x1200 /* Scroll Y */
# define	RM_I_SCW	0x8100 /* Set cursor window */
# define	RM_I_SD		0x6400 /* Suspend detect */
# define	RM_I_SDD	0x6600 /* Set detect data */
# define	RM_I_SDLR	0x4300 /* Set display list register */
# define	RM_I_SDP	0x6100 /* Set detect parameters */
# define	RM_I_SDS	0x6700 /* Students for a democratic society */
# define	RM_I_SELMG	0x2200 /* Select MCP/Group */
# define	RM_I_SELVO	0x2700 /* Select video origin */
# define	RM_I_SET	0x0800 /* Set parameter */
# define	RM_I_SETDC	0x2300 /* Set display class ranges */
# define	RM_I_SETM	0x5200 /* Set matrix */
# define	RM_I_SKES	0x7600 /* Set keyboard echo state */
# define	RM_I_SLFS	0x7100 /* Set local function state */
# define	RM_I_SLCF	0x7400 /* Set local cursor function */
# define	RM_I_SLKF	0x7200 /* Set local keyboard function */
# define	RM_I_STM	0x8b00 /* Set tablet mode */
# define	RM_I_SM		0x5400 /* Store current matrix */
# define	RM_I_SPS	0x1a00 /* Sense peripheral status */
# define	RM_I_STDLR	0x4900 /* Store display list register */
# define	RM_I_SUBDLR	0x4600 /* Subtract display list registers */

# define	RM_I_TRANS	0x5900 /* Translate matrix */

# define	RM_I_WAITL	0x2a00 /* Wait for video line */
# define	RM_I_WAITVR	0x2900 /* Wait for vertical retrace */
# define	RM_I_WBLK	0x3e00 /* Write keyboard block */
# define	RM_I_WBLKRP	0x3f00 /* Write keyboard block reverse packing */
# define	RM_I_WC		0x0f00 /* Write conic */
# define	RM_I_WC32	0x8a00 /* Write conic 32 bits */
# define	RM_I_WCSG	0x2d00 /* Write cursor state global */
# define	RM_I_WCSL	0x8400 /* Write cursor state local */
# define	RM_I_WCSP	0x2c00 /* Write cursor state pixel */
# define	RM_I_WCSS	0x1600 /* Write cursor state screen */
# define	RM_I_WCVU	0x8d00 /* Write colored vector unlinked */
# define	RM_I_WI		0x0a00 /* Write image */
# define	RM_I_WIV	0x8000 /* Write image vectors */
# define	RM_I_WKB	0x1800 /* Write keyboard */
# define	RM_I_WPB	0x1000 /* Write plot box */
# define	RM_I_WPI	0x7f00 /* Write packed image */
# define	RM_I_WPP	0x3600 /* Write plot point */
# define	RM_I_WPT	0x3800 /* Write point */
# define	RM_I_WPV	0x3700 /* Write plot vector */
# define	RM_I_WR		0x0d00 /* Write rastor */
# define	RM_I_WRCT	0x8200 /* Write random colored text */
# define	RM_I_WRP	0x3900 /* Write random pixel */
# define	RM_I_WT		0x0c00 /* Write text */
# define	RM_I_WVL	0x0e00 /* Write vector linked */
# define	RM_I_WVU	0x3500 /* Write vector unlinked */

# define	RM_I_XIM	0x1e00 /* Execute instruction memory */
# define	RM_I_XORDLR	0x8900 /* XOR display list registers */

# define	RM_I_ZOOM	0x2600 /* Zoom */
/*
 * Flags for normal format instructions.
 */
# define	RM_F_DF		0x0001	/* Data flag */
# define	RM_F_OF1	0x0002	/* Operand flag word 1 */
# define	RM_F_OF2	0x0004	/* Operand flag word 2 */
# define	RM_F_RP		0x0008	/* Reverse packing flag */
# define	RM_F_BK		0x0010	/* Reverse background flag */
# define	RM_F_AD		0x0020	/* Additive write */
# define	RM_F_AIX1	0x0040	/* Index reg. 1 addressing */
# define	RM_F_AIX2  	0x0080	/* Index reg. 2 addressing */
# define	RM_F_AREL	0x00c0	/* Relative addressing */
/*
 * Operand flag word 1
 */
# define	RM_F1_WMSK	0x0001	/* Write mask */
# define	RM_F1_FGD	0x0002	/* Foreground */
# define	RM_F1_BGD	0x0004	/* Background */
# define	RM_F1_IX1	0x0008	/* Index 1 */
# define	RM_F1_IX2	0x0010	/* Index 2 */
# define	RM_F1_ORG	0x0020	/* Origin */
# define	RM_F1_WIN	0x0040	/* Window */
# define	RM_F1_SCN	0x0080	/* Scan */
# define	RM_F1_DIM	0x0100	/* Dimensions */
# define	RM_F1_SPC	0x0200	/* Spacing */
# define	RM_F1_SIZ	0x0400	/* Size */
# define	RM_F1_LAF	0x0800	/* Logical/arithmetic function */
# define	RM_F1_CON	0x1000	/* Conic equation */
# define	RM_F1_BAS	0x2000	/* Baseline */
# define	RM_F1_SCR	0x4000	/* Scroll count */
# define	RM_F1_COP	0x8000	/* Start point */
/*
 * Operand flag word 2.
 */
# define	RM_F2_RMSK	0x0001	/* Read mask */
# define	RM_F2_WEW	0x0002	/* Write enable window */
# define	RM_F2_WOF	0x0004	/* Write enable window offset */
# define	RM_F2_VTX	0x0008	/* Vector texture */
# define	RM_F2_DCL	0x0010  /* Display class */
# define	RM_F2_IMG	0x0020	/* Image mode */
