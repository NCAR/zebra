/*
 * Contour.h -- Public contour interface.
 */

/*		Copyright (C) 1987,88,89,90,91 by UCAR
 *	University Corporation for Atmospheric Research
 *		   All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced
 * or used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular purpose.
 * UCAR does not indemnify any infringement of copyright, patent, or trademark
 * through use or modification of this software.  UCAR does not provide 
 * maintenance or updates for its software.
 */

/*
 * Public prototypes for the contouring interfaces
 */
extern void FC_Init FP((XColor *colors, int count, int center, XColor *out,
			XRectangle clip, int flagged, double flagval));
extern void FillContour FP((Widget w, Drawable d, float *array, int xdim, 
			    int ydim, int xlo, int ylo, int xhi, int yhi, 
			    double ccenter, double cstep));
extern void CO_Init FP((XColor *colors, int count, int center, XColor *out,
			XRectangle clip, int flagged, double flagval));
extern void CO_InitMono FP((XColor color, XRectangle clip, int flagged, 
			    double flagval));
extern void Contour FP((Widget w, Drawable d, float *array, int xdim, int ydim,
			int xlo, int ylo, int xhi, int yhi, double ccenter, 
			double cstep, int dolabels, int linewidth));

