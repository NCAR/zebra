/*
 * Skew-t plotting module
 */
static char *rcsid = "$Id: AxisControl.c,v 1.1 1991-10-30 19:25:07 barrett Exp $";
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
# include <config.h>


# include <math.h>
# include <X11/Intrinsic.h>
# include <ui.h>
# include <ui_error.h>
# include <defs.h>
# include <pd.h>
# include <message.h>
# include <DataStore.h>
# include "derive.h"
# include "GraphProc.h"
# include "GC.h"
# include "LayoutControl.h"
# include "DrawText.h"

/*
 * General definitions
 */

# define BADVAL		-999.0
# define AUTO_XMAX	(1<<0)
# define AUTO_YMAX	(1<<1)
# define AUTO_XMIN	(1<<2)
# define AUTO_YMIN	(1<<3)
# define ROUNDIT(x)	((int)(x + 0.5))

extern int	ac_PlotAxis();
extern int	ac_AddAxis();
extern XColor   Tadefclr;
extern int	TransConfig;
typedef enum {L_solid, L_dashed, L_dotted} LineStyle;


# define MAX_AXIS  5

typedef struct _AxisInfo
{
    float		min;
    float		max;
    char		*component;
    int			ticSize;
    float		fontScale;
    char		*axisTitle;
    int			ntic;
    int			changed;
    int			invert;
} AxisInfoRec;

static AxisInfoRec AxisInfoList[4][5];

void ac_TouchAxis ( axis,i)
int axis;
int i;
{
    AxisInfoList[axis][i].changed = 1;
}
int ac_DisplayAxes ( )
{
    int fit=1;
    int pixPos;
    int i;

    pixPos = 0;
    for ( i = 0; i < MAX_AXIS && fit; i++ )
    {
	if ( AxisInfoList[AXIS_BOTTOM][i].component )
	{
	   fprintf ( stdout, "\r bottom pixPos = %d\n", pixPos);
           fit = ac_PlotAxis ( AXIS_BOTTOM, 
		&pixPos, AxisInfoList[AXIS_BOTTOM][i].ticSize, 
		AxisInfoList[AXIS_BOTTOM][i].ntic, 
		AxisInfoList[AXIS_BOTTOM][i].axisTitle, 
		AxisInfoList[AXIS_BOTTOM][i].fontScale, 
		AxisInfoList[AXIS_BOTTOM][i].min,
		AxisInfoList[AXIS_BOTTOM][i].max,
		AxisInfoList[AXIS_BOTTOM][i].invert);
	   pixPos += 2;
	   if ( !fit )
	   {
	       msg_ELog ( EF_PROBLEM, "Not enough room for BOTTOM axes.");
	   }
	   AxisInfoList[AXIS_BOTTOM][i].changed = 0;
	}
    }
    pixPos = 0;
    fit = 1;
    for ( i = 0; i < MAX_AXIS && fit; i++ )
    {
	if ( AxisInfoList[AXIS_TOP][i].component )
	{
	   fprintf ( stdout, "\r top pixPos = %d\n", pixPos);
           fit = ac_PlotAxis ( AXIS_TOP, 
		&pixPos, AxisInfoList[AXIS_TOP][i].ticSize,
		AxisInfoList[AXIS_TOP][i].ntic, 
		AxisInfoList[AXIS_TOP][i].axisTitle, 
		AxisInfoList[AXIS_TOP][i].fontScale, 
		AxisInfoList[AXIS_TOP][i].min, 
		AxisInfoList[AXIS_TOP][i].max,
		AxisInfoList[AXIS_TOP][i].invert);
	   pixPos += 2;
	   if ( !fit )
	   {
	       msg_ELog ( EF_PROBLEM, "Not enough room for TOP axes.");
	   }
	   AxisInfoList[AXIS_TOP][i].changed = 0;
	}
    }
    pixPos = 0;
    fit = 1;
    for ( i = 0; i < MAX_AXIS && fit; i++ )
    {
	if ( AxisInfoList[AXIS_LEFT][i].component )
	{
	   fprintf ( stdout, "\r left pixPos = %d\n", pixPos);
           fit = ac_PlotAxis ( AXIS_LEFT, 
		&pixPos, AxisInfoList[AXIS_LEFT][i].ticSize, 
		AxisInfoList[AXIS_LEFT][i].ntic, 
		AxisInfoList[AXIS_LEFT][i].axisTitle, 
		AxisInfoList[AXIS_LEFT][i].fontScale, 
		AxisInfoList[AXIS_LEFT][i].min, 
		AxisInfoList[AXIS_LEFT][i].max,
		AxisInfoList[AXIS_LEFT][i].invert);
	   pixPos += 2;
	   if ( !fit )
	   {
	       msg_ELog ( EF_PROBLEM, "Not enough room for LEFT axes.");
	   }
	   AxisInfoList[AXIS_LEFT][i].changed = 0;
	}
    }
    pixPos = 0;
    fit = 1;
    for ( i = 0; i < MAX_AXIS && fit; i++ )
    {
	if ( AxisInfoList[AXIS_RIGHT][i].component )
	{
           fit = ac_PlotAxis ( AXIS_RIGHT, 
		&pixPos, AxisInfoList[AXIS_RIGHT][i].ticSize, 
		AxisInfoList[AXIS_RIGHT][i].ntic, 
		AxisInfoList[AXIS_RIGHT][i].axisTitle, 
		AxisInfoList[AXIS_RIGHT][i].fontScale, 
		AxisInfoList[AXIS_RIGHT][i].min, 
		AxisInfoList[AXIS_RIGHT][i].max,
		AxisInfoList[AXIS_RIGHT][i].invert);
	   pixPos += 2;
	   if ( !fit )
	   {
	       msg_ELog ( EF_PROBLEM, "Not enough room for RIGHT axes.");
	   }
	   AxisInfoList[AXIS_RIGHT][i].changed = 0;
	}
    }
}

int 
ac_ChangeAxisMax ( side, id, max )
int	side;
int	id;
float	max;
{
/*
 * Make sure the id is less than the maximum number allowed
 */
    if ( id >= MAX_AXIS )
    {
	msg_ELog( EF_PROBLEM, 
	    "Axis id (%d) out of range. Must be less than %d.",id,MAX_AXIS);
	return(0);
    }
    if ( AxisInfoList[side][id].component )
    {
	AxisInfoList[side][id].max = max;
	AxisInfoList[side][id].changed = 1;
    }
    else
    {
	msg_ELog( EF_PROBLEM, "Axis id (%d) has not been set", id);
	return(0);
    }
    return(id);
}

int 
ac_ChangeAxisMin ( side, id, min )
int	side;
int	id;
float	min;
{
/*
 * Make sure the id is less than the maximum number allowed
 */
    if ( id >= MAX_AXIS )
    {
	msg_ELog( EF_PROBLEM, 
	    "Axis id (%d) out of range. Must be less than %d.",id,MAX_AXIS);
	return(0);
    }
    if ( AxisInfoList[side][id].component )
    {
	AxisInfoList[side][id].min = min;
	AxisInfoList[side][id].changed = 1;
    }
    else
    {
	msg_ELog( EF_PROBLEM, "Axis id (%d) has not been set", id);
	return(0);
    }
    return(id);
}

int 
ac_ChangeAxisTitle ( side, id, newTitle )
int	side;
int	id;
char	*newTitle;
{
/*
 * Make sure the id is less than the maximum number allowed
 */
    if ( id >= MAX_AXIS )
    {
	msg_ELog( EF_PROBLEM, 
	    "Axis id (%d) out of range. Must be less than %d.",id,MAX_AXIS);
	return(0);
    }
    if ( AxisInfoList[side][id].component )
    {
	if ( AxisInfoList[side][id].axisTitle && newTitle )
	{
	    if ( strlen(AxisInfoList[side][id].axisTitle) <
		 strlen( newTitle )
	       )
	    {
		free ( AxisInfoList[side][id].axisTitle);
		AxisInfoList[side][id].axisTitle = 
		    (char*)malloc ( (strlen(newTitle)+1)* sizeof(char));
	    }
	    strcpy ( AxisInfoList[side][id].axisTitle, newTitle );
	}
	else if ( AxisInfoList[side][id].axisTitle && !newTitle )
	{
	    free ( AxisInfoList[side][id].axisTitle);
	    AxisInfoList[side][id].axisTitle = NULL;
	}
	else if ( !AxisInfoList[side][id].axisTitle && newTitle )
	{
	    AxisInfoList[side][id].axisTitle = 
		    (char*)malloc ( (strlen(newTitle)+1)* sizeof(char));
	    strcpy ( AxisInfoList[side][id].axisTitle, newTitle );
	}
	AxisInfoList[side][id].changed = 1;
    }
    else
    {
	msg_ELog( EF_PROBLEM, "Axis id (%d) has not been set", id);
	return(0);
    }
    return(id);
}

int
ac_AxisId ( side, cName )
int	side;
char	*cName;
{
    int id = -1;
    int i;
    if ( cName )
    {
	for ( i = 0; i < MAX_AXIS; i++)
	{
	    if ( AxisInfoList[side][i].component &&
		 strcmp(AxisInfoList[side][i].component, cName) == 0)
		break;
	}
	if ( i < MAX_AXIS )
	    id = i;
    }
    return ( id );
}
int
ac_NewAxisId ( side, cName )
int	side;
char	*cName;
{
    int id = -1;
    int i;
    static init = 1;
    if ( init )
    {
	init = 0;
	for ( i = 0; i < MAX_AXIS; i++)
	{
	    AxisInfoList[AXIS_BOTTOM][i].component = NULL;
	    AxisInfoList[AXIS_LEFT][i].component = NULL;
	    AxisInfoList[AXIS_RIGHT][i].component = NULL;
	    AxisInfoList[AXIS_TOP][i].component = NULL;
	}
    }
    if ( cName )
    {
	for ( i = 0; i < MAX_AXIS; i++)
	{
	    if ( AxisInfoList[side][i].component &&
		 strcmp(AxisInfoList[side][i].component, cName) == 0)
		break;
	}
	if ( i < MAX_AXIS )
	{
	    msg_ELog( EF_PROBLEM, 
	    "Axis already exists for component %s", cName);
	}
	else
	{
	    for ( i = 0; i < MAX_AXIS && AxisInfoList[side][i].component; i++);
	    if ( i < MAX_AXIS )
		id = i;
	}
    }
    return ( id );
}

int
ac_AddAxis ( side, cName,ticSize, ntic, min, max,axisTitle, fontScale, invert )
int	side;
char	*cName;
int	ticSize;
int	ntic;
float	min;
float	max;
char	*axisTitle;
float	fontScale;
int	invert;
{
	int id;
/*
 * Make sure the id is less than the maximum number allowed
 */
    id = ac_NewAxisId ( side, cName );
    if ( id < 0 )
    {
	msg_ELog( EF_PROBLEM, 
	    "Invalid component name for Axis Control.");
	return(0);
    }
    if ( axisTitle )
    {
	AxisInfoList[side][id].axisTitle = 
		(char*)malloc ( (strlen(axisTitle)+1)*sizeof(char));
	strcpy ( AxisInfoList[side][id].axisTitle, axisTitle );
    }
    else
    {
	AxisInfoList[side][id].axisTitle = NULL;
    }
    AxisInfoList[side][id].component = 
		(char*)malloc ( (strlen(cName)+1)*sizeof(char));
    strcpy ( AxisInfoList[side][id].component, cName );
    AxisInfoList[side][id].max = max;
    AxisInfoList[side][id].ticSize = ticSize;
    AxisInfoList[side][id].fontScale = fontScale;
    AxisInfoList[side][id].ntic = ntic;
    AxisInfoList[side][id].min = min;
    AxisInfoList[side][id].invert = invert;
    AxisInfoList[side][id].changed = 1;
    return(id);
}

int
ac_PlotAxis ( side, pixPos, ticSize, ntic, axisTitle, fontScale,min,max,invert)
int	side;
int	*pixPos;
int	ticSize;
int	ntic;
char	*axisTitle;
float	fontScale;
float	min,max;
int	invert;
{
	int i;
	float x_coord[2],y_coord[2];
	int	increment = 0;
	float	scale = .1;
	char	ticLabel[15];
	float	ticLoc;
	int	x1,y1,x2,y2,labelExtent,maxX;
	int	fit = 1;
	int	axisPix,ticPix,labelPix,titleOffset;
	int	titlePix = 0;
	float	savemin, savemax;
        unsigned int	saveConfig;
	
	axisPix = *pixPos;
	ticPix = axisPix + ticSize;
	labelPix = ticPix + 2;
	titleOffset = 2;
	saveConfig = TransConfig;
	switch (side)
	{
	    case AXIS_BOTTOM:
		savemin = UX0;
		savemax = UX1;
		TransConfig = TransConfig & ~INVERT_Y;
		TransConfig = invert ? 
			TransConfig | INVERT_X : TransConfig & ~INVERT_X;
		lc_SetUserCoord ( min,max, UY0,UY1);
		x_coord[0] = UX0;
		x_coord[1] = UX1;
		y_coord[0] = devY(UY0)+ axisPix;
		y_coord[1] = devY(UY0)+ axisPix;
		y_coord[0] = userY((int)y_coord[0]);
		y_coord[1] = userY((int)y_coord[1]);
	
		if ( devY (y_coord[0]) > 
		     GWHeight(Graphics)-(int)(GWHeight(Graphics)*AxisY0[side]) )
		{
		    fit = 0;
		    break;
		}
		gp_Pline( x_coord,y_coord,2,L_solid, Tadefclr.pixel);
		while ( increment == 0 )
		{
		    scale = scale * 10.0;
		    increment = ROUNDIT((UX1-UX0)*scale/ntic);
		}
		ticLoc = (int)UX0;
		while ( ticLoc < UX0 )
		    ticLoc+= increment/scale;
		labelExtent = devX(UX0);
		while ( ticLoc < UX1 )
		{
		    x_coord[0] = ticLoc;
		    x_coord[1] = ticLoc;
		    y_coord[0] = devY(UY0) + axisPix;
		    y_coord[1] = devY(UY0) + ticPix;
		    if ( y_coord[1] > GWHeight(Graphics)-
			(int)(GWHeight(Graphics)*AxisY0[side]) )
		    {
		        fit = 0;
		        break;
		    }
		    y_coord[0] = userY((int)y_coord[0]);
		    y_coord[1] = userY((int)y_coord[1]);
		    gp_Pline( x_coord,y_coord,2,L_solid, Tadefclr.pixel);
		    sprintf ( ticLabel, "%.2f",ticLoc);
		    DT_TextBox ( Graphics, GWFrame(Graphics), 
				devX(x_coord[0]),devY(UY0)+labelPix,
				ticLabel, 0.0, fontScale, 
				JustifyCenter, JustifyTop, &x1,&y1,&x2,&y2);
		    if ( y1 > GWHeight(Graphics)-
			(int)(GWHeight(Graphics)*AxisY0[side]) )
		    {
		        fit = 0;
		        break;
		    }
		    if ( (x1 > labelExtent && !invert) ||
			 (x2 < labelExtent && invert) )
		    {
		        DrawText ( Graphics, GWFrame(Graphics), Gcontext,
				devX(x_coord[0]),devY(UY0)+labelPix,
				ticLabel, 0.0,fontScale, JustifyCenter, JustifyTop);
		        labelExtent = invert ? x1 : x2;
		    }
		    ticLoc+= increment/scale;
		}
		titlePix = (y1 - devY(UY0));
		if ( axisTitle )
		{
		    titlePix += titleOffset;
		    DT_TextBox ( Graphics, GWFrame(Graphics),  
		    		devX( UX0 + ((UX1-UX0)*0.5)),
		    		devY(UY0) + titlePix,
				ticLabel, 0.0,fontScale, JustifyCenter, JustifyTop,
				&x1,&y1,&x2,&y2);
		    if ( y1 > 
		     GWHeight(Graphics)-(int)(GWHeight(Graphics)*AxisY0[side]) )
		    {
		        fit = 0;
		        break;
		    }
		    DrawText ( Graphics, GWFrame(Graphics), Gcontext,
		    		devX( UX0 + ((UX1-UX0)*0.5)),
		    		devY(UY0) + titlePix,
				axisTitle, 0.0,fontScale, 
				JustifyCenter, JustifyTop);
		
		}
		*pixPos = (y1 - devY(UY0));
		lc_SetUserCoord ( savemin,savemax, UY0,UY1);
	    break;
	    case AXIS_TOP:
		savemin = UX0;
		savemax = UX1;
		TransConfig = TransConfig & ~INVERT_Y;
		TransConfig = invert ? 
			TransConfig | INVERT_X : TransConfig & ~INVERT_X;
		lc_SetUserCoord ( min,max, UY0,UY1);
		x_coord[0] = UX0;
		x_coord[1] = UX1;
		y_coord[0] = devY(UY1)- axisPix;
		y_coord[1] = devY(UY1)- axisPix;
		if ( y_coord[0] < 
		     GWHeight(Graphics)-(int)(GWHeight(Graphics)*AxisY1[side]) )
		{
		    fit = 0;
		    break;
		}
		y_coord[0] = userY((int)y_coord[0]);
		y_coord[1] = userY((int)y_coord[1]);
	
		gp_Pline( x_coord,y_coord,2,L_solid, Tadefclr.pixel);
		while ( increment == 0 )
		{
		    scale = scale * 10.0;
		    increment = ROUNDIT((UX1-UX0)*scale/ntic);
		}
		ticLoc = (int)UX0;
		while ( ticLoc < UX0 )
		    ticLoc+= increment/scale;
		labelExtent = devX(UX0);
		while ( ticLoc < UX1 )
		{
		    x_coord[0] = ticLoc;
		    x_coord[1] = ticLoc;
		    y_coord[0] = devY(UY1) - axisPix;
		    y_coord[1] = devY(UY1) - ticPix;
		    if ( y_coord[1] < GWHeight(Graphics)-
			(int)(GWHeight(Graphics)*AxisY1[side]) )
		    {
		        fit = 0;
		        break;
		    }
		    y_coord[0] = userY((int)y_coord[0]);
		    y_coord[1] = userY((int)y_coord[1]);
		    gp_Pline( x_coord,y_coord,2,L_solid, Tadefclr.pixel);
		    sprintf ( ticLabel, "%.2f",ticLoc);
		    DT_TextBox ( Graphics, GWFrame(Graphics), 
				devX(x_coord[0]),devY(UY1)-labelPix,
				ticLabel, 0.0, fontScale, 
				JustifyCenter, JustifyBottom, &x1,&y1,&x2,&y2);
		    if ( y2 < GWHeight(Graphics)-
			(int)(GWHeight(Graphics)*AxisY1[side]) )
		    {
		        fit = 0;
		        break;
		    }
		    if ( (x1 > labelExtent && !invert) ||
			 (x2 < labelExtent && invert) )
		    {
		        DrawText ( Graphics, GWFrame(Graphics), Gcontext,
				devX(x_coord[0]),devY(UY1)-labelPix,
				ticLabel, 0.0,fontScale, JustifyCenter, 
				JustifyBottom);
		        labelExtent = invert ? x1 : x2;
		    }
		    ticLoc+= increment/scale;
		}
		titlePix = devY(UY1)-y2;
		if ( axisTitle )
		{
		    titlePix += titleOffset;
		    DT_TextBox ( Graphics, GWFrame(Graphics),  
		    		devX( UX0 + ((UX1-UX0)*0.5)),
		    		devY(UY1) - titlePix,
				ticLabel, 0.0,fontScale, JustifyCenter, 
				JustifyBottom, &x1,&y1,&x2,&y2);
		    if ( y2 < 
		     GWHeight(Graphics)-(int)(GWHeight(Graphics)*AxisY1[side]) )
		    {
		        fit = 0;
		        break;
		    }
		    DrawText ( Graphics, GWFrame(Graphics), Gcontext,
		    		devX( UX0 + ((UX1-UX0)*0.5)),
		    		devY(UY1) - titlePix,
				axisTitle, 0.0,fontScale, 
				JustifyCenter, JustifyBottom);
		
		}
		*pixPos = devY(UY1)-y2;
		lc_SetUserCoord ( savemin,savemax, UY0,UY1);
	    break;
	    case AXIS_LEFT:
		savemin = UY0;
		savemax = UY1;
		TransConfig = TransConfig & ~INVERT_X;
		TransConfig = invert ? 
			TransConfig | INVERT_Y : TransConfig & ~INVERT_Y;
		lc_SetUserCoord ( UX0,UX1, min,max);
		x_coord[0] = devX(UX0)- axisPix;
		x_coord[1] = devX(UX0)- axisPix;
		y_coord[0] = min;
		y_coord[1] = max;
		if ( (int)x_coord[0] < (int)(GWWidth(Graphics)*AxisX0[side]) )
		{
		    fit = 0;
		    break;
		}
		x_coord[0] = userX((int)x_coord[0]);
		x_coord[1] = userX((int)x_coord[1]);
	
		gp_Pline( x_coord,y_coord,2,L_solid, Tadefclr.pixel);
		while ( increment == 0 )
		{
		    scale = scale * 10.0;
		    increment = ROUNDIT((max-min)*scale/ntic);
		}
		ticLoc = (float)(int)min;
		while ( ticLoc < (float)min )
		    ticLoc+= increment/scale;
		labelExtent = devY(min);
		maxX = devX(UX0);
		while ( ticLoc < max )
		{
		    y_coord[0] = ticLoc;
		    y_coord[1] = ticLoc;
		    x_coord[0] = devX(UX0) - axisPix;
		    x_coord[1] = devX(UX0) - ticPix;
		    if ( (int)x_coord[1]<(int)(GWWidth(Graphics)*AxisX0[side]) )
		    {
		        fit = 0;
		        break;
		    }
		    x_coord[0] = userX((int)x_coord[0]);
		    x_coord[1] = userX((int)x_coord[1]);
		    gp_Pline( x_coord,y_coord,2,L_solid, Tadefclr.pixel);
		    sprintf ( ticLabel, "%.2f",ticLoc);
		
		    DT_TextBox ( Graphics, GWFrame(Graphics), 
				devX(UX0)-labelPix,devY(ticLoc),
				ticLabel, 0.0, fontScale, 
				JustifyRight, JustifyCenter, &x1,&y1,&x2,&y2);
		    if ( x1 < (int)(GWWidth(Graphics)*AxisX0[side]) )
		    {
		        fit = 0;
		        break;
		    }
		    if ( (y1 < labelExtent && !invert) ||
			 (y2 > labelExtent && invert) )
		    {
		        DrawText ( Graphics, GWFrame(Graphics), Gcontext,
				devX(UX0)-labelPix,devY(ticLoc),
				ticLabel, 0.0,fontScale, JustifyRight, 
				JustifyCenter);
		        labelExtent = y2;
		        labelExtent = invert ? y2 : y1;
			maxX = maxX < x1 ? maxX : x1;
		    }
		    ticLoc+= increment/scale;
		}
		titlePix = devX(UX0) - maxX;
		if ( axisTitle )
		{
		    titlePix += titleOffset;
		    DT_TextBox ( Graphics, GWFrame(Graphics),  
		    		devX(UX0) - titlePix,
		    		devY( min + ((max-min)*0.5)),
				ticLabel, 90.0,fontScale, JustifyCenter, JustifyBottom,
				&x1,&y1,&x2,&y2);
		    if ( x1 < (int)(GWWidth(Graphics)*AxisX0[side]) )
		    {
		        fit = 0;
		        break;
		    }
		    DrawText ( Graphics, GWFrame(Graphics), Gcontext,
		    		devX(UX0) - titlePix,
		    		devY( min + ((max-min)*0.5)),
				axisTitle, 90.0,fontScale, 
				JustifyCenter, JustifyBottom);
		
		}
		*pixPos = devX(UX0)- x2;
		lc_SetUserCoord ( UX0,UX1,savemin,savemax);
	    break;
	    case AXIS_RIGHT:
		savemin = UY0;
		savemax = UY1;
		TransConfig = TransConfig & ~INVERT_X;
		TransConfig = invert ? 
			TransConfig | INVERT_Y : TransConfig & ~INVERT_Y;
		lc_SetUserCoord ( UX0,UX1, min,max);
		x_coord[0] = devX(UX1)+ axisPix;
		x_coord[1] = devX(UX1)+ axisPix;
		y_coord[0] = min;
		y_coord[1] = max;
		if ( (int)x_coord[0] > (int)(GWWidth(Graphics)*AxisX1[side]) )
		{
		    fit = 0;
		    break;
		}
		x_coord[0] = userX((int)x_coord[0]);
		x_coord[1] = userX((int)x_coord[1]);
	
		gp_Pline( x_coord,y_coord,2,L_solid, Tadefclr.pixel);
		while ( increment == 0 )
		{
		    scale = scale * 10.0;
		    increment = ROUNDIT((max-min)*scale/ntic);
		}
		ticLoc = (float)(int)min;
		while ( ticLoc < (float)min )
		    ticLoc+= increment/scale;
		labelExtent = devY(min);
		maxX = devX(UX1); while ( ticLoc < max )
		{
		    y_coord[0] = ticLoc;
		    y_coord[1] = ticLoc;
		    x_coord[0] = devX(UX1) + axisPix;
		    x_coord[1] = devX(UX1) + ticPix;
		    if ( (int)x_coord[1]>(int)(GWWidth(Graphics)*AxisX1[side]) )
		    {
		        fit = 0;
		        break;
		    }
		    x_coord[0] = userX((int)x_coord[0]);
		    x_coord[1] = userX((int)x_coord[1]);
		    gp_Pline( x_coord,y_coord,2,L_solid, Tadefclr.pixel);
		    sprintf ( ticLabel, "%.2f",ticLoc);
		
		    DT_TextBox ( Graphics, GWFrame(Graphics), 
				devX(UX1)+labelPix,devY(ticLoc),
				ticLabel, 0.0, fontScale, 
				JustifyLeft, JustifyCenter, &x1,&y1,&x2,&y2);
		    if ( x2 > (int)(GWWidth(Graphics)*AxisX1[side]) )
		    {
		        fit = 0;
		        break;
		    }
		    if ( (y1 < labelExtent && !invert) ||
			 (y2 > labelExtent && invert) )
		    {
		        DrawText ( Graphics, GWFrame(Graphics), Gcontext,
				devX(UX1)+labelPix,devY(ticLoc),
				ticLabel, 0.0,fontScale, JustifyLeft, 
				JustifyCenter);
		        labelExtent = invert ? y2 : y1;
			maxX = maxX > x2 ? maxX : x2;
		    }
		    ticLoc+= increment/scale;
		}
		titlePix = maxX - devX(UX1);
		if ( axisTitle )
		{
		    titlePix += titleOffset;
		    DT_TextBox ( Graphics, GWFrame(Graphics),  
		    		devX(UX1) + titlePix,
		    		devY( min + ((max-min)*0.5)),
				ticLabel, -90.0,fontScale, JustifyCenter, JustifyBottom,
				&x1,&y1,&x2,&y2);
		    if ( x1 < (int)(GWWidth(Graphics)*AxisX0[side]) )
		    {
		        fit = 0;
		        break;
		    }
		    DrawText ( Graphics, GWFrame(Graphics), Gcontext,
		    		devX(UX1) + titlePix,
		    		devY( min + ((max-min)*0.5)),
				axisTitle, -90.0,fontScale, 
				JustifyCenter, JustifyBottom);
		
		}
		*pixPos = x2 - devX(UX1);
		lc_SetUserCoord ( UX0,UX1,savemin,savemax);
	    break;
	}
	TransConfig = saveConfig;
	return(fit);
}

