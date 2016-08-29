TickVector = matrix(ncol = 3, byrow = TRUE, data = c(
    0, -1, -1,
    -1, 0, -1,
    0, 1, -1,
    1, 0, -1,
    -1, -1, 0,
    1, -1, 0,
    -1, 1, 0,
    1, 1, 0 ))
Vertex = matrix(ncol = 3, byrow = TRUE, data = c(
	1, 1, 1,  #xlim[1], ylim[1], zlim[1]
	1, 1, 2,  #xlim[1], ylim[1], zlim[2]
	1, 2, 1,
	1, 2, 2,
	2, 1, 1,
	2, 1, 2,
	2, 2, 1,
	2, 2, 2 ))

AxisStart = c(1, 1, 3, 5, 1, 5, 3, 7)

TransVector = function(u, T, v = 0)
{
	for (i in 1:4) {
		sum = 0
		for (j in 1:4)
			sum = sum + u[j] * T[j,i]
		v[i] = sum
    }
	v
}
	
lowest = function(y1, y2, y3, y4)
{
	(y1 <= y2) && (y1 <= y3) && (y1 <= y4)		
}

labelAngle = function(x1, y1, x2, y2)
{
	dx = abs(x2 - x1)
	if(x2 > x1)
	{
		dy = y2 - y1
	}else{
		dy = y1 - y2
	}
	
	if(dx == 0)
	{
		if(dy > 0)
		{
			angle = 90
		}else
		{
			angle = 270
		}
	}else{
		angle = 180 * atan2(dy, dx)
	}
	angle
}	


PerspAxis = function(x, y, z, axis, axisType, nTicks, tickType, label, encm, dd)
{
    ## don't know how to use numeric on the switch...
    axisType = as.character(axisType)
    tickType = as.character(tickType)
    
	u1 = u2 = u3 = c(0.,0.,0.,0.)
	tickLength = .03
	
	switch(axisType,
		   '1' = {min = x[1]; max = x[2]; range = x},
		   '2' = {min = y[1]; max = y[2]; range = y},
		   '3' = {min = z[1]; max = z[2]; range = z}
            )
            
 	d_frac = 0.1 * (max - min)
	nint = nTicks - 1
	if(!nint) nint = nint + 1
    
    ## pretty is not working...
	i = nint
	pretty(c(min, max), nint)
	
	while((min < range[1] - d_frac || range[2] + d_frac < max) && i < 20) 
	{
		nint = nint + i
		min = range[1]
		max = range[2]
		#pretty(c(min, max), nint)
    }
    
    ## axp is not working..
	axp = 0
	axp[1] = min
    axp[2] = max
    axp[3] = nint
    
	# Do the following calculations for both ticktypes
    # Vertex is a 8*3 matrix; i.e. the vertex of a box
    # AxisStart is a vector of length 8
    # axis is a output 
    # u1, u2 are the vectors in 3-d 
        # the range of x,y,z
	switch (axisType,
        '1' = {
          u1[1] = min
          u1[2] = y[Vertex[AxisStart[axis], 2]]
          u1[3] = z[Vertex[AxisStart[axis], 3]]
        },
        '2' = {
          u1[1] = x[Vertex[AxisStart[axis], 1]]
          u1[2] = min
          u1[3] = z[Vertex[AxisStart[axis], 3]]
        },
        '3' = {
          u1[1] = x[Vertex[AxisStart[axis], 1]]
          u1[2] = y[Vertex[AxisStart[axis], 2]]
          u1[3] = min
        }
	)
    u1[1] = u1[1] + tickLength*(x[2]-x[1])*TickVector[axis, 1]
    u1[2] = u1[2] + tickLength*(y[2]-y[1])*TickVector[axis, 2]
    u1[3] = u1[3] + tickLength*(z[2]-z[1])*TickVector[axis, 3]
    u1[4] = 1
    
    ##axisType, 1 = 'draw x-axis'
    ##          2 = 'draw y-axis'
    ##          3 = 'draw z-axis'
    switch (axisType,
		'1' = {
		u2[1] = max
		u2[2] = u1[2]
		u2[3] = u1[3]
		},
		'2' = {
		u2[1] = u1[1]
		u2[2] = max
		u2[3] = u1[3]
		},
		'3' = {
		u2[1] = u1[1]
		u2[2] = u1[2]
		u2[3] = max
		}
    )
	u2[4] = 1
    
    
    ## ticktype is not working...
	switch(tickType,
		'1' = { 
		u3[1] = u1[1] + tickLength*(x[2]-x[1])*TickVector[axis, 1]
		u3[2] = u1[2] + tickLength*(y[2]-y[1])*TickVector[axis, 2]
		u3[3] = u1[3] + tickLength*(z[2]-z[1])*TickVector[axis, 3]
		},
		'2' = {
		u3[1] = u1[1] + 2.5*tickLength*(x[2]-x[1])*TickVector[axis, 1]
		u3[2] = u1[2] + 2.5*tickLength*(y[2]-y[1])*TickVector[axis, 2]
		u3[3] = u1[3] + 2.5*tickLength*(z[2]-z[1])*TickVector[axis, 3]
		}
    )
    
    ## u3 is the the labels at the center of each axes
    switch(axisType,
		'1' = {
		u3[1] = (min + max)/2
		},
		'2' = {
		u3[2] = (min + max)/2
		},
		'3' = {
		u3[3] = (min + max)/2
		}
    )
    
    u3[4] = 1
    
    ## transform the 3-d into 2-d
    v1 = TransVector(u1, VT)
	v2 = TransVector(u2, VT)
	v3 = TransVector(u3, VT)
    
    v1 = v1/v1[4]
    v2 = v2/v2[4]
    v3 = v3/v3[4]
 
    ## label at center of each axes
    srt = labelAngle(v1[1], v1[2], v2[1], v2[2])
	text(v3[1], v3[2], label, 0.5, srt = srt)

    ## tickType is not working.. when = '2'
	switch(tickType,
    '1' = {
        arrows(v1[1], v1[2], v2[1], v2[2], #USER = 1,
                0.1, 10, 2, dd)
		   },
    ## '2' is not working...
    '2' = 
	{
		for(i in 1:length(at))
		{
			switch(axisType, 
				'1' = {
				u1[0] = REAL(at)[i]
				u1[1] = y[Vertex[AxisStart[axis]][1]]
				u1[2] = z[Vertex[AxisStart[axis]][2]]
				},
				'2' = {
				u1[0] = x[Vertex[AxisStart[axis]][0]]
				u1[1] = REAL(at)[i]
				u1[2] = z[Vertex[AxisStart[axis]][2]]
				},
				'3' = {
				u1[0] = x[Vertex[AxisStart[axis]][0]]
				u1[1] = y[Vertex[AxisStart[axis]][1]]
				u1[2] = REAL(at)[i]
				}
			)
			u1[4] = 1
			u2[1] = u1[1] + tickLength*(x[2]-x[1])*TickVector[axis][1]
			u2[2] = u1[2] + tickLength*(y[2]-y[1])*TickVector[axis][2]
			u2[3] = u1[3] + tickLength*(z[2]-z[1])*TickVector[axis][3]
			u2[4] = 1
			u3[1] = u2[1] + tickLength*(x[2]-x[1])*TickVector[axis][1]
			u3[2] = u2[2] + tickLength*(y[2]-y[1])*TickVector[axis][2]
			u3[3] = u2[3] + tickLength*(z[2]-z[1])*TickVector[axis][3]
			u3[4] = 1
			v1 = TransVector(u1[1], u1[2], u1[3] , VT)
			v2 = TransVector(u2[1], u2[2], u2[3] , VT)
			v3 = TransVector(u2[1], u2[2], u2[3] , VT)
			## Draw tick line			
			  lines(c(v1[1]/v1[4], v2[1]/v2[4]), c(v1[2]/v1[4], v2[2]/v2[4]))

			## Draw tick label
			text(v3[1]/v3[4], v3[2]/v3[4], lab, .5, .5, 0,);
		}
	}
	)
	
	
}


PerspAxes = function(x, y, z, 
					xlab, xenc, 
					ylab, yenc, 
					zlab, zenc, 
					nTicks, tickType, pGEDevDesc, dd)
{
	xAxis = yAxis = zAxis = 0 ## -Wall 
    u0 = u1 = u2 = u3 = 0
    
	u0[1] = x[1]
    u0[2] = y[1]
    u0[3] = z[1]
    u0[4] = 1
    u1[1] = x[2]
    u1[2] = y[1]
    u1[3] = z[1]
    u1[4] = 1
    u2[1] = x[1]
    u2[2] = y[2]
    u2[3] = z[1]
    u2[4] = 1
    u3[1] = x[2]
    u3[2] = y[2]
    u3[3] = z[1]
    u3[4] = 1
	
	v0 = TransVector(u0, VT)
	v1 = TransVector(u1, VT)
	v2 = TransVector(u2, VT)
	v3 = TransVector(u3, VT)
    
    v0 = v0/v0[4]
    v1 = v1/v1[4]
    v2 = v2/v2[4]
    v3 = v3/v3[4]
    
    ## Figure out which X and Y axis to draw
    ## but not sure how it works..
    if (lowest(v0[2], v1[2], v2[2], v3[2]))
	{
		xAxis = 1
		yAxis = 2
    } else if (lowest(v1[2], v0[2], v2[2], v3[2])) {
		xAxis = 1
		yAxis = 4
    } else if (lowest(v2[2], v1[2], v0[2], v3[2])) {
		xAxis = 3
		yAxis = 2
    } else if (lowest(v3[2], v1[2], v2[2], v0[2])) {
		xAxis = 3
		yAxis = 4
    } else
		warning("Axis orientation not calculated")
    ## drawing x and y axes
	PerspAxis(x, y, z, xAxis, '1', nTicks, tickType, xlab, xenc, dd)
    PerspAxis(x, y, z, yAxis, '2', nTicks, tickType, ylab, yenc, dd)
	
    ## Figure out which Z axis to draw
    if (lowest(v0[1], v1[1], v2[1], v3[1])) 
	{
		zAxis = 5
		}else if (lowest(v1[1], v0[1], v2[1], v3[1])) {
			zAxis = 6
		}else if (lowest(v2[1], v1[1], v0[1], v3[1])) {
			zAxis = 7
		}else if (lowest(v3[1], v1[1], v2[1], v0[1])) {
			zAxis = 8
		}else
	warning("Axis orientation not calculated")
    
    ## drawing the z-axis
    PerspAxis(x, y, z, zAxis, '3', nTicks, tickType, zlab, zenc, dd);
}


