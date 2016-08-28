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
AxisStart = c(0, 0, 2, 4, 0, 4, 2, 6)


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


plot(0,0)
PerspAxis = function(x, y, z, axis, axisType, nTicks, tickType, label, encm, dd)
{
	u1 = c(0.,0.,0.,0.)
	u2 = c(0.,0.,0.,0.)
	u3 = c(0.,0.,0.,0.)
	tickLength = .03
	range = NULL
	
	print(axisType)
	switch(axisType,
		   '0' = {min = x[1]; max = x[2]; range = x},
		   '1' = {min = y[1]; max = y[1]; range = y},
		   '2' = {min = z[1]; max = z[1]; range = z}
	)
	print('working??')
	d_frac = 0.1 * (max - min)
	nint = nTicks - 1
	if(!nint) nint = nint + 1
	i = nint
	pretty(c(min, max), nint)
	
	print(min)
	while((min < range[1] - d_frac || range[2] + d_frac < max) && i < 20) 
	{
		nint = nint + i
		min = range[1]
		max = range[2]
		pretty(c(min, max), nint)
    }
	axp = 0
	axp[1] = min
    axp[2] = max
    axp[3] = nint
	# Do the following calculations for both ticktypes
	switch (axisType,
        '0' = {
          u1[1] = min;
          u1[2] = y[Vertex[AxisStart[axis]][2]];
          u1[3] = z[Vertex[AxisStart[axis]][3]]
        },
        '1' = {
          u1[1] = x[Vertex[AxisStart[axis]][1]];
          u1[2] = min;
          u1[3] = z[Vertex[AxisStart[axis]][3]]
        },
        '2' = {
          u1[1] = x[Vertex[AxisStart[axis]][1]];
          u1[2] = y[Vertex[AxisStart[axis]][2]];
          u1[3] = min
        }
	)
    u1[1] = u1[1] + tickLength*(x[2]-x[1])*TickVector[axis][1]
    u1[2] = u1[2] + tickLength*(y[2]-y[1])*TickVector[axis][2]
    u1[3] = u1[3] + tickLength*(z[2]-z[1])*TickVector[axis][3]
    u1[4] = 1
    switch (axisType,
		'0' = {
		u2[1] = max;
		u2[2] = u1[2];
		u2[3] = u1[3]
		},
		'1' = {
		u2[1] = u1[1];
		u2[2] = max;
		u2[3] = u1[3]
		},
		'2' = {
		u2[1] = u1[1];
		u2[2] = u1[2];
		u2[3] = max
		}
    )
	u2[4] = 1
	
	#print('working??')
	switch(tickType,
		'1' = { 
		u3[1] = u1[1] + tickLength*(x[2]-x[1])*TickVector[axis][1];
		u3[2] = u1[2] + tickLength*(y[2]-y[1])*TickVector[axis][2];
		u3[3] = u1[3] + tickLength*(z[2]-z[1])*TickVector[axis][3]
		},
		'2' = {
		u3[1] = u1[1] + 2.5*tickLength*(x[2]-x[1])*TickVector[axis][1];
		u3[2] = u1[2] + 2.5*tickLength*(y[2]-y[1])*TickVector[axis][2];
		u3[3] = u1[3] + 2.5*tickLength*(z[2]-z[1])*TickVector[axis][3]
		}
    )
    switch(axisType,
		'0' = {
		u3[1] = (min + max)/2
		},
		'1' = {
		u3[2] = (min + max)/2
		},
		'2' = {
		u3[3] = (min + max)/2
		}
    )
    u3[4] = 1
    v1 = TransVector(u1, VT)
	v2 = TransVector(u2, VT)
	v3 = TransVector(u3, VT)
	
	print(u3)

	text(v3[1]/v3[4], v3[2]/v3[4], label, .5,
			srt = labelAngle(v1[1]/v1[4], v1[2]/v1[4], v2[1]/v2[4], v2[2]/v2[4])
		)
		
	## draw
	switch(tickType,
    '1' = {
	arrows(v1[1]/v1[4], v1[2]/v1[4],
	       v2[1]/v2[4], v2[2]/v2[4], #USER = 1,
	       0.1, 10, 2, dd)
		   },
    '2' = 
	{
		for(i in 1:length(at))
		{
			switch(axisType, 
				'0' = {
				u1[0] = REAL(at)[i];
				u1[1] = y[Vertex[AxisStart[axis]][1]];
				u1[2] = z[Vertex[AxisStart[axis]][2]]
				},
				'1' = {
				u1[0] = x[Vertex[AxisStart[axis]][0]];
				u1[1] = REAL(at)[i];
				u1[2] = z[Vertex[AxisStart[axis]][2]]
				},
				'2' = {
				u1[0] = x[Vertex[AxisStart[axis]][0]];
				u1[1] = y[Vertex[AxisStart[axis]][1]];
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
	xAxis=0; yAxis=0; zAxis=0; ## -Wall 
	u0 = 0
	u1 = 0
	u2 = 0
	u3 = 0
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
	
    ## Figure out which X and Y axis to draw
    if (lowest(v0[1]/v0[3], v1[1]/v1[3], v2[1]/v2[3], v3[1]/v3[3]))
	{
		xAxis = 0
		yAxis = 1
    } else if (lowest(v1[1]/v1[3], v0[1]/v0[3], v2[1]/v2[3], v3[1]/v3[3])) {
		xAxis = 0
		yAxis = 3
    } else if (lowest(v2[1]/v2[3], v1[1]/v1[3], v0[1]/v0[3], v3[1]/v3[3])) {
		xAxis = 2
		yAxis = 1
    } else if (lowest(v3[1]/v3[3], v1[1]/v1[3], v2[1]/v2[3], v0[1]/v0[3])) {
		xAxis = 2
		yAxis = 3
    } else
		warning("Axis orientation not calculated")
		
	PerspAxis(x, y, z, xAxis, 0, nTicks, tickType, xlab, xenc, dd)
    PerspAxis(x, y, z, yAxis, 1, nTicks, tickType, ylab, yenc, dd)
	
    ## Figure out which Z axis to draw
    if (lowest(v0[1]/v0[4], v1[1]/v1[4], v2[1]/v2[4], v3[1]/v3[4])) 
	{
		zAxis = 4
		}else if (lowest(v1[1]/v1[4], v0[1]/v0[4], v2[1]/v2[4], v3[1]/v3[4])) {
			zAxis = 5
		}else if (lowest(v2[1]/v2[4], v1[1]/v1[4], v0[1]/v0[4], v3[1]/v3[4])) {
			zAxis = 6
		}else if (lowest(v3[1]/v3[4], v1[1]/v1[4], v2[1]/v2[4], v0[1]/v0[4])) {
			zAxis = 7
		}else
	warning("Axis orientation not calculated")
    PerspAxis(x, y, z, zAxis, 2, nTicks, tickType, zlab, zenc, dd);
}


