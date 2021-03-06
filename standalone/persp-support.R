####Shade function
#####################################################################
LimitCheck = function ( lim ) {
    s = 0.5 * abs(lim[2] - lim[1])
    c = 0.5 * (lim[2] + lim[1])
    c(s, c)
}


XRotate = function ( angle ) {
    TT = diag(1, 4)
    rad = angle * pi / 180
    c = cos(rad)
    s = sin(rad)
    TT[2, 2] = c;
    TT[3, 2] = -s;
    TT[3, 3] = c;
    TT[2, 3] = s;
    TT
}

YRotate = function ( angle ) {
    TT = diag(1, 4)
    rad = angle * pi / 180
    c = cos(rad)
    s = sin(rad)
    TT[1, 1] = c;
    TT[3, 0] = s;
    TT[3, 3] = c;
    TT[1, 3] = -s;
    TT
}

ZRotate = function ( angle ) {
    TT = diag(1, 4)
    rad = angle * pi / 180
    c = cos(rad)
    s = sin(rad)
    TT[1, 1] = c;
    TT[2, 1] = -s;
    TT[2, 2] = c;
    TT[1, 2] = s;
    TT
}
        
SetUpLight = function ( theta, phi ) {
    u = c(0, -1, 0, 1)
    VT = diag(1, 4)
    VT = VT %*% XRotate(-phi)
    VT = VT %*% ZRotate(theta)
    Light = u %*% VT
}

FacetShade = function( u, v, Shade = 0.5, Light ) {
    nx = u[2] * v[3] - u[3] * v[2]
    ny = u[3] * v[1] - u[1] * v[3]
    nz = u[1] * v[2] - u[2] * v[1]
    sum = sqrt(nx * nx + ny * ny + nz * nz)
    if (sum == 0) sum = 1
    nx = nx/sum
    ny = ny/sum
    nz = nz/sum
    sum = 0.5 * (nx * Light[1] + ny * Light[2] + nz * Light[3] + 1)
    sum^Shade   
}

shadeCol = function ( z, x, y, xs, ys, zs, col, ncol = length(col), ltheta, lphi, Shade, Light) {
    u = v = 0
    nx = nrow(z)
    ny = ncol(z)
    nx1 = nx - 1
    ny1 = ny - 1
    cols = 0
    
    indx = 0:(length(z))
    Light = SetUpLight(ltheta, lphi)
    for(k in 1:(nx1 * ny1)){
        nv = 0
        i = (indx[k]) %% nx1 
        j = (indx[k]) %/% nx1
       icol = (i + j * nx1) %% ncol + 1

        u[1] = xs * (x[i+1+1] - x[i+1])
	    u[2] = ys * (y[j+1] - y[j+1+1])
	    u[3] = zs * (z[(i+1)+j*nx+1] - z[i+(j+1)*nx+1])
	    v[1] = xs * (x[i+1+1] - x[i+1])
	    v[2] = ys * (y[j+1+1] - y[j+1])
	    v[3] = zs * (z[(i+1)+(j+1)*nx+1] - z[i+j*nx+1])
        icol = (i + j * nx1) %% ncol
	    shade = FacetShade(u, v, Shade = Shade, Light = Light)
        ##one condiction here..if any bugs then check here...
        #
        #
        shadedCol = col2rgb(col[icol + 1])/ 255
        cols[k] = rgb(shade * shadedCol[1], shade * shadedCol[2], shade * shadedCol[3])
    }
        cols
}
## shade end...
#####################################################################

## font = 1 -> draw front face
## x, y, z are the range of x, y, z-axis
## EdgeDone is not been used in this time
## VT = trans
## lty = lty..
## a function from C
PerspBox = function(front = 1, x, y, z, EdgeDone, VT, lty, lwd = lwd )
{
    u0 = u1 = u2 = u3 = 0
    v0 = v1 = v2 = v3 = 0
    for (f in 1:6) {
        p0 = Face[f, 1]
        p1 = Face[f, 2]
        p2 = Face[f, 3]
        p3 = Face[f, 4]

        u0[1] = x[Vertex[p0, 1]]
        u0[2] = y[Vertex[p0, 2]]
        u0[3] = z[Vertex[p0, 3]]
        u0[4] = 1
        u1[1] = x[Vertex[p1, 1]]
        u1[2] = y[Vertex[p1, 2]]
        u1[3] = z[Vertex[p1, 3]]
        u1[4] = 1
        u2[1] = x[Vertex[p2, 1]]
        u2[2] = y[Vertex[p2, 2]]
        u2[3] = z[Vertex[p2, 3]]
        u2[4] = 1
        u3[1] = x[Vertex[p3, 1]]
        u3[2] = y[Vertex[p3, 2]]
        u3[3] = z[Vertex[p3, 3]]
        u3[4] = 1

        v0 = TransVector(u0, VT)
        v1 = TransVector(u1, VT)
        v2 = TransVector(u2, VT)
        v3 = TransVector(u3, VT)
        
        v0 = v0/v0[4]
        v1 = v1/v1[4]
        v2 = v2/v2[4]
        v3 = v3/v3[4]
        
        d = v1 - v0
        e = v2 - v1
        
        nearby = (d[1]*e[2] - d[2]*e[1]) < 0
        
        ## draw the face line by line rather than polygon
        if ((front && nearby) || (!front && !nearby)) {
            if (!EdgeDone[Edge[f, 1]]){
                grid.lines(c(v0[1], v1[1]), c(v0[2], v1[2]), default.units = 'native',
                    gp = gpar(lty = lty, lwd = lwd), vp = 'clipon')
                EdgeDone[Edge[f, 1]] = EdgeDone[Edge[f, 1]] + 1
                }
            if (!EdgeDone[Edge[f, 2]]){
                grid.lines(c(v1[1], v2[1]), c(v1[2], v2[2]), default.units = 'native',
                    gp = gpar(lty = lty, lwd = lwd), vp = 'clipon')
                EdgeDone[Edge[f, 2]] = EdgeDone[Edge[f, 2]] + 1
                }
            if (!EdgeDone[Edge[f, 3]]){
                grid.lines(c(v2[1], v3[1]), c(v2[2], v3[2]), default.units = 'native',
                    gp = gpar(lty = lty, lwd = lwd), vp = 'clipon')
                EdgeDone[Edge[f, 3]] = EdgeDone[Edge[f, 3]] + 1
                }
            if (!EdgeDone[Edge[f, 4]]){
                grid.lines(c(v3[1], v0[1]), c(v3[2], v0[2]), default.units = 'native',
                    gp = gpar(lty = lty, lwd = lwd), vp = 'clipon')
                EdgeDone[Edge[f, 4]] = EdgeDone[Edge[f, 4]] + 1
                }
        }
    }
    EdgeDone
}

dPolygon = function(plot){

    x = plot$x; y = plot$y; z = plot$z
    xr = plot$xr; yr = plot$yr; zr = plot$zr
    dbox = plot$dbox; lim = plot$lim; mar = plot$mar
    col = plot$col
        

    ## the total number of polygon that we need to draw
    s = length(x)
    total = length(z) - s - 1
    print(total)

    ## set the temp value for x,y,z prepare for subsetting
    xTmp = rep(x, s)
    yTmp = rep(y,each = s)
    zTmp = as.numeric(z)
    
    ## the drawing order is along x-axis, and then along y-axis
    ## then create a vector like a 4Xn matrix, 
    ## i.e the first column contain all the first points for every polygons
    ## the second column contain all the second points for every polygons and so on 
    pBreak = c(1:total, 1 + 1:total, 1 + s + 1:total, s + 1:total)
    xBreak = xTmp[pBreak]
    yBreak = yTmp[pBreak]
    zBreak = zTmp[pBreak]
    
    ## draw the box if required
    ## the vectors now has four paths, every paths contain the information of every points of every polygon
    ## now we need to change the order of this vector, so that the first four index should be the order for drawing 
    ## the first points, not the first four points for the first four polygon
    ## points subsetting 
    plot.index = rep(
        c(1, 1 + total, 
        1 + 2 * total, 
        1 + 3 * total ),
        total) + rep(0:(total - 1), each = 4)

    ## sequence for 'problem's polygons index, e.g
    ## along x-axis, there are n-1 polygons, n is the number of points in x direction
    ## we don't want to draw the nth polygon, hence we deleted those polygon
    dp = rep((4 * seq(s,total,s)), each = 4) - (3:0)

    ## final subsetting
    xCoor = xBreak[c(plot.index)][-dp]
    yCoor = yBreak[c(plot.index)][-dp]
    zCoor = zBreak[c(plot.index)][-dp]
    
    ## vectorize the cols
    colRep = rep_len(col, length(xCoor))
    
    ## use the first corner of every polygon to determind the order for drawing
    corn.id = 4* 1:(length(xCoor)/4)
    xc = xCoor[corn.id]
    yc = yCoor[corn.id]
    zc = zCoor[corn.id] 
    
    
    ## method for using the zdepth for changing the drawing order for every polygon
    orderTemp = cbind(xc, yc, 0, 1) %*% trans 
    zdepth = orderTemp[, 4]

    ## the zdepth of a set of 4 points of each polygon
    a = order(zdepth, decreasing = TRUE)
    oo = rep(1:4, length(a)) + rep(a - 1, each = 4) * 4

    xyCoor = trans3d(xCoor[oo],
                    yCoor[oo],
                    zCoor[oo], trans)
                    
    colRep = colRep[a]
    

    ## record the total number of polygon
    pMax = length(xyCoor$x) / 4

    pout = list(xyCoor = xyCoor, pMax = pMax, colRep = colRep, polygonOrder = a)
    pout
}



DrawFacets = function(plot, z, x, y, xs, ys, zs, col, ncol = length(col), ltheta, lphi, Shade, Light)
{
    pout = dPolygon(plot)
    xyCoor = pout$xyCoor
    pMax = pout$pMax; colRep = pout$colRep
    polygonOrder = pout$polygonOrder
    polygons = cbind(xyCoor$x, xyCoor$y)
    polygon.id = rep(1:pMax, each = 4)
    
    if (!is.na(Shade)) {
        if(is.finite(Shade) && Shade <= 0 ) Shade = 1
        shadedCol = shadeCol(z, x, y,                       ## x, y, z
                xs, ys, zs,                                 ## xs, ys, zs 
                plot$col, length(plot$col),                 ## col, ncol
                ltheta, lphi, Shade, Light = Light)         ## ltheta, lphi, Shade(not shade)
        cols = shadedCol[polygonOrder]

    } else {
        cols = rep_len(plot$col, length(polygons[,1]))
    }

    grid.polygon(polygons[,1], polygons[,2], id = polygon.id,
                    default.units = 'native', vp = 'clipon',
                    gp = gpar(col = plot$border, fill = cols, lty = plot$lty, lwd = plot$lwd))

}
## method for check wheater the axes is front or behind.
## return a boxInfo that contain a vector of logical value that tells which face is
## front or behind. and a vector of points order as: x1, y1, z1, x2, y2, z2 and so on 

## still need to understand this algorithm... 
per.box = function(xlim, ylim, zlim, trans){
        
    Near = vector(length = 6)
    o1 = o2 = o3 = o4 = numeric(0)
    for (i in 1:6) {
        p = Face[i, ]

        pt = Vertex[p[1], ]
        u1 = c(xlim[pt[1]] , ylim[pt[2]], zlim[pt[3]], 1)

        pt = Vertex[p[2], ]
        u2 = c(xlim[pt[1]] , ylim[pt[2]], zlim[pt[3]], 1)

        pt = Vertex[p[3], ]
        u3 = c(xlim[pt[1]] , ylim[pt[2]], zlim[pt[3]], 1)

        pt = Vertex[p[4], ]
        u4 = c(xlim[pt[1]] , ylim[pt[2]], zlim[pt[3]], 1)

        ## return the points of drawing edges
        o1 = c(o1,u1[1:3])
        o2 = c(o2,u2[1:3])
        o3 = c(o3,u3[1:3])
        o4 = c(o4,u4[1:3])

        v1 = u1 %*% trans
        v2 = u2 %*% trans
        v3 = u3 %*% trans
        v4 = u4 %*% trans

        dd = v2/v2[4] - v1/v1[4]
        ee = v3/v3[4] - v2/v2[4]

        Near[i] = (dd[1]*ee[2] - dd[2]*ee[1]) < 0
    }
    out = list(Near = Near, O = c(o1, o2, o3, o4))
    out
}

TransVector = function(u, T) {
    u %*% T
}

lowest = function (y1, y2, y3, y4) {
    (y1 <= y2) && (y1 <= y3) && (y1 <= y4)		
}

labelAngle = function(x1, y1, x2, y2){

    dx = abs(x2 - x1)
    if ( x2 > x1 ) {
        dy = y2 - y1
    } else {
        dy = y1 - y2
    }

    if (dx == 0) {
        if( dy > 0 ) {
            angle = 90
        } else {
            angle = 270
        }
    } else {
        angle = 180/pi * atan2(dy, dx)
    }
    angle
}	

PerspAxis = function(x, y, z, axis, axisType, 
                    nTicks, tickType, label, 
                    VT, lwd = 1, lty, col.axis = 1,
                    col.lab = 1, cex.lab = 1){

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
    
    if(!nint)nint = nint + 1
    i = nint

    ticks = axisTicks(c(min, max), FALSE, nint = nint)
    min = ticks[1]
    max = ticks[length(ticks)]
    nint = length(ticks) - 1
            
    ## but maybe not this one... haven't test yet...
    while((min < range[1] - d_frac || range[2] + d_frac < max) && i < 20) {
        nint = i + 1
        ticks = axisTicks(c(min, max), FALSE)
        range = range(ticks)
        nint = length(ticks) - 1
    }
    
    ## axp seems working...
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
    #text(v3[1], v3[2], label, 0.5, srt = srt)
    grid.text(label = label, x = v3[1], y = v3[2],
          just = "centre", rot = srt,
          default.units = "native", vp = 'clipoff',
          gp = gpar(col = col.lab, lwd = lwd, cex = cex.lab)
          )

    ## tickType is not working.. when = '2'
    switch(tickType,
    '1' = {
    arrow = arrow(angle = 10, length = unit(0.1, "in"),
                    ends = "last", type = "open")  
	## drawing the tick..
    grid.lines(x = c(v1[1], v2[1]), y = c(v1[2], v2[2]),
          default.units = "native", arrow = arrow, vp = 'clipoff',
          gp = gpar(col = 1, lwd = lwd , lty = lty )
          )
       },
    ## '2' seems working
    '2' = {
        at = axisTicks(range, FALSE, axp, nint = nint)
        for(i in 1:length(at)){
            switch(axisType, 
                '1' = {
                u1[1] = at[i]
                u1[2] = y[Vertex[AxisStart[axis], 2]]
                u1[3] = z[Vertex[AxisStart[axis], 3]]
                },
                '2' = {
                u1[1] = x[Vertex[AxisStart[axis], 1]]
                u1[2] = at[i]
                u1[3] = z[Vertex[AxisStart[axis], 3]]
                },
                '3' = {
                u1[1] = x[Vertex[AxisStart[axis], 1]]
                u1[2] = y[Vertex[AxisStart[axis], 2]]
                u1[3] = at[i]
                }
            )
            
            tickLength = 0.03
            
            u1[4] = 1
            u2[1] = u1[1] + tickLength*(x[2]-x[1])*TickVector[axis, 1]
            u2[2] = u1[2] + tickLength*(y[2]-y[1])*TickVector[axis, 2]
            u2[3] = u1[3] + tickLength*(z[2]-z[1])*TickVector[axis, 3]
            u2[4] = 1
            u3[1] = u2[1] + tickLength*(x[2]-x[1])*TickVector[axis, 1]
            u3[2] = u2[2] + tickLength*(y[2]-y[1])*TickVector[axis, 2]
            u3[3] = u2[3] + tickLength*(z[2]-z[1])*TickVector[axis, 3]
            u3[4] = 1
            v1 = TransVector(u1, VT)
            v2 = TransVector(u2, VT)
            v3 = TransVector(u3, VT)
                        
            v1 = v1/v1[4]
            v2 = v2/v2[4]
            v3 = v3/v3[4]
            
            ## Draw tick line
            grid.lines(x = c(v1[1], v2[1]), y = c(v1[2], v2[2]),
                default.units = "native", vp = 'clipoff',
                gp = gpar(col = col.axis, lwd = lwd, lty = lty)
                )

            ## Draw tick label
            lab = at[i]
            #text(v3[1], v3[2], label, 0.5, srt = srt)
            grid.text(label = lab, x = v3[1], y = v3[2],
                  just = "centre",
                  default.units = "native", vp = 'clipoff',
                  gp = gpar(col = col.axis, adj = 1, pos = 0.5, cex = 1)
                  )
            }
        }
    )
}


PerspAxes = function(x, y, z, 
                    xlab, 
                    ylab, 
                    zlab, 
                    nTicks, tickType, VT, 
					## parameters in par
                    lwd = 1, lty = 1, col.axis = 1, col.lab = 1, cex.lab = 1)
{
    xAxis = yAxis = zAxis = 0 ## -Wall 
    u0 = u1 = u2 = u3 = 0

    u0[1] = x[1]; u0[2] = y[1]; u0[3] = z[1]; u0[4] = 1
    u1[1] = x[2]; u1[2] = y[1]; u1[3] = z[1]; u1[4] = 1
    u2[1] = x[1]; u2[2] = y[2]; u2[3] = z[1]; u2[4] = 1
    u3[1] = x[2]; u3[2] = y[2]; u3[3] = z[1]; u3[4] = 1

    v0 = TransVector(u0, VT)
    v1 = TransVector(u1, VT)
    v2 = TransVector(u2, VT)
    v3 = TransVector(u3, VT)

    v0 = v0/v0[4]
    v1 = v1/v1[4]
    v2 = v2/v2[4]
    v3 = v3/v3[4]

    if (lowest(v0[2], v1[2], v2[2], v3[2])) {
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
    PerspAxis(x, y, z, xAxis, '1', nTicks, tickType, xlab, VT, lwd = lwd, lty = lty, col.axis = col.axis, col.lab = col.lab, cex.lab = cex.lab)
    PerspAxis(x, y, z, yAxis, '2', nTicks, tickType, ylab, VT, lwd = lwd, lty = lty, col.axis = col.axis, col.lab = col.lab, cex.lab = cex.lab)

    ## Figure out which Z axis to draw
    if (lowest(v0[1], v1[1], v2[1], v3[1])) {
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
    PerspAxis(x, y, z, zAxis, '3', nTicks, tickType, zlab, VT, lwd = lwd, lty = lty, col.axis = col.axis, col.lab = col.lab, cex.lab = cex.lab)
}