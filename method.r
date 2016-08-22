dBox = function(boxInfo, pMax)
{
    frontCount = 6 - sum(boxInfo$Near)
    boxPoints = boxInfo$O

    ## first case
    initial = rep(rep(1:3,4) + rep((0:3) * (length(boxInfo$O)/4), each = 3), 6)
    ## movement  
    move = rep(seq(0,by = 3, length = 6), each = 12)
    projection = initial + move

    boxPolygon = boxPoints[projection]
    bpoints = matrix(boxPolygon, nc = 3, byrow = TRUE)

    ## trans to 2d
    e = trans3d(bpoints[,1], bpoints[,2], bpoints[,3], trans)
    box.id = rep(1:6, each = 4)

    ##figure out which faces are front/behind.
    bPoint = cbind(x = e$x, y = e$y)
    bOrder = rep(boxInfo$Near, each = 4)

    bfront = bPoint[bOrder == 1, ]
    bbehind = bPoint[bOrder == 0, ]

    boxF.id = rep(1:frontCount, each = 4)	
    boxB.id = rep((pMax + 1):(pMax + 6 - frontCount), each = 4)

    bout = list(bfront = bfront, bbehind = bbehind, 
                boxF.id = boxF.id, boxB.id = boxB.id, frontCount = frontCount)

    bout
} 

dPolygon = function(plot)
{
    x = plot$x
    y = plot$y
    z = plot$z
    xr = plot$xr
    yr = plot$yr
    zr = plot$zr
    dbox = plot$dbox
    lim = plot$lim
    mar = plot$mar

    x = plot$x; y = plot$y; z = plot$z
    xr = plot$xr; yr = plot$yr; zr = plot$zr
    dbox = plot$dbox; lim = plot$lim; mar = plot$mar
        
    ## the total number of polygon that we need to draw
    s = length(x)
    total = length(z) - s - 1

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
                    
    ## record the total number of polygon
    pMax = length(xyCoor$x) / 4

    pout = list(xyCoor = xyCoor, pMax = pMax)
    pout
}

## method for check wheater the axes is front or behind.
## return a boxInfo that contain a vector of logical value that tells which face is
## front or behind. and a vector of points order as: x1, y1, z1, x2, y2, z2 and so on 

## still need to understand this algorithm... 
per.box = function(xlim, ylim, zlim, trans)
{
    # points on edges (x,y,z)
    Vertex <- matrix(ncol = 3, byrow = TRUE, data = c(
        1, 1, 1,  #xlim[1], ylim[1], zlim[1]
        1, 1, 2,  #xlim[1], ylim[1], zlim[2]
        1, 2, 1,
        1, 2, 2,
        2, 1, 1,
        2, 1, 2,
        2, 2, 1,
        2, 2, 2 ))

    # the points of Vertex belonging to a face
    Face  <- matrix (ncol = 4, byrow = TRUE, data = c(
        1, 2, 6, 5,
        3, 7, 8, 4,
        1, 3, 4, 2,
        5, 6, 8, 7,
        1, 5, 7, 3,
        2, 4, 8, 6 ))
        
    Near <- vector(length = 6)
    o1 = o2 = o3 = o4 = numeric(0)
    for (i in 1:6) {
        p <- Face[i, ]

        pt <- Vertex[p[1], ]
        u1 <- c(xlim[pt[1]] , ylim[pt[2]], zlim[pt[3]], 1)

        pt <- Vertex[p[2], ]
        u2 <- c(xlim[pt[1]] , ylim[pt[2]], zlim[pt[3]], 1)

        pt <- Vertex[p[3], ]
        u3 <- c(xlim[pt[1]] , ylim[pt[2]], zlim[pt[3]], 1)

        pt <- Vertex[p[4], ]
        u4 <- c(xlim[pt[1]] , ylim[pt[2]], zlim[pt[3]], 1)

        ## return the points of drawing edges
        o1 = c(o1,u1[1:3])
        o2 = c(o2,u2[1:3])
        o3 = c(o3,u3[1:3])
        o4 = c(o4,u4[1:3])

        v1 <- u1 %*% trans
        v2 <- u2 %*% trans
        v3 <- u3 %*% trans
        v4 <- u4 %*% trans

        dd <- v2/v2[4] - v1/v1[4]
        ee <- v3/v3[4] - v2/v2[4]

        Near[i] <- (dd[1]*ee[2] - dd[2]*ee[1]) < 0
    }
    out <<- list(Near = Near, O = c(o1, o2, o3, o4))
    out
}