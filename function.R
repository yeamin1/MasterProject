cat('loading', sep = '\n')
library(grid)
per = function(x = 0, y = 0, z = 0, newpage = TRUE, dbox = TRUE, ...)
{
	## box parameters
	xx = yy = zz = NULL
    
    
	if(newpage == TRUE)
	{
		par(mar = c(4,4,4,4))
		grid.newpage()
        xr = range(x)
        yr = range(y)
        zr = range(z, na.rm = TRUE)
		lim = unlist(
					trans3d(xr, yr, zr, trans)
					)
	}else{
		plot = recordPlot()[[1]][[3]][[2]]
		x = plot[[2]]
		y = plot[[3]]
		z = plot[[4]]
        xr = range(x)
        yr = range(y)
        zr = range(z, na.rm = TRUE)
		dbox = plot[[19]]
		lim = par('usr')
	}
	mar = par('mar')
	## initialize and getting the information from the 'Basic' plot
	vp = plotViewport(mar, xscale = lim[1:2], yscale = lim[3:4])
	pushViewport(vp)

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
	if(dbox == TRUE)
	{
		## the order for axis-drawing
		xe.i = c(rep(1:2,4),  2,2,1,1,1,1,2,2, 2,2,1,1,2,2,1,1)
		ye.i = c(1,1,2,2,1,1,2,2, rep(1:2,4), rep(2:1,each = 4))
		ze.i = c(rep(1:2,each = 4), 1,1,1,1,2,2,2,2, rep(1:2,4))

		xRan = xr[xe.i]
		yRan = yr[ye.i]
		zRan = zr[ze.i]

		axes <<- trans3d(xRan, yRan, zRan, trans)
		axes.id = rep(1:12, each = 2)
		grid.polygon(axes$x, axes$y, id = axes.id,
		default.units = 'native',
		gp = gpar(col = 'orange',
		fill = 'NA'))
	}
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
	xm = matrix(xCoor, nr = 4, byrow = FALSE)
	ym = matrix(yCoor, nr = 4, byrow = FALSE)
	zm = matrix(zCoor, nr = 4, byrow = FALSE)
	xc = xm[1,]
	yc = ym[1,]
	zc = zm[1,]

	## method for using the zdepth for changing the drawing order for every polygon
	orderTemp = cbind(xc, yc, zc, rep(1, length(xc))) %*% trans 
	zdepth = orderTemp[, 4]

	## the zdepth of a set of 4 points of each polygon
	a = order(zdepth, decreasing = TRUE)
	oo = rep(1:4, length(a)) + rep(a - 1, each = 4) * 4

	xyCoor = trans3d(xCoor[oo],
					yCoor[oo],
					zCoor[oo], trans)  

	out <<- cbind(xyCoor$x, xyCoor$y)
	grid.id = rep(1:(dim(out)[1] / 4 ), each = 4)
	grid.polygon(out[,1], out[,2], id = grid.id,
					default.units = 'native',
					gp = gpar(col =2,fill = 'Blue'))
	popViewport()
}
cat('done', sep = '\n')
