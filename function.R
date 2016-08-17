cat('loading', sep = '\n')
library(grid)

## initialize and create a viewport prepare for drawing
perInit = function(newpage = TRUE)
{
	##[[1]] is the all the grapical information that transfer into grid
	##[[3]] is the persp call information
	##[[2]] is the plot details
	info = plot[[1]][[3]][[2]]
	## create a list that store all information from the persp
	## then pass the information to per for drawing.
	out = list(x = info[[2]], y = info[[3]], z = info[[4]],
				xr = info[[5]], yr = info[[6]], zr = info[[7]],
				fill = info[[14]], dbox = info[[19]], 
				lim = par('usr'), mar = par('mar'), newpage = newpage
				)
				
	vp = plotViewport(out$mar, xscale = out$lim[1:2], 
							   yscale = out$lim[3:4])
	pushViewport(vp)
	plot <<- out	
}


per = function(x = 0, y = 0, z = 0, plot = NULL, dbox = TRUE, ...)
{
	if(is.null(plot))
	{
		par(mar = c(4,4,4,4))
		grid.newpage()
		xr = range(x)
        yr = range(y)
        zr = range(z, na.rm = TRUE)
		lim = unlist(
					trans3d(xr, yr, zr, trans)
					)
		print(lim)
	}else
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
	}
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

	out = cbind(xyCoor$x, xyCoor$y)
	grid.id = rep(1:(dim(out)[1] / 4 ), each = 4)
	grid.polygon(out[,1], out[,2], id = grid.id,
					default.units = 'native',
					gp = gpar(col =2,fill = 'Blue'))
}

perFinal = function()
{
	upViewport()
}

cat('done', sep = '\n')
