cat('loading', sep = '\n')
library(grid)

## initialize and create a viewport prepare for drawing
perInit = function(x, y, z,newpage = FALSE, dbox = TRUE)
{
	
	## if newpage is true, then we use the information outside the record
	if(!(missing(x) | missing(y) | missing(z))){
		out = list(x = x, y = y, z = z,
					xr = range(x), yr = range(y), zr = range(z, na.rm = TRUE),
					lim = par('usr'), mar = c(2,2,2,2), dbox = dbox)
					
	}else{
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
	}
	
	if(newpage == TRUE)
		grid.newpage()
		
	vp = plotViewport(out$mar, 
						xscale = out$lim[1:2], 
						yscale = out$lim[3:4])
	pushViewport(vp)
	plot <<- out
}


per = function(plot = NULL, ...)
{
	pout = dPolygon(plot)
	boxInfo = per.box(plot$xr, plot$yr, plot$zr, trans)
	
	
	## polygon Information extraction
	xyCoor = pout$xyCoor
	pMax = pout$pMax
	
	## box Information extraction
	if(plot$dbox == TRUE)
	{
		bout = dBox(boxInfo,pMax)
		bfront = bout$bfront
		bbehind = bout$bbehind
		boxF.id = bout$boxF.id
		boxB.id = bout$boxB.id
		frontCount = bout$frontCount
	}else
	{
		bfront = bbehind = cbind(0,0)
		boxF.id = boxB.id = 0	
		frontCount = 0
	}
	
	polygons = cbind(xyCoor$x, xyCoor$y)
	polygon.id = rep(1:pMax, each = 4) + frontCount


	## actual drawing
	grid.polygon(bfront[,1], bfront[,2], id = boxF.id,
					default.units = 'native',
					gp = gpar(col =1,fill = 'NA'))
					
	grid.polygon(polygons[,1], polygons[,2], id = polygon.id,
					default.units = 'native',
					gp = gpar(col =2,fill = 'Blue'))
					
	grid.polygon(bbehind[,1], bbehind[,2], id = boxB.id,
					default.units = 'native',
					gp = gpar(col =1,fill = 'NA'))
					
					
}
perFinal = function()
{
	upViewport()
  print('compeleted')
}

cat('done', sep = '\n')