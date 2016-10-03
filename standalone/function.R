## initialize and create a viewport prepare for drawing
perInit = function(plot, trans, newpage = FALSE, dbox = TRUE)
{
    ##[[1]] is the all the grapical information that transfer into grid
    ##[[3]] is the persp call information
    ##[[2]] is the plot details eg: x, y, z, xlim, ylim, zlim, col ...
    info = plot[[1]][[3]][[2]]
    ## create a list that store all information from the persp
    ## then pass the information to per for drawing.
    ## x is [[2]]; y is [[3]]; z is [[4]]
    ## xr is [[5]]; yr is [[6]]; zr is [[7]]
    ## col is [[14]]; border is [[15]]; box is [[19]]
    ## axes is [[20]], nTicks is [[21]]
    ## tickType is [[22]]
    ## xlab/ylab/zlab = [[23]]/[[24]]/[[25]]
	## main is in plot[[1]][[4]][[2]][[2]]
    out = list(x = info[[2]], y = info[[3]], z = info[[4]],
                xr = info[[5]], yr = info[[6]], zr = info[[7]],
                col = info[[14]], border = info[[15]], dbox = info[[19]],
                lim = par('usr'), mar = par('mar'), newpage = newpage, 
                axes = info[[20]], nTicks = info[[21]], tickType = info[[22]],
                trans = trans, xlab = info[[23]], ylab = info[[24]], zlab = info[[25]],
				## parameters in 'par' that need added to per
                lwd = info$lwd, lty = info$lty, col.axis = info$col.axis,
				col.lab = info$col.lab, cex.lab = info$cex.lab
				#main = plot[[1]][[4]][[2]][[2]]
                )
    if(out$newpage == TRUE)
        grid.newpage()

    vp = plotViewport(out$mar, xscale = out$lim[1:2], yscale = out$lim[3:4],name = 'clipon',
                    clip = 'on')
    pushViewport(vp)
    upViewport()
    
    vp = plotViewport(out$mar, xscale = out$lim[1:2], yscale = out$lim[3:4],name = 'clipoff',
                    clip = 'off')
    pushViewport(vp)
    upViewport()
    out
}

## actual drawing by passing the plot into the function
## calculation are done from the function of the 'method.r' file
## only simple function call and few calculation are been done on this function
per = function(plot = NULL, ...)
{
    trans = plot$trans
    pout = dPolygon(plot)
    boxInfo = per.box(plot$xr, plot$yr, plot$zr, trans)
	
	main = plot$main
    
    ## lty and lwd 
    lwd = plot$lwd
    if(is.null(lwd))
        lwd = 1
    lty = plot$lty
    if(is.null(lty))
        lty = 1
        
    ## polygon Information extraction
    xyCoor = pout$xyCoor
    pMax = pout$pMax

    ##col/border
    colRep = pout$colRep
	col.axis = plot$col.axis
	col.lab = plot$col.lab
	
	## cex
	cex.lab = plot$cex.lab
	
	#print(col.axis)
    ## the border just using the frist from persp
    border = plot$border[1]
    nTicks = plot$nTicks
    tickType = plot$tickType

    ##x,y,z labels
    xlab = plot$xlab
    ylab = plot$ylab
    zlab = plot$zlab
    
    ##axes
    axes = plot$axes
    
    ## box Information extraction
    if (plot$dbox == TRUE) {
        xr = plot$xr
        yr = plot$yr
        zr = plot$zr
        
        
        if(axes == TRUE){
        PerspAxes(x = xr, y = yr, z = zr, 
            xlab = xlab, xenc = 5, ylab = ylab, yenc = 5, zlab = zlab, zenc = 5, 
            nTicks = nTicks, tickType = tickType, pGEDevDesc = 1, dd = 1, VT = trans, lwd = lwd, col.axis = col.axis,
            lty = lty, col.lab = col.lab, cex.lab = cex.lab)
            }
    } else {
        xr = yr = zr = c(0,0)
    }

    polygons = cbind(xyCoor$x, xyCoor$y)
    polygon.id = rep(1:pMax, each = 4)

    ## draw the behind face first
    PerspBox(0, xr, yr, zr, VT = plot$trans, lty = 1, lwd = lwd)
        
    cols = rep_len(plot$col, length(polygons[,1]))
    
    ##testing for change the colos
    colRep <<- colll[oo]
    
    
    grid.polygon(polygons[,1], polygons[,2], id = polygon.id,
                    default.units = 'native', vp = 'clipon',
                    gp = gpar(col = border, fill = colll[a], lty = lty, lwd = lwd)
                   )
    ## then draw the front with 'dotted'
    PerspBox(1, xr, yr, zr, VT = plot$trans, lty = 'dotted', lwd = lwd)

}
