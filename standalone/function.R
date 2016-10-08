## initialize and create a viewport prepare for drawing
perInit = function ( plot, trans, newpage = FALSE, dbox = TRUE ) {
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
    ## shade is 0.8, ltheta/lphi = [[16]]/[[17]]
    out = list(x = info[[2]], y = info[[3]], z = info[[4]],
                xr = info[[5]], yr = info[[6]], zr = info[[7]],
                col = info[[14]], border = info[[15]], dbox = info[[19]],
                lim = par('usr'), mar = par('mar'), newpage = newpage, 
                axes = info[[20]], nTicks = info[[21]], tickType = info[[22]],
                trans = trans, xlab = info[[23]], ylab = info[[24]], zlab = info[[25]],
				## parameters in 'par' that need added to per
                lwd = info$lwd, lty = info$lty, col.axis = info$col.axis,
				col.lab = info$col.lab, cex.lab = info$cex.lab, 
                shade = info[[18]], ltheta = info[[16]], lphi = info[[17]],
                expand = info[[13]], scale = info[[12]]
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
	
    ## title(not use yet...)
	main = plot$main
    
    ## lty and lwd 
    if(is.null(plot$lwd)) lwd = 1 else lwd = plot$lwd
    if(is.null(plot$lty)) lty = 1 else lty = plot$lty
        
    ##col/border
    colRep = pout$colRep
	col.axis = plot$col.axis
	col.lab = plot$col.lab
	
    ##x, y, z lim
    xr = plot$xr
    yr = plot$yr
    zr = plot$zr
    
	## cex
	cex.lab = plot$cex.lab
	
	#print(col.axis)
    ## the border just using the frist from persp
    border = plot$border[1]

    ## box Information extraction
    if (plot$dbox == TRUE) {
        axes = plot$axes
        if(axes == TRUE){
        ##axes information
        xlab = plot$xlab
        ylab = plot$ylab
        zlab = plot$zlab
        nTicks = plot$nTicks
        tickType = plot$tickType
        
        PerspAxes(xr, yr, zr, ##x, y, z
            xlab, ylab, zlab, ## xlab, xenc, ylab, yenc, zlab, zenc
            nTicks, tickType, trans, ## nTicks, tickType, VT
            lwd, lty, col.axis, col.lab, cex.lab) ## lwd, lty, col.axis, col.lab, cex.lab
            }
    } else {
        xr = yr = zr = c(0,0)
    }

    ## polygon Information extraction
    xyCoor = pout$xyCoor
    pMax = pout$pMax
    polygons = cbind(xyCoor$x, xyCoor$y)
    polygon.id = rep(1:pMax, each = 4)
    
    
    
    ##shade
    shade = plot$shade
    if(!is.na(shade)){
    
        expand = plot$expand
        scale = plot$scale
        ltheta = plot$ltheta
        lphi = plot$lphi
        colOrder = plot
        
        xs = LimitCheck(xr)[1]
        ys = LimitCheck(yr)[1]
        zs = LimitCheck(zr)[1]
        
        
        if(!scale) xs = ys = zs = max(xs, ys, zs)
        
        
        shadedCol = shadeCol(plot$z, plot$x, plot$x,    ##x, y, z
                            1/xs, 1/ys, expand/zs,                    ##xs, ys, zs 
                            plot$col[1], 1,             ##col, ncol   ##multiple color is not working for now..
                            ltheta, lphi, shade)        ## ltheta, lphi, Shade(not shade)
        polygonOrder = pout$polygonOrder
        cols = shadedCol[polygonOrder]
    }else{
        cols = rep_len(plot$col, length(polygons[,1]))
    }

    ## draw the behind face first
    ## return the EdgeDone inorder to not drawing the same Edege two times.
    EdgeDone = rep(0, 12)
    EdgeDone = PerspBox(0, xr, yr, zr, EdgeDone, trans, 1, lwd)
    
    grid.polygon(polygons[,1], polygons[,2], id = polygon.id,
                    default.units = 'native', vp = 'clipon',
                    gp = gpar(col = border, fill = cols, lty = lty, lwd = lwd)
                   )
    ## then draw the front with 'dotted'
    PerspBox(1, xr, yr, zr, EdgeDone, trans, 'dotted', lwd)

}
