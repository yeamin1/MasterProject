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
        out = list(x = info[[2]], y = info[[3]], z = info[[4]],
                    xr = info[[5]], yr = info[[6]], zr = info[[7]],
                    col = info[[14]], border = info[[15]], dbox = info[[19]],
                    lim = par('usr'), mar = par('mar'), newpage = newpage,
                    trans = trans
                    )
					
    if(out$newpage == TRUE)
        grid.newpage()
        
    vp = plotViewport(out$mar, 
                        xscale = out$lim[1:2], 
                        yscale = out$lim[3:4]
                        )
    pushViewport(vp)
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

    ## polygon Information extraction
    xyCoor = pout$xyCoor
    pMax = pout$pMax
    
    ##col/border
    col = plot$col
    border = plot$border

    ## box Information extraction
    if (plot$dbox == TRUE) {
        bout = dBox(boxInfo,pMax)
        bfront = bout$bfront
        bbehind = bout$bbehind
        boxF.id = bout$boxF.id
        boxB.id = bout$boxB.id
        frontCount = bout$frontCount
        
        ## I used 'nTicks = 6', then the axes works, but no idea why...
        ## not know how to fix...
        PerspAxes(x = plot$xr, y = plot$yr, z = plot$zr, 
            xlab = 'x', xenc = 5, ylab = 'y', yenc = 5, zlab = 'z', zenc = 5, 
            nTicks = 6, tickType = '2', pGEDevDesc = 1, dd = 1, VT = trans)
    } else {
        bfront = bbehind = cbind(0,0)
        boxF.id = boxB.id = 0	
        frontCount = 0
    }

    polygons = cbind(xyCoor$x, xyCoor$y)
    polygon.id = rep(1:pMax, each = 4) + frontCount
    
    ## order: behind-polygon-front
    grid.polygon(bbehind[,1], bbehind[,2], id = boxB.id,
                    default.units = 'native',
                    gp = gpar(col = 1, fill = 'NA')
                    )
    grid.polygon(polygons[,1], polygons[,2], id = polygon.id,
                    default.units = 'native',
                    gp = gpar(col = border, fill = col)
                    )
    grid.polygon(bfront[,1], bfront[,2], id = boxF.id,
                    default.units = 'native',
                    gp = gpar(col = 1, fill = 'NA', lty = 'dotted')
                    )
}

perFinal = function()
{
    upViewport()
    print('compeleted')
}