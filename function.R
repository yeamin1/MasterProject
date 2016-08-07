cat('loading', sep = '\n')
per = function(x,y,z,...)
{
  
  lim = par('usr')
  
  ## initialize and getting the information from the 'Basic' plot
  pushViewport(plotViewport(par('mar')))
  vp = viewport(0.5,0.5,1,1,name = 'VP:PLOTlayout',xscale = lim[1:2], yscale = lim[3:4])
  pushViewport(vp)
  
  
  ## the total number of polygon that we need to draw
  s = length(x)
  total = length(z) - s - 1
  
  ## the drawing order is along x-axis, and then along y-axis
  ## then create a vector like a 4Xn matrix, 
  ## i.e the first column contain all the first points for every polygons
  ## the second column contain all the second points for every polygons and so on 
  p.index = c(1:total, 1 + 1:total, 1 + s + 1:total, s + 1:total)
  
  ## the vectors now has four paths, every paths contain the information of every points of every polygon
  ## now we need to change the order of this vector, so that the first four index should be the order for drawing 
  ## the first points, not the first four points for the first four polygon
  plot.index = rep(
    c(1, 1 + total, 
      1 + 2 * total, 
      1 + 3 * total ),
    total) + rep(0:(total - 1), each = 4)
  
  ## sequence for 'problem's polygons index, e.g
  ## along x-axis, there are n-1 polygons, n is the number of points in x direction
  ## we don't want to draw the nth polygon, hence we deleted those polygon
  dp = rep((4 * seq(s,total,s)), each = 4) - (3:0)
  
  
  ## points subsetting 
  xTmp = rep(x, s)
  yTmp = rep(y,each = s)
  zTmp = as.numeric(z)
  
  xCoor = xTmp[p.index][plot.index][-dp]
  yCoor = yTmp[p.index][plot.index][-dp]
  zCoor = zTmp[p.index][plot.index][-dp]
  
  ## calculate the center point of every polygon, hence we can find the order of every centriod, and
  ## we knew the order for every polygons.
  xm = matrix(xCoor, nr = 4, byrow = FALSE)
  xc = colSums(xm)/4
  ym = matrix(yCoor, nr = 4, byrow = FALSE)
  yc = colSums(ym)/4
  zm = matrix(zCoor, nr = 4, byrow = FALSE)
  zc = colSums(zm)/4
  
  ## method for using the zdepth for changing the drawing order for every polygon
  orderTemp = cbind(xc, yc, zc, rep(1, length(xc))) %*% trans 
  zdepth = orderTemp[, 3] / orderTemp[, 4]
  
  ## the zdepth of a set of 4 points of each polygon
  a = order(zdepth)
  oo = rep(1:4, length(a)) + rep(a-1, each = 4) * 4
  
  xyCoor = trans3d(xCoor[oo],
                   yCoor[oo],
                   zCoor[oo], trans)  

  out = cbind(xyCoor$x, xyCoor$y)
  
  grid.id = rep(1:(dim(out)[1] / 4 ),each = 4)
  grid.polygon(out[,1],out[,2],default.units = 'native',gp = gpar(col =1,fill = 'Blue'),id = grid.id)
  
  ## drawing the axis
  xr = range(x)
  yr = range(y)
  zr = range(z,na.rm = TRUE)
  
  xe.i = c(1,2,1,2,1,2,1,2,  2,2,1,1,1,1,2,2,  2,2,1,1,2,2,1,1)
  ye.i = c(1,1,2,2,1,1,2,2,  1,2,1,2,1,2,1,2,  2,2,2,2,1,1,1,1)
  ze.i = c(1,1,1,1,2,2,2,2,  1,1,1,1,2,2,2,2,  1,2,1,2,1,2,1,2)
  edge = trans3d(xr[xe.i],yr[ye.i],zr[ze.i],trans)
  
  ## dont' know how to draw multiple lines with one function call, hence I used loop
  for(j in 0:(length(xe.i)/2 - 1))
  {
    i = (1:2) + 2*j
    grid.lines(edge$x[i],edge$y[i], default.units = 'native')
  }
  
}


cat('done', sep = '\n')
