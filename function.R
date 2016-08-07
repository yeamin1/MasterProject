cat('loading', sep = '\n')
per = function(...)
{
  
  lim = par('usr')
  
  ## initialize and getting the information from the 'Basic' plot
  pushViewport(plotViewport(par('mar')))
  vp = viewport(0.5,0.5,1,1,name = 'VP:PLOTlayout',xscale = lim[1:2], yscale = lim[3:4])
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
  
  ## drawing the axis, here I cheatting the every axis-lines as polygons (drawing two times for every axis rather than one)
  rLength = length(which(yBreak %in% range(yBreak)))
  xRan = which(xBreak %in% range(xBreak))[1:2]
  yRan = which(yBreak %in% range(yBreak))[c(1, rLength)]
  zRan = which(zBreak %in% range(zBreak, na.rm = TRUE))[c(1, 4 + 1)]
  
  ## the order for axis-drawing
  xe.i = rep(c(1,2,1,2,1,2,1,2,  2,2,1,1,1,1,2,2,  2,2,1,1,2,2,1,1), each = 2)
  ye.i = rep(c(1,1,2,2,1,1,2,2,  1,2,1,2,1,2,1,2,  2,2,2,2,1,1,1,1), each = 2)
  ze.i = rep(c(1,1,1,1,2,2,2,2,  1,1,1,1,2,2,2,2,  1,2,1,2,1,2,1,2), each = 2)
  
  ## the index of the points of every axis
  xx = xRan[xe.i]
  yy = yRan[ye.i]
  zz = zRan[ze.i]
  
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
  xCoor = xBreak[c(plot.index, xx)][-dp]
  yCoor = yBreak[c(plot.index, yy)][-dp]
  zCoor = zBreak[c(plot.index, zz)][-dp]
  
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
  grid.polygon(out[,1],out[,2],default.units = 'native',gp = gpar(col =2,fill = 'Blue'),id = grid.id)
  
}


cat('done', sep = '\n')
