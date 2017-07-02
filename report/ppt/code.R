testPersp = function(theta=120, phi = 20, expand = 0.5, col = 'White',
                     box = TRUE, border = 'orange', 
                     ticktype = 'simple', nticks = 5, ...) {
  x = seq(-10,10,length = 30)
  y = seq(-10,10,length = 30)
  f <- function(x, y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
  z <- outer(x, y, f)
  z[is.na(z)] <- 1
  par(mar = c(2,2,2,2))
  persp(x, y, z, theta = theta, 
        phi = phi, expand = expand, 
        col = col, box = box, border = border, 
        ticktype = ticktype, nticks = nticks, ...)
  
}
library(gridGraphics)


col = rgb(0, 191, 255, maxColorValue = 255)
trans = testPersp(col = col, border = 'white')

grid.echo()
lobj = grid.ls(print = FALSE)
obj = grid.get(lobj$name[16])
col = obj$gp$fill
col[500] = 'red'
col[1:300] = rgb(0, 190, 255, maxColorValue = 255)
grid.edit(lobj$name[16], gp = gpar(fill = col))




pdf('persp_diff_%d.pdf', onefile = FALSE)
dev.control("enable")

dev.off()
