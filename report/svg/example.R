Volcano.contour.full = function()
{
  x <- 10*1:nrow(volcano)
  y <- 10*1:ncol(volcano)
  filled.contour(x, y, volcano, color = terrain.colors,
                 plot.title = title(main = "The Topography of Maunga Whau",
                                    xlab = "Meters North", ylab = "Meters West"),
                 plot.axes = { axis(1, seq(100, 800, by = 100))
                   axis(2, seq(100, 600, by = 100)) },
                 key.title = title(main = "Height\n(meters)"),
                 key.axes = axis(4, seq(90, 190, by = 10)))
}
Volcano.contour = function()
{
  x <- 10*1:nrow(volcano)
  y <- 10*1:ncol(volcano)
  filled.contour(x, y, volcano, color = terrain.colors, axes = FALSE)
}





Volcano.persp.svg = function(theta = 110, phi = 60)
{
  z <- 2 * volcano
  x <- 10 * (1:nrow(z))
  y <- 10 * (1:ncol(z))
  z0 <- min(z) - 20
  z <- rbind(z0, cbind(z0, z, z0), z0)
  x <- c(min(x) - 1e-10, x, max(x) + 1e-10)
  y <- c(min(y) - 1e-10, y, max(y) + 1e-10)
  fill <- matrix("green3", nrow = nrow(z)-1, ncol = ncol(z)-1)
  fill[ , i2 <- c(1,ncol(fill))] <- "green"
  fill[i1 <- c(1,nrow(fill)) , ] <- "green"
  fcol <- fill
  zi <- volcano[ -1,-1] + volcano[ -1,-61] +
    volcano[-87,-1] + volcano[-87,-61]  ## / 4
  fcol[-i1,-i2] <-
    terrain.colors(20)[cut(zi,
                           stats::quantile(zi, seq(0,1, length.out = 21)),
                           include.lowest = TRUE)]
  persp(x, y, 2*z, theta = theta, phi = phi, col = fcol, scale = FALSE,
        ltheta = -120, shade = 0.4, border = NA, box = FALSE)
  title('Maungawhau', col.main = 'red') ## on hide
  title('Maungawhau', col.main = 'black')
}

Volcano.persp = function()
{
    par(mar = c(0, 0, 2, 0))
    Volcano.persp.svg(theta = 0, phi = 60)
}


## persp() + filled.contour()
windows(7, 9.708333)
grid.newpage()
pushViewport(viewport(y=1, height=.5, just="top", width = 1))
grid.echo(Volcano.persp, newpage=FALSE)
upViewport()

pushViewport(viewport(x = 0.56, y=0.25, height=.5, width = 1))

grid.echo(Volcano.contour, newpage=FALSE)
# grid.edit('graphics-plot-1-rect-1', gp = gpar(fill = 'white'))
# grid.edit('graphics-plot-1-box-1', gp = gpar(fill = 'white'))
downViewport('graphics-plot-1')
#grid.rect(gp = gpar(fill = 'white', col = 'white'))
upViewport()

pdf('test.pdf', width = 7, height = 9.708333)
dev.off()



## hyperlink
Volcano.persp.svg()

grid.echo()
grid.hyperlink('graphics-plot-1-main-1', 'https://en.wikipedia.org/wiki/Maungawhau')
grid.export()

###
library(gridSVG)
library(gridGraphics)

x = seq(-10,10,length = 36)
y = seq(-10,10,length = 36)
f <- function(x, y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
z <- outer(x, y, f)
z[is.na(z)] <- 1

#source('C:/Users/yeamin/Desktop/master/gridGraphics_script/loading.R')

#setwd('C:/Users/yeamin/Desktop/master/MasterProject/report/svg') ## home
setwd('C:/Users/yeamin/Desktop/mproject/MasterProject/report/svg') ##uni

surface = function(){
  alpha = 1
  border = rgb(139/255,	136/255, 120/255)
  trans = persp(x, y, z, theta = 20,
                phi = 20, expand = 0.5,  
                col = rgb(255/255,	250/255,	205/255), box = TRUE, border = border,col.axis = 'red', ticktype = 'detail',
                col.lab = 'red', shade = 0.5)
  par(new = TRUE)
  trans = persp(x, y, z, theta = 20,
                phi = 20, expand = 0.5,  
                col = rgb(255/255,	250/255,	205/255, alpha = alpha), box = FALSE, border = border,col.axis = 'red', ticktype = 'detail',
                col.lab = 'red')
  title('A sinc surface')
  grid.echo()
  
  nx = length(x) - 1; ny = length(y) - 1
  nz = nx * ny
  
  orderTemp = cbind(rep(x[-1], ny), rep(y[-1],each = nx), 0, 1) %*% trans 
  zdepth = orderTemp[, 4]
  a = order(zdepth, decreasing = TRUE)
  zz = z[1:nx, 1:ny]
  
  
  displays = paste('Value of z:', round(as.numeric(zz[a]), 4))
  ## display the value of z
  grid.text(displays, x = 0.75, y = 0.7, rot = -3.5,
            gp=gpar(fontface="italic",  col = rgb(0,191/255,255/255, alpha = 0)), name = 'labels')
}

surface()
## more actions
grid.text('Reset',x = 0.1, y = 0.93, just = c('left', 'top'), name = 'reset')
grid.text('Shading',x = 0.1, y = 0.9, just = c('left', 'top'), name = 'shading')
grid.text('Change opacity',x = 0.1, y = 0.87, just = c('left', 'top'), name = 'alpha')
grid.text('Chagne color',x = 0.1, y = 0.84, just = c('left', 'top'), name = 'color')


grid.rect(x = c(0.1, 0.13, 0.16, 0.19, 0.22), y = rep(0.81, 5), width = 0.02, height = 0.02, 
          just = c('left', 'top'), gp = gpar(fill = c('#FF4500', '#00008B', '#D3D3D3', '#F8F8FF', '#9932CC'), col = NA),
          name = 'color')

grid.script(file="example.js")
grid.export("example.svg", strict = FALSE)


##output
pdf("Rplot_2_%0d.pdf", onefile=FALSE)
dev.control("enable")

dev.off()


Volcano = function() {
  theta = 110; phi = 60
  z <- 2 * volcano
  x <- 10 * (1:nrow(z))
  y <- 10 * (1:ncol(z))
  z0 <- min(z) - 20
  z <- rbind(z0, cbind(z0, z, z0), z0)
  x <- c(min(x) - 1e-10, x, max(x) + 1e-10)
  y <- c(min(y) - 1e-10, y, max(y) + 1e-10)
  fill <- matrix("green3", nrow = nrow(z)-1, ncol = ncol(z)-1)
  fill[ , i2 <- c(1,ncol(fill))] <- "green"
  fill[i1 <- c(1,nrow(fill)) , ] <- "green"
  fcol <- fill
  zi <- volcano[ -1,-1] + volcano[ -1,-61] +
    volcano[-87,-1] + volcano[-87,-61]  ## / 4
  fcol[-i1,-i2] <-
    terrain.colors(20)[cut(zi,
                           stats::quantile(zi, seq(0,1, length.out = 21)),
                           include.lowest = TRUE)]
  par(bg = "slategray")
  trans = persp(x, y, 2*z, theta = theta, phi = phi, col = fcol, scale = FALSE,
              ltheta = -120, shade = 0.4, border = NA, box = FALSE)
  grid.echo()
  
  nx = length(x) - 1; ny = length(y) - 1
  nz = nx * ny
  
  orderTemp = cbind(rep(x[-1], ny), rep(y[-1],each = nx), 0, 1) %*% trans 
  zdepth = orderTemp[, 4]
  a = order(zdepth, decreasing = TRUE)
  zz = z[1:nx, 1:ny]
  
  
  displays = paste('value of z:', round(as.numeric(zz[a]), 4))
  ## display the value of z
  grid.text(displays, x = 0.1, y = 0.01, just=c("left", "bottom"),
            gp=gpar(fontface="italic",  col = rgb(0,191/255,255/255, alpha = 0)), name = 'labels')

}

#source('C:/Users/yeamin/Desktop/mproject/gridGraphics_script/loading.R')
setwd('C:/Users/yeamin/Desktop/mproject/MasterProject/report/svg') ##uni
Torus()
grid.text('alpha~~',x = 0.1, y = 0.9, just = c('left', 'top'), name = 'alpha')
grid.text('chagne color',x = 0.1, y = 0.88, just = c('left', 'top'), name = 'change')

grid.rect(x = c(0.1, 0.13, 0.16, 0.19, 0.22), y = rep(0.85, 5), width = 0.02, height = 0.02, 
          just = c('left', 'top'), gp = gpar(fill = c('red', 'blue', 'green', 'yellow', 'purple'), col = NA),
          name = 'color')

grid.script(file="example.js")
grid.export("example_tours.svg", strict = FALSE)


