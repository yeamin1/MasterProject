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
  par(mar = c(0, 0, 0, 0))
  par(bg = "slategray")
  x <- 10*1:nrow(volcano)
  y <- 10*1:ncol(volcano)
  filled.contour(x, y, volcano, color = terrain.colors, axes = FALSE)
}





Volcano.persp.svg = function()
{
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
  persp(x, y, 2*z, theta = theta, phi = phi, col = fcol, scale = FALSE,
        ltheta = -120, shade = 0.4, border = NA, box = FALSE)
  title('Maungawhau', col.main = 'red') ## on hide
  title('Maungawhau', col.main = 'black')
}

## persp() + filled.contour()
windows(7, 9.708333)
grid.newpage()
pushViewport(viewport(y=1, height=.5, just="top", width = 1))
grid.echo(Volcano.persp, newpage=FALSE)
upViewport()

pushViewport(viewport(x = 0.56, y=0.25, height=.35, width = 0.88))

grid.echo(Volcano.contour, newpage=FALSE)
# grid.edit('graphics-plot-1-rect-1', gp = gpar(fill = 'white'))
# grid.edit('graphics-plot-1-box-1', gp = gpar(fill = 'white'))
downViewport('graphics-plot-1')
grid.rect(gp = gpar(fill = 'slategray', col = 'slategray'))
upViewport()


## hyperlink
Volcano.persp.svg()

grid.echo()
grid.hyperlink('graphics-plot-1-main-1', 'https://en.wikipedia.org/wiki/Maungawhau')
grid.export()

###
library(gridSVG)

x = seq(-10,10,length = 20)
y = seq(-10,10,length = 20)
f <- function(x, y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
z <- outer(x, y, f)
z[is.na(z)] <- 1

source('C:/Users/yeamin/Desktop/master/gridGraphics_script/loading.R')

setwd('C:/Users/yeamin/Desktop/master/MasterProject/report/svg')
trans = persp(x, y, z, theta = 20,
              phi = 20, expand = 0.5,  
              col = 'purple', box = TRUE, border = 'orange',col.axis = 'red', ticktype = 'detail',
              col.lab = 'red')
par(new = TRUE)
trans = persp(x, y, z, theta = 20,
              phi = 20, expand = 0.5,  
              col = 'orange', box = TRUE, border = 'White',col.axis = 'red', ticktype = 'detail',
              col.lab = 'red')
title('clickme~~')
grid.echo()

#nx = length(x) - 1; ny = length(y) - 1
#nz = nx * ny
#display = paste('value of z:', round(as.numeric(z[1:nx, 1:ny]), 4))
#labels = paste('label.', 1:nz, sep = '')
## display the value of z
#for (i in 1:nz) {
##  grid.text(display[i], x = 0.1, y = 0.01, just=c("left", "bottom"),
#            gp=gpar(fontface="bold.italic", col = 'orange'), name = labels[i])
#}


#for (i in 1:nz) {
#  grid.garnish(paste("label.", i, sep=""),
#               visibility="hidden")
#}

grid.script(file="example.js")
grid.export("example.svg", strict = FALSE)



