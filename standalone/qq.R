#setwd('H:/Documents/mproject/MasterProject-master')
setwd('C:/Users/yeamin/Desktop/mproject/MasterProject/standalone/') ##at uni
setwd('H:/Documents/MasterProject-master/MasterProject') ## at home
setwd('H:/Documents/MasterProject-master/MasterProject/standalone')
setwd('H:/Documents/MasterProject-master/MasterProject-0.0000001/standalone')
setwd('C:/Users/yeamin/Desktop/master/MasterProject/standalone')

x = seq(-10,10,length = 60)
y = seq(-10,10,length = 60)
f <- function(x, y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
z <- outer(x, y, f)
z[is.na(z)] <- 1

## main example
source('loading.R')
#windows()
par(mar = c(2,2,2,2))
trans = persp(x, y, z, theta = 20, xlim = c(-5,5),
               phi = 20, expand = 0.5, 
               col = 'White', box = TRUE, border = 'orange',col.axis = 'red', ticktype = 'detail',
              col.lab = 'red')
plot = recordPlot()
plotInfo = perInit(plot, trans, newpage = TRUE)
per(plot = plotInfo)

windows()
source('loading.R')
trans = testPersp(col.axis = 'blue', ticktype = 'detail', cex.lab = 2)
echoTest(trans)



trans = testPersp(expand = 100)
echoTest(trans)

source('loading.R')
trans = testPersp(xlim = c(-5,5),col = 2)
echoTest(trans)


trans = testPersp(xlim = c(-10,10),col = 2)
echoTest(trans)



###
n = 50
tx = seq(-10,10,length = n)
ty = seq(-10,10,length = n)
f <- function(x, y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
tz <- outer(tx, ty, f)
tz[is.na(tz)] <- 1

windows()
source('loading.R')
ltheta = -135; lphi = 0
trans = persp(tx, ty, tz, col = 'grey', shade = 0.3, phi = 30, theta = 40, ltheta = ltheta, lphi = lphi, scale = TRUE)
plot = recordPlot()
plotInfo = perInit(plot, trans, newpage = TRUE)
per(plot = plotInfo)

source('random/shade.R')
Light = SetUpLight(theta = ltheta,phi =lphi)
colll = DrawFacets(tz, tx, ty, n, n, indx = 0:(length(tz) - 1), 1, 1, 1, col = 'grey', ncol = length(col), Light = Light)
per(plot = plotInfo)

SetUpLight(theta = 30,phi =40)

2353

