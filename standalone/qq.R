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
ltheta = -135; lphi = 90
trans = persp(tx, ty, tz, col = 'grey', shade = 0.8, phi = 30, theta = 40, ltheta = ltheta, lphi = lphi, scale = TRUE)



source('random/shade.R')
source('loading.R')
trans = testPersp(shade = 0.5)
plot = recordPlot()
plotInfo = perInit(plot, trans = trans, newpage = TRUE)
per(plot = plotInfo)


##n = 16
25