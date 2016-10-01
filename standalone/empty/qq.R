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
trans = persp(x, y, z, theta = 20, 
               phi = 20, expand = 0.5, 
               col = 'White', box = TRUE, border = 'orange',col.axis = 'red', ticktype = 'detail',
              col.lab = 'red')
plot = recordPlot()
plotInfo = perInit(plot, trans, newpage = TRUE)
per(plot = plotInfo)
perFinal()

windows()
source('loading.R')
trans = testPersp(col.axis = 'blue', ticktype = 'detail', cex.lab = 2)
echoTest(trans)

##testing example
windows()
plot(1,1, col.axis = 'red', cex.lab = 2)
grid.text(1293871293, 0.5, 0.5, gp = gpar(cex = 2))
