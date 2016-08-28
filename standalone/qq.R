#setwd('H:/Documents/mproject/MasterProject-master')
#setwd('C:/Users/yeamin/Desktop/mproject/MasterProject') ##at uni
#setwd('H:/Documents/MasterProject-master/MasterProject') ## at home
setwd('H:/Documents/MasterProject-master/MasterProject/standalone')

x = seq(-10,10,length = 100)
y = seq(-10,10,length = 100)
f <- function(x, y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
z <- outer(x, y, f)
z[is.na(z)] <- 1

source('loading.R')
par(mar = c(0,0,0,0))
trans <- persp(x, y, z, theta = 20, 
               phi = 30, expand = 0.5, 
               col = "orange", box = TRUE)
plot = recordPlot()
plotInfo = perInit(plot)
per(plot = plotInfo)
perFinal()



perInit(newpage = TRUE, x = x, y = y, z = z, dbox = TRUE)
per(plot = plot)
perFinal()


##axis
source('tmp.R')
PerspAxis(x = range(x), y = range(y), z = range(z, na.rm = TRUE), dd = 1,
          axisType = 1, nTicks = 5, tickType = 1, label = 'x')

source('tmp.R')
PerspAxes(x, y, z, xlab = 'x', xenc = 5, ylab = 'y', yenc = 5, zlab = 'z', zenc = 5, 
          nTicks = 5, tickType = 1, pGEDevDesc = 1, dd = 0)
