#setwd('H:/Documents/mproject/MasterProject-master')
setwd('C:/Users/yeamin/Desktop/mproject/MasterProject/standalone/') ##at uni
#setwd('H:/Documents/MasterProject-master/MasterProject') ## at home
#setwd('H:/Documents/MasterProject-master/MasterProject/standalone')
setwd('H:/Documents/MasterProject-master/MasterProject-0.0000001/standalone')


x = seq(-10,10,length = 100)
y = seq(-10,10,length = 100)
f <- function(x, y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
z <- outer(x, y, f)
z[is.na(z)] <- 1

## main example
source('loading.R')
windows()
par(mar = c(2,2,2,2))
trans <- persp(x, y, z, theta = 120, 
               phi = 30, expand = 0.5, 
               col = c('White','blue','brown','purple'), box = TRUE, border = 'white', 
               ticktype = 'detail', nticks = 10, xlab = 'xx', ylab = 'yy', zlab = 'zz', r = 5, scale = FALSE, 
              ltheta = 1, axes = TRUE, main = 'wiefjoijoiwejf')
plot = recordPlot()
plotInfo = perInit(plot, trans, newpage = TRUE)
per(plot = plotInfo)
perFinal()


windows()
vp = viewport(0.5,0.5)
pushViewport(vp)
grid.rect(gp = gpar(col = 'red'))

grid.ls()

