#setwd('H:/Documents/mproject/MasterProject-master')
setwd('C:/Users/yeamin/Desktop/mproject/MasterProject/standalone/') ##at uni
#setwd('H:/Documents/MasterProject-master/MasterProject') ## at home
#setwd('H:/Documents/MasterProject-master/MasterProject/standalone')



x = seq(-10,10,length = 100)
y = seq(-10,10,length = 100)
f <- function(x, y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
z <- outer(x, y, f)
z[is.na(z)] <- 1

## main example
source('loading.R')
par(mar = c(2,2,2,2))
trans <- persp(x, y, z, theta = 120, 
               phi = 30, expand = 0.5, 
               col = c('White','blue','brown','purple'), box = TRUE, border = 'white', 
               ticktype = 'detail', nticks = 10, xlab = 'bilibili', ylab = 'dilidili', r = 5, scale = FALSE, ltheta = 1, axes = FALSE)
plot = recordPlot()
plotInfo = perInit(plot, trans, newpage = TRUE)
per(plot = plotInfo)
perFinal()
