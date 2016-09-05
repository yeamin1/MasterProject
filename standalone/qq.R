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
trans <- persp(x, y, z, theta = 20, 
               phi = 30, expand = 0.5, 
               col = "orange", box = TRUE, border = 'black', 
               ticktype = 'detailed')

plot = recordPlot()
plotInfo = perInit(plot, trans, newpage = FALSE)
per(plot = plotInfo)
perFinal()


##'white' example
trans <- persp(x, y, z, theta = 20, 
                phi = 30, expand = 0.5, 
                col = "orange", box = FALSE)
plot = recordPlot()
plotInfo = perInit(plot, trans, newpage = FALSE)
per(plot = plotInfo)
perFinal()


##null
axisTicks(range, FALSE, axp = axp, 6)
