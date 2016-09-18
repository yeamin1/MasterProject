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
               col = "NA", box = TRUE, border = 'NA', 
               ticktype = 'simple', nticks = 10)
plot = recordPlot()
plotInfo = perInit(plot, trans, newpage = TRUE)
per(plot = plotInfo)
perFinal()


f()

## just make the test easilier
f = function()
{
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
                 ticktype = 'detailed', nticks = 5)
  
  plot = recordPlot()
  plotInfo = perInit(plot, trans, newpage = TRUE)
  per(plot = plotInfo)
  perFinal()
}

windows()
system.time(f())

f()
a



source('loading.R')
plot(0,0, xlim = c(-0.5, 0.5), ylim = c(-0.5, 0.5))
PerspBox(1, x = range(x), y = range(y), z = range(z), VT = trans)
