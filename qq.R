library(grid)
## subsetting the information from the example, but I am not using this as example, but I will create a new 3-d plot
#example(persp)
#x <- recordPlot()
#info = x[[1]][[3]][[2]]
#x = unlist(info[2])
#y = unlist(info[3])
#z = info[4][[1]]
##polygon version
#setwd('H:/Documents/mproject')
#setwd('C:/Users/yeamin/Desktop/project/mproject')

source('function.r')

x = seq(-10,10,length = 55)
y = seq(-10,10,length = 55)
f <- function(x, y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
z <- outer(x, y, f)

par(mar = c(0,0,0,0))
system.time(trans <- persp(x, y, z, theta = 20, phi = 30, expand = 0.5, col = "NA"))

system.time(per(x,y,z))




