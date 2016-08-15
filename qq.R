#library(grid)
## subsetting the information from the example, but I am not using this as example, but I will create a new 3-d plot
#example(persp)
#x <- recordPlot()
#info = x[[1]][[3]][[2]]
#x = unlist(info[2])
#y = unlist(info[3])
#z = info[4][[1]]
##polygon version
#setwd('H:/Documents/MasterProject-master')
#setwd('C:/Users/yeamin/Desktop/project/mproject')
#setwd('H:/Documents/mproject/MasterProject-master')
setwd('C:/Users/yeamin/Desktop/mproject/MasterProject')


x = seq(-10,10,length = 55)
y = seq(-10,10,length = 55)
f <- function(x, y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
z <- outer(x, y, f)
z[is.na(z)] <- 1

source('function.r')
par(mar = c(0,0,0,0))
trans <- persp(x, y, z, theta = 20, 
               phi = 30, expand = 0.5, 
               col = "orange", box = TRUE)
plot = recordPlot()
per(x,y,z, newpage = FALSE)


per(x = x, y = y, z = z, grid.newpage = TRUE)


##polypath
grid.newpage()
vp = plotViewport(mar, xscale = lim[1:2], yscale = lim[3:4])
pushViewport(vp)
n = length(out[,1])
x = out[1:n,1]
y = out[1:n,2]
id = grid.id[1:n]
a = grid.path(x, y, id=id, gp=gpar(fill="orange"), default.units="native", rule = 'winding')
