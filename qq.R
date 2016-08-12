##school
#setwd('H:/Documents/MasterProject-master')
##home
setwd('C:/Users/yeamin/Desktop/master/MasterProject')
##laptop
#setwd('C:/Users/yeamin/Desktop/project/mproject')


x = seq(-10,10,length = 55)
y = seq(-10,10,length = 55)
f <- function(x, y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
z <- outer(x, y, f)


source('function.r')
par(mar = c(0,0,0,0))
trans <- persp(x, y, z, theta = 20, phi = 30, expand = 0.5, col = "white", box = TRUE)
system.time(per(newpage = FALSE))
system.time(per(x = x, y = y, z = z, grid.newpage = TRUE,))




a = trans3d(xR,yR,zR,trans)
plot(a$x, a$y)
