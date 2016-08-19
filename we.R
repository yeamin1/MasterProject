boxInfo = per.box(xlim,ylim,zlim,trans)
boxPoints = boxInfo$O

## first case
initial = rep(rep(1:3,4) + rep((0:3) * length(boxInfo$O[,1]), each = 3), 6)
## movement  
move = rep(seq(0,by = 3, length = 6), each = 12)
projection = initial + move

a = boxPoints[projection]
bpoints = matrix(a, nc = 3, byrow = TRUE)

e = trans3d(bpoints[,1], bpoints[,2], bpoints[,3], trans)
box.id = rep(1:6, each = 4)
bout = cbind(x = e$x, y = e$y, box.id, order = rep(boxInfo$Near, each = 4))
