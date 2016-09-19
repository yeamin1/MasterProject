dBox = function(boxInfo, pMax){
    frontCount = 6 - sum(boxInfo$Near)
    boxPoints = boxInfo$O

    ## first case
    initial <<- rep(rep(1:3,4) + rep((0:3) * (length(boxInfo$O)/4), each = 3), 6)
    ## movement  
    move <<- rep(seq(0,by = 3, length = 6), each = 6)
    
    projection = initial + move
    
    boxPolygon = boxPoints[projection]
    bpoints = matrix(boxPolygon, nc = 3, byrow = TRUE)

    ## trans to 2d
    e = trans3d(bpoints[,1], bpoints[,2], bpoints[,3], trans)
    box.id = rep(1:6, each = 4)

    ##figure out which faces are front/behind.
    bPoint = cbind(x = e$x, y = e$y)
    bOrder = rep(boxInfo$Near, each = 4)

    bfront = bPoint[bOrder == 1, ]
    bbehind = bPoint[bOrder == 0, ]

    boxF.id = rep(1:frontCount, each = 4)	
    boxB.id = rep((pMax + 1):(pMax + 6 - frontCount), each = 4)

    bout <<- list(bfront = bfront, bbehind = bbehind, 
                boxF.id = boxF.id, boxB.id = boxB.id, frontCount = frontCount)
    bout
} 
