library(gridGraphics)

testPersp <- function(theta=120, phi = 20, expand = 0.5, col = 'White',
                        box = TRUE, border = 'orange', 
                        ticktype = 'simple', nticks = 5, ...) {
    x = seq(-10,10,length = 100)
    y = seq(-10,10,length = 100)
    f <- function(x, y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
    z <- outer(x, y, f)
    z[is.na(z)] <- 1
    par(mar = c(2,2,2,2))
    trans <- persp(x, y, z, theta = theta, 
                   phi = phi, expand = expand, 
                   col = col, box = TRUE, border = border, 
                   ticktype = ticktype, nticks = nticks, ...)
                   
    trans
}

echoTest <- function(trans = trans) {
    plot = recordPlot()
    plotInfo = perInit(plot, trans = trans, newpage = FALSE)
    per(plot = plotInfo)
    perFinal()
}




## test on theta
trans = testPersp(15)
echoTest(trans)

trans = testPersp(60)
echoTest(trans)

trans = testPersp(120)
echoTest(trans)

trans = testPersp(160)
echoTest(trans)

trans = testPersp(220)
echoTest(trans)

trans = testPersp(280)
echoTest(trans)

trans = testPersp(350)
echoTest(trans)


## test on phi
trans = testPersp(phi = 15)
echoTest(trans)

trans = testPersp(phi = 22.5)
echoTest(trans)

trans = testPersp(phi = 50)
echoTest(trans)

trans = testPersp(phi =80)
echoTest(trans)

trans = testPersp(phi = 120)
echoTest(trans)

trans = testPersp(phi =310)
echoTest(trans)

trans = testPersp(phi = 720)
echoTest(trans)


## test on expand
trans = testPersp(expand = 0.01)
echoTest(trans)

trans = testPersp(expand = 0.2)
echoTest(trans)

trans = testPersp(expand = 0.35)
echoTest(trans)

trans = testPersp(expand = 2)
echoTest(trans)

trans = testPersp(expand = 5)
echoTest(trans)

trans = testPersp(expand = 10)
echoTest(trans)

trans = testPersp(expand = 100)
echoTest(trans)

## test on col
trans = testPersp(col = 1)
echoTest(trans)

trans = testPersp(col = 2)
echoTest(trans)

trans = testPersp(col = 5)
echoTest(trans)

trans = testPersp(col = 7)
echoTest(trans)

trans = testPersp(col = 9)
echoTest(trans)

trans = testPersp(col = 10)
echoTest(trans)

trans = testPersp(col = 290)
echoTest(trans)

trans = testPersp(col = 1)
echoTest(trans)

trans = testPersp(col = 1)
echoTest(trans)

trans = testPersp(col = 1)
echoTest(trans)

trans = testPersp(col = 1)
echoTest(trans)

trans = testPersp(col = 1)
echoTest(trans)

trans = testPersp(col = 1)
echoTest(trans)




