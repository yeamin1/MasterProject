testPersp = function(theta=120, phi = 20, expand = 0.5, col = 'White',
                        box = TRUE, border = 'orange', 
                        ticktype = 'simple', nticks = 5, ...) {
    x = seq(-10,10,length = 60)
    y = seq(-10,10,length = 60)
    f <- function(x, y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
    z <- outer(x, y, f)
    z[is.na(z)] <- 1
    par(mar = c(2,2,2,2))
    trans <- persp(x, y, z, theta = theta, 
                   phi = phi, expand = expand, 
                   col = col, box = box, border = border, 
                   ticktype = ticktype, nticks = nticks, ...)
                   
    trans
}

echoTest = function(trans = trans) {
    plot = recordPlot()
    plotInfo = perInit(plot, trans = trans, newpage = TRUE)
    per(plot = plotInfo)
    grid.text('gird', x = unit(0.01, 'npc'), y = unit(0.9, 'npc'))
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

## test on lim
	trans = testPersp(xlim = c(-5,5),col = 2)
	echoTest(trans)

	trans = testPersp(ylim = c(-5,5))
	echoTest(trans)

	trans = testPersp(zlim = c(-5,5))
	echoTest(trans)

## test on label
	trans = testPersp(xlab = 'xx', ylab = 'yy', zlab = 'zz')
	echoTest(trans)

## test on r
	trans = testPersp(r = 5)
	echoTest(trans)

	trans = testPersp(r = 10)
	echoTest(trans)

	trans = testPersp(r = 20)
	echoTest(trans)

##test on d
	trans = testPersp(d = 0.5)
	echoTest(trans)

	trans = testPersp(d = 0)
	echoTest(trans)

	trans = testPersp(d = 1)
	echoTest(trans)

## test on scale
	trans = testPersp(scale = FALSE)
	echoTest(trans)

	trans = testPersp(scale = TRUE)
	echoTest(trans)

## test on expand
	trans = testPersp(expand = 0.5)
	echoTest(trans)

	trans = testPersp(expand = 1)
	echoTest(trans)

	trans = testPersp(expand = 0.25)
	echoTest(trans)

	trans = testPersp(d = 2)
	echoTest(trans)

## test on col
	trans = testPersp(col = 1)
	echoTest(trans)

	trans = testPersp(col = 'blue')
	echoTest(trans)

	trans = testPersp(col = 1:5)
	echoTest(trans)

	trans = testPersp(col = 1:10)
	echoTest(trans)

	trans = testPersp(col = 1:100)
	echoTest(trans)

## test on border
	trans = testPersp(border = 1)
	echoTest(trans)

	trans = testPersp(border = 'brown')
	echoTest(trans)

	trans = testPersp(border = 5:6)
	echoTest(trans)

## test on axes
	trans = testPersp(axes = TRUE)
	echoTest(trans)

	trans = testPersp(axes = FALSE)
	echoTest(trans)

## if box = False then not drawing any axes even axes = TRUE
	trans = testPersp(box = FALSE, axes = TRUE)
	echoTest(trans)

	trans = testPersp(box = FALSE, axes = FALSE)
	echoTest(trans)

## test on ticktype
	trans = testPersp(ticktype = 'detail', axes = TRUE, box = FALSE)
	echoTest(trans)
