xlim = range(x); ylim = range(y); zlim = range(z, na.rm = TRUE)
per.box = function(xlim,ylim,zlim, pmat)
{
	# points on edges (x,y,z)
	Vertex <- matrix(ncol = 3, byrow = TRUE, data = c(
		1, 1, 1,  #xlim[1], ylim[1], zlim[1]
		1, 1, 2,  #xlim[1], ylim[1], zlim[2]
		1, 2, 1,
		1, 2, 2,
		2, 1, 1,
		2, 1, 2,
		2, 2, 1,
		2, 2, 2))

	# the points of Vertex belonging to a face
	Face  <- matrix (ncol = 4, byrow = TRUE, data = c(
		1, 2, 6, 5,
		3, 7, 8, 4,
		1, 3, 4, 2,
		5, 6, 8, 7,
		1, 5, 7, 3,
		2, 4, 8, 6  ))
		
		Near <- vector(length = 6)
		o1 = o2 = o3 = o4 = numeric(0)
		for (i in 1:6) {
			p <- Face[i, ]

			pt <- Vertex[p[1], ]
			u1 <- c(xlim[pt[1]] , ylim[pt[2]], zlim[pt[3]], 1)

			pt <- Vertex[p[2], ]
			u2 <- c(xlim[pt[1]] , ylim[pt[2]], zlim[pt[3]], 1)

			pt <- Vertex[p[3], ]
			u3 <- c(xlim[pt[1]] , ylim[pt[2]], zlim[pt[3]], 1)

			pt <- Vertex[p[4], ]
			u4 <- c(xlim[pt[1]] , ylim[pt[2]], zlim[pt[3]], 1)

			## return the points of drawing edges
			o1 = c(o1,u1[1:3])
			o2 = c(o2,u2[1:3])
			o3 = c(o3,u3[1:3])
			o4 = c(o4,u4[1:3])

			v1 <- u1 %*% pmat
			v2 <- u2 %*% pmat
			v3 <- u3 %*% pmat
			v4 <- u4 %*% pmat

			dd <- v2/v2[4] - v1/v1[4]
			ee <- v3/v3[4] - v2/v2[4]

			Near[i] <- (dd[1]*ee[2] - dd[2]*ee[1]) < 0
		}
		out = list(Near = Near, O = c(o1, o2, o3, o4))
		out
}