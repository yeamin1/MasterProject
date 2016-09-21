XRotate = function(angle)
{
	rad = angle*180/ pi
	c = cos(rad)
	s = sin(rad)
	T = rbind(c(1, 0, 0, 0),
			  c(0, c, -s, 0),
			  c(0, s, c, 0),
			  c(0, 0, 0, 0)
			  )
	T
}


YRotate = function(angle)
{
	rad = angle*180/ pi
	c = cos(rad)
	s = sin(rad)
	T = rbind(c(c, 0, s, 0),
			  c(0, 1, 0, 0),
			  c(-s, 0, c, 0),
			  c(0, 0, 0, 0)
			  )
	T
}

ZRotate = function(angle)
{
	rad = angle*180/ pi
	c = cos(rad)
	s = sin(rad)
	T = rbind(c(c, -s, 0, 0),
			  c(s, c, 0, 0),
			  c(0, 0, 1, 0),
			  c(0, 0, 0, 0)
			  )
	T
}

SetUpLight = function(theta, phi)
{
	u = c(0, -1, 0, 1)
	VT = XRotate(-phi) %*% ZRotate(theta)
	Light = TransVector(u, VT)
	Light
}


FacetShade = function(u, v, theta, phi)
{
	Light = SetUpLight(theta, phi)
	
	nx = u[2] * v[3] - u[3] * v[2]
	ny = u[3] * v[1] - u[1] * v[3]
	nz = u[1] * v[2] - u[2] * v[1]
	sum = sqrt(nx * nx + ny * ny + nz * nz)
	if (sum == 0) sum = 1
	nx = nx/sum
	ny = ny/sum
	nz = nz/sum
	sum = 0.5 * (nx * Light[1] + ny * Light[2] + nz * Light[3] + 1)
	
	sum^2
	
}


DrawFacets = function(z, x, y, nx, ny,
		       indx, xs, ys, zs, theta, phi,
		       col, ncol, border)
{
	shade = 0
	v = u = rep(0,4)
	nx1 = nx - 1
    ny1 = ny - 1
    n = nx1 * ny1
	
	for (k in 1:n) {
	nv = 1
	i = indx[k] %% nx1 + 1
	j = indx[k] / nx1
	icol = (i + j * nx1) %% ncol
	    u[1] = xs * (x[i+1] - x[i])
	    u[2] = ys * (y[j] - y[j+1])
	    u[3] = zs * (z[(i+1)+j*nx] - z[i+(j+1)*nx])
	    v[1] = xs * (x[i+1] - x[i])
	    v[2] = ys * (y[j+1] - y[j])
	    v[3] = zs * (z[(i+1)+(j+1)*nx] - z[i+j*nx])
		
		print((i+1)+(j+1)*nx)
		
	    shade = FacetShade(u, v, theta, phi)
	u[1] = x[i]; u[2] = y[j]
	u[3] = z[i + j * nx]; u[4] = 1
	#if (R_FINITE(u[0]) &&  R_FINITE(u[1]) && R_FINITE(u[2])) {
	    V = TransVector(u, VT)
	    xx[nv] = v[1] / v[4]
	    yy[nv] = v[2] / v[4]
	    nv = nv + 1
	#}

	u[1] = x[i + 1]; u[2] = y[j]
	u[3] = z[i + 1 + j * nx]; u[4] = 1
	#if (R_FINITE(u[0]) &&  R_FINITE(u[1]) && R_FINITE(u[2])) {
	    v = TransVector(u, VT)
	    xx[nv] = v[1] / v[4]
	    yy[nv] = v[2] / v[4]
	    nv = nv + 1
	#}

	u[1] = x[i + 1]; u[2] = y[j + 1]
	u[3] = z[i + 1 + (j + 1) * nx]; u[4] = 1
	#if (R_FINITE(u[0]) &&  R_FINITE(u[1]) && R_FINITE(u[2])) {
	    v = TransVector(u, VT)
	    xx[nv] = v[1] / v[4]
	    yy[nv] = v[2] / v[4]
	    nv = nv + 1;
	#}

	u[1] = x[i]; u[2] = y[j + 1]
	u[3] = z[i + (j + 1) * nx]; u[4] = 1
	#if (R_FINITE(u[0]) &&  R_FINITE(u[1]) && R_FINITE(u[2])) {
	    v = TransVector(u, VT)
	    xx[nv] = v[1] / v[4]
	    yy[nv] = v[2] / v[4]
	    nv = nv + 1
	#}

	if (nv > 3) {
	    newcol = col[icol]
	    if (DoLighting) {
				newcol = R_RGB(newcol * shade, newcol * shade, newcol * shade);
	    }
	}
	newcol
	}
}

#z, x, y, nx, ny,
#		       indx, xs, ys, zs, theta, phi,
#		       col, ncol, border

# indx = (nrow(z) - 1)*(ncol(z) - 1)
# DrawFacets(z = z, x = x, y = y, nx = nrow(z), 
#				ny = ncol(z), indx = indx, 
#				xs = 1/0.5, ys = 1/0.5, zs = 0.5/0.5, 
#				theta = 60, phi = 30, 
#				col = rep('red' , nrow(z) * ncol(z)), 
#				ncol = length(col), border = border[1])
