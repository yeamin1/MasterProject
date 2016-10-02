SetToIdentity = function(T)
    T <<- diag(1, 4)

    
Accumulate = function(T)
{
    sum = 0
    sum = sum + VT %*% T
    U = sum
    VT <<- U
}
    
Accumulates = function(T)
{
    U = matrix(0, nr = 4, nc = 4)
    for(i in 1:4)
    for(i in 1:4) {
	for(j in 1:4) {
	    sum = 0;
	    for(k in 1:4)
            sum = sum + VT[i, k] * T[k, j];
	    U[i, j] = sum;
        print(sum)
	}
    }
    for(i in 1:4)
	for(j in 1:4)
	    VT[i, j] <<- U[i, j];
}
    
XRotate = function(angle)
{
    SetToIdentity(T)
    rad = angle * pi / 180
    c = cos(rad)
    s = sin(rad)
    T = rbind(c(1, 0, 0, 0),
              c(0, c, s, 0),
              c(0, -s, c, 0),
              c(0, 0, 0, 1))
    
    Accumulate(T)
}

YRotate = function(angle)
{
    SetToIdentity(T)
    rad = angle * pi / 180
    c = cos(rad)
    s = sin(rad)
    T = rbind(c(c, 0, -s, 0),
              c(0, 1, 0, 0),
              c(s, 0, c, 0),
              c(0, 0, 0, 1))
    
    Accumulate(T)
}

ZRotate = function(angle)
{
    SetToIdentity(T)
    rad = angle * pi / 180
    c = cos(rad)
    s = sin(rad)
    T = rbind(c(c, s, 0, 0),
              c(-s, c, 0, 0),
              c(0, 0, 1, 0),
              c(0, 0, 0, 1))
    
    Accumulate(T)
}
        
SetUpLight = function(theta, phi)
{
    u = 0
    SetToIdentity(VT)
    XRotate(-phi)
    ZRotate(theta)
    Light <<- TransVector(u, VT)
}


FacetShade = function(u, v)
{
    nx = u[2] * v[3] - u[3] * v[2]
    ny = u[3] * v[1] - u[1] * v[3]
    nz = u[1] * v[2] - u[2] * v[1]
    
    sum = sqrt(nx * nx + ny * ny + nz * nz)
    
    if (sum == 0) sum = 1
    nx = nx/sum
    ny = ny/sum
    nz = nz/sum
    
    sum = 0.5 * (nx * Light[1] + ny * Light[2] + nz * Light[3] + 1)
    
    sum^Shade   
}

DrawFacets = function(z, x, y, nx, ny, index, xs, ys, zs, col, ncol, border)
{

    nx1 = nx - 1
    ny1 = ny - 1
    cols = 0
    n = nx1 * ny1
    for(k in 1:n){
        nv = 0
        i = indx[k] %% nx1 + 1
        j = indx[k] / nx1 + 1
        icol = (i + j * nx1) %% ncol
    
        u[1] = xs * (x[i+1] - x[i])
	    u[2] = ys * (y[j] - y[j+1])
	    u[3] = zs * (z[(i+1)+j*nx] - z[i+(j+1)*nx])
	    v[1] = xs * (x[i+1] - x[i])
	    v[2] = ys * (y[j+1] - y[j])
	    v[3] = zs * (z[(i+1)+(j+1)*nx] - z[i+j*nx])
	    shade = FacetShade(u, v)
        
        if(nv > 2)
        {
            newcol = col[icol]
            cols[k] = rgb(shade * newcol, shade * newcol, shade * newcol)
        }
        
    }
    cols

}