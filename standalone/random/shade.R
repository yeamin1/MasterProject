SetToIdentity = function(TT)
    VT <<- diag(1, 4)

    
Accumulate = function(TT)
{
    sum = 0
    sum = sum + VT %*% TT
    U = sum
    VT <<- U
}
    
Accumulates = function(TT)
{
    U = matrix(0, nr = 4, nc = 4)
    for(i in 1:4)
    for(i in 1:4) {
	for(j in 1:4) {
	    sum = 0;
	    for(k in 1:4)
            sum = sum + VT[i, k] * TT[k, j];
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
    SetToIdentity(TT)
    rad = angle * pi / 180
    c = cos(rad)
    s = sin(rad)
    TT = rbind(c(1, 0, 0, 0),
              c(0, c, s, 0),
              c(0, -s, c, 0),
              c(0, 0, 0, 1))
    
    Accumulate(TT)
}

YRotate = function(angle)
{
    SetToIdentity(TT)
    rad = angle * pi / 180
    c = cos(rad)
    s = sin(rad)
    TT = rbind(c(c, 0, -s, 0),
              c(0, 1, 0, 0),
              c(s, 0, c, 0),
              c(0, 0, 0, 1))
    
    Accumulate(TT)
}

ZRotate = function(angle)
{
    SetToIdentity(TT)
    rad = angle * pi / 180
    c = cos(rad)
    s = sin(rad)
    TT = rbind(c(c, s, 0, 0),
              c(-s, c, 0, 0),
              c(0, 0, 1, 0),
              c(0, 0, 0, 1))
    
    Accumulate(TT)
}
        
SetUpLight = function(theta, phi)
{
    u = c(0, -1, 0, 1)
    SetToIdentity(VT)
    XRotate(-phi)
    ZRotate(theta)
    Light <<- TransVector(u, VT)
}


FacetShade = function(u, v, Shade = 0.3)
{
    nx = u[2] * v[3] - u[3] * v[2]
    ny = u[3] * v[1] - u[1] * v[3]
    nz = u[1] * v[2] - u[2] * v[1]
    
    sum = sqrt(nx * nx + ny * ny + nz * nz)
    
    #print(sum)
    if (sum == 0) sum = 1
    nx = nx/sum
    ny = ny/sum
    nz = nz/sum
    
    sum = 0.5 * (nx * Light[1] + ny * Light[2] + nz * Light[3] + 1)
    
    #print(Shade)
    
    sum^Shade   
}

DrawFacets = function(z, x, y, nx, ny, indx = 0:(length(z)), xs = 0, ys = 0, zs = 0, col, ncol, border)
{
    aa = b = 0 ## testing 
    shade = 0
    u = v = 0
    nx1 = nx - 1
    ny1 = ny - 1
    cols = 0
    n = nx1 * ny1
    for(k in 1:n){
        nv = 0
        i = (indx[k]) %% nx1 
        j = (indx[k]) %/% nx1
        icol = (i + j * nx1) %% ncol + 1

        u[1] = xs * (x[i+1+1] - x[i+1])
	    u[2] = ys * (y[j+1] - y[j+1+1])
	    u[3] = zs * (z[(i+1)+j*nx+1] - z[i+(j+1)*nx]+1)
	    v[1] = xs * (x[i+1+1] - x[i+1])
	    v[2] = ys * (y[j+1+1] - y[j+1])
	    v[3] = zs * (z[(i+1)+(j+1)*nx+1] - z[i+j*nx+1])
        
        #print(x)
        
        #u[1] = xs * x[4*(k-1) + 4] - x[4*(k - 1) + 1]
        #u[2] = ys * x[4*(k-1) + 1] - x[4 + 4*(k - 1)]
        #u[3] = zs * (z[(i+1)+j*nx+1] - z[i+(j+1)*nx]+1)
        
        
        #v[1] = xs * x[4*(k-1) + 4] - x[4*(k - 1) + 1]
        #v[2] = ys * x[4*(k-1) + 4] - x[4*(k - 1) + 1]
	    #v[3] = zs * (z[(i+1)+(j+1)*nx+1] - z[i+j*nx+1])

        #print(n)
	    shade = FacetShade(u, v)
        #print(FacetShade(u, v))
        aa[i] <<- shade
        #if(nv > 2)
        #print(icol)
        newcol <<- col2rgb(col[icol]) / 255
        b[i] <<- shade * newcol[1]
        cols[k] = rgb(shade * newcol[1], shade * newcol[2], shade * newcol[3])
        
    }
        cols
}


DepthOrder = function(z, x, y, nx, ny, depth, indx)
{
    nx1 = nx - 1;
    ny1 = ny - 1;
    for(i in 1:(nx1 * ny1))
    indx[i] = i
    for(i in 1:nx1)
    {
        for(j in 1:ny1)
        {
         d = -Inf
         for(ii in 1)
         {
            for(jj in 1)
            {
                u[1] = x[i+ii]
                u[2] = y[j+jj]
                u[3] = 0
                u[4] = 1
                
                v = TransVector(u, VT)
                if(v[3] > d) d = v[3]
                
                }
         }
        depth[i+j*nx1] = -d
        print(i)
        }
    }
    order(depth)
}