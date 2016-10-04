SetToIdentity = function(){
    diag(1, 4)
}

    
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
    TT = SetToIdentity()
    rad = angle * pi / 180
    c = cos(rad)
    s = sin(rad)
    TT[2, 2] = c;
    TT[3, 2] = -s;
    TT[3, 3] = c;
    TT[2, 3] = s;
    TT
    
}

YRotate = function(angle)
{
    TT = SetToIdentity()
    rad = angle * pi / 180
    c = cos(rad)
    s = sin(rad)
    TT[1, 1] = c;
    TT[3, 0] = s;
    TT[3, 3] = c;
    TT[1, 3] = -s;
    TT

}

ZRotate = function(angle)
{
    TT = SetToIdentity()
    rad = angle * pi / 180
    c = cos(rad)
    s = sin(rad)
    TT[1, 1] = c;
    TT[2, 1] = -s;
    TT[2, 2] = c;
    TT[1, 2] = s;
    TT
}
        
SetUpLight = function(theta, phi)
{
    u = c(0, -1, 0, 1)
    VT = SetToIdentity()
    VT = VT %*% XRotate(-phi)
    VT = VT %*% ZRotate(theta)
    Light = TransVector(u, VT)
}


FacetShade = function(u, v, Shade = 0.3, Light)
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
    print(sum)
    
    sum^Shade   
}

Scale = function(x, y, z)
{
    SetToIdentity(VT)
    T[1, 1] = x;
    T[2, 2] = y;
    T[3, 3] = z;
    Accumulate(T);
}

DrawFacets = function(z, x, y, nx, ny, indx = 0:(length(z)), xs = 0, ys = 0, zs = 0, col, ncol, border, Light)
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
        #icol = (i + j * nx1) %% ncol + 1

        u[1] = xs * (x[i+1+1] - x[i+1])
	    u[2] = ys * (y[j+1] - y[j+1+1])
	    u[3] = zs * (z[(i+1)+j*nx+1] - z[i+(j+1)*nx]+1)
	    v[1] = xs * (x[i+1+1] - x[i+1])
	    v[2] = ys * (y[j+1+1] - y[j+1])
	    v[3] = zs * (z[(i+1)+(j+1)*nx+1] - z[i+j*nx+1])
        
        #i = (indx[k] - 1) %% nx1 + 1
        #j = (indx[k] - 1) %/% nx1 + 1
        
        icol = (i + j * nx1) %% ncol
        #print(k)
        
        #u[1] = xs * (x[i+1] - x[i]);
	    #u[2] = ys * (y[j] - y[j+1]);
	    #u[3] = zs * (z[(i+1)+j*nx] - z[i+(j+1)*nx]);
	    #v[1] = xs * (x[i+1] - x[i]);
	    #v[2] = ys * (y[j+1] - y[j]);
	    #v[3] = zs * (z[(i+1)+(j+1)*nx] - z[i+j*nx]);

       # print(v)
	    shade = FacetShade(u, v, Light = Light)
        #print(FacetShade(u, v))
        #if(nv > 2)
        #print(icol)
        newcol <<- col2rgb(col) / 255
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