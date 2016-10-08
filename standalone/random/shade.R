LimitCheck = function(lim)
{
    s = 0.5 * abs(lim[2] - lim[1])
    c = 0.5 * (lim[2] + lim[1])
    c(s, c)
}


XRotate = function ( angle ) {
    TT = diag(1, 4)
    rad = angle * pi / 180
    c = cos(rad)
    s = sin(rad)
    TT[2, 2] = c;
    TT[3, 2] = -s;
    TT[3, 3] = c;
    TT[2, 3] = s;
    TT
}

YRotate = function ( angle ) {
    TT = diag(1, 4)
    rad = angle * pi / 180
    c = cos(rad)
    s = sin(rad)
    TT[1, 1] = c;
    TT[3, 0] = s;
    TT[3, 3] = c;
    TT[1, 3] = -s;
    TT
}

ZRotate = function ( angle ) {
    TT = diag(1, 4)
    rad = angle * pi / 180
    c = cos(rad)
    s = sin(rad)
    TT[1, 1] = c;
    TT[2, 1] = -s;
    TT[2, 2] = c;
    TT[1, 2] = s;
    TT
}
        
SetUpLight = function ( theta, phi ) {
    u = c(0, -1, 0, 1)
    VT = diag(1, 4)
    VT = VT %*% XRotate(-phi)
    VT = VT %*% ZRotate(theta)
    Light = u %*% VT
}


FacetShade = function( u, v, Shade = 0.5, Light ) {
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



shadeCol = function ( z, x, y, xs, ys, zs, col, ncol, ltheta, lphi, Shade ) {
    u = v = 0
    nx = nrow(z)
    ny = ncol(z)
    nx1 = nx - 1
    ny1 = ny - 1
    cols = 0
    
    indx = 0:(length(z))
    Light = SetUpLight(ltheta, lphi)
    ## need vectorized
    for(k in 1:(nx1 * ny1)){
        nv = 0
        i = (indx[k]) %% nx1 
        j = (indx[k]) %/% nx1
               
        ## vectorized color still not working...
        icol = (i + j * nx1) %% ncol + 1

        u[1] = xs * (x[i+1+1] - x[i+1])
	    u[2] = ys * (y[j+1] - y[j+1+1])
	    u[3] = zs * (z[(i+1)+j*nx+1] - z[i+(j+1)*nx+1])
	    v[1] = xs * (x[i+1+1] - x[i+1])
	    v[2] = ys * (y[j+1+1] - y[j+1])
	    v[3] = zs * (z[(i+1)+(j+1)*nx+1] - z[i+j*nx+1])
        icol = (i + j * nx1) %% ncol
	    shade = FacetShade(u, v, Shade = Shade, Light = Light)
        ##one condiction here..if any bugs then check here...
        #
        #
        newcol = col2rgb(col) / 255
        cols[k] = rgb(shade * newcol[1], shade * newcol[2], shade * newcol[3])
    }
        cols
}
