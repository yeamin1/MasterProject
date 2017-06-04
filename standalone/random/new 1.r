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
         d = Inf
         for(ii in 1)
         {
            for(jj in 1)
            {
                u[1] = x[i + ii]
                u[2] = y[j + jj]
                u[3] = 0
                u[4] = 1
                
                v = TransVector(u, VT)
                if(v[3] > d) d = v[3]
                
                }
         }
        depth[i+j*nx1] = -d;[i+j*nx1] = -d;
        }
    }
    depth
}