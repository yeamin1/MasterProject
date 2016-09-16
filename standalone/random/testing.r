PerspBox = function(front, x, y, z, EdgeDone)
{
   
    for (f = 0; f < 6; f++) {
        p0 = Face[f, 0]
        p1 = Face[f, 1]
        p2 = Face[f, 2]
        p3 = Face[f, 3]

        u0[1] = x[Vertex[p0, 1]]
        u0[2] = y[Vertex[p0, 2]]
        u0[3] = z[Vertex[p0, 3]]
        u0[4] = 1
        u1[1] = x[Vertex[p1, 1]]
        u1[2] = y[Vertex[p1, 2]]
        u1[3] = z[Vertex[p1, 3]]
        u1[4] = 1
        u2[1] = x[Vertex[p2, 1]]
        u2[2] = y[Vertex[p2, 2]]
        u2[3] = z[Vertex[p2, 3]]
        u2[4] = 1
        u3[1] = x[Vertex[p3, 1]]
        u3[2] = y[Vertex[p3, 2]]
        u3[3] = z[Vertex[p3, 3]]
        u3[4] = 1

        TransVector(u0, VT, v0)
        TransVector(u1, VT, v1)
        TransVector(u2, VT, v2)
        TransVector(u3, VT, v3)

        d = e = 0
        for (i in 1:4) {
            d[i] = v1[i]/v1[3] - v0[i]/v0[3]
            e[i] = v2[i]/v2[3] - v1[i]/v1[3]
        
        }
        nearby = (d[1]*e[2] - d[2]*e[1]) < 0
        
        for(i in 1:12)
        {
            EdgeDone[i] = 0
        }
        
        v0 = v0/v0[4]
        v1 = v1/v1[4]
        v2 = v2/v2[4]
        v3 = v3/v3[4]
        
        if ((front && nearby) || (!front && !nearby)) {
            if (!EdgeDone[Edge[f][1]]){
                lines(c(v0[1], v1[1]), c(v0[2], v1[2]))
                EdgeDone[Edge[f][1]] = EdgeDone[Edge[f][1]] + 1
                }
            if (!EdgeDone[Edge[f][2]]++){
                lines(c(v1[1], v2[1]), c(v1[2], v2[2]))
                EdgeDone[Edge[f][2]] = EdgeDone[Edge[f][2]] + 1
                }
            if (!EdgeDone[Edge[f][3]]++){
                lines(c(v2[1], v3[1]), c(v2[2], v3[2]))
                EdgeDone[Edge[f][3]] = EdgeDone[Edge[f][3]] + 1
                }
            if (!EdgeDone[Edge[f][4]]++){
                lines(c(v3[1], v0[1]), c(v3[2], v0[2]))
                EdgeDone[Edge[f][4]] = EdgeDone[Edge[f][4]] + 1
                }
     
    }
}