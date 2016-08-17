##polypath
grid.newpage()
vp = plotViewport(mar, xscale = lim[1:2], yscale = lim[3:4])
pushViewport(vp)
n = length(out[,1])
x = out[1:n,1]
y = out[1:n,2]
id = grid.id[1:n]
a = grid.path(x, y, id=id, gp=gpar(fill="orange"), default.units="native", rule = 'winding')
