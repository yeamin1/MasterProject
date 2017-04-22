var obj, input, polygons, total,main;
var pDcolor = [];

polygonon = function()
{
    this.setAttribute('fill', "rgb(1,255,255)")
}
polygonout = function()
{
    str = this.id;
    polygon_index = str.replace(/polygon.[0-9]./, '');
    
    color = pDcolor[polygon_index];
    this.setAttribute('fill', color)
}

mainon = function()
{
    this.setAttribute('fill', "rgb(255,0,0)")
}
mainout = function()
{
    this.setAttribute('fill', main_default)
}
// count the 'actual' number of surface in the plot
countSurface = function(){
    totalSurface = 0;
    for(i = 1; i < 5; i++)
    {    
        obj_count = document.getElementById('polygon.' + i);
        if(obj_count != null){
            totalSurface = totalSurface + 1;
        }            
    }
    return(totalSurface);
}

nSurface = countSurface();
polygons = document.getElementsByTagName('polygon');
total = polygons.length/nSurface;

for(i = 1; i <= total; i++){
    obj = document.getElementById('polygon.' + nSurface + '.' + i);
    pDcolor[i] = obj.getAttribute('fill');

    obj.onmouseover = polygonon;
    obj.onmouseout = polygonout;
}

// change the main title
main = document.getElementById('graphics-plot-2-main-1.1.1.text');
main_default = main.getAttribute('fill');
main.onmouseover = mainon;
main.onmouseout = mainout;