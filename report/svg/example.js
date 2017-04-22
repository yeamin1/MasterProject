var obj, input, polygons, total,main;
var pDcolor = [];
var shade_end = false;

polygonon = function()
{
    this.setAttribute('fill', "rgb(1,255,255)");
    this.setAttribute("fill-opacity",1);

}
polygonout = function()
{
    str = this.id;
    polygon_index = str.replace(/polygon.[0-9]./, '');
    
    color = pDcolor[polygon_index];
    this.setAttribute('fill', color);
    if(shade_end == false){
        this.setAttribute("fill-opacity",1);
    }else{
        this.setAttribute("fill-opacity",0);
    }
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

addShade = function(){
    var polygons_odd = [], opacity_odd = [], id = [], id;

    for(i = 1; i <= total; i++){
        var pos = 0;
        polygons_odd[i] = document.getElementById('polygon.' + nSurface + '.' + i);
        opacity_odd[i] = polygons_odd[i].getAttribute('fill-opacity');
    }
    id = setInterval(frame, 20);
    function frame() {
        if (pos == 100) {
        window.shade_end = true;
        clearInterval(id);
        } else {
            pos = pos + 1; 
            for(i = 1; i <= total; i++){
            polygons_odd[i].setAttribute("fill-opacity", (opacity_odd[i] * 100 - pos)/100);
            polygons_odd[i].setAttribute("stroke-opacity", (opacity_odd[i] * 100 - pos)/100);
            }

        }
    }
}



nSurface = countSurface();
polygons = document.getElementsByTagName('polygon');
total = polygons.length/nSurface;

for(i = 1; i <= total; i++){
    obj = document.getElementById('polygon.' + 2 + '.' + i);
    pDcolor[i] = obj.getAttribute('fill');
    obj.onmouseover = polygonon;
    obj.onmouseout = polygonout;
}

// change the main title
main = document.getElementById('graphics-plot-2-main-1.1.1.text');
main_default = main.getAttribute('fill');
main.onmouseover = mainon;
main.onmouseout = mainout;
main.onclick = addShade;

// get number...maybe later...
// obj = document.getElementById('polygon.' + nSurface + '.' + 2);
// oldCol = obj.getAttribute('fill');
// pat = /\d+/g;
// rgb = oldCol.match(pat);

//get opacity


    
