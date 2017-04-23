var obj, input, polygons, total,main;
var pDcolor = [];
var shade_end = false;

polygonon = function()
{
    str = this.id;
    polygon_index = str.replace(/polygon.[0-9]./, '');
    // highlight the polygon
    this.setAttribute('fill', "rgb(255,100,100)");
    if(shade_end == false){
        opacity = this.getAttribute('fill-opacity');
        this.setAttribute("fill-opacity",opacity);
    }else{
    this.setAttribute("fill-opacity",1);
    }
    // show the 'value'
    label = document.getElementById('labels.' + '1.' + polygon_index + '.text');
    label.setAttribute("fill-opacity",1);

}
polygonout = function()
{
    str = this.id;
    polygon_index = str.replace(/polygon.[0-9]./, '');
    
    color = pDcolor[polygon_index];
    this.setAttribute('fill', color);
    
    label = document.getElementById('labels.' + '1.' + polygon_index + '.text');
    label.setAttribute("fill-opacity",0);
    
    opacity = this.getAttribute('fill-opacity');
    
    if(shade_end == false){
        this.setAttribute("fill-opacity",opacity);
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
    // show the shaded surface
    reset()
    show_shaded(1);
    animate(Surface_id = nSurface, action = 'shaded');
}

addAlpha = function()
{
    // hide the shaded surface
    reset();
    show_shaded(0);
    animate(Surface_id = nSurface, action = 'alpha', alpha = 0.5);
    
}

addColor = function()
{
    // hide the shaded surface
    show_shaded(0);
}

animate = function(Surface_id, action, alpha, color)
{
    var polygons_odd = [], opacity_odd = [], id = [], id;

    for(i = 1; i <= total; i++){
        var pos = 0;
        polygons_odd[i] = document.getElementById('polygon.' + Surface_id + '.' + i);
        opacity_odd[i] = polygons_odd[i].getAttribute('fill-opacity');
    }
    id = setInterval(frame, 20);
    function frame() {
        if (pos == 100) {
        if(action == 'shaded'){
            window.shade_end = true;
        }else{
            window.shade_end = false;
        }
        
        clearInterval(id);
        } else {
            pos = pos + 1; 
            for(i = 1; i <= total; i++){
            
            if(action == 'shaded'){
                polygons_odd[i].setAttribute("stroke-opacity", parseInt(opacity_odd[i]) - pos/100);
                polygons_odd[i].setAttribute("fill-opacity", parseInt(opacity_odd[i]) - pos/100);
            }
            if(action == 'alpha'){
                polygons_odd[i].setAttribute("fill-opacity", parseInt(opacity_odd[i]) - pos/(100 * 1/(1 - alpha)));
            }
            if(action == 'color'){
                
            }
            }

        }
    }
    
}

reset = function()
{
    for(i = 1; i <= total; i++){
    obj = document.getElementById('polygon.' + 2 + '.' + i);
    obj.setAttribute('stroke-opacity', 1);
    obj.setAttribute('fill-opacity', 1);
    }
}


show_shaded = function(x)
{
    for(i = 1; i <= total; i++){
    obj_shaded = document.getElementById('polygon.' + 1 + '.' + i);
    obj_shaded.setAttribute('stroke-opacity', x);
    obj_shaded.setAttribute('fill-opacity', x);
    }
}

nSurface = countSurface();
polygons = document.getElementsByTagName('polygon');
total = polygons.length/nSurface;

// initial setting 
for(i = 1; i <= total; i++){
    obj = document.getElementById('polygon.' + 2 + '.' + i);
    pDcolor[i] = obj.getAttribute('fill');
    obj.onmouseover = polygonon;
    obj.onmouseout = polygonout;
    // hide all the labels
    label = document.getElementById('labels.' + '1.' + i + '.text');
    label.setAttribute('stroke-opacity', 0);
    label.setAttribute('fill-opacity', 0);
    // hide the shaded surface
    obj_shaded = document.getElementById('polygon.' + 1 + '.' + i);
    obj_shaded.setAttribute('stroke-opacity', 0);
    obj_shaded.setAttribute('fill-opacity', 0);
}

// change the main title
main = document.getElementById('graphics-plot-2-main-1.1.1.text');
main_default = main.getAttribute('fill');
main.onmouseover = mainon;
main.onmouseout = mainout;
main.onclick = addShade;
alpha = document.getElementById('alpha.1.1.text');
change_color = document.getElementById('change.1.1.text');
alpha.onmouseover = mainon;
alpha.onmouseout = mainout;
change_color.onmouseover = mainon;
change_color.onmouseout = mainout;
alpha.onclick = addAlpha; 

// get number...maybe later...
// obj = document.getElementById('polygon.' + nSurface + '.' + 2);
// oldCol = obj.getAttribute('fill');
// pat = /\d+/g;
// rgb = oldCol.match(pat);

//get opacity


    
