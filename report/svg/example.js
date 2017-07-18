var obj, input, polygons, total, main, current_color;
var pDcolor = [];

polygonon = function()
{
    str = this.id;
    color = this.getAttribute('fill');
    window.current_color = color;
    polygon_index = str.replace(/polygon.[0-9]./, '');
    // highlight the polygon
    this.setAttribute('fill', "rgb(255,100,100)");

    opacity = this.getAttribute('fill-opacity');
    this.setAttribute("fill-opacity",opacity);

    // show the 'value'
    label = document.getElementById('labels.' + '1.' + polygon_index + '.text');
    label.setAttribute("fill-opacity", 1);

}

polygonout = function()
{
    str = this.id;
    polygon_index = str.replace(/polygon.[0-9]./, '');
    
    // labeling
    label = document.getElementById('labels.' + '1.' + polygon_index + '.text');
    label.setAttribute("fill-opacity",0);
    
    this.setAttribute('fill', current_color);

}

mainon = function()
{
    if(this.id.includes('color')){
        oCol = this.getAttribute('fill-opacity');
        this.setAttribute('fill-opacity', 0.2);
    }else{
        this.setAttribute('fill', "rgb(255,0,0)");
    }
}
mainout = function()
{
    if(this.id.includes('color')){
        this.setAttribute('fill-opacity', 1);
    }else{
       this.setAttribute('fill', main_default)
    }
    
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
    animate(Surface_id = nSurface, action = 'shaded');
}

addAlpha = function()
{
    // hide the shaded surface
    reset();
    show_shaded(0);
    animate(Surface_id = nSurface, action = 'alpha', alpha = 0.25);
    
}

color_fill = function()
{
    reset();
    color_new = this.getAttribute('fill');
    animate(Surface_id = nSurface, action = 'color', alpha = 1, colors = color_new);
    
}


function Create2DArray(rows) {
  var arr = [];

  for (var i=0;i<rows;i++) {
     arr[i] = [];
  }

  return arr;
}

animate = function(Surface_id, action, alpha, colors)
{
    var polygons_odd = [], opacity_odd = [], id = [], id, color_current = []; 
    var color_new = [], polygons_new = [];
    var newCol;

    for(i = 1; i <= total; i++){
        var pos = 0;
        polygons_odd[i] = document.getElementById('polygon.' + Surface_id + '.' + i);
        opacity_odd[i] = polygons_odd[i].getAttribute('fill-opacity');
        color_current[i] = polygons_odd[i].getAttribute('fill');
    }
    
    if(action == 'shaded'){
        var steps = Create2DArray(total + 1);
        oldCol = obj.getAttribute('fill');
        pat = /\d+/g;
        orgb = oldCol.match(pat);
        or = parseInt(orgb[0]);
        og = parseInt(orgb[1]);
        ob = parseInt(orgb[2]);
        
        for(i = 1; i <= total; i++){
            var pos = 0;
            polygons_new[i] = document.getElementById('polygon.' + 1 + '.' + i);
            color_new[i] = polygons_new[i].getAttribute('fill');
            
            nrgb = color_new[i].match(pat);
            nr = parseInt(nrgb[0]);
            ng = parseInt(nrgb[1]);
            nb = parseInt(nrgb[2]);

            // return the color for rest when onmouseout
            window.new_rgb = newCol;
            steps[i][0] = (nr - or) / 100;
            steps[i][1] = (ng - og) / 100;
            steps[i][2] = (nb - ob) / 100;
        }
    }
    
    if(action == 'color'){
        newCol = colors;
        oldCol = obj.getAttribute('fill');
        pat = /\d+/g;
        orgb = oldCol.match(pat);
        or = parseInt(orgb[0]);
        og = parseInt(orgb[1]);
        ob = parseInt(orgb[2]);
        
        nrgb = newCol.match(pat);
        nr = parseInt(nrgb[0]);
        ng = parseInt(nrgb[1]);
        nb = parseInt(nrgb[2]);
        
        // return the color for rest when onmouseout
        window.new_rgb = newCol;
        var step = [];
        step[0] = (nr - or) / 100;
        step[1] = (ng - og) / 100;
        step[2] = (nb - ob) / 100; 
    }
    
    id = setInterval(frame, 15);
    function frame() {
        if (pos == 100) {
        clearInterval(id);
        } else {
        pos = pos + 1; 
        for(i = 1; i <= total; i++){
        
        if(action == 'shaded'){
            col_out = 'rgb(' + parseInt(or + steps[i][0] * pos) + ',' + 
                   parseInt(og + steps[i][1] * pos) + ',' + 
                   parseInt(ob + steps[i][2] * pos) + ')';
            polygons_odd[i].setAttribute('fill', col_out);
        }
        if(action == 'alpha'){
            polygons_odd[i].setAttribute("fill-opacity", parseInt(opacity_odd[i]) - 
                                                        pos/(100 * 1/(1 - alpha)));
        }
        if(action == 'color'){
            col_out = 'rgb(' + parseInt(or + step[0] * pos) + ',' + 
                               parseInt(og + step[1] * pos) + ',' + 
                               parseInt(ob + step[2] * pos) + ')';
            polygons_odd[i].setAttribute('fill', col_out);
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

/////////////////////////////////////////////////////////////////////////////////////////////////////
// initial setting 
nSurface = countSurface();
polygons = document.getElementsByTagName('polygon');
total = polygons.length/nSurface;


for(i = 1; i <= total; i++){
    obj = document.getElementById('polygon.' + 2 + '.' + i);
    pDcolor[i] = obj.getAttribute('fill');
    obj.onmouseover = polygonon;
    obj.onmouseout = polygonout;
    new_rgb = pDcolor[1];
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
//main.onclick = addShade;
alpha = document.getElementById('alpha.1.1.text');
change_color = document.getElementById('color.1.1.text');
alpha.onmouseover = mainon;
alpha.onmouseout = mainout;
change_color.onmouseover = mainon;
change_color.onmouseout = mainout;
alpha.onclick = addAlpha; 
shading = document.getElementById('shading.1.1.text');
shading.onmouseover = mainon;
shading.onmouseout = mainout;
shading.onclick = addShade;
reseting = document.getElementById('reset.1.1.text');
reseting.onmouseover = mainon;
reseting.onmouseout = mainout;
reseting.onclick = reset;

// avriable colors.
var col_default = [];
for(i = 1; i <= 5; i ++)
{
    color_id = document.getElementById('color.2.' + i);
    col_default[i] = color_id.getAttribute('fill');
    color_id.onmouseover = mainon;
    color_id.onmouseout = mainout;
    color_id.onclick = color_fill;
}
    
