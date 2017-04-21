var obj, input, polygons, total,main

polygons = document.getElementsByTagName('polygon');
total = polygons.length/2;


console.log(polygons.length);

polygonon = function()
{
    this.setAttribute('fill', "rgb(1,255,255)")
}
polygonout = function()
{
    this.setAttribute('fill', "rgb(255,255,255)")
}

mainon = function()
{
    this.setAttribute('fill', "rgb(255,0,0)")
}
mainout = function()
{
    this.setAttribute('fill', main_default)
}

for(i = 1; i <= total; i++){
    obj = document.getElementById('polygon.1.' + i);
    obj.onmouseover = polygonon;
    obj.onmouseout = polygonout;
}

// change the main title
main = document.getElementById('graphics-plot-1-main-1.1.1.text');
main_default = main.getAttribute('fill');
main.onmouseover = mainon;
main.onmouseout = mainout;