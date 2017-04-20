highlight = function(i) {
  var polygon = document.getElementById("polygon." + 1 + '.i');
  var label = document.getElementById("label." + i + '.1');
  //point.setAttribute("r", point.getAttribute("r")*2);
  label.setAttribute("visibility", "visible");
}

dim = function(i) {
  var polygon = document.getElementById("polygon." + 1 + '.i');
  var label = document.getElementById("label." + i + '.1');

  //point.setAttribute("r", point.getAttribute("r")/2);
  label.setAttribute("visibility", "hidden");
}


var obj;
var input;
//for(i = 1; i <= 15; i++){
    var i = 1
    
    obj = document.getElementById('polygon.1.' + i);
    console.log(obj);

    obj.addEventListener("onmouseover", function(){
        highlight(i)
        }
    );
    obj.addEventListener("onmouseout", function(){
        dim(i)
        }
    );
//}