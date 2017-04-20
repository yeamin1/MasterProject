highlight = function(i) {
  var point = document.getElementById("point." + i + '.1');
  var label = document.getElementById("label." + i);
  point.setAttribute("r", point.getAttribute("r")*2);
  label.setAttribute("visibility", "visible");
}

dim = function(i) {
  var point = document.getElementById("point." + i + '.1');
  var label = document.getElementById("label." + i);
  point.setAttribute("r", point.getAttribute("r")/2);
  label.setAttribute("visibility", "hidden");
}