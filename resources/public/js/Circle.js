var Circle;
Circle = (function () {
    
  Circle.name = 'Circle';

  function Circle(point, radius) {
    this.point = point;
    this.radius = radius;
    this.state = "normal";
  }
  
  
  Circle.prototype.highlight = function() {
        this.state = "highlight"; 
  };
    
  Circle.prototype.unHighlight = function() {
        this.state = "normal"; 
  };
    
  Circle.prototype.isInside = function(pt) {
        return Math.pow(pt.x - this.point.x, 2) + Math.pow(pt.y - this.point.y, 2) < Math.pow(this.radius, 2); 
  };

  Circle.prototype.draw = function(context) {
      
      
      context.beginPath();
      if (this.state == "normal") {
          context.strokeStyle = "#756975";
      } else {
          context.strokeStyle = "#dddddd";
      }
      context.arc(this.point.x, this.point.y, this.radius, 0, Math.PI*2, true);
      var lineWidth = context.lineWidth;
      context.lineWidth = 1;
      context.stroke();
      context.lineWidth = lineWidth;
      context.closePath();
  };

  return Circle;
  
})();
