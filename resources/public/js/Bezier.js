var Bezier;

Bezier = (function() {

  Bezier.name = 'Bezier';

  function Bezier(z1, z2, z3) {
    this.z1 = z1;
    this.z2 = z2;
    this.z3 = z3;
    this.x = 0;
    this.y = 0;
    this.color = "black";
  }

  Bezier.prototype.setColor = function(color) {
      this.color = color;
  }

  Bezier.prototype.rotate = function(teta) {
    return new Bezier(this.z1.rotate(teta), this.z2.rotate(teta), this.z3.rotate(teta));
  };

  Bezier.prototype.add = function(b) {
    var angle, z;
    z = new Complex(this.x, this.y);
    z = z.add(this.z1).add(this.z2).add(this.z3);
    angle = this.z3.argument() - b.z1.argument();
    var bezier = b.rotate(angle);
    bezier.moveTo(z.re(), z.im());
    return bezier;
  };

  Bezier.prototype.moveTo = function(x, y) {
    this.x = x;
    this.y = y;
  };


  Bezier.prototype.draw = function(context) {
    var controlX1, controlX2, controlY1, controlY2, endX, endY, xAux, yAux;
    xAux = this.x;
    yAux = this.y;
    xAux += this.z1.re();
    yAux += this.z1.im();
    controlX1 = xAux;
    controlY1 = yAux;
    xAux += this.z2.re();
    yAux += this.z2.im();
    controlX2 = xAux;
    controlY2 = yAux;
    xAux += this.z3.re();
    yAux += this.z3.im();
    endX = xAux;
    endY = yAux;
    context.beginPath();
    context.strokeStyle = this.color;
    context.moveTo(this.x, this.y);
    context.bezierCurveTo(controlX1, controlY1, controlX2, controlY2, endX, endY);
    context.stroke();
    context.closePath();
  };

  return Bezier;

})();