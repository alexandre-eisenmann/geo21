var Point;
Point = (function () {

  Point.name = 'Point';

  function Point(x, y) {
    this.x = x;
    this.y = y;
    this.c = new Complex(x,y);
  }

  Point.prototype.crossProduct = function(w) {
    return this.x * w.y - this.y*w.x;
  };

  Point.prototype.times = function(s) {
    return new Point(this.x * s,this.y * s);
  };

  Point.prototype.add = function(p) {
    var res = this.c.add(p.c);
    return new Point(res.re(),res.im());
  };

  Point.prototype.equal = function(p) {
    return this.x == p.x && this.y == p.y;
  };

  return Point;
})();
