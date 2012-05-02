var Complex;

Complex = (function() {

  Complex.name = 'Complex';

  function Complex(a, b) {
    this.a = a;
    this.b = b;
  }

  Complex.prototype.add = function(z) {
    return new Complex(this.a + z.a, this.b + z.b);
  };

  Complex.prototype.multiply = function(z) {
    return new Complex(this.a * z.a - this.b * z.b, this.a * z.b + this.b * z.a);
  };

  Complex.prototype.magnitude = function() {
    return Math.sqrt(this.a * this.a + this.b * this.b);
  };

  Complex.prototype.conjugate = function() {
    return new Complex(this.a, -this.b);
  };

  Complex.prototype.argument = function() {
    return Math.atan2(this.b, this.a);
  };

  Complex.prototype.re = function() {
    return this.a;
  };

  Complex.prototype.im = function() {
    return this.b;
  };

  Complex.prototype.rotate = function(teta) {
    return new Complex(Math.cos(teta), Math.sin(teta)).multiply(this);
  };

  Complex.prototype.toString = function() {
    if (this.b < 0) {
      return "" + this.a + " - " + (Math.abs(this.b)) + "i";
    } else {
      return "" + this.a + " + " + this.b + "i";
    }
  };

  return Complex;

})();