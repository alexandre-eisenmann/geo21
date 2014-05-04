var Segment;
Segment = (function () {

  Segment.name = 'Segment';

  function Segment(canvas,p1, p2) {
    this.p1 = p1;
    this.p2 = p2;

    var myLine = canvas.createEmptyLine()
    .attr("x1", this.p1.x)
    .attr("y1", this.p1.y)
    .attr("x2", this.p2.x)
    .attr("y2", this.p2.y)
    .style("stroke", "rgb(6,120,155)");
  }

  //http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
  Segment.prototype.intersection = function(segment) {
    var p = this.p1;
    var r = new Point(this.p2.x - this.p1.x,this.p2.y - this.p1.y);
    var q = segment.p1;
    var s = new Point(segment.p2.x - segment.p1.x,segment.p2.y - segment.p1.y);
    var qMinusP = new Point(q.x - p.x, q.y - p.y);
    var rXs = r.crossProduct(s);
    if (rXs != 0) {
      var t = qMinusP.crossProduct(s) / rXs;
      if (t >= 0 && t <= 1) {
        return p.add(r.times(t));
      }
    }
    return null;
  };





  return Segment;
})();
