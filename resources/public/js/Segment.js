var Segment;
Segment = (function () {

  Segment.name = 'Segment';

  function Segment(canvas,p1, p2, visible) {
    this.p1 = p1;
    this.p2 = p2;
    var self = this;

    if (visible) {
      this.line = canvas.createEmptyLine()
      .attr("x1", this.p1.x)
      .attr("y1", this.p1.y)
      .attr("x2", this.p2.x)
      .attr("y2", this.p2.y)
      .attr("stroke-width", 1)
      .attr("opacity", 0.5)
      .style("stroke", "black");

      this.p1Handle = canvas.context().append("circle")
          .attr("id","p1")
          .attr("stroke-width", 2)
          .attr("stroke", "red")
          .attr("fill","transparent")
          .attr("cx",self.p1.x)
          .attr("cy",self.p1.y)
          .attr("opacity", 0.5)
          .attr("r",6);

      var context = canvas.context();

      this.p2Handle = context.append("circle")
          .attr("id","p2")
          .attr("stroke-width", 2)
          .attr("stroke", "red")
          .attr("fill","transparent")
          .attr("cx",self.p2.x)
          .attr("cy",self.p2.y)
          .attr("opacity", 0.5)
          .attr("r",6);


        var afterDragging = function() {
          context.cutLine = context.draggingSegment;
          context.draggingPoint = null;
          context.draggingSegment = null;
        }

        this.p1Handle.on("mousedown",function() {
          context.draggingPoint = "p1";
          context.draggingSegment = self;
        }).on("mouseup",function() {
          afterDragging();
        });

        this.p2Handle.on("mousedown",function() {
          context.draggingPoint = "p2";
          context.draggingSegment = self;
        }).on("mouseup",function() {
          afterDragging();
        });



        context.on("mousemove.segment", function() {
          var pos = d3.mouse(this);
          if (context.draggingSegment) {
              if (context.draggingPoint == "p1") {
                  context.draggingSegment.setP1(pos[0],pos[1]);
              } else if (context.draggingPoint == "p2") {
                  context.draggingSegment.setP2(pos[0],pos[1]);
              }
          }
        });
      }



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


  Segment.prototype.setP1 = function(x,y) {
    this.p1 = new Point(x,y);
    this.line
    .attr("x1", x)
    .attr("y1", y);

    this.p1Handle
        .attr("cx",x)
        .attr("cy",y);
  }

  Segment.prototype.setP2 = function(x,y) {
    this.p2 = new Point(x,y);
    this.line
    .attr("x2", x)
    .attr("y2", y)

    this.p2Handle
    .attr("cx",x)
    .attr("cy",y);

  };




  return Segment;
})();
