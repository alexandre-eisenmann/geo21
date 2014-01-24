var Canvas = (function() {

    function Canvas(svg) {

      this.svg = svg;
      this.draggingPolygon = null;
      this.coord = null;
      var self = this;

      this.svg
         .on("mouseup",function() {
           self.draggingEnd()
         })
         .on("mouseout",function() {
           if (!d3.event.relatedTarget ||
               d3.event.relatedTarget.constructor.name == "HTMLBodyElement" ||
               d3.event.relatedTarget.constructor.name == "HTMLHtmlElement" ) {
             self.draggingEnd();
           }
         })
         .on("mousemove",function () {

              if (self.coord && self.draggingPolygon) {
                  var pos = d3.mouse(this);
                  var translate = { x: pos[0] - self.coord[0],y:pos[1] - self.coord[1]};
                  self.draggingPolygon.pull({x: pos[0], y: pos[1]});
                  //self.draggingPolygon.translate(translate);

              }
          });
    }


    Canvas.prototype.draggingEnd = function() {
      this.draggingPolygon = null;
      this.coord=null;
    };

    Canvas.prototype.draggingStart = function(polygon,coord) {
      this.draggingPolygon = polygon;
      this.coord = coord;
    };

    Canvas.prototype.polygonBeingDragged = function(polygon,coord) {
      return this.draggingPolygon;
    };


    Canvas.prototype.createEmptyPolygon = function() {
       return this.svg.append("g");
    };



    return Canvas;

})();
