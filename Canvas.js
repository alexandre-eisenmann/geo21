var Canvas = (function() {

    function Canvas(svg) {

      this.svg = svg;
      this.polygons = [];
      this.draggingPolygon = null;
      this.coord = null;
      var self = this;

      this.svg
         .on("mouseup.canvas",function() {
           self.draggingEnd()
         })
         .on("mouseout.canvas",function() {
           if (!d3.event.relatedTarget ||
               d3.event.relatedTarget.constructor.name == "HTMLBodyElement" ||
               d3.event.relatedTarget.constructor.name == "HTMLHtmlElement" ) {
             self.draggingEnd();
           }
         })
         .on("mousemove.canvas",function () {

              if (self.coord && self.draggingPolygon) {
                  var pos = d3.mouse(this);
                  var translate = { x: pos[0] - self.coord.x,y:pos[1] - self.coord.y};
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

    Canvas.prototype.addPolygon = function(polygon) {
       this.polygons.push(polygon);
    };

    Canvas.prototype.removePolygon = function(polygon) {
       polygon.remove();
       var index = this.polygons.indexOf(polygon);
       if (index > -1) {
         this.polygons.splice(index, 1);
       }
    };

    Canvas.prototype.split = function(segment) {
      var toBeDeleted = [];
      var copyArray = [];
      for(var i=0; i < this.polygons.length; i++) {
        copyArray.push(this.polygons[i]);
      }
      for(var i=0; i < copyArray.length; i++) {
        copyArray[i].split(segment);
        toBeDeleted.push(copyArray[i]);
      }
      for(var i=0; i < toBeDeleted.length; i++) {
        this.removePolygon(toBeDeleted[i]);
      }

    };

    Canvas.prototype.createEmptyLine = function() {
       return this.svg.append("svg:line");
    };


    Canvas.prototype.context = function() {
       return this.svg
    };


    return Canvas;

})();
