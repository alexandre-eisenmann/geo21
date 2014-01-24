    var Polygon = (function() {


      function Polygon(canvas, data, color) {

        this.data = data;
        this.color = color;
        this.canvas = canvas;
        var self = this;
        this.angle = 0;
        this.offset = {x:0, y:0};
        this.refPoint;

        var lineFunction = d3.svg.line()
            .x(function(d) { return d.x; })
            .y(function(d) { return d.y; })
            .interpolate("linear");

        this.polygonGroup = this.canvas.createEmptyPolygon();


        var lineGraph = this.polygonGroup.append("path")
                                      .attr("d", lineFunction(data))
                                      .attr("stroke", "none")
                                      .attr("stroke-width", 2)
                                      .attr("fill", this.color)
                                      .attr("opacity", 0.5);
        this.centerOfMass = this.centroid();

        this.polygonGroup.append("circle")
            .attr("id","centroid")
            .attr("stroke", "red")
            .attr("stroke-width", 2)
            .attr("fill", "none")
            .attr("cx",self.centerOfMass.x)
            .attr("cy",self.centerOfMass.y)
            .attr("r",5);


        this.polygonGroup.on("mousedown",function() {
             self.refPoint = d3.mouse(this);
             d3.select(this).append("circle")
               .attr("stroke", "#C0C0C0")
               .attr("stroke-width", 2)
               .attr("fill", "none")
               .attr("cx",self.refPoint[0])
               .attr("cy",self.refPoint[1])
               .attr("r",2);
             canvas.draggingStart(self,self.refPoint);
          })
         .on("mouseover",function() {
             var p = canvas.polygonBeingDragged();
             if (!p || p == self)
               lineGraph.attr("stroke", "black");
          })
         .on("mouseout",function() {
             var p = canvas.polygonBeingDragged();
             if (!p || p != self)
               lineGraph.attr("stroke", "none");
          });
      }


      Polygon.prototype.updateTransform = function() {
         self = this;
         this.polygonGroup.attr("transform", function(d) {
             return "translate(" + self.offset.x+ " "+self.offset.y+")rotate(" + self.angle + " "+ self.centerOfMass.x+ " "+ self.centerOfMass.y + ")";
          });
      };

      Polygon.prototype.pull = function(to) {
         var com = {x: this.centerOfMass.x + this.offset.x, y: this.centerOfMass.y + this.offset.y};
         var sourcePosition = new Complex(this.refPoint[0] - com.x,this.refPoint[1] - com.y);
         var newCenterOfMass = com;
         var targetPosition = new Complex(to.x - newCenterOfMass.x,to.y - newCenterOfMass.y);
         var sourceAngle = ((sourcePosition.argument() * 180 / Math.PI) + 360) % 360;
         var targetAngle = ((targetPosition.argument() * 180 / Math.PI) + 360) % 360;
         var dAngle = (targetAngle - sourceAngle + 360) % 360;
         var dOffset = {x: newCenterOfMass.x - this.centerOfMass.x, y:newCenterOfMass.y - this.centerOfMass.y};
         this.rotate(dAngle);
         this.translate(dOffset);
         this.updateTransform();
      };

      Polygon.prototype.translate = function(z) {
         this.offset.x = z.x;
         this.offset.y = z.y;
         this.updateTransform();
      };

      Polygon.prototype.rotate = function(angle) {
         this.angle = angle;
         this.updateTransform();
      };


      Polygon.prototype.centroid = function(z) {
          var n = this.data.length - 1;
          var centroid = {x:0, y:0}
          for(var i=0 ; i < n ; i++) {
             centroid.x += this.data[i].x;
             centroid.y += this.data[i].y;
          }
          centroid.x = centroid.x/n;
          centroid.y = centroid.y/n;
          return centroid;

      };

      return Polygon;

    })();
