    var Polygon = (function() {


      function Polygon(canvas, data, color) {

        this.data = data;
        this.color = color;
        this.canvas = canvas;
        var self = this;
        this.angle = 0;
        this.offset = {x:0, y:0};
        this.refPoint;
        
        this.line =  null;

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
             var mouseClick = d3.mouse(this);
             self.refPoint = {x: mouseClick[0], y: mouseClick[1]};
             d3.select(this).append("circle")
               .attr("stroke", "#C0C0C0")
               .attr("stroke-width", 2)
               .attr("fill", "none")
               .attr("cx",self.refPoint.x)
               .attr("cy",self.refPoint.y)
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
         var sourcePosition = new Complex(this.refPoint.x - com.x,this.refPoint.y - com.y);
         
         var r1 = sourcePosition.magnitude();
         var r2 = new Complex(to.x - com.x,to.y - com.y).magnitude();
         var c1 = new Complex(to.x + (to.x - com.x)*r1/r2, to.y + (to.y - com.y)*r1/r2);
         var c2 = new Complex(to.x - (to.x - com.x)*r1/r2, to.y - (to.y - com.y)*r1/r2);
         var C = new Complex(com.x, com.y);
         var d1 = c1.add(C.multiply(new Complex(-1,0))).magnitude();
         var d2 = c2.add(C.multiply(new Complex(-1,0))).magnitude();
         var com2 = {x: c1.re(), y: c1.im()};
         if (d2 < d1) com2 = {x: c2.re(), y: c2.im()};
         newCenterOfMass = 
           {x: com2.x, 
            y: com2.y};
         
         if (!self.line) {
           self.line = this.canvas.context().append("svg:line");
         } else {
           self.line
          .attr("x1",newCenterOfMass.x)
          .attr("y1", newCenterOfMass.y)
          .attr("x2", to.x)
          .attr("y2", to.y)
          .style("stroke", "rgb(6,120,155)");
          
    	}
         
           
         var targetPosition = new Complex(to.x - newCenterOfMass.x,to.y - newCenterOfMass.y);
         var sourceAngle = ((sourcePosition.argument() * 180 / Math.PI) + 360) % 360;
         var targetAngle = ((targetPosition.argument() * 180 / Math.PI) + 360) % 360;
         var dAngle = (targetAngle - sourceAngle + 360) % 360;
         var dOffset = {x: newCenterOfMass.x - this.centerOfMass.x , y:this.centerOfMass.y - com.y};
         this.rotate(dAngle);
//          this.translate(dOffset);
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
