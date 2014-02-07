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

        var centroidUI = this.polygonGroup.append("circle")
            .attr("id","centroid")
            .attr("stroke-width", 1)
            .attr("fill", this.color)
            .attr("cx",self.centerOfMass.x)
            .attr("cy",self.centerOfMass.y)
            .attr("opacity", 0.0)
            .attr("r",10);

        this.polygonGroup.on("mousedown",function() {
             var mouseClick = d3.mouse(this);
             self.refPoint = {x: mouseClick[0], y: mouseClick[1]};
             canvas.draggingStart(self,self.refPoint);
          })
         .on("mouseover",function() {
             var p = canvas.polygonBeingDragged();
             if (!p || p == self) {
               lineGraph.attr("opacity", 0.3);
               centroidUI.attr("opacity", 0.3);
             }
          })
         .on("mouseout",function() {
             var p = canvas.polygonBeingDragged();
             if (!p || p != self) {
               lineGraph.attr("opacity", 0.5);
               centroidUI.attr("opacity", 0.0);      
             }
          });
          
        centroidUI.on("mouseover", function() {
           d3.select(this).attr("stroke", "black")
                          .attr("stroke-width",2);
           
        }).on("mouseout", function() {
             d3.select(this).attr("stroke", "none");
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
         var sourcePosition = new Complex(this.refPoint.x - this.centerOfMass.x,this.refPoint.y - this.centerOfMass.y);
         
         var dist = new Complex(this.refPoint.x - this.centerOfMass.x, this.refPoint.y - this.centerOfMass.y).magnitude();
         var newCenterOfMass = {x: this.centerOfMass.x + to.x - this.refPoint.x, 
                                y: this.centerOfMass.y + to.y - this.refPoint.y};
         
         if (dist > 10) {
           var r1 = sourcePosition.magnitude();
           var r2 = new Complex(to.x - com.x,to.y - com.y).magnitude();
           var c1 = new Complex(to.x + (to.x - com.x)*r1/r2, to.y + (to.y - com.y)*r1/r2);
           var c2 = new Complex(to.x - (to.x - com.x)*r1/r2, to.y - (to.y - com.y)*r1/r2);
           var C = new Complex(com.x, com.y);
           var d1 = c1.add(C.multiply(new Complex(-1,0))).magnitude();
           var d2 = c2.add(C.multiply(new Complex(-1,0))).magnitude();
           newCenterOfMass = {x: c1.re(), y: c1.im()};
           if (d2 < d1) newCenterOfMass = {x: c2.re(), y: c2.im()};
  
           var targetPosition = new Complex(to.x - newCenterOfMass.x,to.y - newCenterOfMass.y);
           var sourceAngle = ((sourcePosition.argument() * 180 / Math.PI) + 360) % 360;
           var targetAngle = ((targetPosition.argument() * 180 / Math.PI) + 360) % 360;
           var dAngle = (targetAngle - sourceAngle + 360) % 360;
           this.rotate(dAngle);
         }
         
           
         var dOffset = {x: newCenterOfMass.x - this.centerOfMass.x , y:newCenterOfMass.y - this.centerOfMass.y };
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
