
class Bezier
  constructor: (@z1, @z2, @z3) ->
    @x = 0
    @y = 0

rotate: (teta) -> new Bezier(@z1.rotate(teta), @z2.rotate(teta), @z3.rotate(teta))

  add: (b) -> 
   z = new Complex(@x,@y)
   z = z.add(this.z1).add(this.z2).add(this.z3);
   angle = this.z3.argument() - b.z1.argument();
   bezier = b.rotate(angle);
   bezier.moveTo(z.re(), z.im());
   return bezier;   

  moveTo: (x, y) -> 
    @x = x
    @y = y


  draw: (context) ->
    xAux = @x
    yAux = @y
    xAux += @z1.re() #-48
    yAux += @z1.im() #-120
    controlX1 = xAux
    controlY1 = yAux
    xAux += @z2.re() #248
    yAux += @z2.im() #0
    controlX2 = xAux
    controlY2 = yAux
    xAux+= @z3.re() #0
    yAux += @z3.im() #160
    endX = xAux
    endY = yAux
    context.beginPath()
    context.moveTo(@x, @y)
    context.bezierCurveTo controlX1, controlY1, controlX2, controlY2, endX, endY
    context.lineWidth = 10
    context.stroke()
    context.closePath()
