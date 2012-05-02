class Complex
  constructor: (@a, @b) ->

  add: (z) ->  new Complex @a+z.a, @b+z.b
   
  multiply: (z) -> new Complex @a*z.a - @b*z.b , @a*z.b + @b*z.a

  magnitude: -> Math.sqrt @a*@a + @b*@b
   
  conjugate: -> new Complex @a, -@b

  argument: -> Math.atan2 @b,@a

  re: -> @a
  im: -> @b

  rotate: (teta) -> new Complex(Math.cos(teta),Math.sin(teta)).multiply(this)
  
  toString: ->
       if @b < 0
          "#{@a} - #{Math.abs(@b)}i"
       else 
          "#{@a} + #{@b}i"
