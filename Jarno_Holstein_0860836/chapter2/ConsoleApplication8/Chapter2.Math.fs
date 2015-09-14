namespace Chapter2
  module Math =
    [<Measure>]
    type m

    [<Measure>]
    type kg

    [<Measure>]
    type s

    [<Measure>]
    type N = kg * m / s^2

    type Vector2<[<Measure>] 'a> =
      {
        X : float<'a>
        Y : float<'a>
      }
   
      static member Zero : Vector2<'a> = { X = 0.0<_>; Y = 0.0<_> }

      static member ( + ) (v1:Vector2<'a>,v2:Vector2<'a>):Vector2<'a> = { X = v1.X + v2.X; Y = v1.Y + v2.Y }
      static member ( + ) (v:Vector2<'a>,k:float<'a>):Vector2<'a> = { X = v.X + k; Y = v.Y + k }
      static member ( + ) (k:float<'a>,v:Vector2<'a>):Vector2<'a> = v + k
  
      static member ( ~- ) (v:Vector2<'a>):Vector2<'a> = { X = -v.X; Y = -v.Y }
      
      static member ( - ) (v1:Vector2<'a>,v2:Vector2<'a>):Vector2<'a> = v1 + (-v2)
      static member ( - ) (v:Vector2<'a>,k:float<'a>):Vector2<'a> = v + (-k)
      static member ( - ) (k:float<'a>,v:Vector2<'a>):Vector2<'a> = k + (-v)
 
      static member ( * ) (v1:Vector2<'a>,v2:Vector2<'b>):Vector2<'a * 'b> = { X = v1.X * v2.X; Y = v1.Y * v2.Y }
      static member ( * ) (v:Vector2<'a>,f:float<'b>):Vector2<'a * 'b> = { X = v.X * f; Y = v.Y * f }
      static member ( * ) (f:float<'b>,v:Vector2<'a>):Vector2<'b * 'a> = { X = f * v.X; Y = f * v.Y }

      static member ( / ) (v:Vector2<'a>,f:float<'b>):Vector2<'a / 'b> = v * (1.0 / f)

      member this.Length : float<'a> = sqrt((this.X * this.X + this.Y * this.Y))

      static member Distance(v1:Vector2<'a>,v2:Vector2<'a>) = (v1-v2).Length
      static member Normalize(v:Vector2<'a>):Vector2<1> = v / v.Length

      member this.Normalized = this / this.Length

      static member Dot(v1:Vector2<'a>,v2:Vector2<'a>) = v1.X * v2.X + v1.Y * v2.Y
