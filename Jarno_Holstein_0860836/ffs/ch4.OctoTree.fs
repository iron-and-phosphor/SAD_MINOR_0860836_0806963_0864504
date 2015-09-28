namespace Chapter4
  module OctoTree =
    open Chapter2.Math

    type Vector3<[<Measure>] 'a> =
      {
        X : float<'a>
        Y : float<'a>
        Z : float<'a>
      }
    
      static member Zero : Vector3<'a> = { X = 0.0<_>; Y = 0.0<_>; Z = 0.0<_> }

      static member ( + ) (v1:Vector3<'a>,v2:Vector3<'a>):Vector3<'a> = { X = v1.X + v2.X; Y = v1.Y + v2.Y; Z = v1.Z + v2.Z }
      static member ( + ) (v:Vector3<'a>,k:float<'a>):Vector3<'a> = { X = v.X + k; Y = v.Y + k; Z = v.Z + k }
      static member ( + ) (k:float<'a>,v:Vector3<'a>):Vector3<'a> = v + k
      
      static member ( ~- ) (v:Vector3<'a>):Vector3<'a> = { X = -v.X; Y = -v.Y; Z = -v.Z }
      
      static member ( - ) (v1:Vector3<'a>,v2:Vector3<'a>):Vector3<'a> = v1 + (-v2)
      static member ( - ) (v:Vector3<'a>,k:float<'a>):Vector3<'a> = v + (-k)
      static member ( - ) (k:float<'a>,v:Vector3<'a>):Vector3<'a> = k + (-v)
 
      static member ( * ) (v1:Vector3<'a>,v2:Vector3<'b>):Vector3<'a * 'b> = {X = v1.Y * v2.Z - v1.Z * v2.Y;Y = v1.Z * v2.X - v1.X * v2.Z; Z = v1.X * v2.Y - v1.Y * v2.X }
      static member ( * ) (v:Vector3<'a>,f:float<'b>):Vector3<'a * 'b> = { X = v.X * f; Y = v.Y * f; Z = v.Z * f }
      static member ( * ) (f:float<'b>,v:Vector3<'a>):Vector3<'b * 'a> = { X = f * v.X; Y = f * v.Y; Z = f * v.Z}

      static member ( / ) (v:Vector3<'a>,f:float<'b>):Vector3<'a / 'b> = v * (1.0 / f)

      member this.Length : float<'a> = sqrt((this.X * this.X + this.Y * this.Y))

      static member Distance(v1:Vector3<'a>,v2:Vector3<'a>) = (v1-v2).Length
      static member Normalize(v:Vector3<'a>):Vector3<1> = v / v.Length

      member this.Normalized = this / this.Length

      static member Dot(v1:Vector3<'a>,v2:Vector3<'a>) = v1.X * v2.X + v1.Y * v2.Y + v1.Z * v2.Z
(*
      *)

    type Range<[<Measure>] 'u> =
      {
        Min  : Vector3<'u>
        Size : Vector3<'u>
      }

      member this.IsIn v =
        let d = v - this.Min
        v.X >= this.Min.X && v.Y >= this.Min.Y && d.X <= this.Size.X && d.Y <= this.Size.Y

    type OctoTree<'a, 'b, [<Measure>] 'u> =
      | Leaf of Range<'u> * List<'a> * Option<'b>
      | Node of Range<'u> * OctoTree<'a,'b,'u> * OctoTree<'a,'b,'u> *  OctoTree<'a,'b,'u> * OctoTree<'a,'b,'u> 
                          * OctoTree<'a,'b,'u> * OctoTree<'a,'b,'u> *  OctoTree<'a,'b,'u> * OctoTree<'a,'b,'u>* Option<'b>
      with
        member this.Range =
          match this with
          | Leaf(r,_,_) -> r
          | Node(r,_,_,_,_,_,_,_,_,_) -> r
        member this.State =
          match this with
          | Leaf(_,_,Some s) -> s
          | Node(_,_,_,_,_,_,_,_,_,Some s) -> s
          | _ -> failwith "Null state"
        member this.ToList =
          match this with
          | Leaf(_,l,_) -> l
          | Node(_,a,b,c,d,e,f,g,h,_) -> a.ToList @ b.ToList @ c.ToList @ d.ToList @ e.ToList @ f.ToList @ g.ToList @ h.ToList


    let rec mk_empty (min:Range<_>) (range:Range<_>) =
      if min.Size.X < range.Size.X ||
         min.Size.Y < range.Size.Y ||
         min.Size.Z < range.Size.Z then
        let size' = range.Size / 2.0
        let r111 = { Min = range.Min; Size = size' }
        let r112 = { Min = range.Min + { size' with Y = 0.0<_>; Z = 0.0<_> }; Size = size' }
        let r121 = { Min = range.Min + { size' with X = 0.0<_>; Z = 0.0<_> }; Size = size' }
        let r122 = { Min = range.Min + { size' with Z = 0.0<_>}; Size = size' }
        let r211 = { Min = range.Min + { size' with X = 0.0<_>; Y = 0.0<_> }; Size = size' }
        let r212 = { Min = range.Min + { size' with Y = 0.0<_> }; Size = size' }
        let r221 = { Min = range.Min + { size' with X = 0.0<_> }; Size = size' }
        let r222 = { Min = range.Min + size'; Size = size' }
        Node(range,
             mk_empty min r111,
             mk_empty min r112,
             mk_empty min r121,
             mk_empty min r122,
             mk_empty min r211,
             mk_empty min r212,
             mk_empty min r221,
             mk_empty min r222,
             None)
      else
        Leaf(range,[],None)

    let rec insert position a =
      function
      | Leaf(range,l,s) -> Leaf(range,a :: l,s)
      | Node(r, n111, n112, n121, n122, n211, n212, n221, n222, s) ->
          let n111', n112', n121', n122', n211', n212', n221', n222' =
            if n111.Range.IsIn (position a) then
              (insert position a n111), n112, n121, n122, n211, n212, n221, n222
            elif n112.Range.IsIn (position a) then
              n111, (insert position a n112), n121, n122, n211, n212, n221, n222
            elif n121.Range.IsIn (position a) then
              n111, n112, (insert position a n121), n122, n211, n212, n221, n222
            elif n122.Range.IsIn (position a) then
              n111, n112, n121, (insert position a n122), n211, n212, n221, n222
            elif n211.Range.IsIn (position a) then
              n111, n112, n121, n122, (insert position a n211), n212, n221, n222
            elif n212.Range.IsIn (position a) then
              n111, n112, n121, n122, n211, (insert position a n212), n221, n222
            elif n221.Range.IsIn (position a) then
              n111, n112, n121, n122, n211, n212, (insert position a n221), n222
            else
              n111, n112, n121, n122, n211, n212, n221, (insert position a n222)
          Node(r, n111', n112', n121', n122', n211', n212', n221', n222', s)

    let rec fold (f: 'b -> 'b -> 'b -> 'b -> 'b -> 'b -> 'b -> 'b -> 'b) (z: 'a list -> 'b) =
      function
      | Leaf(range,l,_) -> Leaf(range,l,Some(z l))
      | Node(range, n111, n112, n121, n122, n211, n212, n221, n222, _) ->
          let n111, n112, n121, n122, n211, n212, n221, n222 = 
            fold f z n111,fold f z n112,fold f z n121,
            fold f z n122,fold f z n211,fold f z n212,
            fold f z n221,fold f z n222
          Node(range, n111, n112, n121, n122, n211, n212, n221, n222, Some(f n111.State n112.State n121.State n122.State n211.State n212.State n221.State n222.State))