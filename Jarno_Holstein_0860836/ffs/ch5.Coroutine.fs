namespace Chapter5
  module Coroutines =

    open Microsoft.FSharp
    open Microsoft.FSharp.Core
    open System

    type Coroutine<'a> = Unit -> CoroutineStep<'a>
    and CoroutineStep<'a> =
      Return of 'a
      | Yield of Coroutine<'a>
      | ArrowYield of Coroutine<'a>

    type CoroutineBuilder() =
 
      member this.Return(x:'a) : Coroutine<'a> =
        fun s -> Return x

      member this.Bind(p : Coroutine<'a>,
                       k : 'a -> Coroutine<'b>) : Coroutine<'b> =
        fun s ->
          match p s with
          | Return x -> k x s
          | Yield p' -> Yield(this.Bind(p',k))
          | ArrowYield p' -> ArrowYield(this.Bind(p',k))

      member this.Combine(p1:Coroutine<'a>,
                          p2:Coroutine<'b>) : Coroutine<'b> =
        this.Bind(p1, fun _ -> p2)

      member this.Zero() : Coroutine<Unit> = this.Return()

      member this.ReturnFrom(s:Coroutine<'a>) = s

      member this.Delay s = s()
      member this.Run s = s

    let co = CoroutineBuilder()

    let yield_ : Coroutine<Unit> = fun s -> Yield(fun s -> Return())
    let arrow_yield_ : Coroutine<Unit> = fun s -> ArrowYield(fun s -> Return())

    let ignore_ (s:Coroutine<'a>) : Coroutine<Unit> =
      co{
        let! _ = s
        return ()
      }

    let rec (.||) (s1:Coroutine<'a>) (s2:Coroutine<'b>) : Coroutine<Choice<'a,'b>> =
      fun s ->
        match s1 s,s2 s with
        | Return x, _        -> Return(Choice1Of2 x)
        | _, Return y        -> Return(Choice2Of2 y)
        | ArrowYield k1, _   ->
          co{
            let! res = k1
            return Choice1Of2 res
          } |> Yield
        | _, ArrowYield k2   ->
          co{
            let! res = k2
            return Choice2Of2 res
          } |> Yield
        | Yield k1, Yield k2 -> (.||) k1 k2 |> Yield

    let (.||>) s1 s2 = ignore_ (s1 .|| s2)

    let rec (=>) (c:Coroutine<bool>) (s:Coroutine<'a>) : Coroutine<'a> =
      co{
        let! x = c
        if x then
          do! arrow_yield_
          let! res = s
          return res
        else
          do! yield_
          return! (=>) c s
      }

    let rec repeat_ (s:Coroutine<Unit>) : Coroutine<Unit> =
      co{
        do! s
        return! repeat_ s
      }

    let wait_doing (action:float -> Coroutine<Unit>) (interval:float) : Coroutine<Unit> =
      let time : Coroutine<DateTime> = fun _ -> Return(DateTime.Now)
      co{
        let! t0 = time
        let rec wait() =
          co{
              let! t = time
              let dt = (t - t0).TotalSeconds
              if dt < interval then
                do! yield_
                do! action dt
                return! wait()
          }
        do! wait()
      }

    let wait = wait_doing (fun (dt:float) -> co{ return () })