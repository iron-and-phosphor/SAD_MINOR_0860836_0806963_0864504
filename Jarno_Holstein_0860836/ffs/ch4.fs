namespace Chapter4
  module LargeAsteroidFieldSimulation =

    open System
    open System.Threading
    open Chapter2.Math
    open Chapter3.SmallAsteroidFieldSimulation

    let f0 = create_field 200

    type Barycenter =
      {
        Position : Vector2<m>;
        Mass : float<kg>
      }
      
      member this.ToAsteroid = { Position = this.Position; Mass = this.Mass; Name = ""; Velocity = Vector2<_>.Zero }
      
      static member (+/) (b1:Barycenter, b2:Barycenter) =
        let new_mass = b1.Mass + b2.Mass
        if new_mass < 0.01<_> then
          {
            Position = Vector2<_>.Zero
            Mass     = 0.0<_>
          }
        else
          {
            Position = (b1.Position * b1.Mass + b2.Position * b2.Mass) / new_mass
            Mass     = new_mass
          }
      static member OfAsteroidList (l:Asteroid list) =
        let positions_weighted_sum = l |> Seq.map (fun a -> a.Position * a.Mass) |> Seq.sum
        let masses_sum = l |> Seq.sumBy (fun a -> a.Mass)
        {
          Position = if masses_sum > 0.01<_> then positions_weighted_sum / masses_sum else Vector2<_>.Zero
          Mass     = masses_sum
        }

    
    let fast_simulation_step (asteroids:Asteroid list) =
      
      let empty_tree = QuadTree.mk_empty { Min = Vector2<_>.Zero; Size = { X = field_size / 8.0; Y = field_size / 8.0 } }
                                         { Min = Vector2<_>.Zero; Size = { X = field_size; Y = field_size } }

      let vv = 
        if(Console.KeyAvailable) then
          let key_pressed = Console.ReadKey(true)
          match key_pressed.Key with
          |ConsoleKey.UpArrow -> -500.0<m/s>
          |ConsoleKey.DownArrow -> 500.0<m/s>
          | _ -> 0.0<m/s>
        else 0.0<m/s>
      
      let vh = 
        if(Console.KeyAvailable) then
          let key_pressed = Console.ReadKey(true)
          match key_pressed.Key with
          |ConsoleKey.RightArrow -> 500.0<m/s>
          |ConsoleKey.LeftArrow -> -500.0<m/s>
          | _ -> 0.0<m/s>
        else 0.0<m/s>                                 

      let rec new_force (asteroids:Asteroid list) :Asteroid list =
        match asteroids with
        | x::xs -> 
          {
            Position = { X = x.Position.X ; Y = x.Position.Y }
            Velocity = { X = x.Velocity.X + vh; Y = x.Velocity.Y + vv}
            Mass     = x.Mass
            Name     = x.Name
          }:: new_force xs
        |[] -> []          

      let tree = (new_force asteroids) |> List.fold (fun t a -> QuadTree.insert (fun (a:Asteroid) -> a.Position) a t) empty_tree
      
      let tree = tree |> QuadTree.fold (fun a b c d -> (a +/ b) +/ (c +/ d)) Barycenter.OfAsteroidList

      let local_forces (others:Barycenter) asteroid_group =
        [
          for a in asteroid_group do
            let forces =
                 seq{
                   for a' in asteroid_group do
                     if a' <> a then
                       yield force(a,a')
                 }
            let F_local = Seq.sum forces
            let F = 
              if others.Mass < 0.01<_> then
                F_local
              else
                F_local + force(a,others.ToAsteroid)
            let p',v' = clamp(a.Position,a.Velocity)
            yield
              {
                a with
                    Position = p' + dt * v'
                    Velocity = v' + dt * F / a.Mass
              }
        ]

      
      let rec traverse (others:Barycenter) =
        function
        | QuadTree.Leaf(r,a,b) -> QuadTree.Leaf(r,local_forces others a, b)
        | QuadTree.Node(r,q11,q12,q21,q22,b) ->
          let q11' = traverse (others +/ q12.State +/ q21.State +/ q22.State) q11
          let q12' = traverse (others +/ q11.State +/ q21.State +/ q22.State) q12
          let q21' = traverse (others +/ q12.State +/ q11.State +/ q22.State) q21
          let q22' = traverse (others +/ q12.State +/ q21.State +/ q11.State) q22
          QuadTree.Node(r,q11',q12',q21',q22',b)

      
      (traverse { Position = Vector2<_>.Zero; Mass = 0.0<_> } tree).ToList


    
    let s = Diagnostics.Stopwatch()
    let print_framerate (asteroids:Asteroid list) =
      do Console.Clear()
      let dt = s.Elapsed
      let dt = 1.0 / dt.TotalSeconds
      do Console.WriteLine(dt.ToString("0#.#"))

    
    let base_simulation print_scene simulation_step =
      let rec simulation m =
        do print_scene m
        do s.Reset()
        do s.Start()
        let m' = simulation_step m
        do s.Stop()
        do simulation m'
      do simulation f0

    
    let slow_simulation() = base_simulation print_scene simulation_step
    let slow_simulation_framerate() = base_simulation print_framerate simulation_step

    let fast_simulation() = base_simulation print_scene fast_simulation_step
    let fast_simulation_framerate() = base_simulation print_framerate fast_simulation_step

    [<EntryPoint>]
    let main args =
      do fast_simulation()
      0