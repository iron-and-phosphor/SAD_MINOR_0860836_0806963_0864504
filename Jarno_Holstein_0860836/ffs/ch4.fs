namespace Chapter4
  module LargeAsteroidFieldSimulation =

    open System
    open System.Threading
    open Chapter2.Math
    open Chapter4.OctoTree
    
    type Asteroid3d =
      {
        Position : Vector3<m>
        Velocity : Vector3<m/s>
        Mass     : float<kg>
        Name     : string
      }

    let dt = 60.0<s>
    let G = 6.67e-11<m^3 * kg^-1 * s^-2>

    let earth_radius = 6.37e6<m>
    let field_size = earth_radius * 60.0
    let max_velocity = 2.3e4<m/s>
    let earth_mass  = 5.97e24<kg>
    let moon_mass = 7.35e22<kg>


    let create_3d_field num_asteroids =
      
      let lerp (x:float<'u>) (y:float<'u>) (a:float) = x * a + y * (1.0 - a)
      
      let rand = Random()
      
      [
        for i = 1 to num_asteroids do
          
          let m = (lerp earth_mass moon_mass (rand.NextDouble())) * 1.0e-4
          let x = lerp 0.0<m> field_size (rand.NextDouble())
          let y = lerp 0.0<m> field_size (rand.NextDouble())
          let z = lerp 0.0<m> field_size (rand.NextDouble())
          let vx = max_velocity * (rand.NextDouble() * 2.0 - 1.0) * 0.1
          let vy = max_velocity * (rand.NextDouble() * 2.0 - 1.0) * 0.1
          let vz = max_velocity * (rand.NextDouble() * 2.0 - 1.0) * 0.1
          
          yield
            {
              Position = { X = x; Y = y; Z = z }
              Velocity = { X = vx; Y = vy; Z = vz }
              Mass     = m
              Name     = "a" // string(char((int 'a') + rand.Next(27)))
            }

      ]

    let f0 = create_3d_field 200

    type Barycenter =
      {
        Position : Vector3<m>;
        Mass : float<kg>
      }
      
      member this.ToAsteroid3d :Asteroid3d = { Position = this.Position; Velocity = Vector3<_>.Zero; Mass = this.Mass; Name = "" }
      
      static member (+/) (b1:Barycenter, b2:Barycenter) =
        let new_mass = b1.Mass + b2.Mass
        if new_mass < 0.01<_> then
          {
            Position = Vector3<_>.Zero
            Mass     = 0.0<_>
          }
        else
          {
            Position = (b1.Position * b1.Mass + b2.Position * b2.Mass) / new_mass
            Mass     = new_mass
          }
      static member OfAsteroidList (l:Asteroid3d list) =
        let positions_weighted_sum = l |> Seq.map (fun a -> a.Position * a.Mass) |> Seq.sum
        let masses_sum = l |> Seq.sumBy (fun a -> a.Mass)
        {
          Position = if masses_sum > 0.01<_> then positions_weighted_sum / masses_sum else Vector3<_>.Zero
          Mass     = masses_sum
        }

    let force3d (a:Asteroid3d,a':Asteroid3d) =
      let dir = a'.Position - a.Position
      let dist = dir.Length + 1.0<m>
      G * a.Mass * a'.Mass * dir / (dist * dist * dist)

    let clamp3d (p:Vector3<_>,v:Vector3<_>) =
      let p,v =
        if p.X < 0.0<_> then
          { p with X = 0.0<_> }, { v with X = -v.X }
        else p,v
      let p,v =
        if p.X > field_size then
          { p with X = field_size }, { v with X = -v.X }
        else p,v
      let p,v =
        if p.Y < 0.0<_> then
          { p with Y = 0.0<_> }, { v with Y = -v.Y }
        else p,v
      let p,v =
        if p.Y > field_size then
          { p with Y = field_size }, { v with Y = -v.Y }
        else p,v
      let p,v =
        if p.Z < 0.0<_> then
          { p with Z = 0.0<_> }, { v with Z = -v.Z }
        else p,v
      let p,v =
        if p.Y > field_size then
          { p with Z = field_size }, { v with Z = -v.Z }
        else p,v
      p,v


    let fast_simulation_step (asteroids:Asteroid3d list) :Asteroid3d list=
      
      let empty_tree = OctoTree.mk_empty { Min = Vector3<_>.Zero; Size = { X = field_size / 8.0; Y = field_size / 8.0; Z = field_size / 8.0 } }
                                         { Min = Vector3<_>.Zero; Size = { X = field_size; Y = field_size; Z = field_size } }

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

      let vd = 
        if(Console.KeyAvailable) then
          let key_pressed = Console.ReadKey(true)
          match key_pressed.Key with
          |ConsoleKey.W -> 500.0<m/s>
          |ConsoleKey.S -> -500.0<m/s>
          | _ -> 0.0<m/s>
        else 0.0<m/s>                                 

      let rec new_force (asteroids:Asteroid3d list) :Asteroid3d list =
        match asteroids with
        | x::xs -> 
          {
            Position = { X = x.Position.X ; Y = x.Position.Y; Z = x.Position.Z }
            Velocity = { X = x.Velocity.X + vh; Y = x.Velocity.Y + vv ; Z = x.Velocity.Z + vd}
            Mass     = x.Mass
            Name     = x.Name
          }:: new_force xs
        |[] -> []           

      let tree = (new_force asteroids) |> List.fold (fun t a -> OctoTree.insert (fun (a:Asteroid3d) -> a.Position) a t) empty_tree
      
      let tree = tree |> OctoTree.fold (fun a b c d e f g h-> ((a +/ b) +/ (c +/ d)) +/ ((e +/ f) +/ (g +/ h))) Barycenter.OfAsteroidList

      let local_forces (others:Barycenter) asteroid_group =
        [
          for a in asteroid_group do
            let forces =
                 seq{
                   for a' in asteroid_group do
                     if a' <> a then
                       yield force3d(a,a')
                 }
            let F_local = Seq.sum forces
            let F = 
              if others.Mass < 0.01<_> then
                F_local
              else
                F_local + force3d(a,others.ToAsteroid3d)
            let p',v' = clamp3d(a.Position,a.Velocity)
            yield
              {
                a with
                    Position = p' + dt * v'
                    Velocity = v' + dt * F / a.Mass
              }
        ]

      
      let rec traverse (others:Barycenter) =
        function
        | OctoTree.Leaf(r,a,b) -> OctoTree.Leaf(r,local_forces others a, b)
        | OctoTree.Node(r, q111, q112, q121, q122, q211, q212, q221, q222, b) ->
          let q111' = traverse (others +/ q112.State +/ q121.State +/ q122.State +/ q211.State +/ q212.State +/ q221.State +/ q222.State) q111
          let q112' = traverse (others +/ q111.State +/ q121.State +/ q122.State +/ q211.State +/ q212.State +/ q221.State +/ q222.State) q112
          let q121' = traverse (others +/ q111.State +/ q112.State +/ q122.State +/ q211.State +/ q212.State +/ q221.State +/ q222.State) q121
          let q122' = traverse (others +/ q111.State +/ q112.State +/ q121.State +/ q211.State +/ q212.State +/ q221.State +/ q222.State) q122
          let q211' = traverse (others +/ q111.State +/ q112.State +/ q121.State +/ q122.State +/ q212.State +/ q221.State +/ q222.State) q211
          let q212' = traverse (others +/ q111.State +/ q112.State +/ q121.State +/ q122.State +/ q211.State +/ q221.State +/ q222.State) q212
          let q221' = traverse (others +/ q111.State +/ q112.State +/ q121.State +/ q122.State +/ q211.State +/ q212.State +/ q222.State) q221
          let q222' = traverse (others +/ q111.State +/ q112.State +/ q121.State +/ q122.State +/ q211.State +/ q212.State +/ q221.State) q222
          OctoTree.Node(r, q111', q112', q121', q122', q211', q212', q221', q222', b)
      
      (traverse { Position = Vector3<_>.Zero; Mass = 0.0<_> } tree).ToList


    
    let s = Diagnostics.Stopwatch()
    let print_framerate (asteroids:Asteroid3d list) =
      do Console.Clear()
      let dt = s.Elapsed
      let dt = 1.0 / dt.TotalSeconds
      do Console.WriteLine(dt.ToString("0#.#"))

    let print_3d_scene (asteroids:Asteroid3d list) =
      do Console.Clear()
      for i = 0 to 79 do
        Console.SetCursorPosition(i, 0)
        Console.Write("*")
        Console.SetCursorPosition(i, 23)
        Console.Write("*")
      for j = 0 to 23 do
        Console.SetCursorPosition(0,j)
        Console.Write("*")
        Console.SetCursorPosition(79,j)
        Console.Write("*")
      let set_cursor_on_body (b:Asteroid3d)=
        Console.SetCursorPosition(((b.Position.X / 4.0e8<m>) * 78.0 + 1.0) |> int, ((b.Position.Y / 4.0e8<m>) * 23.0 + 1.0) |> int)
      for a in asteroids do
        do set_cursor_on_body a
        do Console.Write(((a.Position.Z / 4.0e8<m>) * 8.0 + 1.0) |> int)
      do Thread.Sleep(100)

    
    let base_simulation print_3d_scene simulation_step =
      let rec simulation m =
        do print_3d_scene m
        do s.Reset()
        do s.Start()
        let m' = simulation_step m
        do s.Stop()
        do simulation m'
      do simulation f0

    
    (*let slow_simulation() = base_simulation print_scene simulation_step*)
    
    let fast_simulation() = base_simulation print_3d_scene fast_simulation_step

    [<EntryPoint>]
    let main args =
      do fast_simulation()
      0