namespace Chapter1
   module BallSimulation =
    open System

    let dt = 0.1
    let g = -9.81

    let simulation_step (y,v) =
       let y',v' = (y + v * dt, v + g * dt)

       if y' < 0.0 then
         (0.0,-v' * 0.7)
       else
         (y',v')

    let print_scene (y1,y2,y3,y4) =
      do Console.Clear()
      let y1,y2,y3,y4 = int y1,int y2,int y3,int y4
  
      for j = 10 downto 0 do
        for i = 0 to 30 do
          if ((y1+1) = j && i = 15) || ((y2+1) = i && j = 5) || ((9-y3) = j && i = 20) || ((29-y4) = i && j = 5) then
            Console.Write("b")
          elif j = 0 || i = 0 || j = 10 || i = 30 then
            Console.Write("*")
          else
            Console.Write(" ")
        Console.Write("\n")
      ignore(Console.ReadKey())

    let simulation() =
      let pauze = Console.ReadKey()
      let rec simulation (y1,v1,y2,v2,y3,v3,y4,v4) =
        do print_scene (y1,y2,y3,y4)
        let y1',v1' = simulation_step (y1,v1)
        let y2',v2' = simulation_step (y2,v2)
        let y3',v3' = simulation_step (y3,v3)
        let y4',v4' = simulation_step (y4,v4)
        if abs v1' > 0.1 then
          do simulation (y1',v1',y2',v2',y3',v3',y4',v4')
        if abs v2' > 0.1 then
          do simulation (y1',v1',y2',v2',y3',v3',y4',v4')
        if abs v3' > 0.1 then
          do simulation (y1',v1',y2',v2',y3',v3',y4',v4')
        if abs v4' > 0.1 then
          do simulation (y1',v1',y2',v2',y3',v3',y4',v4')
      do simulation (5.0,0.0,15.0,-2.0,7.0,-2.0,20.0,-2.0)


