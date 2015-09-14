// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open Chapter2.RocketSimulation
[<EntryPoint>]
let main argv = 
    do simulation()
    printfn "%A" argv
    0 // return an integer exit code
