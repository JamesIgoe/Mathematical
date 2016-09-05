// Learn more about F# at http://fsharp.net

#light
module Stat

let public PrintFloat text x = printfn "Printing %s: %f..." text x
let public PrintInt text x = printfn "Printing %s: %d..." text x

let private diffSquared mean x = (x - mean) ** 2.0

let public SquaredDifferenceArray items = 
    let mean = Array.average items
    items |> Array.map (fun x -> diffSquared mean x)

let public SquaredDifferenceArrayPartial items = 
    let mean = Array.average items
    let diffSquaredAlt = diffSquared mean
    items |> Array.map (fun x -> diffSquaredAlt x)

let public SumOfSquares items = 
    let diffs = SquaredDifferenceArray items
    Array.sum diffs

let public Variance items = SumOfSquares(items) / (float(items.Length-1))

let public StdDev items = sqrt(Variance(items))

