module Samples

// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.

#light
open Stat
open Print
open EulerLib
open System.Numerics
open System
open System.Threading

//generates array of random values
let arrayCreate count = Array.zeroCreate<float> count
//let arrayCreate count = Array.create count 0.0
let RandomArray count =
    let arr = arrayCreate count
    let r = System.Random(DateTime.Now.Millisecond)
    for i = 0 to count - 1 do
        arr.[i] <- r.NextDouble()
    arr

let RandomArrayMultiplied count mult =
    let arr = arrayCreate count
    let r = System.Random(DateTime.Now.Millisecond)
    for i = 0 to count - 1 do
        arr.[i] <- r.NextDouble() * mult
    arr

//using libary
let arrF = [|0.0..0.1..99.0|]
let var = Stat.Variance arrF
let sd1 = Stat.StdDev arrF 
let result = Stat.SumOfSquares(RandomArray 200)
let var2 = Stat.Variance(RandomArray 200)
let sd2 = Stat.StdDev(RandomArray 200)

//iter print from another library
Stat.SquaredDifferenceArray(arrF) |> Array.iter(fun x -> printfn "%f" x)
Stat.SquaredDifferenceArray(arrF) |> Array.iter(fun x -> Print.PrintFloat "Diff: " x)
arrF |> Array.iter(fun x -> Print.PrintFloat "Value: " x)

//filter examples
let arrSD = Stat.SquaredDifferenceArray(arrF)
let resultSD = arrSD |> Array.filter (fun x -> x < 5.0)
let arrSD2 = Stat.SquaredDifferenceArray([|0.1..0.1..9.0|])
let resultSD2 = arrSD2 |> Array.filter (fun x -> x < 5.0)
let resultSD2Inv = arrSD2 |> Array.filter (fun x -> x > 5.0)

//using mutable
let mutable tempVar = 123
tempVar <- 124

//basic string manipulation
let daughter = "kimberly"  // string
let firstChar = daughter.[0]   // first character, 'k'
let last = daughter.[daughter.Length-1]  


//partial evaluation of functions
let sumVals x y = x + y    
    // Original function that we know and love
let sumX = sumVals 10
    // Note: no 2nd param supplied.
    // sumX is a new function generated from partially applied sumVals.
    // Another way to say this is that “sumX is a partial application of sumVals.”
let sum = sumX 20
    // Invokes sumX, passing in expected int (parameter y from original)

//partial evaluation of functions - in use within Array.iter
let arrSDAlt = Stat.SquaredDifferenceArrayPartial(arrF)
let arrSDAlt2 = Stat.SquaredDifferenceArrayPartial(RandomArrayMultiplied 10 100.0)

//recursion
let rec loopExample current endVal acc =
    if  current > endVal  then
        acc
    else
        let result = current + acc
        let next = current + 1
        loopExample next endVal result
       
let resultLoop = loopExample 1 99 0


//more recursion
let rec factorial n = 
    if n <= 1 then 1   
    else n * factorial(n-1)

let resultFactorial = factorial 100

//more recursion, but tal
let rec factorialTailRec n acc = 
    if n <= 1 then 
        acc   
    else 
        let newAcc = n * acc
        factorialTailRec (n-1) newAcc
        
let factorial2 = factorialTailRec 10 1


//tail recursion
let rec factorialTail n result =
    if n <= 1 then result
    else
        factorialTail (n - 1) (n * result)

let resultTail = factorialTail 10 1


//stuff for higher order functions
let nums = Array.init 10 (fun n -> n)


//pipelining
let a x = x + 1
let b x = x + 2
let c x = x + 3
let d x = x + 4
let e x = x + 5
let r1 = a(b(c(d(e(100)))))
let r2 = 100 |> e |> d |> c |> b |> a


//more pipelining, but with a calc
let processes str =
    System.Diagnostics.Process.GetProcesses()
    |> Array.map(fun a -> a.ProcessName)
    |> Array.filter(fun a -> a.Contains(str))
let running = processes "p"


