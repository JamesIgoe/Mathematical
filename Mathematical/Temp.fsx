
open System
open System.Threading
open System.Threading.Tasks
open System.Reflection


let rec remainders (num:int) (rem:int) (arr:list<int>) =
    let filteredList = arr |> List.filter (fun x -> x = num)
    if filteredList.Length > 1 || num = 0 then
        arr.Length - 1
    elif (num*10) < rem then
        remainders (num*10) rem arr
    else
        let newRem = (num*10) % rem
        let newArr = List.append arr [newRem]
        remainders newRem rem newArr

let tempstart = System.DateTime.UtcNow

let remaindersParallel =
    [|1..999|] 
    |> Array.Parallel.map (fun x -> remainders 1 x [])

let remaindersMax arr = 
    Array.max arr
    
let remaindersIndex arr max = 
    let idx = arr |> Array.findIndex (fun x -> x = max)
    idx+1

let result = 
    let remainders = remaindersParallel
    let max = remaindersMax remainders
    let idx = remaindersIndex remainders max
    (idx, max)

let FactoralTest = Fac

printfn "%f" ((System.DateTime.UtcNow).Subtract(tempstart).TotalSeconds)

