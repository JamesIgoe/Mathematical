#light
module EulerLib

open Stat
open Print
open System.Numerics
open System
open System.Collections


//#region Unclassified

let public Reverse (s:string) = new string(s |> Seq.toArray |> Array.rev)

let rec public Remainders (num:int) (rem:int) (arr:list<int>) =
    let filteredList = arr |> List.filter (fun x -> x = num)
    if filteredList.Length > 1 || num = 0 then
        arr.Length - 1
    elif (num*10) < rem then
        Remainders (num*10) rem arr
    else
        let newRem = (num*10) % rem
        let newArr = List.append arr [newRem]
        Remainders newRem rem newArr

let public CreateArrayOfInt count = Array.zeroCreate<int> count
let public CreateArrayOfDouble count = Array.zeroCreate<float> count

let public IsPalindromicBase2 (n:int) =
     let forward = System.Convert.ToString(n,2) |> Seq.map (fun c -> int c - int '0') |> Seq.toArray
     let rev = Array.rev forward
     if forward = rev then
          true
     else
         false

//#endregion

//#region Primes

let public IsPrime n = 
    let upperBound = int32(sqrt(float(n)))
    let allNumbers = [2..upperBound]
    let divisors = allNumbers |> List.filter (fun div -> n % div = 0 && n <> div)
    if List.length divisors = 0 then 
        true
    else 
        false

let public GetPrimes max =
  let primes = new BitArray(max+1, true)
  seq { 2 .. max } |>
  Seq.filter (fun n ->
    if primes.[int n] then
      for i in int64 n * int64 n..int64 n..int64 max do primes.[int i] <- false
    primes.[int n])

let rec public FindDistinctPrimeFactors (number:int) (primeIndex:int) (primes:array<int>) (acc:array<int>) = 
    let currentPrime = primes.[primeIndex]
    if number = 1 then
        acc
    else
        if number % currentPrime = 0 then
            let newAcc = Array.append acc [|currentPrime|]
            let nextNum = (number/currentPrime) 
            FindDistinctPrimeFactors nextNum primeIndex primes newAcc
        else
            let newDiv = primes.[primeIndex+1]
            if number % newDiv = 0 then
                let newRem = number / newDiv
                let newAcc = Array.append acc [|newDiv|]
                FindDistinctPrimeFactors newRem (primeIndex) primes newAcc
            else
                FindDistinctPrimeFactors number (primeIndex+1) primes acc

//#endregion

//#region Random Arrays

//let arrayCreate count = Array.create count 0.0
let public RandomArray count =
    let arr = CreateArrayOfDouble count
    let r = System.Random(DateTime.Now.Millisecond)
    for i = 0 to count - 1 do
        arr.[i] <- r.NextDouble()
    arr

let public RandomArrayMultiplied count mult =
    let arr = CreateArrayOfDouble count
    let r = System.Random(DateTime.Now.Millisecond)
    for i = 0 to count - 1 do
        arr.[i] <- r.NextDouble() * mult
    arr

//#endregion

//#region Factorial

let rec public FactorialBigInt (n:BigInteger) : BigInteger =
    if n <= 1I then 
        1I
    else
        n * FactorialBigInt (n - 1I)

let rec public Factorial n acc =
     if n <= 1 then
          acc 
     else
          let newAcc = n * acc
          Factorial (n-1) newAcc

//#endregion

//#region Powers

let rec public PowerBigInt (num:bigint) (power:bigint) (max:bigint) (acc:bigint) =
    if  power = max  then
        acc  * num
    else
        PowerBigInt num (power + 1I) max (num * acc)

let public PowerBySelf (num:int) = 
    let newNum = float num
    (newNum)**(newNum)

//#endregion

//#region Fibonacci

let rec public FibonacciArray first second max arr =
    if first + second > max then
        arr
    else
        let next = first + second
        let newArr = List.append arr [next]
        FibonacciArray  second next max newArr
        
let rec public FibonnaciBigInt (first:BigInteger) (second:BigInteger) max acc =
    let lenY = (second.ToString()).Length
    if lenY >= max then
        acc + 1
    else
        let next = first + second
        let item = acc + 1
        FibonnaciBigInt second next max item

//#endregion

//#region Collatz

let rec public CollatzCount (n:int64) count = 
    if n = 1L then
        count
    elif n % 2L > 0L then
        CollatzCount ((3L*n) + 1L) (count + 1)
    else
        CollatzCount (n/2L)  (count + 1)

let rec public CollatzSequenceCount num count =
    if num = 1 then
        count
    else
        let rem = if num % 2 = 0 then num/2 else (3*num) + 1
        CollatzSequenceCount rem (count+1)

let rec public CollatzSequenceArray num count arr =
    if num = 1 then
        arr
    else
        let rem = if num % 2 = 0 then num/2 else (3*num) + 1
        let newArr = List.append  arr [rem]
        CollatzSequenceArray rem (count+1) newArr

//#endregion

//#region Terra

let rec public TerrasSequenceCount num count =
    if num = 2 || count = 10 then
        count
    else
        let rem = if (num-1) % 2 = 0 then (num-1)/2 else (3*(num-1) + 1)/2
        TerrasSequenceCount rem (count+1)

let rec public TerrasSequenceArray num count arr =
    if num = 1 || count = 10 then
        arr
    else
        let rem = if num % 2 = 0 then num/2 else (3*(num) + 1)/2
        let newArr = List.append  arr [rem]
        TerrasSequenceArray rem (count+1) newArr

//#endregion

//#region Narcissim

let public IsNarcissisticBool num power = 
    let numAsString = num.ToString()
    let arr = [for i=0 to (numAsString.Length - 1) do 
                    yield (float(numAsString.Chars(i).ToString()))**power]
    if num = int(List.sum arr) then
        true
    else 
        false

        
let IsNarcissisticNumeric num power =
    let numAsString = num.ToString()
    let arr = [for i=0 to (numAsString.Length - 1) do
                yield (float(numAsString.Chars(i).ToString()))**power]
    if num = int(List.sum arr) then
        num
    else
        0

//#endregion

//#region Tuples

let public First (x,y,z) = x
let public Second (x,y,z) = y
let public Third (x,y,z) = z
let public TupleProd (a,b,c,d) = a * b * c * d

//#endregion
