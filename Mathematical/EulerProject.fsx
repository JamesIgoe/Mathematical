#light

// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.

#load "Stat.fs"
#load "Print.fs"
#load "EulerLib.fs"
open Stat
open Print
open EulerLib
open System.Numerics
open System
open System.Threading
open System.Threading.Tasks



//Project Euler #1 - If we list all the natural numbers below 10 that are multiples of 3 or 5, 
//we get 3, 5, 6 and 9. The sum of these multiples is 23.  
//Find the sum of all the multiples of 3 or 5 below 1000.
let start_1 = System.DateTime.Now
let natNums = [|1..999|] |> Array.filter (fun x -> x % 3 = 0|| x % 5 = 0) |> Array.sum
printfn "%s" ((System.DateTime.Now - start_1).ToString())


//Project Euler #2 - Find the sum of all the even-valued terms in the Fibonacci sequence 
//which do not exceed four million.
let start_2 = System.DateTime.Now

let evenArray = (1::2::(EulerLib.FibonacciArray 1 2 4000000 [])) |> List.filter (fun x -> x % 2 = 0) |> List.sum
printfn "%s" ((System.DateTime.Now - start_2).ToString())


//Prjoect Euler - #6:
//The sum of the squares of the first ten natural numbers is,
//12 + 22 + ... + 102 = 385
//The square of the sum of the first ten natural numbers is,
//(1 + 2 + ... + 10)2 = 552 = 3025
//Hence the difference between the sum of the squares of the first ten natural numbers 
//and the square of the sum is 3025  385 = 2640.
//Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
let start_6 = System.DateTime.Now
let sqrSumNat listNat = 
    let sumNat = Array.sum listNat
    sumNat * sumNat
let sumSqrListNat listNat = 
    let sqrListNat = listNat |> Array.map (fun x -> x * x )
    Array.sum sqrListNat
let diffNat100 = sumSqrListNat [|1..100|] - sqrSumNat [|1..100|]
printfn "%s" ((System.DateTime.Now - start_6).ToString())


//Prjoect Euler - #7:
//By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
//What is the 10001st prime number?
//let isPrime n = 
//    let upperBound = int32(sqrt(float(n)))
//    let allNumbers = [2..upperBound]
//    let divisors = allNumbers |> List.filter (fun div -> n % div = 0 && n <> div)
//    if List.length divisors = 0 then 
//        true
//    else 
//        false

let rec recursePrimes num count max = 
    if count = max then
        num-1
    else
        if EulerLib.IsPrime num then
            recursePrimes (num+1) (count+1) max
        else
            recursePrimes (num+1) (count) max

let UT_isPrime_3 = EulerLib.IsPrime 3
let UT_isPrime_4 = EulerLib.IsPrime 4
let UT_isPrime_5 = EulerLib.IsPrime 5
let UT_isPrime_6 = EulerLib.IsPrime 6
let UT_isPrime_7 = EulerLib.IsPrime 7
let UT_isPrime_104743 = EulerLib.IsPrime 104743

let UT_recursePrimes_3 = recursePrimes 2 0 2
let UT_recursePrimes_4 = recursePrimes 2 0 4
let UT_recursePrimes_5 = recursePrimes 2 0 5
let UT_recursePrimes_6 = recursePrimes 2 0 6
let UT_recursePrimes_7 = recursePrimes 2 0 7

let start_7 = System.DateTime.Now
let UT_recursePrimes_104743 = recursePrimes 2 0 10001
printfn "%s" ((System.DateTime.Now - start_7).ToString())



//Project Euler - #8
let arrStr = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"

let readArrStr arrStr = 
    let len = String.length arrStr
    let arrNum = Array.create len 0
    for i=0 to len-1 do
        arrNum.[i] <- System.Convert.ToInt32(arrStr.Chars(i).ToString())
    for i=0 to len-5 do
        arrNum.[i] <-  arrNum.[i]  * arrNum.[i+1]  * arrNum.[i+2]  * arrNum.[i+3]  * arrNum.[i+4]
    Array.max arrNum


let start_8 = System.DateTime.Now
let resultArrStr = readArrStr arrStr
printfn "%s" ((System.DateTime.Now - start_8).ToString())


//Project Euler - #20
//n! means n  (n  1)  ...  3  2  1
//Find the sum of the digits in the number 100!
//tail recursion

let resPete = EulerLib.FactorialBigInt 100I

let rec factorialTailBigIntTrue (n:BigInteger) (acc:BigInteger) : BigInteger =
    if n <= 1I then 
        acc
    else
        let newAcc = n * acc
        factorialTailBigIntTrue (n - 1I) newAcc

let resPete2 = factorialTailBigIntTrue 100I 1I

let sumFactorial = 
    let resFactorialBigInt = EulerLib.FactorialBigInt 100I
    let strCharFactorial = resFactorialBigInt.ToString()
    let max = strCharFactorial.Length
    let arrNum = Array.create max 0    
    for i=0 to max-1 do
        arrNum.[i] <- System.Convert.ToInt32(strCharFactorial.Chars(i).ToString())
    Array.sum arrNum


//Project Euler #5
//2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
//What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

//posibility 1
//build multi-dimension array
//for each number calculate multiple * 100 for each column
//then for each number from 1 to max number, see if in row

//possibility 2
//for every number from 1 to some maximum (or until)
//check to see if any divisior from 1 to 20 has remainer

let remainder div num =
    let divisors = [|1..div|]
    let rem = divisors 
                |> Array.map (fun d -> snd(System.Math.DivRem(num,d))) 
                |> Array.sum
    rem

let isEvenlyDivisble div num = 
    let sum = remainder div num
    if sum = 0 then
        true
    else
        false
  
let smallestWithNoRemainder div = 
    let mutable state = false
    let mutable i = 1
    while state = false do
        state <- isEvenlyDivisble div (i)
        i <- i+1
    i-1

////stefano ricciardi's
//let rec gcd (a:int64, b:int64) =
//    match b with
//       | x when x = 0L -> a
//       | _ -> gcd (b, a % b)
// 
//let rec lcm (a:int64, b:int64) = (a * b) / gcd (a,b)
//
//let rec lcmlist (list: int64 list) =
//    match list with
//        | [] -> 1L
//        | [a] ->  a
//        | [a;b] ->  lcm(a,b)
//        | h::t  ->  lcm(h, lcmlist t)
//
//let generateListTo top =
//    let l = [for i in 1L .. int64 top ->  i]
//    l
// 
//let resolveProblem5 = generateListTo 20 |>  lcmlist

let FindPrimeFactors n = 
    let primes = EulerLib.GetPrimes n
    let filteredList = primes |> Seq.filter(fun x -> n % x = 0)
    Seq.toList filteredList

let UT_FindPrimeFactos_2 = FindPrimeFactors 2
let UT_FindPrimeFactos_3 = FindPrimeFactors 3
let UT_FindPrimeFactos_4 = FindPrimeFactors 4
let UT_FindPrimeFactos_5 = FindPrimeFactors 5
let UT_FindPrimeFactos_6 = FindPrimeFactors 6
let UT_FindPrimeFactos_7 = FindPrimeFactors 7
let UT_FindPrimeFactos_8 = FindPrimeFactors 8
let UT_FindPrimeFactos_9 = FindPrimeFactors 9
let UT_FindPrimeFactos_10 = FindPrimeFactors 10

//unti test
let unitTest2_0 = isEvenlyDivisble 10 2520
let unitTest2_1 = isEvenlyDivisble 10 2521
let unitTest2_2 = isEvenlyDivisble 10 2519
let unitTest2_3 = isEvenlyDivisble 3 6
//unit test
let unitTest3_1 = smallestWithNoRemainder 1
let unitTest3_2 = smallestWithNoRemainder 2
let unitTest3_3 = smallestWithNoRemainder 3
let unitTest3_4 = smallestWithNoRemainder 4
let unitTest3_5 = smallestWithNoRemainder 10
let unitTest3_6 = smallestWithNoRemainder 11
let unitTest3_7 = smallestWithNoRemainder 12
let unitTest3_8 = smallestWithNoRemainder 13
let unitTest3_9 = smallestWithNoRemainder 14
let unitTest3_10 = smallestWithNoRemainder 20



//Project Euler - Problem #10
//The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
//Find the sum of all the primes below two million.
open System.Collections
open System.Numerics

let start_10_2 = System.DateTime.Now

let getPrimesBigInt max =
  let primes = new BitArray(max+1, true)
  seq { 2 .. max } |>
  Seq.filter (fun n ->
    if primes.[int n] then
      for i in bigint n * bigint n..bigint n..bigint max do primes.[int i] <- false
    primes.[int n])

let resPrimesAlt : BigInteger = 
    let arrPrimeBigInt = Array.ofSeq(getPrimesBigInt 2000000)   
    let rec sumArrayBigInt itemCounter (endVal:BigInteger) (acc:BigInteger): BigInteger =
        if  BigInteger(arrPrimeBigInt.[itemCounter]) = endVal  then
            acc + BigInteger(arrPrimeBigInt.[itemCounter])
        else
            let result = BigInteger(arrPrimeBigInt.[itemCounter]) + acc
            sumArrayBigInt (itemCounter+1) endVal result
    let endVal = BigInteger(arrPrimeBigInt.[Array.length arrPrimeBigInt - 1])
    let acc = BigInteger(0)
    let result = sumArrayBigInt 0 endVal acc
    result

printfn "%s" ((System.DateTime.Now - start_10_2).ToString())


let isPrime10 n = 
    let upperBound = int32(sqrt(float(n)))
    let allNumbers = [2..upperBound]
    let divisors = allNumbers |> List.filter (fun div -> n % div = 0 && n <> div)
    if List.length divisors = 0 then 
        true
    else 
        false

//Project Euler - Problem #16
//215 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
//What is the sum of the digits of the number 2^1000?
//recursive function to sum array of BigInteger

let rec powerBigInt (num:bigint) (power:bigint) (max:bigint) (acc:bigint) =
    if  power = max  then
        acc  * num
    else
        powerBigInt num (power + 1I) max (num * acc)

let sumPower (num:bigint) (power:bigint) (max:bigint) (acc:bigint)  = 
    let strCharProd = (powerBigInt num power max acc).ToString()
    let max = strCharProd.Length
    let arrNum = Array.create max 0    
    for i=0 to max-1 do
        arrNum.[i] <- int32(strCharProd.Chars(i).ToString())
    Array.sum arrNum

let resultPower11_1000 = powerBigInt 11I 1I 1000I 1I
let resultSumPower11_1000 = sumPower 2I 1I 1000I 1I
let resultSumPower11_10000 = sumPower 2I 1I 10000I 1I


//Problem # 2 Redux
//    * calculate Fibonacci series up to 4MM
//    * filter list for evens
//    * sum filtered lits
//fibonacci via recursion - rewritten as tail
let rec fibsRecTail2 x y acc max =
    if y > max then
        acc
    else
        let next = x + y
        let newAcc = List.append acc [next]
        fibsRecTail2 y next newAcc max

let Prob2Redux = 
    let lst = fibsRecTail2 1 1 [1;1] 4000000
    let sumEvenArray = lst |> List.filter (fun x -> x % 2 = 0) |> List.sum
    sumEvenArray

let Prob2Redux2 = (fibsRecTail2 1 1 [1;1] 4000000) |> List.filter (fun x -> x % 2 = 0) |> List.sum
   
let Prob2Redux3 max = 
    (fibsRecTail2 1 1 [1;1] max) |> List.filter (fun x -> x % 2 = 0) |> List.sum

let prob2Answer = Prob2Redux3 4000000

    
//Prjoect Euler - #6 Redux:
//The sum of the squares of the first ten natural numbers is,
//12 + 22 + ... + 102 = 385
//The square of the sum of the first ten natural numbers is,
//(1 + 2 + ... + 10)2 = 552 = 3025
//Hence the difference between the sum of the squares of the first ten natural numbers 
//and the square of the sum is 3025  385 = 2640.
//Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

let sqrSumNat2 listNat = Array.sum listNat |> (fun x -> x*x)
let sumSqrListNat2 listNat = listNat |> Array.map (fun x -> x * x ) |> Array.sum
let diffNat100_2 = sqrSumNat2 [|1..100|] - sumSqrListNat2 [|1..100|]

let diffNat100_3 n = ([|1..n|] |> Array.sum |> (fun x -> x*x)) - ([|1..n|] |> Array.map (fun x -> x * x ) |> Array.sum)

let test6_1 = sumSqrListNat2 [|1..100|]
let test6_2 = sqrSumNat2 [|1..100|]

let test6_3 = diffNat100_3 100

//Problem 25
//The Fibonacci sequence is defined by the recurrence relation:
//    F_(n) = F_(n−1) + F_(n−2), where F_(1) = 1 and F_(2) = 1.
//Hence the first 12 terms will be:
//    F_(1) = 1
//    F_(2) = 1
//    F_(3) = 2
//    F_(4) = 3
//    F_(5) = 5
//    F_(6) = 8
//    F_(7) = 13
//    F_(8) = 21
//    F_(9) = 34
//    F_(10) = 55
//    F_(11) = 89
//    F_(12) = 144
//The 12th term, F_(12), is the first term to contain three digits.
//What is the first term in the Fibonacci sequence to contain 1000 digits?

       
let rec fibsRecTailBigIntSingle (x:BigInteger) (y:BigInteger) acc max =
    let lenY = (y.ToString()).Length
    if lenY >= max then
        acc + 1
    else
        let next = x + y
        let item = acc + 1
        fibsRecTailBigIntSingle y next item max

let Prob25_1 = fibsRecTailBigIntSingle 1I 1I 1 1000
    

//Project Euler #3
//The prime factors of 13195 are 5, 7, 13 and 29.
//What is the largest prime factor of the number 600851475143?
open System.Collections
open System.Numerics

let getPrimesInt64 max =
  let primes = new BitArray(max+1, true)
  seq { 2 .. max } |>
  Seq.filter (fun n ->
    if primes.[int n] then
      for i in bigint n * bigint n..bigint n..bigint max do primes.[int i] <- false
    primes.[int n])

//use this size for your array of primes
let primes = getPrimesInt64 100000

let FindLargestPrimeFactor (n:BigInteger) = 
    let filteredList = primes |> Seq.filter(fun x -> n % bigint x = 0I)
    Seq.max filteredList

//result
let start_03 = System.DateTime.UtcNow
let resultingPrimeFactor = FindLargestPrimeFactor 600851475143I
printfn "%f" ((System.DateTime.UtcNow).Subtract(start_03).TotalSeconds)


//Problem # 1 Redux
let sumNums1_2 = [|1..999|] |> Array.filter (fun x -> x % 3 = 0|| x % 5 = 0) |> Array.sum
let sumNums1_3 = [|1..999|] |> Array.sumBy (fun x -> if x % 3 = 0|| x % 5 = 0 then x else 0)
let sumNums1_4 = [1..999] |> Seq.filter (fun x -> x % 3 = 0|| x % 5 = 0) |> Seq.sum
let sumNums1_5 = [1..999] |> Seq.sumBy (fun x -> if x % 3 = 0|| x % 5 = 0 then x else 0)


//Problem 4
//A palindromic number reads the same both ways. 
//The largest palindrome made from the product of two 2-digit numbers is 9009 = 91  99.
//Find the largest palindrome made from the product of two 3-digit numbers.
let corrMatrix start last =
    seq {for i in start..last do
            for j in start..last do
                if i < j then yield (i*j)}

let palindromes = 
    (corrMatrix 100 999) |> Seq.filter (fun x -> string x = EulerLib.Reverse(string x)) |> Seq.max


//Problem 9
//A Pythagorean triplet is a set of three natural numbers, a  b  c, for which, a2 + b2 = c2
//For example, 32 + 42 = 9 + 16 = 25 = 52.
//There exists exactly one Pythagorean triplet for which a + b + c = 1000.
//Find the product abc.
let triplets start last =
    seq {for i in start..last do
            for j in start..last do
                yield (float i,float j, sqrt(float((i*i) + (j*j))))}

let start_9_1 = System.DateTime.Now

let pythTriple1000 = 
    let values = triplets 1 1000
    let lst = Seq.toArray values
    let filter = lst |> Array.filter (fun x -> EulerLib.First(x) + EulerLib.Second(x) + EulerLib.Third (x) = float 1000)
    let final = filter |> Array.filter( fun x -> EulerLib.First(x) < EulerLib.Second(x)  && EulerLib.Second(x) < EulerLib.Third (x))
    let result = filter |> Array.map( fun x -> EulerLib.First(x) * EulerLib.Second(x) * EulerLib.Third (x))
    result.[0]

printfn "%s" ((System.DateTime.Now-start_9_1).ToString())

let start_9_2 = System.DateTime.Now

let pythTriple1000_2 = 
    let values = triplets 1 1000 |> Seq.filter (fun x -> EulerLib.First(x) < EulerLib.Second(x))
    let filter = values |> Seq.filter (fun x -> EulerLib.First(x) + EulerLib.Second(x) + EulerLib.Third (x) = float 1000)
    let result = filter |> Seq.map (fun x -> EulerLib.First(x) * EulerLib.Second(x) * EulerLib.Third (x))
    (Seq.toArray result).[0]
    
printfn "%s" ((System.DateTime.Now-start_9_2).ToString())


//Problem 13
//Work out the first ten digits of the sum of the following one-hundred 50-digit numbers.
//
//37107287533902102798797998220837590246510135740250
//46376937677490009712648124896970078050417018260538
//74324986199524741059474233309513058123726617309629
//91942213363574161572522430563301811072406154908250
//23067588207539346171171980310421047513778063246676
//89261670696623633820136378418383684178734361726757
//28112879812849979408065481931592621691275889832738
//44274228917432520321923589422876796487670272189318
//47451445736001306439091167216856844588711603153276
//70386486105843025439939619828917593665686757934951
//62176457141856560629502157223196586755079324193331
//64906352462741904929101432445813822663347944758178
//92575867718337217661963751590579239728245598838407
//58203565325359399008402633568948830189458628227828
//80181199384826282014278194139940567587151170094390
//35398664372827112653829987240784473053190104293586
//86515506006295864861532075273371959191420517255829
//71693888707715466499115593487603532921714970056938
//54370070576826684624621495650076471787294438377604
//53282654108756828443191190634694037855217779295145
//36123272525000296071075082563815656710885258350721
//45876576172410976447339110607218265236877223636045
//17423706905851860660448207621209813287860733969412
//81142660418086830619328460811191061556940512689692
//51934325451728388641918047049293215058642563049483
//62467221648435076201727918039944693004732956340691
//15732444386908125794514089057706229429197107928209
//55037687525678773091862540744969844508330393682126
//18336384825330154686196124348767681297534375946515
//80386287592878490201521685554828717201219257766954
//78182833757993103614740356856449095527097864797581
//16726320100436897842553539920931837441497806860984
//48403098129077791799088218795327364475675590848030
//87086987551392711854517078544161852424320693150332
//59959406895756536782107074926966537676326235447210
//69793950679652694742597709739166693763042633987085
//41052684708299085211399427365734116182760315001271
//65378607361501080857009149939512557028198746004375
//35829035317434717326932123578154982629742552737307
//94953759765105305946966067683156574377167401875275
//88902802571733229619176668713819931811048770190271
//25267680276078003013678680992525463401061632866526
//36270218540497705585629946580636237993140746255962
//24074486908231174977792365466257246923322810917141
//91430288197103288597806669760892938638285025333403
//34413065578016127815921815005561868836468420090470
//23053081172816430487623791969842487255036638784583
//11487696932154902810424020138335124462181441773470
//63783299490636259666498587618221225225512486764533
//67720186971698544312419572409913959008952310058822
//95548255300263520781532296796249481641953868218774
//76085327132285723110424803456124867697064507995236
//37774242535411291684276865538926205024910326572967
//23701913275725675285653248258265463092207058596522
//29798860272258331913126375147341994889534765745501
//18495701454879288984856827726077713721403798879715
//38298203783031473527721580348144513491373226651381
//34829543829199918180278916522431027392251122869539
//40957953066405232632538044100059654939159879593635
//29746152185502371307642255121183693803580388584903
//41698116222072977186158236678424689157993532961922
//62467957194401269043877107275048102390895523597457
//23189706772547915061505504953922979530901129967519
//86188088225875314529584099251203829009407770775672
//11306739708304724483816533873502340845647058077308
//82959174767140363198008187129011875491310547126581
//97623331044818386269515456334926366572897563400500
//42846280183517070527831839425882145521227251250327
//55121603546981200581762165212827652751691296897789
//32238195734329339946437501907836945765883352399886
//75506164965184775180738168837861091527357929701337
//62177842752192623401942399639168044983993173312731
//32924185707147349566916674687634660915035914677504
//99518671430235219628894890102423325116913619626622
//73267460800591547471830798392868535206946944540724
//76841822524674417161514036427982273348055556214818
//97142617910342598647204516893989422179826088076852
//87783646182799346313767754307809363333018982642090
//10848802521674670883215120185883543223812876952786
//71329612474782464538636993009049310363619763878039
//62184073572399794223406235393808339651327408011116
//66627891981488087797941876876144230030984490851411
//60661826293682836764744779239180335110989069790714
//85786944089552990653640447425576083659976645795096
//66024396409905389607120198219976047599490197230297
//64913982680032973156037120041377903785566085089252
//16730939319872750275468906903707539413042652315011
//94809377245048795150954100921645863754710598436791
//78639167021187492431995700641917969777599028300699
//15368713711936614952811305876380278410754449733078
//40789923115535562561142322423255033685442488917353
//44889911501440648020369068063960672322193204149535
//41503128880339536053299340368006977710650566631954
//81234880673210146739058568557934581403627822703280
//82616570773948327592232845941706525094512325230608
//22918802058777319719839450180888072429661980811197
//77158542502016545090413245809786882778948721859617
//72107838435069186155435662884062257473692284509516
//20849603980134001723930671666823555245252804609722
//53503534226472524250874054075591789781264330331690

open System.Collections
open System.Numerics

let hugeArray = 
    [|
    37107287533902102798797998220837590246510135740250I;
    46376937677490009712648124896970078050417018260538I;
    74324986199524741059474233309513058123726617309629I;
    91942213363574161572522430563301811072406154908250I;
    23067588207539346171171980310421047513778063246676I;
    89261670696623633820136378418383684178734361726757I;
    28112879812849979408065481931592621691275889832738I;
    44274228917432520321923589422876796487670272189318I;
    47451445736001306439091167216856844588711603153276I;
    70386486105843025439939619828917593665686757934951I;
    62176457141856560629502157223196586755079324193331I;
    64906352462741904929101432445813822663347944758178I;
    92575867718337217661963751590579239728245598838407I;
    58203565325359399008402633568948830189458628227828I;
    80181199384826282014278194139940567587151170094390I;
    35398664372827112653829987240784473053190104293586I;
    86515506006295864861532075273371959191420517255829I;
    71693888707715466499115593487603532921714970056938I;
    54370070576826684624621495650076471787294438377604I;
    53282654108756828443191190634694037855217779295145I;
    36123272525000296071075082563815656710885258350721I;
    45876576172410976447339110607218265236877223636045I;
    17423706905851860660448207621209813287860733969412I;
    81142660418086830619328460811191061556940512689692I;
    51934325451728388641918047049293215058642563049483I;
    62467221648435076201727918039944693004732956340691I;
    15732444386908125794514089057706229429197107928209I;
    55037687525678773091862540744969844508330393682126I;
    18336384825330154686196124348767681297534375946515I;
    80386287592878490201521685554828717201219257766954I;
    78182833757993103614740356856449095527097864797581I;
    16726320100436897842553539920931837441497806860984I;
    48403098129077791799088218795327364475675590848030I;
    87086987551392711854517078544161852424320693150332I;
    59959406895756536782107074926966537676326235447210I;
    69793950679652694742597709739166693763042633987085I;
    41052684708299085211399427365734116182760315001271I;
    65378607361501080857009149939512557028198746004375I;
    35829035317434717326932123578154982629742552737307I;
    94953759765105305946966067683156574377167401875275I;
    88902802571733229619176668713819931811048770190271I;
    25267680276078003013678680992525463401061632866526I;
    36270218540497705585629946580636237993140746255962I;
    24074486908231174977792365466257246923322810917141I;
    91430288197103288597806669760892938638285025333403I;
    34413065578016127815921815005561868836468420090470I;
    23053081172816430487623791969842487255036638784583I;
    11487696932154902810424020138335124462181441773470I;
    63783299490636259666498587618221225225512486764533I;
    67720186971698544312419572409913959008952310058822I;
    95548255300263520781532296796249481641953868218774I;
    76085327132285723110424803456124867697064507995236I;
    37774242535411291684276865538926205024910326572967I;
    23701913275725675285653248258265463092207058596522I;
    29798860272258331913126375147341994889534765745501I;
    18495701454879288984856827726077713721403798879715I;
    38298203783031473527721580348144513491373226651381I;
    34829543829199918180278916522431027392251122869539I;
    40957953066405232632538044100059654939159879593635I;
    29746152185502371307642255121183693803580388584903I;
    41698116222072977186158236678424689157993532961922I;
    62467957194401269043877107275048102390895523597457I;
    23189706772547915061505504953922979530901129967519I;
    86188088225875314529584099251203829009407770775672I;
    11306739708304724483816533873502340845647058077308I;
    82959174767140363198008187129011875491310547126581I;
    97623331044818386269515456334926366572897563400500I;
    42846280183517070527831839425882145521227251250327I;
    55121603546981200581762165212827652751691296897789I;
    32238195734329339946437501907836945765883352399886I;
    75506164965184775180738168837861091527357929701337I;
    62177842752192623401942399639168044983993173312731I;
    32924185707147349566916674687634660915035914677504I;
    99518671430235219628894890102423325116913619626622I;
    73267460800591547471830798392868535206946944540724I;
    76841822524674417161514036427982273348055556214818I;
    97142617910342598647204516893989422179826088076852I;
    87783646182799346313767754307809363333018982642090I;
    10848802521674670883215120185883543223812876952786I;
    71329612474782464538636993009049310363619763878039I;
    62184073572399794223406235393808339651327408011116I;
    66627891981488087797941876876144230030984490851411I;
    60661826293682836764744779239180335110989069790714I;
    85786944089552990653640447425576083659976645795096I;
    66024396409905389607120198219976047599490197230297I;
    64913982680032973156037120041377903785566085089252I;
    16730939319872750275468906903707539413042652315011I;
    94809377245048795150954100921645863754710598436791I;
    78639167021187492431995700641917969777599028300699I;
    15368713711936614952811305876380278410754449733078I;
    40789923115535562561142322423255033685442488917353I;
    44889911501440648020369068063960672322193204149535I;
    41503128880339536053299340368006977710650566631954I;
    81234880673210146739058568557934581403627822703280I;
    82616570773948327592232845941706525094512325230608I;
    22918802058777319719839450180888072429661980811197I;
    77158542502016545090413245809786882778948721859617I;
    72107838435069186155435662884062257473692284509516I;
    20849603980134001723930671666823555245252804609722I;
    53503534226472524250874054075591789781264330331690I |]
    
let rec SumHugeArray (counter:int) (acc:BigInteger) arr = 
    if counter = (Array.length arr) then
        acc
    else
        let accumulator = arr.[counter] + acc
        SumHugeArray (counter+1) accumulator arr
    
let tryIt = 
    let num = SumHugeArray 0 0I hugeArray
    let result = num.ToString()
    result.Substring(0,10)


//Project Euler - Problem #14
//The following iterative sequence is defined for the set of positive integers:
// - n → n/2 (n is even)
// - n → 3n + 1 (n is odd)
//Using the rule above and starting with 13, we generate the following sequence:
//13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
//It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
//Which starting number, under one million, produces the longest chain?
//NOTE: Once the chain starts the terms are allowed to go above one million.

let rec CollatzSequence num count =
    if num = 1 then
        count
    else
        let rem = if num % 2 = 0 then num/2 else (3*num) + 1
        CollatzSequence rem (count+1)

let rec CollatzSequenceArray num count arr =
    if num = 1 then
        arr
    else
        let rem = if num % 2 = 0 then num/2 else (3*num) + 1
        let newArr = List.append  arr [rem]
        CollatzSequenceArray rem (count+1) newArr

let rec TerrasSequence num count =
    if num = 2 || count = 10 then
        count
    else
        let rem = if (num-1) % 2 = 0 then (num-1)/2 else (3*(num-1) + 1)/2
        TerrasSequence rem (count+1)

let rec TerrasSequenceArray num count arr =
    if num = 1 || count = 10 then
        arr
    else
        let rem = if num % 2 = 0 then num/2 else (3*(num) + 1)/2
        let newArr = List.append  arr [rem]
        TerrasSequenceArray rem (count+1) newArr
              
let unitTest4 = CollatzSequence 4 1        
let unitTest4_1 = CollatzSequenceArray 4 1 [4]        
let unitTest4_2 = TerrasSequence 4 1
let unitTest4_3 = TerrasSequenceArray 3 1 [3] 
                
open System.Numerics
open System.Collections

let getPrimesForCollatz max =
  let primes = new BitArray(max+1, true)
  seq { 2 .. max } |>
  Seq.filter (fun n ->
    if primes.[int n] then
      for i in bigint n * bigint n..bigint n..bigint max 
        do primes.[int i] <- false
    primes.[int n])

let rec Collatz (n:int64) count = 
    if n = 1L then
        count
    elif n % 2L > 0L then
        Collatz ((3L*n) + 1L) (count + 1)
    else
        Collatz (n/2L)  (count + 1)

let CollatzSequenceMax start max =
    let nums = [start..max]
    let result = [ for i in nums -> Collatz (int64 i) 1 ]
    let longest = List.max result
    let idx = result |> List.findIndex (fun x -> x = longest)
    nums.[idx]

let CollatzSequenceMaxParallel start max =
    let nums = [|start..max|]
    let result = nums |> Array.Parallel.map (fun x -> Collatz x 1)
    let longest = Array.max result
    let idx = result |> Array.findIndex (fun x -> x = longest)
    nums.[idx]

let unitTestx_1_1 = CollatzSequenceMax 1 99
let unitTestx_2_1 = CollatzSequenceMax 1 999
let unitTestx_3_1 = CollatzSequenceMax 1 9999
let unitTestx_4_1 = CollatzSequenceMax 1 99999

let unitTestx_5_1 = CollatzSequenceMaxParallel (int64 1) (int64 99)
let unitTestx_6_1 = CollatzSequenceMaxParallel (int64 1) (int64 999)
let unitTestx_7_1 = CollatzSequenceMaxParallel (int64 1) (int64 9999)
let unitTestx_8_1 = CollatzSequenceMaxParallel (int64 1) (int64 99999)

let start_14 = System.DateTime.Now
let LargestCollatzSequence = CollatzSequenceMax 1 999999
printfn "CollatzSequenceMax - %s" ((System.DateTime.Now - start_14).ToString())

let start_14_2 = System.DateTime.Now
let LargestCollatzSequence_2 = CollatzSequenceMaxParallel (int64 1) (int64 999999)
printfn "CollatzSequenceMaxParallel - %s" ((System.DateTime.Now - start_14_2).ToString())

//Problem 21
//Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
//If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.
//For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. 
//The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
//Evaluate the sum of all the amicable numbers under 10000.

let filteredDivisors21 num = [1..num] |> List.filter (fun x -> num % x = 0 && x < num)

let rec factorList num max arr = 
    if num = max then
        arr
    else
        let newArr = List.append arr [(num), List.sum (filteredDivisors21 (num))]
        factorList (num+1) max newArr

let crossProductMatch l1 l2 =
  seq { for el1 in l1 do
          for el2 in l2 do
            if el1 = el2 then yield el1, el2 }
           
let resultingPairs = 
    let list1 = factorList 1 9999 [] |> List.filter (fun x -> fst(x) <> snd(x))
    let list2 = list1 |> List.map (fun x -> (snd(x),fst(x)))
    let listMatched = Seq.toList(crossProductMatch list1 list2)
    let listFiltered = listMatched |> List.map (fun x -> fst(x)) |> List.map (fun x -> fst(x))
    List.sum listFiltered

//redux - Problem 21
let DivisorsSum num = [1..num] |> List.filter (fun x -> num % x = 0 && x < num) |> List.sum

let rec matchLists num max arr =
    if num = max then
        (List.sum arr)/2
    else
        let newTest = DivisorsSum num
        let check = DivisorsSum(newTest)
        let newArr = arr
        if num = check && newTest <> check then
            matchLists (num+1) max (List.append newArr [newTest+check])
        else
            matchLists (num+1) max newArr

let rec matchListsHelper max = matchLists 1 max []

let UT_matchLists_10 = matchListsHelper 10
let UT_matchLists_100 = matchListsHelper 100
let UT_matchLists_1000 = matchListsHelper 1000

let start_21 = System.DateTime.Now
let UT_matchLists_10000 = matchListsHelper 10000
printfn "%s" ((System.DateTime.Now - start_21).ToString())

//Problem 23
//A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. 
//For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, 
// which means that 28 is a perfect number.
//
//A number n is called deficient if the sum of its proper divisors is less than n, 
// and it is called abundant if this sum exceeds n.
//
//As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written 
//as the sum of two abundant numbers is 24. By mathematical analysis, 
// it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. 
//However, this upper limit cannot be reduced any further by analysis 
//even though it is known that the greatest number that cannot be expressed as the sum of 
//two abundant numbers is less than this limit.
//
//Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

let ProperDivisors num = [1..num] |> List.filter (fun x -> num % x = 0 && x < num)

let ifAbundant div num  = 
    if List.sum div > num then
        [num]
    else
        []

let rec allAbundantNumbers num max acc = 
    if num = max then
        acc
    else
        let div = ProperDivisors num
        let newAcc = List.append acc (ifAbundant div num)
        allAbundantNumbers (num+1) max newAcc

let sumAbundants l1 l2 max =
    seq { for el1 in l1 do
          for el2 in l2 do
            let sum = (el1 + el2)
            if sum <= max then yield sum }

let getAllAbundantSums max = 
    let listAbundantNumbers = allAbundantNumbers 1 max []
    let allAbundantSums = sumAbundants listAbundantNumbers listAbundantNumbers max |> Seq.toList
    let allNums = [1..max]
    let result = System.Linq.Enumerable.Except(allNums,allAbundantSums) |> Seq.toList
    List.sum result
    //result
    //Seq.sum result

let startAS = System.DateTime.Now
let UT_getAllAbundantSums_5 = getAllAbundantSums 28123
printfn "UT_getAllAbundantSums_5 - %s" ((System.DateTime.Now - startAS).ToString())



//Problem #15
//Starting in the top left corner of a 2×2 grid, 
//there are 6 routes (without backtracking) to the bottom right corner.
//How many routes are there through a 20×20 grid?
//
//Thanks to: http://realultimateprogramming.blogspot.com/2009/03/project-euler-problem-15.html
//Requires combinatorics but lay explanation is that there are n!
let rec factorialBI (n:bigint) (acc:bigint) = 
    if n <= 1I then 
        acc   
    else 
        let newAcc = n * acc
        factorialBI (n-1I) newAcc

let combine (sides:bigint) =         
    let factorial1 = factorialBI (sides * 2I) 1I
    let factorial2 = factorialBI sides 1I
    factorial1/(factorial2 * factorial2)

let UT_combine_2 = combine 2I      
let UT_combine_3 = combine 3I      
let UT_combine_4 = combine 4I      
let UT_combine_5 = combine 5I      
let UT_combine_6 = combine 6I      
let UT_combine_7 = combine 7I      
let UT_combine_10 = combine 10I      
let UT_combine_20 = combine 20I



//Problem 11
//In the 20×20 grid below, four numbers along a diagonal line have been marked in red.
//
//08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
//49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
//81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
//52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
//22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
//24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
//32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
//67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
//24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
//21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
//78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
//16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
//86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
//19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
//04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
//88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
//04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
//20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
//20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
//01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48
//
//The product of these numbers is 26 × 63 × 78 × 14 = 1788696.
//
//What is the greatest product of four adjacent numbers in any direction 
// (up, down, left, right, or diagonally) in the 20×20 grid?


let matrices = 
    [[08; 02; 22; 97; 38; 15; 00; 40; 00; 75; 04; 05; 07; 78; 52; 12; 50; 77; 91; 08];
    [49; 49; 99; 40; 17; 81; 18; 57; 60; 87; 17; 40; 98; 43; 69; 48; 04; 56; 62; 00];
    [81; 49; 31; 73; 55; 79; 14; 29; 93; 71; 40; 67; 53; 88; 30; 03; 49; 13; 36; 65];
    [52; 70; 95; 23; 04; 60; 11; 42; 69; 24; 68; 56; 01; 32; 56; 71; 37; 02; 36; 91];
    [22; 31; 16; 71; 51; 67; 63; 89; 41; 92; 36; 54; 22; 40; 40; 28; 66; 33; 13; 80];
    [24; 47; 32; 60; 99; 03; 45; 02; 44; 75; 33; 53; 78; 36; 84; 20; 35; 17; 12; 50];
    [32; 98; 81; 28; 64; 23; 67; 10; 26; 38; 40; 67; 59; 54; 70; 66; 18; 38; 64; 70];
    [67; 26; 20; 68; 02; 62; 12; 20; 95; 63; 94; 39; 63; 08; 40; 91; 66; 49; 94; 21];
    [24; 55; 58; 05; 66; 73; 99; 26; 97; 17; 78; 78; 96; 83; 14; 88; 34; 89; 63; 72];
    [21; 36; 23; 09; 75; 00; 76; 44; 20; 45; 35; 14; 00; 61; 33; 97; 34; 31; 33; 95];
    [78; 17; 53; 28; 22; 75; 31; 67; 15; 94; 03; 80; 04; 62; 16; 14; 09; 53; 56; 92];
    [16; 39; 05; 42; 96; 35; 31; 47; 55; 58; 88; 24; 00; 17; 54; 24; 36; 29; 85; 57];
    [86; 56; 00; 48; 35; 71; 89; 07; 05; 44; 44; 37; 44; 60; 21; 58; 51; 54; 17; 58];
    [19; 80; 81; 68; 05; 94; 47; 69; 28; 73; 92; 13; 86; 52; 17; 77; 04; 89; 55; 40];
    [04; 52; 08; 83; 97; 35; 99; 16; 07; 97; 57; 32; 16; 26; 26; 79; 33; 27; 98; 66];
    [88; 36; 68; 87; 57; 62; 20; 72; 03; 46; 33; 67; 46; 55; 12; 32; 63; 93; 53; 69];
    [04; 42; 16; 73; 38; 25; 39; 11; 24; 94; 72; 18; 08; 46; 29; 32; 40; 62; 76; 36];
    [20; 69; 36; 41; 72; 30; 23; 88; 34; 62; 99; 69; 82; 67; 59; 85; 74; 04; 36; 16];
    [20; 73; 35; 29; 78; 31; 90; 01; 74; 31; 49; 71; 48; 86; 81; 16; 23; 57; 05; 54];
    [01; 70; 54; 71; 83; 51; 54; 69; 16; 92; 33; 48; 61; 43; 52; 01; 89; 19; 67; 04]]

let gathermatricesValues =
    let max = List.length matrices
    let acrossValues = 
        [for row=0 to (max-4) do
            for col=0 to (max-4) do
                yield EulerLib.TupleProd (matrices.[row].[col], matrices.[row].[col+1], matrices.[row].[col+2], matrices.[row].[col+3])]
    let downValues = 
        [for row=0 to (max-4) do
            for col=0 to (max-4) do
                yield EulerLib.TupleProd (matrices.[row].[col], matrices.[row+1].[col], matrices.[row+2].[col], matrices.[row+3].[col])]
    let diagonalDownRight = 
        [for row=0 to (max-4) do
            for col=0 to (max-4) do
                yield EulerLib.TupleProd (matrices.[row].[col], matrices.[row+1].[col+1], matrices.[row+2].[col+2], matrices.[row+3].[col+3])]
    let diagonalDownLeft = 
        [for row=0 to (max-4) do
            for col=3 to (max-1) do
                yield EulerLib.TupleProd (matrices.[row].[col], matrices.[row+1].[col-1], matrices.[row+2].[col-2], matrices.[row+3].[col-3])]
    [List.max acrossValues; List.max downValues; List.max diagonalDownRight; List.max diagonalDownLeft] 
    |> List.max


//alt idea
//extract 4x4 sub-matrix, starting with first cell [0..3,0..3]
//extract up to max-4 in both directions
//sum all across
//sum all down
//sum diagonals
//return list

//Problem 12
//The sequence of triangle numbers is generated by adding the natural numbers. So the 7^(th) triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten terms would be:
//1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
//Let us list the factors of the first seven triangle numbers:
//     1: 1
//     3: 1,3
//     6: 1,2,3,6
//    10: 1,2,5,10
//    15: 1,3,5,15
//    21: 1,3,7,21
//    28: 1,2,4,7,14,28
//We can see that 28 is the first triangle number to have over five divisors.
//What is the value of the first triangle number to have over five hundred divisors?
//Solution
//seq and yield?
//create triangle number, maybe rec?
//like a Fibonacci
//once new number created, filter for all factors <= number
//get count fo divisors
//Repeat until divisors lover 500

let filteredDivisors (num:bigint) = [|1I..num|] |> Array.filter (fun x -> num % x = 0I)

let rec triangleNumbers (current:bigint) (last:bigint) (max:bigint) arr = 
    if current >  max then
        arr
    else
        let newCurrent = current + 1I
        let newLast = newCurrent + last
        let newArr = Array.append arr [|newLast|]
        triangleNumbers newCurrent newLast max newArr

let longestDivisor = 
    let arr = triangleNumbers 0I 0I 100I [||]
    let test = arr |> Array.map (fun x -> filteredDivisors x)
    let max = Array.max test
    let index = test |> Array.findIndex (fun x -> x = max)
    arr.[index+1]
    
let UT_triangleNumbers_1000 = 
    let lastNum = Array.max (triangleNumbers 0I 0I 1000I [||])
    let div = Array.length(filteredDivisors lastNum)
    div

let UT_TriangleNumbers = triangleNumbers 0I 0I 100I [||]
    
let rec triangleNumbersAlt (current:bigint) (max:bigint) arr = 
    if current >  max then
        arr
    else
        let newTriangle = ((current * current) + current)/2I
        let newArr = Array.append arr [|newTriangle|]
        triangleNumbersAlt (current+1I) max newArr

let UT_TriangleNumbersAlt_100 = triangleNumbersAlt 0I 100I [||]
let UT_TriangleNumbersAlt_1000 = triangleNumbersAlt 0I 1000I [||]
let UT_TriangleNumbersAlt_10000 = triangleNumbersAlt 0I 10000I [||]

let getPrimesForFactorization (max) =
  let primes = new BitArray(max+1, true)
  seq { 2 .. max } |>
  Seq.filter (fun n ->
    if primes.[int n] then
      for i in int64 n * int64 n..int64 n..int64 max do primes.[int i] <- false
    primes.[int n])

let findPrimeFactors n = (getPrimesForFactorization n) |> Seq.filter (fun x -> bigint n % bigint x = 0I)

let test2222 = Seq.toArray (findPrimeFactors 103)

let filteredDivisorCount (num:bigint) = 
    let interim = [|1I..(num/2I)|] |> Array.filter (fun x -> num % x = 0I)
    (Array.length interim) + 1

//let UT_FilteredDivisorCount_4950 = filteredDivisorCount 4950I

let rec triangleNumbersCount (current:bigint) (triangleNum:bigint) (max:int) = 
    let count = filteredDivisorCount(triangleNum)
    if count >= max then
        (triangleNum, count)
    else
        let newTriangle = (current * (current + 1I))/2I
        triangleNumbersCount (current+1I) newTriangle max

let UT_triangleNumbersCount_1 = triangleNumbersCount 1I 1I 1
let UT_triangleNumbersCount_2 = triangleNumbersCount 1I 1I 2
let UT_triangleNumbersCount_3 = triangleNumbersCount 1I 1I 3
let UT_triangleNumbersCount_4 = triangleNumbersCount 1I 1I 4
let UT_triangleNumbersCount_5 = triangleNumbersCount 1I 1I 5
let UT_triangleNumbersCount_10 = triangleNumbersCount 0I 1I 10
let UT_triangleNumbersCount_50 = triangleNumbersCount 0I 1I 50

let start_12 = System.DateTime.UtcNow

//let UT_triangleNumbersCount_100 = triangleNumbersCount 0I 1I 100
let UT_triangleNumbersCount_200 = triangleNumbersCount 0I 1I 200

printfn "%f" ((System.DateTime.UtcNow).Subtract(start_12).TotalSeconds)

//fastest
//seq triangle numbers
//toArray to then get prime factors (tuple with power?)
//then raise to power as equation
//solve

//By starting at the top of the triangle below and moving to adjacent numbers on the row below, 
//the maximum total from top to bottom is 23.
//3
//7 5
//2 4 6
//8 5 9 3
//That is, 3 + 7 + 4 + 9 = 23.
//Find the maximum total from top to bottom of the triangle below:

let grid = 
              [[75;];
             [95;64];
            [17;47;82];
           [18;35;87;10];
          [20;04;82;47;65];
         [19;01;23;75;03;34];
        [88;02;77;73;07;63;67];
       [99;65;04;28;06;16;70;92];
      [41;41;26;56;83;40;80;70;33];
     [41;48;72;33;47;32;37;16;94;29];
    [53;71;44;65;25;43;91;52;97;51;14];
   [70;11;33;28;77;73;17;78;39;68;17;57];
  [91;71;52;38;17;14;91;43;58;50;27;29;48];
 [63;66;04;68;89;53;67;30;73;16;69;87;40;31];
[04;62;98;27;23;09;70;98;73;93;38;53;60;04;23]]

let rec SumBelow3 colPos items maxPos (topArray:list<int>) (bottomArray:list<int>) (acc:list<int>) =
    if colPos > maxPos then
        acc
    else
        let newSums = [for i=0 to (items) do 
                        yield topArray.[colPos] + bottomArray.[colPos + i]]
        let newAcc = List.append acc newSums
        SumBelow3 (colPos+1) items maxPos topArray bottomArray newAcc

let rec SumBelow4 colPos items maxPos (topArray:list<int>) (bottomArray:list<int>) (acc:list<int>) =
    if colPos > maxPos then
        acc
    else
        let newSums = [for i=0 to items do 
                        yield topArray.[colPos] + bottomArray.[colPos + i]]
        let newAcc = List.append acc newSums
        SumBelow4 (colPos+2) items maxPos topArray bottomArray newAcc

let SampleSum3 = 
    let start = SumBelow3 0 1 13 grid.[13] grid.[14] []
    //start.Length //28
    //start
    let stepUp = SumBelow3 0 4 12 grid.[12] start []
    //stepUp.Length
    stepUp
    let stepUpAgain = SumBelow3 0 8 11 grid.[11] stepUp [] 
    stepUpAgain

let SampleSum4 = 
    let start = SumBelow3 0 1 13 grid.[13] grid.[14] []
    //start.Length //28
    //start
    let stepUp = SumBelow4 0 3 12 grid.[12] start []
    //stepUp.Length
    //stepUp
    let stepUpAgain = SumBelow4 0 7 11 grid.[11] stepUp []
    //stepUpAgain.[(stepUpAgain.Length)-1]
    stepUpAgain


//Problem 19
//You are given the following information, but you may prefer to do some research for yourself.
//
//    * 1 Jan 1900 was a Monday.
//    * Thirty days has September, April, June and November.
//    * All the rest have thirty-one, saving February alone, Which has twenty-eight, rain or shine.
//    * And on leap years, twenty-nine.
//    * A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
//
//How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

let rec IterateDays (startDate:System.DateTime) (endDate:System.DateTime) counter = 
    if startDate > endDate then
        counter
    else
        if (startDate.DayOfWeek.ToString() = "Sunday" && startDate.Day = 1) then 
               IterateDays (startDate.AddDays(1.0)) endDate (counter+1)
        else
               IterateDays (startDate.AddDays(1.0)) endDate counter

let dateTest =   IterateDays (DateTime(1901,1,1)) (DateTime(2000,12,31)) 0


//Problem 26
//A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:
//
//    ^(1)/_(2)	= 	0.5
//    ^(1)/_(3)	= 	0.(3)
//    ^(1)/_(4)	= 	0.25
//    ^(1)/_(5)	= 	0.2
//    ^(1)/_(6)	= 	0.1(6)
//    ^(1)/_(7)	= 	0.(142857)
//    ^(1)/_(8)	= 	0.125
//    ^(1)/_(9)	= 	0.(1)
//    ^(1)/_(10)	= 	0.1
//
//Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that ^(1)/_(7) has a 6-digit recurring cycle.
//
//Find the value of d < 1000 for which ^(1)/_(d) contains the longest recurring cycle in its decimal fraction part.

let seqInverse = {1..999} |> Seq.map (fun x -> 1m / decimal x)
let listInverse = [1..999] |> List.map (fun x -> 1m / decimal x)

let rec divisors (num:decimal) (rem:decimal) arr (max:decimal) = 
    if rem > max  then
        arr
    else
        let newRem = num / rem
        let newArr = List.append arr [newRem]
        divisors num (rem+1m) newArr max

let test = divisors 1m 97m [] 999m


let rec longDivision (num:int) (rem:int) arr counter (max:int) = 
    if counter > max  then
        arr
    elif num < rem then
        let newDiv = 0
        let newArr = List.append arr [newDiv]
        longDivision  (num*10) rem newArr (counter+1) max
    else
        let newDiv = int(num / rem)
        let newRem = num % rem
        let newArr = List.append arr [newDiv]
        longDivision (newRem*10) rem newArr (counter+1) max


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

let testRemainders = 
    let results = [1..999] |> List.map (fun x -> remainders 1 x [])
    let max = List.max results
    let idx = results |> List.findIndex (fun x -> x = max)
    (idx+1, max)


//problem 29
//Consider all integer combinations of a^(b) for 2 ≤ a ≤ 5 and 2 ≤ b ≤ 5:
//
//    2^(2)=4, 2^(3)=8, 2^(4)=16, 2^(5)=32
//    3^(2)=9, 3^(3)=27, 3^(4)=81, 3^(5)=243
//    4^(2)=16, 4^(3)=64, 4^(4)=256, 4^(5)=1024
//    5^(2)=25, 5^(3)=125, 5^(4)=625, 5^(5)=3125
//
//If they are then placed in numerical order, with any repeats removed, we get the following sequence of 15 distinct terms:
//
//4, 8, 9, 16, 25, 27, 32, 64, 81, 125, 243, 256, 625, 1024, 3125
//
//How many distinct terms are in the sequence generated by a^(b) for 2 ≤ a ≤ 100 and 2 ≤ b ≤ 100?
let rec powerFx num power endPower arr = 
    if power > endPower then
        arr
    else
        let newResult = num**power
        let newArr = List.append arr [newResult]
        powerFx num (power+1.0) endPower newArr

let rec appendPowerFx num endNum power endPower arr =
    if num > endNum then
        arr
    else
        let newResult = powerFx num power endPower []
        let newArr = List.append arr newResult
        appendPowerFx (num+1.0) endNum power endPower newArr

let powerArray = 
    appendPowerFx 2.0 100.0 2.0 100.0 [] |> Set.ofList |> Set.count
    
//Problem 30
//Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:
//
//    1634 = 1^(4) + 6^(4) + 3^(4) + 4^(4)
//    8208 = 8^(4) + 2^(4) + 0^(4) + 8^(4)
//    9474 = 9^(4) + 4^(4) + 7^(4) + 4^(4)
//
//As 1 = 1^(4) is not a sum it is not included.
//
//The sum of these numbers is 1634 + 8208 + 9474 = 19316.
//
//Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.

let GetAllNarcissistic5 = 
    [2..999999] |> List.filter ( fun x -> EulerLib.IsNarcissisticBool x 5.0 = true) |> List.sum

let GetAllNarcissistic3 = 
    [2..99999] |> List.filter ( fun x -> EulerLib.IsNarcissisticBool x 3.0 = true)

let GetAllNarcissistic4 = 
    [2..99999] |> List.filter ( fun x -> EulerLib.IsNarcissisticBool x 4.0 = true)

let GetAllNarcissistic6 = 
    [2..999999] |> List.filter ( fun x -> EulerLib.IsNarcissisticBool x 6.0 = true)

let GetAllNarcissistic7 = 
    [2..9999999] |> List.filter ( fun x -> EulerLib.IsNarcissisticBool x 7.0 = true)

let GetAllNarcissistic1 = 
    [2..99] |> List.filter ( fun x -> EulerLib.IsNarcissisticBool x 1.0 = true)


//Problem 48
//The series, 1^(1) + 2^(2) + 3^(3) + ... + 10^(10) = 10405071317.
//Find the last ten digits of the series, 1^(1) + 2^(2) + 3^(3) + ... + 1000^(1000)

let sumOfPowerBySelf = 
    let sum = [1..1000] |> List.map (fun x -> EulerLib.PowerBySelf x) |> List.sum
    let stringOfSum = sum.ToString()
    stringOfSum.Substring(stringOfSum.Length-10)
    
//Problem 47
//The first two consecutive numbers to have two distinct prime factors are:
//
//14 = 2 × 7
//15 = 3 × 5
//
//The first three consecutive numbers to have three distinct prime factors are:
//
//644 = 2² × 7 × 23
//645 = 3 × 5 × 43
//646 = 2 × 17 × 19.
//
//Find the first four consecutive integers to have four distinct primes factors. What is the first of these numbers?
//let rec findConsecutive (arr:list<int>) max position found = 
//    if found=true || position >= max then
//        position
//    else
//        if arr.[position] = 3 && arr.[position+1] = 3 && arr.[position+2] = 3 then
//            findConsecutive arr max (position+1) true
//        else
//            findConsecutive arr max (position+1) false

open EulerLib
            
let fdpf_test_1 = EulerLib.FindDistinctPrimeFactors 101 0 (Seq.toArray(EulerLib.GetPrimes 101)) [||]

let start_47 = System.DateTime.UtcNow
let fdpf_test_2 = [|644|] |> Array.map (fun x -> EulerLib.FindDistinctPrimeFactors x 0 (Seq.toArray(EulerLib.GetPrimes x)) [||])
printfn "%f" ((System.DateTime.UtcNow).Subtract(start_47).TotalSeconds)

let getPalindromesBase10 =
     let base10Array = [|1..1000000|] |> Array.filter (fun x -> string x = EulerLib.Reverse(string x))
     let base02Array = base10Array |> Array.filter (fun x -> EulerLib.IsPalindromicBase2  x)
     Array.sum base02Array

let sumStr num = 
     let arrStr = num.ToString()
     let len = String.length arrStr
     let arrNum = Array.create len 0
     for i=0 to len-1 do
          let numTemp = System.Convert.ToInt32(arrStr.Chars(i).ToString()) 
          arrNum.[i] <- EulerLib.Factorial numTemp 1
     Array.sum arrNum

let sumTest = [|3..99999|] |> Array.map (fun x -> if sumStr x = x then x else 0) |> Array.sum

let LastTenDigitisOfSumOfPowerBySelf =
    let sum = [1..1000] |> List.map (fun x -> EulerLib.PowerBySelf x) |> List.sum
    let stringOfSum = sum.ToString()
    stringOfSum.Substring(stringOfSum.Length-10)

let GetAllNarcissistic_Start = System.DateTime.UtcNow

let GetAllNarcissistic =
    [2..999999]
    |> List.filter ( fun x -> EulerLib.IsNarcissisticBool x 5.0 = true)
    |> List.sum 

printfn "%f" ((System.DateTime.UtcNow).Subtract(GetAllNarcissistic_Start).TotalSeconds)


//Euler #30
open System
open System.Threading
open System.Threading.Tasks


let GetAllNarcissistic2_Start = System.DateTime.UtcNow

let GetAllNarcissistic2 =
    [|2..999999|] 
    |> Array.Parallel.map (fun x -> EulerLib.IsNarcissisticNumeric x 5.0) 
    |> Array.sum

printfn "%f" ((System.DateTime.UtcNow).Subtract(GetAllNarcissistic2_Start).TotalSeconds)

let tempstart = System.DateTime.UtcNow

let testRemaindersParallel =
    let results = [1..999] |> List.map (fun x -> remainders 1 x [])
    let max = List.max results
    let idx = results |> List.findIndex (fun x -> x = max)
    (idx+1, max) 

printfn "%f" ((System.DateTime.UtcNow).Subtract(tempstart).TotalSeconds)

let getNextDouble num = 
    let r = System.Random(DateTime.Now.Millisecond)
    r.NextDouble()

let RandomArrayParallel count =
    let r = System.Random(DateTime.Now.Millisecond)
    let arr = [|0..count|] |> Array.Parallel.map (fun x -> r.NextDouble())
    Array.sum arr
    
let RandomArrayParallelResult = RandomArrayParallel 1001
