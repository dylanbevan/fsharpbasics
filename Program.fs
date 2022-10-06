open FSharp.Data
open XPlot.Plotly


// F# is VERY strongly typed despite (typically) not having types anywhere in the code
let printNumber x =
    printfn "Number is: %i" x

// classes (in the OOO sense) generally don't exist which makes naming at least 50% easier

// All functions only accept one argument!!! - all other implementations are either curried or partial application
let add x y =
    x + y

let addCurried x =
    fun y -> x + y

let addPartial = add 10

printfn "add 10 5 = %i" (add 10 5)
printfn "addCurried 5 10 = %i" (addCurried 10 5)
printfn "addPartial 5 = %i" (addPartial 5)

// All data structures are immutable by default
type Address = { 
    HouseNumber: int;
    Street: string;
    Street2: string;
    City: string;
    Province: string;
    PostalCode: string;
    Country: string;
}

// notice type inference in action
let sampleAddress = { HouseNumber = 10; Street = "My Street"; Street2 = "My other Street"; City = "Cty"; Province = "WA"; PostalCode = "abc123"; Country = "Somewhere Warm" }
// = means equals (not assign) unless in a let statement
sampleAddress.HouseNumber = 15 // this is an equality check
//sampleAddress.HouseNumber <- 15 // this fails because of immutability
let house15 = { sampleAddress with HouseNumber = 15 }

// the language can add erased types to units of measure
[<Measure>] type metres
[<Measure>] type seconds

let distance = 100.0<metres>
let time = 9.58<seconds>

// using `` we can name variables whatever we like - very useful for test names
let ``Usain's Speed`` = distance/time

// reading code can be easier with left to right, top to bottom rather than C#'s inside out, top to bottom
printfn ("Usain's speed is %A") ``Usain's Speed``

``Usain's Speed`` 
|> printfn ("Usain's speed is %A")

// avoid nasty typos in code (e.g. passing in the wrong value, or the wrong order)
let calculateHousePrice costSquareMeter (squareMetres: double<metres>) =
    costSquareMeter * squareMetres

[<Measure>] type feet
let feetPerMetre = 3.2808399<feet/metres>
let convertFeetToMetres (length:double<feet>) =
    length / feetPerMetre

let getCostPerSqMetre = calculateHousePrice 100.0
let getCostPerSqFoot x = convertFeetToMetres x |> calculateHousePrice 100.0

getCostPerSqMetre 56.0<metres> |> printfn "Cost for 56 sqM = %A"
getCostPerSqFoot 56.0<feet> |> printfn "Cost for 56 sqFoot = %A"

    

// null is NOT a thing
// let nullAddress: Address = null

// Use the option type instead
let nullAddress: Option<Address> = None

// pattern matching is incredibly powerful
let printAddress (address: Option<Address>) = 
    // printfn address.Street |> printfn "%s" // won't work because Address could be none
    match address with
    | None -> printfn "No current address"
    | Some a -> printfn "%s" a.Street

printAddress nullAddress

sampleAddress 
|> Some 
|> printAddress

// Option type is an example of a discriminated union
type FancyAnimals =
| Giraffe
| Elephant
| HandsomeDog of name:string

type NotSoFancyAnimals =
| Rat
| BirdEatingSpider

type Animal =
| Fancy of FancyAnimals
| NotFancy of NotSoFancyAnimals

let describeAnimal animal =
    match animal with
    | NotFancy _ -> "meh"
    | Fancy f -> match f with
                 | HandsomeDog n -> sprintf "oooh %s is a fancy dog" n
                 | _ -> "oooh fancy"
    |> printfn "%s"

describeAnimal (NotFancy Rat)
describeAnimal (Fancy <| HandsomeDog "Allie")




// discriminated unions can make exceptions rarer
type DatabaseResult<'a> =
| InvalidId
| Error of exn // shorthand for exception
| Success of 'a // generics in F# are usually 'a, 'b, 'c rather than T, T1, T2

let getData id =
    match System.String.IsNullOrWhiteSpace(id) with
    | true -> InvalidId
    | false -> Success 100

// functions are first class citizens

// declare a type just like C# Func<int,string>
type DatabaseOperation = int -> DatabaseResult<string>

let businessLayerGet (dbOperation: DatabaseOperation) x = 
    // validate x
    dbOperation x

let invalidIdOperation = fun x -> InvalidId // note no types here, the compiler can work it out based on the usage on line 114
let throwExOperation x = failwith "boo it didn't work" // failwith is short for new Exception(msg)
let legitOperation = fun id -> "Woot"

// pass a function in as an argument
let testBusinessLayer = businessLayerGet invalidIdOperation // use partial application

testBusinessLayer 100 |> printfn "%A"

// functions can be chained or pipelined together
let testAndPrint = testBusinessLayer >> printfn "%A"

testAndPrint 100

// concurrency is easier as there is a built in actor
type InboxMessage = 
| Increment
| Decrement
| Get of AsyncReplyChannel<int>

let agent = MailboxProcessor<InboxMessage>.Start(fun inbox ->
        let rec loop n =
            async { 
                let! msg = inbox.Receive()
                let m = match msg with
                        | Increment -> n + 1
                        | Decrement -> n - 1
                        | Get replyChannel -> replyChannel.Reply(n); n
                // printfn "Value is now %i" m
                return! loop(m) }
        loop 0);;

[1..199]
|> List.map(fun x -> match (x % 2) with
                     | 0 -> async { (*printfn "on thread %i" System.Threading.Thread.CurrentThread.ManagedThreadId;*) agent.Post Increment }
                     | _ -> async { (*printfn "on thread %i" System.Threading.Thread.CurrentThread.ManagedThreadId;*) agent.Post Decrement} )
|> Async.Parallel
|> Async.RunSynchronously
|> fun xs -> agent.PostAndReply(fun channel -> Get(channel)) |> printfn "Counter value is %i"

// we can define our own symbols
let (<=>) a b = System.String.Equals(a, b, System.StringComparison.OrdinalIgnoreCase)

"moose" <=> "MoOsE" |> printfn "Are the strings equal? %b"

// F# is tail call optimized, try this in C#  int Reduce(int x) => x == 0 ? 0 : Reduce(x-1); !!
let timer = System.Diagnostics.Stopwatch.StartNew()
let rec reduce x =
    match x with 
    | 0 -> 0
    | _ -> reduce (x-1)

reduce System.Int32.MaxValue |> fun _ -> printfn "Completed in %A" timer.Elapsed


// sometimes things get tedious
let validateString x = 
    match System.String.IsNullOrWhiteSpace(x) with
    | true -> None
    | false -> x |> Some

// define an alias
let vs = validateString

let validateAddress (addr: Address) =
    match vs addr.Street with
    | None -> None
    | Some _ -> match vs addr.Street2 with
                | None -> None
                | Some _ -> match vs addr.City with
                            | None -> None
                            | Some _ -> Some addr
                            // etc

// workflow expressions can help.
// the `bind` function name is important and is of the type Bind(expr, (fun pattern -> {| cexpr |})), where expression is a parameter, and cexpr is a continuation expression, or rephrased what to do next
let executeOnSome f opt =
    match opt with 
    | Some x -> f x
    | None -> None

type MaybeBuilder() =
    member this.Bind(m, f) = executeOnSome f m // given a continuation function f, only execute it if m is Some
    member this.Return(x) = Some x

let maybe = new MaybeBuilder()

let isValidAddress (addr: Address) = 
    maybe 
        {
            let! _ = vs addr.Street
            let! _ = vs addr.Street2
            let! _ = vs addr.City
            let! _ = vs addr.Country
            let! _ = vs addr.PostalCode
            let! _ = vs addr.Province
            return addr
        }

let isValid = isValidAddress sampleAddress |> printfn "address is valid? %A"
let isNotValid = isValidAddress {sampleAddress with Province = ""} |> printfn "address is valid? %A"

// workflow expressions work on ANY type you want, and you declare the bind operations. This is effectively how async/await works 😉


type WindowsData = HtmlProvider<"https://en.wikipedia.org/wiki/Microsoft_Windows">
let windowsData = WindowsData.Load("https://en.wikipedia.org/wiki/Microsoft_Windows")

let chart = windowsData.Tables.Table4.Rows
            |> Array.map(fun r -> (r.``Desktop OS``, r.StatCounter))
            |> Chart.Pie
            |> Chart.WithTitle "Windows Usage"
            |> Chart.WithLegend true
            |> Chart.Show