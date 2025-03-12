module tests

open NUnit.Framework
open FsUnit
open FsCheck
open homework2.Homework2

[<Test>]
let ``Functions counting even numbers should be equal`` () =
    Check.QuickThrowOnFailure (fun l -> 
       countWithFilter l = countWithFold l &&
       countWithFold l = countWithMap l)

[<Test>]
let ``countWithMap should count even numbers correctly`` () =
    let input = [1; 2; 3; 4; 5]
    countWithMap input |> should equal 2

[<Test>]
let ``countWithMap should return 0 for empty list`` () =
    let input = []
    countWithMap input |> should equal 0

[<Test>]
let ``map should apply function to all nodes`` () =
    let tree =Node(1,Node(2,Empty,Empty),Node(3,Empty,Empty))
    let mappedTree = map ((+) 1) tree
    mappedTree |> should equal (Node(2,Node(3,Empty,Empty),Node(4,Empty,Empty)))

[<Test>]
let ``map should return Empty for an empty tree`` () =
    let emptyTree = Empty
    let mappedTree = map (fun x -> x + 1) emptyTree
    mappedTree |> should equal BinTree<int>.Empty

[<Test>]
let ``eval should evaluate CONST correctly`` () =
    let expr = CONST 5.0
    eval expr |> should equal 5.0

[<Test>]
let ``eval should evaluate PLUS correctly`` () =
    let expr = PLUS(CONST 2.0, CONST 3.0)
    eval expr |> should equal 5.0

[<Test>]
let ``eval should evaluate MINUS correctly`` () =
    let expr = MINUS(CONST 5.0, CONST 3.0)
    eval expr |> should equal 2.0

[<Test>]
let ``eval should evaluate MUL correctly`` () =
    let expr = MUL(CONST 2.0, CONST 3.0)
    eval expr |> should equal 6.0

[<Test>]
let ``eval should evaluate DIV correctly`` () =
    let expr = DIV(CONST 6.0, CONST 3.0)
    eval expr |> should equal 2.0

[<Test>]
let ``eval should evaluate UMINUS correctly`` () =
    let expr = UMINUS(CONST 5.0)
    eval expr |> should equal -5.0

[<Test>]
let ``eval should handle nested expressions correctly`` () =
    let expr = PLUS(MUL(CONST 2.0, CONST 3.0), MINUS(CONST 5.0, CONST 1.0))
    eval expr |> should equal 10.0

[<Test>]
let ``eval should handle division by zero`` () = 
    let eps = 1e-10
    let divByZero n = 
        let expr = DIV(CONST n, CONST (eps / 2.))
        eval expr
    (fun () -> divByZero 0.0 |> ignore) |> should throw typeof<System.DivideByZeroException>
    (fun () -> divByZero 1.0 |> ignore) |> should throw typeof<System.DivideByZeroException>
    (fun () -> divByZero -1.0 |> ignore) |> should throw typeof<System.DivideByZeroException>

[<Test>]
let ``primes should start with the first ten prime numbers`` () =
    let firstTenPrimes = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29]
    primes
    |> Seq.take 10
    |> Seq.toList
    |> should equal firstTenPrimes
