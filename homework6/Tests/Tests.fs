namespace homework6

open NUnit.Framework
open FsUnit
open System

module StringCalculatorWorkflowTests =
    let calculator = StringCalculatorBuilder()

    [<Test>]
    let ``Workflow with valid steps returns correct result`` () =
        calculator {
            let! x = "10"
            let! y = "20"
            return x + y
        }
        |> should equal (Some 30)

    [<Test>]
    let ``Workflow with invalid step returns None`` () =
        calculator {
            let! x = "10"
            let! y = "xyz"
            return x + y
        }
        |> should equal None

    [<Test>]
    let ``Workflow with division by zero in computation throws exception`` () =
        let res () = calculator {
            let! x = "10"
            let! y = "20"
            return x / (y - 20)
        }

        try
            res () |> ignore
        with
        | :? DivideByZeroException -> Assert.Pass ()
        | ex -> Assert.Fail()

    [<Test>]
    let ``Workflow with multiple valid steps returns correct result`` () =
        calculator {
            let! a = "5"
            let! b = "10"
            let! c = "15"
            return a + b + c
        }
        |> should equal (Some 30)

    [<Test>]
    let ``Workflow with negative numbers returns correct result`` () =
        calculator {
            let! x = "-3"
            let! y = "7"
            return x * y
        }
        |> should equal (Some -21)

module RoundngWorkflowTests =
    let rounding = RoundngBuilder

    [<Test>]
    let ``Workflow with valid steps returns rounded sum`` () =
        rounding 3 { 
            let! a = 2.0 / 12.0
            let! b = 3.5
            return a / b
        }
        |> should equal 0.048

    [<Test>]
    let ``Workflow with zero digit rounding returns integer`` () =
        rounding 0 {
            let! x = 3.1415
            let! y = 2.71828
            return x + y
        }
        |> should equal 6

    [<Test>]
    let ``Workflow with division by zero in computation throws exception`` () =
        let res () = rounding 1 {
            let! x = 10.
            let! y = 0.001
            return x / y
        }

        try
            res () |> ignore
        with
        | :? DivideByZeroException -> Assert.Pass ()
        | ex -> Assert.Fail()

    [<Test>]
    let ``Workflow returns None when step is invalid`` () =
        rounding 2 {
            let! x = 3.14
            let! y = Double.NaN
            return x + y
        }
        |> should equal Double.NaN

    [<Test>]
    let ``Workflow applies rounding accuracy to each step`` () =
        rounding 2 {
            let! a = 2.345
            let! b = 3.678
            printfn "%f % f %f" a b (a + b)
            return a + b
        }
        |> should equal 6.03