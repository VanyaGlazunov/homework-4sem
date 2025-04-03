namespace homework6

open System

type StringCalculatorBuilder() =
    member this.Bind(x: string, f) =
        match Int32.TryParse x with
        | true, x -> f x
        | false, _ -> None
    member this.Return x = Some x

type RoundngBuilder(acc: int) =
    member this.Bind(x: float, f) = 
        f (Math.Round(x, acc))
    member this.Return (x: float) = Math.Round(x, acc)