module Tests

open NUnit.Framework
open FsUnit
open Lazy
open System
open System.Threading


[<SetUp>]
let Setup () =
    ()

let layzies =
    let supplier = fun () -> 100

    [|
        SimpleLazy(supplier) :> ILazy<int>, 100
        ConcurrentLazy(supplier) :> ILazy<int>, 100
        LockFreeLazy(supplier) :> ILazy<int>, 100
    |] |> Array.map(fun (l, e) -> TestCaseData(l, e))

let layziesThrowExn =
    let supplier = fun () -> invalidOp "" |> ignore
    [|
        SimpleLazy(supplier) :> ILazy<unit>
        ConcurrentLazy(supplier) :> ILazy<unit>
        LockFreeLazy(supplier) :> ILazy<unit>
    |] |> Array.map(fun l  -> TestCaseData l)

let layziesComputeOnce =
    let v1 = ref 99
    let v2 = ref 99
    let v3 = ref 99
    let supplier (v: int ref) = fun () -> Interlocked.Increment v

    [|
        SimpleLazy(supplier v1) :> ILazy<int>, 100
        ConcurrentLazy(supplier v2) :> ILazy<int>, 100
        LockFreeLazy(supplier v3) :> ILazy<int>, 100
    |] |> Array.map(fun (l, e) -> TestCaseData(l, e))

[<TestCaseSource("layzies")>]
let ``Get returns corrected value`` (lzy: ILazy<int>, expected: int) = 
    lzy.Get() |> should equal expected
    
[<TestCaseSource("layziesThrowExn")>]
let ``Get throw correct exn`` (lzy: ILazy<unit>) = 
    (fun () -> lzy.Get()) |> should throw typeof<InvalidOperationException> |> ignore

[<TestCaseSource("layziesComputeOnce")>]
let ``Get computed once`` (lzy: ILazy<int>, expected: int) = 
    lzy.Get() |> should equal expected
    lzy.Get() |> should equal expected
    lzy.Get() |> should equal expected
    lzy.Get() |> should equal expected

let concurrentLazies =
    [|
        fun supplier -> ConcurrentLazy supplier :> ILazy<int>
        fun supplier -> LockFreeLazy supplier :> ILazy<int>
    |]

[<TestCaseSource("concurrentLazies")>]
let ``Concurrent Lazy get should avoid races`` (factory: (unit -> int) -> ILazy<int>) = 
    let v = ref 0
    let supplier () = Interlocked.Increment v
    let lzy = factory supplier

    let threadCount = 6
    let bar = new Barrier(threadCount)
    let jobs = List.init 6 (fun i -> 
        async {
            bar.SignalAndWait()
            return lzy.Get()
        }
    )
    let results = jobs |> Async.Parallel |> Async.RunSynchronously

    Array.TrueForAll (results, (fun i -> i = 1)) |> should equal true
    