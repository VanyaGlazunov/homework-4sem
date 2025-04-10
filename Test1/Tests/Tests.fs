namespace Test1

open NUnit.Framework
open FsUnit

open Test

module Tests = 
    [<Test>]
    let ``signChangeSeq generates alternating sequence [1, -1, 1, -1, ...]`` () =
        let seq = signChangeSeq
        let firstFive = seq |> Seq.take 5 |> Seq.toList
        firstFive |> should equal [1; -1; 1; -1; 1]

    [<Test>]
    let ``signChangeNaturalSeq generates sequence [1, -2, 3, -4, ...]`` () =
        let seq = signChangeNaturalSeq
        let firstFive = seq |> Seq.take 5 |> Seq.toList
        firstFive |> should equal [1; -2; 3; -4; 5]

    [<Test>]
    let ``filter returns elements of the tree satisfying the predicate`` () =
            let tree =
                Node(5,
                    Node(3,
                        Node(1, Empty, Empty),
                        Node(4, Empty, Empty)),
                    Node(8,
                        Node(7, Empty, Empty),
                        Node(10, Empty, Empty)))

            let result = filter (fun x -> x > 4) tree
            List.sort result |> should equal [5; 7; 8; 10]

    [<Test>]
    let ``PriorityQueue push and pop work correctly`` () =
        let pq = PriorityQueue<int>()
        pq.push 5
        pq.push 3
        pq.push 8
        pq.top() |> should equal 3
        pq.pop()
        pq.top |> should equal 5
        pq.pop()
        pq.top() |> should equal 8

    [<Test>]
    let ``PriorityQueue raises exception when popping from empty queue`` () =
        let pq = PriorityQueue<int>()
        Assert.Throws<System.InvalidOperationException>(fun () -> pq.pop() |> ignore)

    [<Test>]
    let ``PriorityQueue raises exception when getting top of empty queue`` () =
        let pq = PriorityQueue<int>()
        Assert.Throws<System.InvalidOperationException>(fun () -> pq.top() |> ignore)
