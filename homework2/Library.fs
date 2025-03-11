namespace homework2

module Homework2 =
    /// Counts even numbers with List.map
    let countWithMap list = 
        List.map (fun x -> 1 - abs x % 2) list |> List.sum

    /// Counts even numbers with List.filter
    let countWithFilter list = 
        List.filter (fun x -> x % 2 = 0) list |> List.length

    /// Counts even numbers with List.Fold
    let countWithFold list = 
        List.fold (fun acc elem -> acc + 1 - abs elem % 2) 0 list

    /// Binary tree type
    type BinTree<'a> = 
        | Node of 'a * BinTree<'a> * BinTree<'a>
        | Empty

    /// Maps every node in given binary tree with given map function. Returns new binary tree containing mapped values
    let map mapping binTree = 
        let rec map m binTree cont = 
            match binTree with
            | Node(x, l, r) ->
                map m l (fun mapL -> map m r (fun mapR -> cont (Node(m x, mapL, mapR))))
            | Empty -> cont Empty
        map mapping binTree id

    /// Arithmetic expression type
    type Expression = 
        | CONST of float
        | PLUS of Expression * Expression
        | MINUS of Expression * Expression
        | MUL of Expression * Expression
        | DIV of Expression * Expression
        | UMINUS of Expression
    
    /// Evaluates given arithmetic expression. Throws Exception if division by zero ouccurs.
    let eval expression = 
        let eps = 1e-10
        let rec eval expr cont = 
            match expr with
            | CONST c -> cont c
            | UMINUS e -> -1. * eval e cont
            | PLUS(l, r) ->
                eval l (fun evalL -> eval r (fun evalR -> cont (evalL + evalR)))
            | MINUS(l, r) ->
                eval l (fun evalL -> eval r (fun evalR -> cont (evalL - evalR)))
            | MUL(l, r) ->
                eval l (fun evalL -> eval r (fun evalR -> cont (evalL * evalR)))
            | DIV(l, r) ->
                eval l (fun evalL -> eval r (fun evalR ->
                        match evalR with
                        | f when f < eps -> failwith "Division by zero"
                        | _ -> cont (evalL / evalR)
                    )
                )
        eval expression id
    
    let primes = 
        let isPrime n = 
            let rec check i =
                i > n / 2 || n % i <> 0 && check (i + 1)
            check 2
        
        let numbers = Seq.initInfinite ((+) 2)
        Seq.filter isPrime numbers