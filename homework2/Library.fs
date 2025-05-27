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
        | Const of float
        | Plus of Expression * Expression
        | Minus of Expression * Expression
        | Mul of Expression * Expression
        | Div of Expression * Expression
        | UMinus of Expression
    
    /// Evaluates given arithmetic expression. Returns None if division by zero ouccurs.
    let eval expression = 
        let eps = 1e-10
        let rec eval expr cont = 
            match expr with
            | Const c -> cont c
            | UMinus e -> eval e (fun subVal -> cont (-1. * subVal))
            | Plus(l, r) ->
                eval l (fun evalL -> eval r (fun evalR -> cont (evalL + evalR)))
            | Minus(l, r) ->
                eval l (fun evalL -> eval r (fun evalR -> cont (evalL - evalR)))
            | Mul(l, r) ->
                eval l (fun evalL -> eval r (fun evalR -> cont (evalL * evalR)))
            | Div(l, r) ->
                eval l (fun evalL -> eval r (fun evalR ->
                        if abs evalR < eps then
                            None
                        else
                            cont (evalL / evalR)
                    )
                )
        eval expression (fun result -> Some result)
    
    /// Generates an infinite sequence of consecutive prime numbers
    let primes = 
        let isPrime n = 
            let rec check i =
                i * i > n || n % i <> 0 && check (i + 1)
            check 2
        
        let numbers = Seq.initInfinite ((+) 2)
        Seq.filter isPrime numbers