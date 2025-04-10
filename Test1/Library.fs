namespace Test1

module Test =
    /// infinite sequence looking like this [1, -1, 1, -1,...]
    let signChangeSeq = 
        Seq.initInfinite (fun ind -> 
            if ind % 2 = 0 then 1
            else -1
        )
        
    /// infinite sequence looking like this [1, -2, 3, -4,...]
    let signChangeNaturalSeq = 
        Seq.mapi (fun ind e -> e * (ind+1)) signChangeSeq

    /// Class representing binary tree with values in verticies.
    type BinTree<'a> = 
    | Node of 'a * BinTree<'a> * BinTree<'a>
    | Empty

    /// Returns list of elements of the given tree for which the given predicate returns "true"
    let filter pred tree = 
        let rec filter pred tree lst =
            match tree with
            | Node(x, l, r) -> 
                filter pred r (filter pred l (if pred x then x :: lst else lst))
            | Empty -> lst
        
        filter pred tree []

    /// Class that represents priority queue with smallest element on top
    type PriorityQueue<'T when 'T : comparison>() = 
        let mutable cont = []
        /// Pops the smallest element from the queue
        member this.pop() = 
            match cont with
            | head :: tail -> cont <- tail
            | [] -> raise (System.InvalidOperationException("Cannot pop from empty queue"))

        /// Pushes given element to the queue
        member this.push (e: 'T) =     
            cont <- List.sort (e :: cont)

        /// Gets the smallest element in the queue if it is not empty, raises InvalidOperationException otherwise.
        member this.top () = 
            match cont with
            | head :: tail -> head
            | [] -> raise (System.InvalidOperationException("Queue is empty")) 
            