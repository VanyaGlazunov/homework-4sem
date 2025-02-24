namespace src

module homework1 =
    /// Computes factorial of a non-negative integer.
    let factorial n acc =
        let rec factorial n acc =
            match n with
            | 0 | 1 -> Ok acc
            | _ when n > 1 -> factorial (n - 1) (acc * n)
            | _ -> Error "Cannot compute factorial of a negative number"
        factorial n 1
    

    /// Computes n-th fibonacci number.
    let fibonacci n =
        let rec fibonacci n a b = 
            match n with
            | 1 -> Ok b
            | _ when n > 1 -> fibonacci (n - 1) (a + b) a  
            | _ -> Error "N must be positive integer!"
        fibonacci n 1 1
    
    /// Reverses a list in linear time.
    let listReverse list = 
        let rec listReverse list acc = 
            match list with
            | head :: tail -> listReverse tail (head :: acc)
            | [] -> acc
        listReverse list []
    
    /// Returns result with index of the firs occurrance of a given element in a given list if it exists, error otherwise.
    let findFirst list element= 
        let rec findFirst list element acc= 
            match list with
            | head :: _ when head = element -> Ok acc
            | _ :: tail -> findFirst tail element (acc + 1)
            | [] -> Error "Not found"
        findFirst list element 0
    
    /// Returns a list that contains m + 1 powers of 2 starting with n-th power; [2^n, 2^(n + 1), ..., 2^(n + m)]
    let powerList n m = 
        let rec powerList n m acc = 
            match m with
            | 0 -> acc
            | _ -> powerList n (m - 1) (acc.Head / 2 :: acc) 
        powerList n m [1 <<< n + m]