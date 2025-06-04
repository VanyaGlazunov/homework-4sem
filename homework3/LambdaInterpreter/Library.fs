namespace LambdaInterpreter

module LambdaInterpreter =
    /// Lambda term.
    type Term = 
    | Var of string
    | App of Term * Term
    | Abs of string * Term

    /// Creates new name for varibale.
    let newName oldName (freeVars: Set<string>) = 
        let rec findName i =
            let candidate = oldName + i.ToString()
            if freeVars.Contains candidate then
                findName (i + 1)
            else
                candidate
        findName 0

    /// Returns set with free variables in given term.
    let rec FV term = 
        match term with
        | Var v -> Set.singleton v
        | App (s, t) -> FV s + FV t
        | Abs (v, t) -> FV t - Set.singleton v

    /// Substitutes all occurences of x with term T in given term. Returns new term.
    let rec substitute term x T = 
        match term with
        | Var v -> 
            if v = x then
                T
            else
                term
        | App (s, t) -> App (substitute s x T, substitute t x T )
        | Abs (v, S) ->
            if x = v then
                term
            elif not (FV(T).Contains v) || not (FV(S).Contains x) then
                Abs (v, substitute S x T)
            else
                let V = newName v (FV(S) + FV(T))
                Abs (V, substitute (substitute S v (Var V)) x T)
    
    /// Does one step of beta reduction in normal order. Returns reduced term if redex was found, None otherwise.
    let rec reduceOnce term = 
        match term with
        | Var v -> None
        | App (s, t) ->
            match s with
            | Abs (v, S) ->
                Some (substitute S v t)
            | _ ->
                match reduceOnce s with
                | Some S -> Some (App (S, t))
                | None -> 
                    match reduceOnce t with
                    | Some T -> Some (App (s, T))
                    | None -> None
        | Abs (y, S) ->
            match reduceOnce S with
            | None -> None
            | Some T -> Some (Abs (y, T))

    /// Reduces term to normal form.
    let rec eval term = 
        match reduceOnce term with
        | None -> term
        | Some t -> eval t

    /// Pretty prints term.
    let rec printTerm term = 
        match term with
        | Var v -> v
        | App (s, t) ->
            let S = match s with
                    | Abs _ -> $"({printTerm s})"
                    | _ -> printTerm s
            let T = match t with
                    | Abs _ -> $"({printTerm t})"
                    | _ -> printTerm t
            match t with 
            | App _ -> $"{S} ({T})"
            | _ -> $"{S} {T}"
        | Abs (v, t) -> $"λ{v}.{printTerm t}"
    