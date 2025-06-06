namespace Lazy

open System.Threading

/// Lock free implementation of Lazy, gurantees that every call to Get returns the same result.
type LockFreeLazy<'a>(supplier: unit -> 'a) = 
    [<VolatileField>]
    let mutable state: Result<'a, exn> option = None
    
    interface ILazy<'a> with
        member l.Get(): 'a = 
            match state with
            | Some (Ok v) -> v
            | Some (Error e) -> raise e
            | None ->
                try 
                    let value = supplier()
                    let newState = Some (Ok value)
                    let current = state
                    let original = Interlocked.CompareExchange(&state, newState, current)
                    
                    match original with
                    | None -> value
                    | Some (Ok existing) -> existing
                    | Some (Error e) -> raise e
                with e -> 
                    let newState = Some (Error e)
                    let current = state
                    let original = Interlocked.CompareExchange(&state, newState, current)
                    
                    match original with
                    | None -> reraise()
                    | Some (Ok v) -> v
                    | Some (Error ex) -> raise ex