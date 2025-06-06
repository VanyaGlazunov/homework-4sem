namespace Lazy

type ConcurrentLazy<'a>(supplier: unit -> 'a) = 
    let mutable result: Result<'a, exn> option = None
    let lockObj = obj()
    interface ILazy<'a> with 
        member l.Get (): 'a = 
            match result with
            | Some (Ok v) -> v
            | Some (Error  e) -> raise e
            | None -> 
                lock lockObj (fun () -> 
                    match result with
                    | Some (Ok v) -> v
                    | Some (Error e) -> raise e
                    | None ->
                        try
                            let v = supplier()
                            result <- Some (Ok v)
                            v
                        with
                        | e ->
                            result <- Some (Error e)
                            reraise ()
                )
