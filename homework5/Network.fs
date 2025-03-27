namespace homework5

open System

type Network () =
    let computers = new ResizeArray<Computer> ()
    let eps = 1e-10

    member n.AddComputer comp =
        computers.Add(comp)

    member n.GetNumberOfInfected () =
        computers |> Seq.toList |>
        List.fold (fun acm c -> acm + if c.IsInfected then 1 else 0) 0
    
    member n.GetHighestChance () = 
        computers |> Seq.toList |>
        List.fold (fun acm c -> max acm c.InfectionChance) 0.

    member n.NextState () = 
        for comp in computers do
            if not comp.IsInfected then
                for neighbour in comp.GetLinks do
                    if neighbour.IsInfected then
                        comp.TryInfect Random.Shared
    
    member n.PrintState () =
        computers |> Seq.toList |>
        List.iter (fun c -> printfn "Computer %d is %s" c.Id (if c.IsInfected then "infected" else "healthy"))

    member n.Start () =
        printfn "Start state:"
        n.PrintState ()

        while n.GetNumberOfInfected () <> computers.Count && n.GetNumberOfInfected () <> 0 && n.GetHighestChance () > eps do
            n.NextState ()
            n.PrintState ()