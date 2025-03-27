namespace homework5

open System

type Computer(os: IOS, infected: bool, id: int) =
    let mutable infected = infected
    let Links = ResizeArray<Computer> ()

    member val Id = id with get
    member c.AddLink comp = Links.Add(comp)
    member c.GetLinks = Links
    member  c.IsInfected 
        with get () = infected
        and private set x = infected <- x
    member c.InfectionChance = os.InfectionChance
    member c.TryInfect (rnd: Random) =
        if not c.IsInfected then
            c.IsInfected <- rnd.NextDouble() < os.InfectionChance