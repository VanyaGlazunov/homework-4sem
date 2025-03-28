namespace homework5

open System

/// Class that represents comupter in local network
type Computer(os: IOS, infected: bool, id: int) =
    let mutable infected = infected
    let Links = ResizeArray<Computer> ()

    /// Gets computer id
    member val Id = id with get
    /// Adds link to a neighbour computer
    member c.AddLink comp = Links.Add(comp)
    /// Gets all links to neighbours
    member c.GetLinks = Links
    /// Gets a value indicating whether the comupter is infected
    member  c.IsInfected 
        with get () = infected
        and private set x = infected <- x
    /// Gets an infection chance of OS installed on the computer
    member c.InfectionChance = os.InfectionChance
    /// Tries to infect the computer.
    member c.TryInfect (rnd: Random) =
        if not c.IsInfected then
            c.IsInfected <- rnd.NextDouble() < os.InfectionChance