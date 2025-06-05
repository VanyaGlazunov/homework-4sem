namespace homework5

/// Interface that represents OS of a computer in a local network
type IOS =
    abstract member Name : string
    abstract member InfectionChance : float

module OS =
    let OS (name: string) (chance: float) = 
        {
            new IOS with
                member s.Name = name
                member s.InfectionChance = chance
        }