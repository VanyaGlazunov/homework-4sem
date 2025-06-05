namespace homework5

open System
open NUnit.Framework
open FsUnit
open Moq

module ComputerTests =
    [<Test>]
    let ``TryInfect should do nothing when computer is already infected`` () =
        let mockOS = Mock<IOS>()
        let computer = Computer(mockOS.Object, true, 1)
        let mockRandom = Mock<Random>()
        
        computer.TryInfect(mockRandom.Object)
        
        computer.IsInfected |> should be True

    [<Test>]
    let ``TryInfect should infect computer when random value is below chance`` () =
        let mockOS = Mock<IOS>()
        mockOS.Setup(fun os -> os.InfectionChance).Returns(0.5) |> ignore
        let mockRandom = Mock<Random>()
        mockRandom.Setup(fun r -> r.NextDouble()).Returns(0.4) |> ignore
        let computer = Computer(mockOS.Object, false, 1)
        
        computer.TryInfect(mockRandom.Object)
        
        computer.IsInfected |> should be True

    [<Test>]
    let ``TryInfect should not infect computer when random value is above chance`` () =
        let mockOS = Mock<IOS>()
        mockOS.Setup(fun os -> os.InfectionChance).Returns(0.5) |> ignore
        let mockRandom = Mock<Random>()
        mockRandom.Setup(fun r -> r.NextDouble()).Returns(0.6) |> ignore
        let computer = Computer(mockOS.Object, false, 1)
        
        computer.TryInfect(mockRandom.Object)
        
        computer.IsInfected |> should be False

module NetworkTests = 
    [<Test>]
    let ``GetNumberOfInfected should return correct count of infected computers`` () =
        let network = Network()
        let mockOS = Mock<IOS>()
        let computer1 = Computer(mockOS.Object, true, 1)
        let computer2 = Computer(mockOS.Object, false, 2)
        
        network.AddComputer(computer1)
        network.AddComputer(computer2)
        
        network.GetNumberOfInfected() |> should equal 1

    [<Test>]
    let ``GetHighestChance should return maximum infection chance in network`` () =
        let network = Network()
        let os1 = OS.OS "OS1" 0.3
        let os2 = OS.OS "OS2" 0.7
        
        network.AddComputer(Computer(os1, false, 1))
        network.AddComputer(Computer(os2, false, 2))
        
        network.GetHighestChance() |> should equal 0.7

    [<Test>]
    let ``NextState should infect neighbors with 100% infection chance`` () =
        let os = OS.OS "TestOS" 1.0
        let infected = Computer(os, true, 1)
        let healthy = Computer(os, false, 2)
        healthy.AddLink(infected)
        
        let network = Network()
        network.AddComputer(infected)
        network.AddComputer(healthy)
        
        network.NextState()
        
        healthy.IsInfected |> should be True

    [<Test>]
    let ``NextState should not infect neighbors with 0% infection chance`` () =
        let os = OS.OS "TestOS" 0.0
        let infected = Computer(os, true, 1)
        let healthy = Computer(os, false, 2)
        healthy.AddLink(infected)
        
        let network = Network()
        network.AddComputer(infected)
        network.AddComputer(healthy)
        
        network.NextState()
        
        healthy.IsInfected |> should be False

    [<Test>]
    let ``Start should terminate when all computers are infected`` () =
        let os = OS.OS "OS" 0.5
        let computer = Computer(os, true, 1)
        let network = Network()
        network.AddComputer(computer)
        
        network.Start()
        
        network.GetNumberOfInfected() |> should equal 1

    [<Test>]
    let ``Start should terminate when no computers are infected`` () =
        let os = OS.OS "OS" 0.5
        let computer = Computer(os, false, 1)
        let network = Network()
        network.AddComputer(computer)
        
        network.Start()
        
        network.GetNumberOfInfected() |> should equal 0

    [<Test>]
    let ``Start should terminate when infection chance drops below epsilon`` () =
        let os = OS.OS "OS" 1e-11
        let computer = Computer(os, true, 1)
        let network = Network()
        network.AddComputer(computer)
        
        network.Start()
        
        network.GetNumberOfInfected() |> should equal 1

    [<Test>]
    let ``Network should propagate infection through multiple hops`` () =
        let os = OS.OS "ContagiousOS" 1.0
        let computer1 = Computer(os, true, 1)
        let computer2 = Computer(os, false, 2)
        let computer3 = Computer(os, false, 3)
        
        computer2.AddLink(computer1)
        computer3.AddLink(computer2)
        
        let network = Network()
        network.AddComputer(computer1)
        network.AddComputer(computer2)
        network.AddComputer(computer3)
        
        network.NextState()
        computer2.IsInfected |> should be True
        
        network.NextState()
        computer3.IsInfected |> should be True
