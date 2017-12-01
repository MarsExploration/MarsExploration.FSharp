module DomainTests

open Domain
open Xunit

[<Fact>]
let ``sample input should retrieve expected results``() =
    let input = { SuperiorRightLimit = {X = 5; Y = 5}
                  ProbesData = [|

                                { InitialPosition = { Coordinates = { X = 1; Y = 2}; Direction = North }
                                  Commands = [| TurnLeft; Move 
                                                TurnLeft; Move 
                                                TurnLeft; Move 
                                                TurnLeft; Move
                                                Move |]}

                                { InitialPosition = { Coordinates = { X = 3; Y = 3}; Direction = East }
                                  Commands = [| Move; Move; TurnRight
                                                Move; Move; TurnRight
                                                Move; TurnRight
                                                TurnRight; Move |]}

                               |] }
    let result = executeProbeCommands(input)

    let firstProbe = result.ProbesFinalPositions |> Seq.head
    Assert.True(firstProbe.Coordinates.X = 1 && 
                firstProbe.Coordinates.Y = 3 && 
                firstProbe.Direction = North)

    let secondProbe = result.ProbesFinalPositions |> Seq.item(1)
    Assert.True(secondProbe.Coordinates.X = 5 && 
                secondProbe.Coordinates.Y = 1 && 
                secondProbe.Direction = East)