module Domain

type Coordinates = { X: int; Y: int }

type Direction = | North | Weast | South | East

type ProbeCommand = | TurnRight | TurnLeft | Move

type Position = { Coordinates: Coordinates
                  Direction: Direction }

type ProbeData = { InitialPosition: Position
                   Commands: ProbeCommand[] }

type Input = { SuperiorRightLimit: int
               ProbesData: ProbeData[] }

type Output = { ProbesFinalPositions: Position seq }

[<Literal>]
let LOWER_LEFT_LIMIT = 0

let private turnRight(previousDirection) =
    match previousDirection with
         | North -> East
         | East -> South
         | South -> Weast
         | Weast -> North

let private turnLeft(previousDirection) =
    match previousDirection with
         | North -> Weast
         | East -> North
         | South -> East
         | Weast -> South
     
let private move(previousPosition, upperRightLimit) =
    match previousPosition.Direction with
         | North -> 
                   let nextPosition = previousPosition.Coordinates.Y + 1
                   if nextPosition <= upperRightLimit then
                      { previousPosition with Coordinates = { previousPosition.Coordinates with Y = nextPosition } }
                   else 
                      previousPosition

         | East -> 
                    let nextPosition = previousPosition.Coordinates.X + 1
                    if nextPosition <= upperRightLimit then
                        { previousPosition with Coordinates = { previousPosition.Coordinates with X = nextPosition } }
                    else
                        previousPosition

         | South -> let nextPosition = previousPosition.Coordinates.Y - 1
                    if nextPosition >= LOWER_LEFT_LIMIT then
                       { previousPosition with Coordinates = { previousPosition.Coordinates with Y = nextPosition } }
                    else 
                       previousPosition

         | Weast -> 
                    let nextPosition = previousPosition.Coordinates.X - 1
                    if nextPosition >= LOWER_LEFT_LIMIT then
                        { previousPosition with Coordinates = { previousPosition.Coordinates with X = nextPosition } }
                    else
                        previousPosition

let private executeProbeCommand upperRightLimit previousPosition command =
    match command with
        | TurnRight -> { previousPosition with Direction = turnRight(previousPosition.Direction) }
        | TurnLeft ->  { previousPosition with Direction = turnLeft(previousPosition.Direction) }
        | Move -> move(previousPosition, upperRightLimit)
    
let executeProbeCommands(input) : Output =
    let probesFinalPositions = 
        input.ProbesData |> 
        Seq.map(fun probeData -> 
                probeData.Commands |> 
                Seq.fold (executeProbeCommand input.SuperiorRightLimit) probeData.InitialPosition)

    { ProbesFinalPositions = probesFinalPositions }
