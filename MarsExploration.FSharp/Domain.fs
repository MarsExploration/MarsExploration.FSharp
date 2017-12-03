module Domain

type Coordinates = { X: int; Y: int }

type Direction = | North | Weast | South | East

type ProbeAction = | TurnRight | TurnLeft | Move

type Position = { Coordinates: Coordinates
                  Direction: Direction }

type ProbeData = { InitialPosition: Position
                   Actions: ProbeAction[] }

type Input = { SuperiorRightLimit: Coordinates
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
     
let private move previousPosition upperRightLimit =
    match previousPosition.Direction with
         | North -> 
                   let nextY = previousPosition.Coordinates.Y + 1
                   if nextY <= upperRightLimit.Y then
                      { previousPosition with Coordinates = { previousPosition.Coordinates with Y = nextY } }
                   else 
                      previousPosition

         | East -> 
                    let nextX = previousPosition.Coordinates.X + 1
                    if nextX <= upperRightLimit.X then
                        { previousPosition with Coordinates = { previousPosition.Coordinates with X = nextX } }
                    else
                        previousPosition

         | South -> let nextY = previousPosition.Coordinates.Y - 1
                    if nextY >= LOWER_LEFT_LIMIT then
                       { previousPosition with Coordinates = { previousPosition.Coordinates with Y = nextY } }
                    else 
                       previousPosition

         | Weast -> 
                    let nextX = previousPosition.Coordinates.X - 1
                    if nextX >= LOWER_LEFT_LIMIT then
                        { previousPosition with Coordinates = { previousPosition.Coordinates with X = nextX } }
                    else
                        previousPosition

let private executeProbeCommand upperRightLimit previousPosition action =
    match action with
        | TurnRight -> { previousPosition with Direction = turnRight(previousPosition.Direction) }
        | TurnLeft ->  { previousPosition with Direction = turnLeft(previousPosition.Direction) }
        | Move -> move previousPosition upperRightLimit
    
let executeProbeCommands(input) : Output =
    let probesFinalPositions = 
        input.ProbesData |> 
        Seq.map(fun probeData -> 
                probeData.Actions |> 
                Seq.fold (executeProbeCommand input.SuperiorRightLimit) probeData.InitialPosition)

    { ProbesFinalPositions = probesFinalPositions }
