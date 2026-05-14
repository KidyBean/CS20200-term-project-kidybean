namespace TermProj

open Microsoft.Xna.Framework

type BugPatch = 
    | NoBug
    | PlayerCollision
    | BlockCollision
    | AbyssCheck
    | WrongAbyssObject
    | ObjectLayerStack
    | WrongInventoryPut
    | InventoryLayerStack
    | PutDownOverlap


type Update = 
    | Walk
    | PushBlock
    | AbyssAndGround
    | Inventory
    | KeyAndDoor


type PatchMap = Set<BugPatch>
type UpdateMap = Set<Update>

type GameUpdate = {
    patchList: PatchMap
    updateList: UpdateMap
}

module GameUpdate = 
    let private _isPatched (bug: BugPatch) (patchMap: PatchMap) = patchMap |> Set.contains bug
    let private _newPatch (bug: BugPatch) (patchMap: PatchMap) = patchMap |> Set.add bug 
    let isPatched (bug: BugPatch) (update: GameUpdate) = _isPatched bug update.patchList
    let newPatch (bug: BugPatch) (update: GameUpdate) = _newPatch bug update.patchList


type GridPosition = { X: int; Y: int }

type StageGrid = {
    objectLayer: int
    width: int
    height: int
    objects: ObjectType[,,]
    ground: GroundType[,]
}

type CompactGrid = {
    width: int
    height: int
    startPos: GridPosition
    goalPos: GridPosition list
    objects: ObjectType[,]
    ground: GroundType[,]
}



module StageGrid = 
    let tupleToGridPos (pos: int*int) = 
        let x, y = pos
        { X = x; Y = y }
    let gridPosToTuple (pos: GridPosition) = 
        (pos.X, pos.Y)
    let gridPosToVector (pos: GridPosition) = 
        Vector2(float32 pos.X, float32 pos.Y)
    
    let newGrid (width: int) (height: int) : StageGrid = 
        let newObjectGrid = Array3D.create GameCore.objectLayer width height Empty
        let newGroundGrid = Array2D.create width height Abyss
        {
            objectLayer = GameCore.objectLayer
            width = width
            height = height
            objects = newObjectGrid
            ground = newGroundGrid
        }
    
    let objectOnPos (pos: GridPosition) (stage: StageGrid) = 
        let layer = stage.objectLayer
        let objectArr = Array.create layer Empty
        for l in 0..layer - 1 do
            let object = stage.objects[l, pos.X, pos.Y]
            objectArr[l] <- object
        objectArr
    let groundOnPos (pos: GridPosition) (stage: StageGrid) = stage.ground[pos.X, pos.Y]

    let putObject (object: ObjectType) (layer: int) (pos: GridPosition) (stage: StageGrid) = 
        stage.objects[layer, pos.X, pos.Y] <- object

    let makeStageGrid (compactMap: CompactGrid) = 
        let stageGrid = newGrid (compactMap.width + 2*GameCore.GridPadding) (compactMap.height + 2*GameCore.GridPadding)
        for x in 0..compactMap.width - 1 do
            for y in 0..compactMap.height - 1 do
                stageGrid.objects[0, x + GameCore.GridPadding, y + GameCore.GridPadding] <- compactMap.objects[x, y]
                stageGrid.ground[x + GameCore.GridPadding, y + GameCore.GridPadding] <- compactMap.ground[x, y]
        let startPos = { X = compactMap.startPos.X + GameCore.GridPadding; Y = compactMap.startPos.Y + GameCore.GridPadding}
        let goalPos = 
            compactMap.goalPos
            |> List.map (fun pos -> { X = pos.X + GameCore.GridPadding; Y = pos.Y + GameCore.GridPadding})
        stageGrid, startPos, goalPos

module StageParser = 
    let objectMap = Map [
        ".", Empty
        "P", Player
        "F", Flag
        "#", Wall
        "S", Spike
        "B", Box
        "K", Key 0
        "K1", Key 1
        "K2", Key 2
        "D", Door 0
        "D1", Door 1
        "D2", Door 2
    ]

    let stringToObject (tocken: string) (isNoGround: bool) =
        let abyss = if isNoGround then AbyssGround else Abyss
        let ground = if isNoGround then AbyssGround else Ground
        let gridCell = objectMap |> Map.tryFind tocken
        match gridCell with
        | Some object -> object, ground
        | None -> Empty, abyss
    
    let makeCompactStage (csvString: string) (isNoGround: bool) = 
        let tockenArr = 
            csvString.Split('\n')
            |> Array.map (fun row -> row.TrimEnd('\r'))
            |> Array.filter (fun row -> row.Length > 0)
            |> Array.map (fun row  -> 
                row.Split(',') 
                |> Array.map(fun tocken -> tocken.Trim()))
        
        let height = tockenArr.Length
        let width = 
            tockenArr |> Array.map (fun row -> row.Length)
            |> Array.max

        
        let abyss = if isNoGround then AbyssGround else Abyss
        let objectMap = Array2D.create width height Empty
        let groundMap = Array2D.create width height abyss
        let mutable startPos = { X = 0; Y = 0 }
        let mutable goalPos = []

        for y in 0..height - 1 do
            for x in 0..tockenArr[y].Length - 1 do
                let object, ground = stringToObject (tockenArr[y][x]) isNoGround
                objectMap[x, y] <- object
                groundMap[x, y] <- ground
                match object with
                | Player -> startPos <- { X = x; Y = y }
                | Flag -> goalPos <- { X = x; Y = y } :: goalPos
                | _ -> ()
        
        {
            width = width
            height = height
            startPos = startPos
            goalPos = goalPos
            objects = objectMap
            ground = groundMap
        }