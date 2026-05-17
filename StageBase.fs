namespace TermProj

open Microsoft.Xna.Framework

type BugPatch = 
    | NoBug
    // Initial bug
    | PlayerCollisionExploit // Exploit
    | StagePositionOutCrash // Crashed
    // Block Push Update
    | WrongObjectPushExploit // Exploit
    | ObjectCollisionExploit // Exploit
    // Ground Update
    | AbyssCheckExploit // Exploit
    | WrongAbyssObjectExploit // Exploit
    // Inventory Update
    | WrongInventoryPutExploit // Exploit
    | InventoryLayerStackCrash // Crash
    | PutDownOverlapExploit // Exploit
    // KeyAndDoorUpdate
    | AnyKeyUsedExploit // Exploit

type Update = 
    | Walk
    | PushBlock
    | AbyssAndGround
    | Inventory
    | KeyAndDoor


type PatchMap = Set<BugPatch>

module GameUpdate = 
    let isPatched (bug: BugPatch) (patchMap: PatchMap) = patchMap |> Set.contains bug
    let newPatch (bug: BugPatch) (patchMap: PatchMap) = patchMap |> Set.add bug 


type GridPosition = { X: int; Y: int } with
    static member (+) (a: GridPosition, b: GridPosition) =
        { X = a.X + b.X; Y = a.Y + b.Y }
    static member (-) (a: GridPosition, b: GridPosition) =
        { X = a.X - b.X; Y = a.Y - b.Y }

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
    
    let directionToGrid = function
        | U -> { X = 0; Y = -1 }
        | D -> { X = 0; Y = 1 }
        | L -> { X = -1; Y = 0 }
        | R -> { X = 1; Y = 0 }
    
    let newGrid (width: int) (height: int) : StageGrid = 
        let newObjectGrid = Array3D.create (GameCore.objectLayer + 1) width height Empty
        let newGroundGrid = Array2D.create width height Abyss
        {
            objectLayer = GameCore.objectLayer
            width = width
            height = height
            objects = newObjectGrid
            ground = newGroundGrid
        }
    
    let isPosOutOfStage (pos: GridPosition) (stage: StageGrid) = 
        let outWidth = pos.X < 0 || pos.X >= stage.width
        let outHeight = pos.Y < 0 || pos.Y >= stage.height
        outWidth || outHeight
    
    let objectOnPos (pos: GridPosition) (stage: StageGrid) = 
        let layer = stage.objectLayer + 1
        let objectArr = Array.create layer Empty
        for l in 0..layer do
            let object = stage.objects[l, pos.X, pos.Y]
            objectArr[l] <- object
        objectArr
    
    let groundOnPos (pos: GridPosition) (stage: StageGrid) = stage.ground[pos.X, pos.Y]

    let putObject (object: ObjectType) (layer: int) (pos: GridPosition) (stage: StageGrid) = 
        stage.objects[layer, pos.X, pos.Y] <- object
    
    let pushObjects (rawObject: ObjectType[]) (pos: GridPosition) (stage: StageGrid) (isBaseRemain: bool) = 
        let objects = rawObject |> Array.filter (fun x -> x <> Empty)
        let numObjects = Array.length objects
        if numObjects > 0 then
            if isBaseRemain then
                let rawOffset = numObjects - GameCore.objectLayer - 1
                let offset = if rawOffset > 0 then rawOffset else 0
                putObject objects[0] 0 pos stage
                for idx in 1..GameCore.objectLayer do
                    if idx < numObjects - offset then
                        putObject objects[idx + offset] idx pos stage
                    else
                        putObject Empty idx pos stage
            else
                let rawOffset = numObjects - GameCore.objectLayer
                let offset = if rawOffset > 0 then rawOffset else 0
                for idx in 0..GameCore.objectLayer - 1 do
                    if idx < numObjects - offset then
                        putObject objects[idx + offset] idx pos stage
                    else
                        putObject Empty idx pos stage
        
    let putObjectToGround (object: ObjectType) (pos: GridPosition) (stage: StageGrid) = 
        match object with
        | Empty -> stage.ground[pos.X, pos.Y] <- Abyss
        | _ -> stage.ground[pos.X, pos.Y] <- ObjectGround object

    let makeStageGrid (compactMap: CompactGrid) = 
        let stageGrid = newGrid (compactMap.width + 2*GameCore.GridPadding) (compactMap.height + 2*GameCore.GridPadding)
        for x in 0..compactMap.width - 1 do
            for y in 0..compactMap.height - 1 do
                stageGrid.objects[0, x + GameCore.GridPadding, y + GameCore.GridPadding] <- compactMap.objects[x, y]
                stageGrid.ground[x + GameCore.GridPadding, y + GameCore.GridPadding] <- compactMap.ground[x, y]
        let startPos = { X = compactMap.startPos.X + GameCore.GridPadding; Y = compactMap.startPos.Y + GameCore.GridPadding}
        stageGrid, startPos



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

    let stringToObject (token: string) (isNoGround: bool) =
        let abyss = if isNoGround then AbyssGround else Abyss
        let ground = if isNoGround then AbyssGround else Ground
        let gridCell = objectMap |> Map.tryFind token
        match gridCell with
        | Some object -> object, ground
        | None -> Empty, abyss
    
    let makeCompactStage (csvString: string) (isNoGround: bool) = 
        let tockenArr = 
            csvString.Split('\n') 
            |> Array.map (fun row -> row.TrimEnd('\r'))
            |> Array.filter (fun row -> row.Length > 0)
            |> Array.map (fun row  -> 
                row.Split(',') |> Array.map(fun tocken -> tocken.Trim()))
        
        let height = tockenArr.Length
        let width = 
            tockenArr |> Array.map (fun row -> row.Length)
            |> Array.max

        
        let abyss = if isNoGround then AbyssGround else Abyss
        let objectMap = Array2D.create width height Empty
        let groundMap = Array2D.create width height abyss
        let mutable startPos = { X = 0; Y = 0 }

        for y in 0..height - 1 do
            for x in 0..tockenArr[y].Length - 1 do
                let object, ground = stringToObject (tockenArr[y][x]) isNoGround
                objectMap[x, y] <- object
                groundMap[x, y] <- ground
                match object with
                | Player -> startPos <- { X = x; Y = y }
                | _ -> ()
        
        {
            width = width
            height = height
            startPos = startPos
            objects = objectMap
            ground = groundMap
        }