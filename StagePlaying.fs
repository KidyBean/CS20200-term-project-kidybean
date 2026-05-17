namespace TermProj

open Microsoft.Xna.Framework

// only for one direction(move after -> not for undo)
type GameMove = 
    | ObjectMove of ObjectType[]*GridPosition*GridPosition // object from to
    | Minor
    | StageVictory
    | PlayerDead
    | StageCrashed of BugPatch



type InStage = {
    patch: PatchMap
    playerPos: GridPosition
    playerDirection: Direction
    stageMap: StageGrid
    inventoryFlag: bool
    inventory: ObjectType[]
    selectedIdx: int
    usedBug: BugPatch Set
    movement: (GameMove*float32) list
    moveTime: float32 option
    moveTimeSpent: float32
    fullTimeSpent: float32
}

type InStageResult<'A> = 
    | Passed of BugPatch Set*'A
    | Blocked
    | CrashRaised of BugPatch

type InStagePlayerState = 
    | Victory
    | Alive
    | Dead

module StageObject = 
    // whitelist
    let (|CanGoThrough|_|) = function
        | Spike -> Some CanGoThrough
        | Flag -> Some CanGoThrough
        | _ -> None
    let (|CanPush|_|) = function
        | Box -> Some CanPush
        | Key _ -> Some CanPush
        | _ -> None
    let (|CanFillAbyss|_|) = function
        | Box -> Some CanFillAbyss
        | _ -> None
    let (|CanPutInventory|_|) = function
        | Box -> Some CanPutInventory
        | Key _ -> Some CanPutInventory
        | _ -> None
    let (|CanMakePlayerDead|_|) = function
        | Spike -> Some CanMakePlayerDead
        | _ -> None
    let (|CanMakePlayerGoal|_|) = function
        | Flag -> Some CanMakePlayerGoal
        | _ -> None

    let moveToPos (prev: GridPosition) (next: GridPosition) (stage: InStage) = 
        let stageMap = stage.stageMap
        let patchList = stage.patch
        let targetObjects = stageMap |> StageGrid.objectOnPos prev
        let targetIdx = if Option.isSome ((|CanGoThrough|_|) targetObjects[0]) then 1 else 0
        let targetObject = targetObjects[targetIdx]

        let canMoveTarget =
            match targetObject with
            | Empty -> true
            | Player -> true
            | CanPush -> true
            | _ -> not (Set.contains WrongObjectPushExploit patchList)
        
        let result =
            if not canMoveTarget then Blocked
            elif targetObject = Empty then Passed (Set [NoBug], Empty)
            elif StageGrid.isPosOutOfStage next stage.stageMap then 
                if Set.contains StagePositionOutCrash patchList then CrashRaised StagePositionOutCrash
                else Blocked
            else Passed (Set [NoBug], targetObject)
        match result with
        | CrashRaised err -> CrashRaised err
        | Blocked -> Blocked
        | Passed (_, Empty) -> Passed (Set [NoBug], [||])
        | _ ->
            let posObjects = stageMap |> StageGrid.objectOnPos next
            let posIdx, posRemain = if Option.isSome ((|CanGoThrough|_|) posObjects[0]) then 1, true else 0, false
            let movableObject = targetObjects[targetIdx..] |> Array.filter (fun x -> x <> Empty)
            let pushBackObject = if targetIdx = 1 then targetObjects[0] else Empty
            match posObjects[posIdx], targetObject with
            | Empty, Player | Empty, CanPush -> 
                StageGrid.pushObjects [|pushBackObject|] prev stage.stageMap false
                Passed (Set [NoBug], movableObject)
        
            | _, Player when not (Set.contains PlayerCollisionExploit patchList) -> 
                StageGrid.pushObjects [|pushBackObject|] prev stage.stageMap false
                Passed (Set [PlayerCollisionExploit], movableObject)
        
            | _, CanPush when not (Set.contains ObjectCollisionExploit patchList) ->
                StageGrid.pushObjects [|pushBackObject|] prev stage.stageMap false
                Passed (Set [ObjectCollisionExploit], movableObject)
        
            | _, _ when not (Set.contains WrongObjectPushExploit patchList) -> 
                StageGrid.pushObjects [|pushBackObject|] prev stage.stageMap false
                Passed (Set [WrongObjectPushExploit], movableObject)
        
            | _,  _ -> Blocked
    
    let moveClearOnPos (move: GameMove) (stage: InStage) = 
        match move with
        | ObjectMove (objects, _, endPos) ->
            let posObjects = stage.stageMap |> StageGrid.objectOnPos endPos
            let posRemain = Option.isSome ((|CanGoThrough|_|) posObjects[0])
            let rawObjects = Array.append posObjects objects
            StageGrid.pushObjects rawObjects endPos stage.stageMap posRemain
        | _ -> ()

    
    let pullDownPos (pos: GridPosition) (stage: InStage) = 
        let stageMap = stage.stageMap
        let patchList = stage.patch
        let targetGround = stageMap |> StageGrid.groundOnPos pos
        match targetGround with
        | Abyss ->
            let targetObjects = stageMap |> StageGrid.objectOnPos pos
            if Set.contains WrongAbyssObjectExploit patchList then
                let baseIdx = targetObjects |> Array.tryFindIndex (fun object -> Option.isSome ((|CanFillAbyss|_|) object))
                match baseIdx with
                | Some v  ->
                    let remain = targetObjects[v + 1..]
                    let ground = targetObjects[v]
                    StageGrid.pushObjects remain pos stage.stageMap false
                    StageGrid.putObjectToGround ground pos stage.stageMap
                    Passed (Set [NoBug], ())
                | None ->
                    StageGrid.pushObjects [|Empty|] pos stage.stageMap false
                    Passed (Set [NoBug], ())
            else
                let remain = targetObjects[1..]
                let ground = targetObjects[0]
                if ground = Empty then Passed (Set[NoBug], ())
                else
                    StageGrid.pushObjects remain pos stage.stageMap false
                    StageGrid.putObjectToGround ground pos stage.stageMap
                    Passed (Set [WrongAbyssObjectExploit], ())
        | _ -> Blocked

    let getObjectOnPos (pos: GridPosition) (stage: InStage) = 
        let stageMap = stage.stageMap
        let patchMap = stage.patch
        let targetObjects = stageMap |> StageGrid.objectOnPos pos
        let overIdx = targetObjects |> Array.tryFindIndex (fun object -> object = Empty)
        match overIdx with
        | Some 0 ->
            let ground = stageMap |> StageGrid.groundOnPos pos
            match ground with
            | ObjectGround object -> 
                match object with
                | CanPutInventory ->
                    StageGrid.putObjectToGround Empty pos stageMap
                    Passed (Set [NoBug], object)
                | _ ->
                    if Set.contains WrongInventoryPutExploit patchMap then Blocked
                    else 
                        StageGrid.putObjectToGround Empty pos stageMap
                        Passed (Set [WrongInventoryPutExploit], object)
            | _ -> Blocked
        | Some v ->
            let object = targetObjects[v - 1]
            match object with
            | CanPutInventory -> 
                if v - 1 > 0 then
                    StageGrid.pushObjects targetObjects[..v - 2] pos stageMap false
                    Passed (Set [NoBug], object)
                else
                    StageGrid.pushObjects [|Empty|] pos stageMap false
                    Passed (Set [NoBug], object)
            | _ ->
                if Set.contains WrongInventoryPutExploit patchMap then Blocked
                else 
                    if v - 1 > 0 then
                        StageGrid.pushObjects targetObjects[..v - 2] pos stageMap false
                        Passed (Set [WrongInventoryPutExploit], object)
                    else
                        StageGrid.pushObjects [|Empty|] pos stageMap false
                        Passed (Set [WrongInventoryPutExploit], object)
        | None ->
            let object = targetObjects[GameCore.objectLayer]
            match object with
            | CanPutInventory -> 
                StageGrid.pushObjects targetObjects[..GameCore.objectLayer - 1] pos stageMap true
                Passed (Set [NoBug], object)
            | _ ->
                if Set.contains WrongInventoryPutExploit patchMap then Blocked
                else 
                    StageGrid.pushObjects targetObjects[..GameCore.objectLayer - 1] pos stageMap true
                    Passed (Set [WrongInventoryPutExploit], object)
    
    let putObjectToPos (object: ObjectType) (pos: GridPosition) (stage: InStage) =
        if object = Empty then Blocked
        else
            let stageMap = stage.stageMap
            let patchMap = stage.patch
            let targetObjects = stageMap |> StageGrid.objectOnPos pos
            let overIdx = targetObjects |> Array.tryFindIndex (fun object -> object = Empty)
            match overIdx with
            | Some 0 ->
                let ground = stageMap |> StageGrid.groundOnPos pos
                match ground with
                | Abyss -> 
                    match object with
                    | CanFillAbyss -> 
                        StageGrid.putObjectToGround object pos stageMap
                        Passed (Set [NoBug], ())
                    | _ -> 
                        if Set.contains WrongAbyssObjectExploit patchMap then Blocked
                        else
                            StageGrid.putObjectToGround object pos stageMap
                            Passed (Set [WrongAbyssObjectExploit], ())
                | ObjectGround _ | Ground | AbyssGround -> 
                    StageGrid.pushObjects [|object|] pos stageMap false
                    Passed (Set [NoBug], ())
            | Some 1 when Option.isSome ((|CanGoThrough|_|)targetObjects[0]) ->
                StageGrid.pushObjects [|object|] pos stageMap true
                Passed (Set [NoBug], ())
            | _ ->
                let isBaseRemain = Option.isSome ((|CanGoThrough|_|)targetObjects[0])
                StageGrid.pushObjects [|object|] pos stageMap isBaseRemain
                Passed (Set [PutDownOverlapExploit], ())


module StagePlayer = 
    let playerResult (stage: InStage) = 
        let playerPos = stage.playerPos
        let patchMap = stage.patch
        let baseObject = (StageGrid.objectOnPos playerPos stage.stageMap)[0]
        let ground = StageGrid.groundOnPos playerPos stage.stageMap
        let abyssResult, exploit = 
            match ground, Set.contains AbyssCheckExploit patchMap with
            | Abyss, true -> Dead, NoBug
            | Abyss, false -> Alive, AbyssCheckExploit
            | _, _ -> Alive, NoBug
        if abyssResult = Dead then
            Passed (Set [exploit], Dead)
        else
            match baseObject with
            | StageObject.CanMakePlayerDead -> Passed (Set [exploit], Dead)
            | StageObject.CanMakePlayerGoal -> Passed (Set [exploit], Victory)
            | _ -> Passed (Set [exploit], Alive)
        
    let playerMove (direction: Direction) (stage: InStage) = 
        let playerPos = stage.playerPos
        let deltaPos = StageGrid.directionToGrid direction
        let objectPos = playerPos + deltaPos
        if StageGrid.isPosOutOfStage objectPos stage.stageMap then 
            if Set.contains StagePositionOutCrash stage.patch then Blocked
            else CrashRaised StagePositionOutCrash
        else
            let objectAfterPos = objectPos + deltaPos
            let result = StageObject.moveToPos objectPos objectAfterPos stage
            match result with
            | Passed (err, objectList) ->
                let result = StageObject.moveToPos playerPos objectPos stage
                match result with
                | Passed (err2, objectList2) ->
                    if Array.isEmpty objectList then
                        Passed (err2, [ObjectMove (objectList2, playerPos, objectPos)])
                    else
                        Passed (Set.union err2 err, [ObjectMove (objectList2, playerPos, objectPos); ObjectMove (objectList, objectPos, objectAfterPos)])
                | Blocked -> Blocked
                | CrashRaised err -> CrashRaised err
            | Blocked -> Blocked
            | CrashRaised err -> CrashRaised err


module StageInventory = 
    let inventoryGet (stage: InStage) =
        let inventory = stage.inventory
        let inventoryIdx = stage.selectedIdx
        let objectPos = stage.playerPos + (stage.playerDirection |> StageGrid.directionToGrid)
        let cellidx = 
            match inventory[inventoryIdx] with
            | Empty -> Some inventoryIdx
            | _ -> inventory |> Array.tryFindIndex (fun x -> x = Empty)
        match cellidx with
        | Some idx ->
            match StageObject.getObjectOnPos objectPos stage with
            | Passed (err, object) ->
                inventory[idx] <- object
                Passed(err, ())
            | Blocked -> Blocked
            | CrashRaised err -> CrashRaised err
        | None -> 
            if Set.contains InventoryLayerStackCrash stage.patch then Blocked
            else 
                match StageObject.getObjectOnPos objectPos stage with
                | Passed _ -> CrashRaised InventoryLayerStackCrash
                | Blocked -> Blocked
                | CrashRaised err -> CrashRaised err
        
    let inventoryPut (stage: InStage) = 
        let inventory = stage.inventory
        let inventoryIdx = stage.selectedIdx
        let objectPos = stage.playerPos + (stage.playerDirection |> StageGrid.directionToGrid)
        let cellObject = inventory[inventoryIdx]
        match cellObject with
        | Empty -> Blocked
        | object ->
            match StageObject.putObjectToPos object objectPos stage with
            | Passed (err, _) -> 
                inventory[inventoryIdx] <- Empty
                Passed (err, [])
            | Blocked -> Blocked
            | CrashRaised err -> CrashRaised err
    
    let inventorySelect (idx: int) = if idx >= GameCore.InventoryStack then Blocked else Passed (Set [NoBug], idx) 
    

module StageInteraction =     
    let (|CanPlayerInteract|_|) = function
        | Door v -> Some CanPlayerInteract
        | _ -> None

    let interactToDoor (door: ObjectType) (stage: InStage) = 
        let playerInventory = stage.inventory
        let patchMap = stage.patch
        match door with
        | Door v -> 
            if Set.contains AnyKeyUsedExploit patchMap then
                let result = 
                    playerInventory |> Array.indexed
                    |> Array.tryFind (fun (_, x) -> x = Key v)
                match result with
                | Some (idx, key) -> Passed (Set [NoBug], idx)
                | None -> Blocked
            else
                let result = 
                    playerInventory |> Array.indexed
                    |> Array.tryFind (fun (_, x) -> 
                        match x with
                        | Key _ -> true
                        | _ -> false
                    )
                match result with
                | Some (idx, Key x) when x = v -> Passed (Set [NoBug], idx)
                | Some (idx, _) -> Passed (Set [AnyKeyUsedExploit], idx)
                | None -> Blocked
        | _ -> Blocked
    
    let doorDelete (keyIdx: int) (objectData: (GridPosition*int)) (stage: InStage) = 
        let objectPos = fst objectData
        let objectIdx = snd objectData        
        let objects = stage.stageMap |> StageGrid.objectOnPos objectPos
        objects[objectIdx] <- Empty
        stage.inventory[keyIdx] <- Empty
        StageGrid.pushObjects objects objectPos stage.stageMap false
    


    let interactMap (object: ObjectType) (objectData: (GridPosition*int)) (stage: InStage) = 
        match object with
        | Door v -> 
            match interactToDoor (Door v) stage with
            | Passed (err, idx) ->
                doorDelete idx objectData stage |> ignore
                Passed (err, [])
            | Blocked -> Blocked
            | CrashRaised err -> CrashRaised err
        | _ -> Blocked

    let playerInteract (direction: Direction) (stage: InStage) = 
        let playerPos = stage.playerPos
        let deltaPos = StageGrid.directionToGrid direction
        let objectPos = playerPos + deltaPos
        if StageGrid.isPosOutOfStage objectPos stage.stageMap then Blocked
        else
            let interlectObject = 
                stage.stageMap |> StageGrid.objectOnPos objectPos
                |> Array.indexed
                |> Array.tryFind (fun (_, x) -> x = Empty || Option.isSome ((|CanPlayerInteract|_|) x))
            match interlectObject with
            | Some (_, Empty) -> Blocked
            | None -> Blocked
            | Some (idx, v) -> interactMap v (objectPos, idx) stage


module InStage = 
    let newStage (map: CompactGrid) (patchMap: PatchMap) (inventoryFlag: bool) = 
        let stageGrid, playerPos = StageGrid.makeStageGrid map
        
        {
            patch = patchMap
            playerPos = playerPos
            playerDirection = Direction.R
            stageMap = stageGrid
            inventoryFlag = inventoryFlag
            inventory = if inventoryFlag then Array.create GameCore.InventoryStack Empty else [||]
            selectedIdx = 0
            usedBug = Set.empty
            movement = []
            moveTime = None
            moveTimeSpent = 0.0f
            fullTimeSpent = 0.0f
        }
    
    let updateByMovement (movement: (GameMove*int) list) (stage: InStage) = 
        movement
        |> List.iter (fun (move, _) -> StageObject.moveClearOnPos move stage)
    
    let update (action: KeyBind) (stage: InStage) (deltaTime: float32) = 
        match stage.moveTime with
        | Some _ -> 
            let nextTime = stage.moveTimeSpent - deltaTime
            if nextTime > 0.0f then
                { stage with moveTimeSpent = nextTime; fullTimeSpent = stage.fullTimeSpent + deltaTime }
            else
                { stage with movement = [(Minor, 0.0f)]; moveTime = None; moveTimeSpent = 0.0f; fullTimeSpent = stage.fullTimeSpent + deltaTime }
        | None ->
            let playerResult = StagePlayer.playerResult stage
            match playerResult with
            | Passed (err, Victory) -> { stage with usedBug = Set.union err stage.usedBug; movement = [(StageVictory, 0.0f)]; moveTime = Some infinityf; moveTimeSpent = infinityf; fullTimeSpent = stage.fullTimeSpent + deltaTime }
            | Passed (err, Dead) -> { stage with usedBug = Set.union err stage.usedBug; movement = [(PlayerDead, 0.0f)]; moveTime = Some infinityf; moveTimeSpent = infinityf; fullTimeSpent = stage.fullTimeSpent + deltaTime }
            | Passed (err, Alive) -> 
                match action with
                | Move direction -> stage
                | GetObject -> stage
                | PutDown -> stage
                | _ -> { stage with fullTimeSpent = stage.fullTimeSpent + deltaTime }
            | _ -> failwith "Game Real Crashed with Unexpected State in PlayerResult. This Cannot Happen Because of PlayerResult Definition."