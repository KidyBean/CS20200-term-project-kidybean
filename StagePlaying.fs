namespace TermProj

open Microsoft.Xna.Framework

// only for one direction(move after -> not for undo)
type GameMove = 
    | ObjectMove of ObjectType[]*GridPosition*GridPosition // object from to
    | Minor
    | NoAction
    | StageVictory
    | PlayerDead
    | StageCrashed of BugPatch


type InStage = {
    patch: PatchMap
    pushFlag: bool
    inventoryFlag: bool
    playerPos: GridPosition
    prevPlayerPos: GridPosition
    playerRealPos: Vector2
    playerDirection: Direction
    cameraTarget: Vector2
    cameraPos: Vector2
    stageMap: StageGrid
    inventory: ObjectType[]
    selectedIdx: int
    usedBug: BugPatch Set
    movement: (GameMove list*float32)
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

    let moveToPos (prev: GridPosition) (next: GridPosition) (stage: InStage) (isPlayer: bool) = 
        let stageMap = stage.stageMap
        let patchList = stage.patch
        let targetObjects = stageMap |> StageGrid.objectOnPos prev
        let targetIdx = 
            if isPlayer then Array.findIndex (fun x -> x = Player) targetObjects
            elif Option.isSome ((|CanGoThrough|_|) targetObjects[0]) then 1 else 0
        let targetObject = targetObjects[targetIdx]

        let canMoveTarget =
            match targetObject with
            | Empty -> true
            | Player -> true
            | CanPush -> true
            | _ -> not (Set.contains WrongObjectPushExploit patchList)
        
        let result =
            if not canMoveTarget then Blocked
            elif targetObject = Empty then Passed (Set.empty, Empty)
            elif StageGrid.isPosOutOfStage next stage.stageMap then 
                if Set.contains StagePositionOutCrash patchList then Blocked
                else CrashRaised StagePositionOutCrash
            else Passed (Set.empty, targetObject)
        match result with
        | CrashRaised err -> CrashRaised err
        | Blocked -> Blocked
        | Passed (_, Empty) -> Passed (Set.empty, [||])
        | _ ->
            let posObjects = stageMap |> StageGrid.objectOnPos next
            let posIdx, posRemain = if Option.isSome ((|CanGoThrough|_|) posObjects[0]) then 1, true else 0, false
            let movableObject = targetObjects[targetIdx..] |> Array.filter (fun x -> x <> Empty)
            let pushBackObject = if targetIdx >= 1 then targetObjects[..targetIdx - 1] else [|Empty|]
            match posObjects[posIdx], targetObject with
            | Empty, Player | Empty, CanPush -> 
                StageGrid.pushObjects pushBackObject prev stage.stageMap false
                Passed (Set.empty, movableObject)
            
            | Empty, _ when not (Set.contains WrongObjectPushExploit patchList) -> 
                StageGrid.pushObjects pushBackObject prev stage.stageMap false
                Passed (Set [WrongObjectPushExploit], movableObject)
        
            | _, Player when not (Set.contains PlayerCollisionExploit patchList) -> 
                StageGrid.pushObjects pushBackObject prev stage.stageMap false
                Passed (Set [PlayerCollisionExploit], movableObject)
        
            | _, _ when not (Set.contains ObjectCollisionExploit patchList) ->
                StageGrid.pushObjects pushBackObject prev stage.stageMap false
                Passed (Set [ObjectCollisionExploit], movableObject)
        
            | _,  _ -> Blocked

    
    let pullDownPos (pos: GridPosition) (stage: InStage) = 
        let stageMap = stage.stageMap
        let patchList = stage.patch
        let targetGround = stageMap |> StageGrid.groundOnPos pos
        match targetGround with
        | Abyss ->
            let targetObjects = stageMap |> StageGrid.objectOnPos pos
            if Set.contains WrongAbyssObjectExploit patchList then
                let playerIdx = targetObjects |> Array.tryFindIndex (fun object -> object = Player)
                let hasPlayer = Option.isSome playerIdx
                let baseIdx = targetObjects |> Array.tryFindIndex (fun object -> Option.isSome ((|CanFillAbyss|_|) object))
                match baseIdx with
                | Some v  ->
                    let remain = if hasPlayer && playerIdx.Value < v then Array.append [|Player|] targetObjects[v + 1..] else targetObjects[v + 1..]
                    let ground = targetObjects[v]
                    StageGrid.pushObjects remain pos stage.stageMap false
                    StageGrid.putObjectToGround ground pos stage.stageMap
                    Passed (Set.empty, ())
                | None ->
                    let remain = if hasPlayer then [|Player|] else [|Empty|]
                    StageGrid.pushObjects remain pos stage.stageMap false
                    Passed (Set.empty, ())
            else
                let idx = if targetObjects[0] <> Player then 0 else 1
                let remain = if idx = 0 then targetObjects[idx + 1..] else Array.append [|Player|] targetObjects[idx + 1..]
                let ground = targetObjects[idx]
                if ground = Empty then Passed (Set.empty, ())
                else
                    StageGrid.pushObjects remain pos stage.stageMap false
                    StageGrid.putObjectToGround ground pos stage.stageMap
                    if Option.isSome ((|CanFillAbyss|_|) ground) then Passed (Set.empty, ())
                    else Passed (Set [WrongAbyssObjectExploit], ())
        | _ -> Passed (Set.empty, ())
    
    let moveClearOnPos (move: GameMove) (stage: InStage) = 
        match move with
        | ObjectMove (objects, _, endPos) ->
            let posObjects = stage.stageMap |> StageGrid.objectOnPos endPos
            let posRemain = Option.isSome ((|CanGoThrough|_|) posObjects[0])
            let rawObjects = Array.append posObjects objects
            StageGrid.pushObjects rawObjects endPos stage.stageMap posRemain
            pullDownPos endPos stage
        | _ -> Passed (Set.empty, ())

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
                    Passed (Set.empty, object)
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
                    Passed (Set.empty, object)
                else
                    StageGrid.pushObjects [|Empty|] pos stageMap false
                    Passed (Set.empty, object)
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
                Passed (Set.empty, object)
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
                        Passed (Set.empty, ())
                    | _ -> 
                        if Set.contains WrongAbyssObjectExploit patchMap then Blocked
                        else
                            StageGrid.putObjectToGround object pos stageMap
                            Passed (Set [WrongAbyssObjectExploit], ())
                | ObjectGround _ | Ground | AbyssGround -> 
                    StageGrid.pushObjects [|object|] pos stageMap false
                    Passed (Set.empty, ())
            | Some 1 when Option.isSome ((|CanGoThrough|_|)targetObjects[0]) ->
                StageGrid.pushObjects [|object|] pos stageMap true
                Passed (Set.empty, ())
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
            | Abyss, true -> Dead, Set.empty
            | Abyss, false -> Alive, Set [AbyssCheckExploit]
            | _, _ -> Alive, Set.empty
        if abyssResult = Dead then
            Passed (exploit, Dead)
        else
            match baseObject with
            | StageObject.CanMakePlayerDead -> Passed (exploit, Dead)
            | StageObject.CanMakePlayerGoal -> Passed (exploit, Victory)
            | _ -> Passed (exploit, Alive)
    
    let deletePlayer (stage: InStage) = 
        let playerPos = stage.playerPos
        let objectOnPos = StageGrid.objectOnPos playerPos stage.stageMap
        let playerIdx = objectOnPos |> Array.tryFindIndex (fun x -> x = Player)
        match playerIdx with
        | Some idx ->
            objectOnPos[idx] <- Empty
            StageGrid.pushObjects objectOnPos playerPos stage.stageMap false
        | None -> ()

        
    let playerMove (direction: Direction) (stage: InStage) = 
        let playerPos = stage.playerPos
        let deltaPos = StageGrid.directionToGrid direction
        let objectPos = playerPos + deltaPos
        if StageGrid.isPosOutOfStage objectPos stage.stageMap then 
            if Set.contains StagePositionOutCrash stage.patch then Blocked
            else CrashRaised StagePositionOutCrash
        else
            let objectAfterPos = objectPos + deltaPos
            let result = 
                if stage.pushFlag then StageObject.moveToPos objectPos objectAfterPos stage false
                else Passed (Set.empty, [||])
            match result with
            | Passed (err, objectList) ->
                let result = StageObject.moveToPos playerPos objectPos stage true
                match result with
                | Passed (err2, objectList2) ->
                    if Array.isEmpty objectList then
                        Passed (err2, [ObjectMove (objectList2, playerPos, objectPos)])
                    else
                        Passed (Set.union err2 err, [ObjectMove (objectList2, playerPos, objectPos); ObjectMove (objectList, objectPos, objectAfterPos)])
                | Blocked ->
                    if Array.isEmpty objectList then
                        Blocked
                    else
                        Passed (err, [ObjectMove ([||], playerPos, playerPos); ObjectMove (objectList, objectPos, objectAfterPos)])
                | CrashRaised err -> CrashRaised err
            | Blocked ->
                if Set.contains PlayerCollisionExploit stage.patch then Blocked
                else
                    let result = StageObject.moveToPos playerPos objectPos stage true
                    match result with
                    | Passed (err, objectList2) -> Passed (err, [ObjectMove (objectList2, playerPos, objectPos)])
                    | Blocked -> Blocked
                    | CrashRaised err -> CrashRaised err
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
                Passed(err, [Minor])
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
                Passed (err, [Minor])
            | Blocked -> Blocked
            | CrashRaised err -> CrashRaised err
    
    let inventorySelect (idx: int) = if idx < 0 || idx >= GameCore.InventoryStack then None else Some idx 
    

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
                | Some (idx, key) -> Passed (Set.empty, idx)
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
                | Some (idx, Key x) when x = v -> Passed (Set.empty, idx)
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
                Passed (err, [Minor])
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


module StageCore = 
    let DamperSizefromCenter = { 
        X = (int GameCore.virtualScreenSize.X)/(2*GameCore.BlockSize) + GameCore.GridPadding/2; 
        Y = (int GameCore.virtualScreenSize.Y)/(2*GameCore.BlockSize) + GameCore.GridPadding/2
    }
    let ScreenSizefromCenter = { 
        X = (int GameCore.virtualScreenSize.X)/(2*GameCore.BlockSize) + 2; 
        Y = (int GameCore.virtualScreenSize.Y)/(2*GameCore.BlockSize) + 2 
    }

    let DeadZone = Vector2(
        GameCore.virtualScreenSize.X/(2.0f*(float32 GameCore.BlockSize))*GameCore.DeadZoneRatio,
        GameCore.virtualScreenSize.Y/(2.0f*(float32 GameCore.BlockSize))*GameCore.DeadZoneRatio 
    )
    
    let rangeDrawBlock (stage: InStage) = 
        let center = StageGrid.vectorToGridPos stage.cameraPos
        let minScreen = center - ScreenSizefromCenter
        let maxScreen = center + ScreenSizefromCenter

        (max minScreen.X 0, min maxScreen.X (stage.stageMap.width - 1)), (max minScreen.Y 0, min maxScreen.Y (stage.stageMap.height - 1))
    
    let centerinStartPos (pos: GridPosition) (map: StageGrid) = 
        let maxX = map.width
        let maxY = map.height
        let centerX = 
            match pos.X - DamperSizefromCenter.X > 0, pos.X + DamperSizefromCenter.X < maxX with
            | true, true -> float32 pos.X
            | true, false -> float32 (maxX - DamperSizefromCenter.X)
            | false, true -> float32 DamperSizefromCenter.X
            | false, false -> float32 (maxX/2)
        let centerY = 
            match pos.Y - DamperSizefromCenter.Y > 0, pos.Y + DamperSizefromCenter.Y < maxY with
            | true, true -> float32 pos.Y
            | true, false -> float32 (maxY - DamperSizefromCenter.Y)
            | false, true -> float32 DamperSizefromCenter.Y
            | false, false -> float32 (maxY/2)
        Vector2(centerX, centerY)
    
    let cameraInRealPos (realPos: Vector2) (cameraPos: Vector2) (stage: InStage) = 
        let maxX = float32 stage.stageMap.width
        let maxY = float32 stage.stageMap.height
        let DamperX = float32 DamperSizefromCenter.X
        let DamperY = float32 DamperSizefromCenter.Y
        let centerX = 
            match realPos.X - DamperX > 0.0f, realPos.X + DamperX < maxX with
            | true, true -> 
                if cameraPos.X + DeadZone.X < realPos.X || cameraPos.X - DeadZone.X > realPos.X then realPos.X
                else cameraPos.X
            | true, false ->  maxX - DamperX
            | false, true -> DamperX
            | false, false -> maxX/2.0f
        let centerY = 
            match realPos.Y - DamperY > 0.0f, realPos.Y + DamperY < maxY with
            | true, true ->
                if cameraPos.Y + DeadZone.Y < realPos.Y || cameraPos.Y - DeadZone.X > realPos.Y then realPos.Y
                else cameraPos.Y
            | true, false -> maxY - DamperY
            | false, true -> DamperY
            | false, false -> maxY/2.0f
        Vector2(centerX, centerY)
    
    let cameraTrace (cameraPos: Vector2) (cameraTarget: Vector2) = 
        let cameraMove = (cameraTarget - cameraPos)*GameCore.cameraTraceSpeed
        cameraPos + cameraMove



module InStage = 
    let moveTimeMap (gameMove: GameMove) = 
        match gameMove with
        | ObjectMove _ -> GameCore.DefaultActionDelay*1.2f, GameCore.DefaultActionDelay*1.2f
        | Minor -> 0.0f, GameCore.DefaultActionDelay
        | NoAction -> 0.0f, 0.0f
        | StageVictory -> 0.0f, infinityf
        | PlayerDead -> 0.0f, infinityf
        | StageCrashed _ -> 0.0f, infinityf

    let newStage (map: CompactGrid) (patchMap: PatchMap) (pushBlockFlag: bool) (isNoGround: bool) (inventoryFlag: bool) = 
        let stageGrid, playerPos = StageGrid.makeStageGrid map isNoGround
        let camera = StageCore.centerinStartPos playerPos stageGrid
        
        {
            patch = patchMap
            pushFlag = pushBlockFlag
            inventoryFlag = inventoryFlag
            playerPos = playerPos
            prevPlayerPos = playerPos
            playerRealPos = StageGrid.gridPosToVector playerPos
            cameraTarget = camera
            cameraPos = camera
            playerDirection = Direction.R
            stageMap = stageGrid
            inventory = if inventoryFlag then Array.create GameCore.InventoryStack Empty else [||]
            selectedIdx = 0
            usedBug = Set.empty
            movement = ([], 0.0f)
            moveTime = None
            moveTimeSpent = 0.0f
            fullTimeSpent = 0.0f
        }
    
    let updateByMovement (gameMove : GameMove list * float32) (stage: InStage)= 
        let movement, _ = gameMove
        movement |> List.fold (fun err move -> 
            match StageObject.moveClearOnPos move stage with
            | Passed (errC, ()) -> Set.union err errC
            | _ -> err
        ) Set.empty
    
    let update (someAction: KeyBind option) (stage: InStage) (deltaTime: float32): InStage = 
        match stage.moveTime with
        | Some movetime when System.Single.IsInfinity movetime -> 
            { stage with fullTimeSpent = stage.fullTimeSpent + deltaTime }
        | Some movetime -> 
            let nextTime = stage.moveTimeSpent - deltaTime
            if nextTime > 0.0f then
                let timeRatio = 1.0f - nextTime/movetime
                let direction = stage.playerPos - stage.prevPlayerPos
                let realPos = StageGrid.gridPosToVector stage.prevPlayerPos + Vector2(float32 direction.X*timeRatio, float32 direction.Y*timeRatio)
                let cameraTarget = StageCore.cameraInRealPos realPos stage.cameraTarget stage
                let cameraPos = StageCore.cameraTrace stage.cameraPos cameraTarget
                { stage with 
                    playerRealPos = realPos
                    cameraTarget = cameraTarget
                    cameraPos = cameraPos
                    moveTimeSpent = nextTime; 
                    fullTimeSpent = stage.fullTimeSpent + deltaTime 
                }
            else
                let err = updateByMovement stage.movement stage
                let realPos = StageGrid.gridPosToVector stage.playerPos
                let cameraTarget = StageCore.cameraInRealPos realPos stage.cameraTarget stage
                let cameraPos = StageCore.cameraTrace stage.cameraPos cameraTarget
                { stage with 
                    usedBug = Set.union err stage.usedBug
                    prevPlayerPos = stage.playerPos;
                    playerRealPos = realPos
                    cameraTarget = cameraTarget
                    cameraPos = cameraPos
                    movement = ([], 0.0f); 
                    moveTime = None; 
                    moveTimeSpent = 0.0f; 
                    fullTimeSpent = stage.fullTimeSpent + deltaTime 
                }
        | None ->
            let playerResult = StagePlayer.playerResult stage
            match playerResult with
            | Passed (err, Victory) -> 
                let transtime, movetime = moveTimeMap StageVictory
                { stage with 
                    usedBug = Set.union err stage.usedBug; 
                    movement = ([StageVictory], transtime); 
                    moveTime = Some movetime; 
                    moveTimeSpent = movetime; 
                    fullTimeSpent = stage.fullTimeSpent + deltaTime 
                }
            | Passed (err, Dead) -> 
                let transtime, movetime = moveTimeMap PlayerDead
                StagePlayer.deletePlayer stage
                { stage with 
                    usedBug = Set.union err stage.usedBug; 
                    movement = ([PlayerDead], transtime); 
                    moveTime = Some movetime; 
                    moveTimeSpent = movetime; 
                    fullTimeSpent = stage.fullTimeSpent + deltaTime 
                }
            | Passed (err, Alive) -> 
                match someAction with
                | Some (Move direction) -> 
                    let cameraTarget = StageCore.cameraInRealPos stage.playerRealPos stage.cameraTarget stage
                    let cameraPos = StageCore.cameraTrace stage.cameraPos cameraTarget
                    let result1 = StagePlayer.playerMove direction stage
                    match result1 with
                    | Passed (err1, objectList) ->
                        let targetPos = 
                            match objectList with
                            | ObjectMove (_, _, targetPos) :: _ -> targetPos
                            | _ -> stage.playerPos
                        let transtime, movetime = moveTimeMap objectList.Head
                        { stage with 
                            playerPos = targetPos
                            playerDirection = direction
                            cameraTarget = cameraTarget
                            cameraPos = cameraPos
                            usedBug = Set.unionMany [err; err1; stage.usedBug]
                            movement = (objectList, transtime); 
                            moveTime = Some movetime; 
                            moveTimeSpent = movetime;
                            fullTimeSpent = stage.fullTimeSpent + deltaTime 
                        }
                    | Blocked ->
                        let result2 = StageInteraction.playerInteract direction stage
                        match result2 with
                        | Passed (err1, gameMove) ->
                            let transtime, movetime = moveTimeMap gameMove.Head
                            { stage with 
                                playerDirection = direction
                                cameraTarget = cameraTarget
                                cameraPos = cameraPos
                                usedBug = Set.unionMany [err; err1; stage.usedBug]
                                movement = (gameMove, transtime); 
                                moveTime = Some movetime; 
                                moveTimeSpent = movetime;
                                fullTimeSpent = stage.fullTimeSpent + deltaTime 
                            }
                        | Blocked ->
                            let transtime, movetime = moveTimeMap Minor
                            { stage with 
                                playerDirection = direction
                                cameraTarget = cameraTarget
                                cameraPos = cameraPos
                                usedBug = Set.union err stage.usedBug
                                movement = ([Minor], transtime); 
                                moveTime = Some movetime; 
                                moveTimeSpent = movetime;
                                fullTimeSpent = stage.fullTimeSpent + deltaTime 
                            }
                        | CrashRaised err1 -> 
                            let transtime, movetime = moveTimeMap (StageCrashed err1)
                            { stage with 
                                playerDirection = direction
                                cameraTarget = cameraTarget
                                cameraPos = cameraPos
                                usedBug = Set.unionMany [err; Set [err1]; stage.usedBug]
                                movement = ([StageCrashed err1], transtime); 
                                moveTime = Some movetime; 
                                moveTimeSpent = movetime;
                                fullTimeSpent = stage.fullTimeSpent + deltaTime 
                            }
                    | CrashRaised err1 ->
                        let transtime, movetime = moveTimeMap (StageCrashed err1)
                        { stage with 
                            playerDirection = direction
                            cameraTarget = cameraTarget
                            cameraPos = cameraPos
                            usedBug = Set.unionMany [err; Set [err1]; stage.usedBug]
                            movement = ([StageCrashed err1], transtime); 
                            moveTime = Some movetime; 
                            moveTimeSpent = movetime;
                            fullTimeSpent = stage.fullTimeSpent + deltaTime 
                        }
                | Some GetObject when stage.inventoryFlag -> 
                    let cameraTarget = StageCore.cameraInRealPos stage.playerRealPos stage.cameraTarget stage
                    let cameraPos = StageCore.cameraTrace stage.cameraPos cameraTarget
                    match StageInventory.inventoryGet stage with
                    | Passed (err, gameMove) ->
                        let transtime, movetime = moveTimeMap gameMove.Head
                        { stage with 
                            cameraTarget = cameraTarget
                            cameraPos = cameraPos
                            usedBug = Set.union err stage.usedBug
                            movement = (gameMove, transtime); 
                            moveTime = Some movetime; 
                            moveTimeSpent = movetime;
                            fullTimeSpent = stage.fullTimeSpent + deltaTime 
                        }
                    | Blocked ->
                        let transtime, movetime = moveTimeMap NoAction
                        { stage with 
                            cameraTarget = cameraTarget
                            cameraPos = cameraPos
                            usedBug = Set.union err stage.usedBug
                            movement = ([NoAction], transtime); 
                            moveTime = None; 
                            moveTimeSpent = movetime;
                            fullTimeSpent = stage.fullTimeSpent + deltaTime
                        }
                    | CrashRaised err1 -> 
                        let transtime, movetime = moveTimeMap (StageCrashed err1)
                        { stage with 
                            cameraTarget = cameraTarget
                            cameraPos = cameraPos
                            usedBug = Set.unionMany [err; Set [err1]; stage.usedBug]
                            movement = ([StageCrashed err1], transtime); 
                            moveTime = Some movetime; 
                            moveTimeSpent = movetime;
                            fullTimeSpent = stage.fullTimeSpent + deltaTime 
                        }
                | Some PutDown when stage.inventoryFlag -> 
                    let cameraTarget = StageCore.cameraInRealPos stage.playerRealPos stage.cameraTarget stage
                    let cameraPos = StageCore.cameraTrace stage.cameraPos cameraTarget
                    match StageInventory.inventoryPut stage with
                    | Passed (err, gameMove) ->
                        let transtime, movetime = moveTimeMap gameMove.Head
                        { stage with 
                            cameraTarget = cameraTarget
                            cameraPos = cameraPos
                            usedBug = Set.union err stage.usedBug
                            movement = (gameMove, transtime); 
                            moveTime = Some movetime; 
                            moveTimeSpent = movetime;
                            fullTimeSpent = stage.fullTimeSpent + deltaTime 
                        }
                    | Blocked ->
                        let transtime, movetime = moveTimeMap NoAction
                        { stage with 
                            cameraTarget = cameraTarget
                            cameraPos = cameraPos
                            usedBug = Set.union err stage.usedBug
                            movement = ([NoAction], transtime); 
                            moveTime = None; 
                            moveTimeSpent = movetime;
                            fullTimeSpent = stage.fullTimeSpent + deltaTime
                        }
                    | CrashRaised err1 -> 
                        let transtime, movetime = moveTimeMap (StageCrashed err1)
                        { stage with 
                            cameraTarget = cameraTarget
                            cameraPos = cameraPos
                            usedBug = Set.unionMany [err; Set [err1]; stage.usedBug]
                            movement = ([StageCrashed err1], transtime); 
                            moveTime = Some movetime; 
                            moveTimeSpent = movetime;
                            fullTimeSpent = stage.fullTimeSpent + deltaTime 
                        }
                | Some (Number v) when stage.inventoryFlag ->
                    let cameraTarget = StageCore.cameraInRealPos stage.playerRealPos stage.cameraTarget stage
                    let cameraPos = StageCore.cameraTrace stage.cameraPos cameraTarget
                    let result = StageInventory.inventorySelect (v - 1)
                    match result with
                    | Some idx ->
                        { stage with 
                            selectedIdx = idx
                            cameraTarget = cameraTarget
                            cameraPos = cameraPos
                            fullTimeSpent = stage.fullTimeSpent + deltaTime 
                        }
                    | None ->
                        { stage with 
                            cameraTarget = cameraTarget
                            cameraPos = cameraPos
                            fullTimeSpent = stage.fullTimeSpent + deltaTime 
                        }
                // for new Key-Button interaction
                | _ -> 
                    let cameraTarget = StageCore.cameraInRealPos stage.playerRealPos stage.cameraTarget stage
                    let cameraPos = StageCore.cameraTrace stage.cameraPos cameraTarget
                    { stage with 
                        cameraTarget = cameraTarget
                        cameraPos = cameraPos
                        fullTimeSpent = stage.fullTimeSpent + deltaTime 
                    }
            | _ -> failwith "Game Real Crashed with Unexpected State in PlayerResult. This Cannot Happen Because of PlayerResult Definition."



