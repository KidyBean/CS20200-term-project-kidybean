namespace TermProj

open Microsoft.Xna.Framework

type GameLog = 
    | PlayerMove of GridPosition*GridPosition
    | ObjectMove of GridPosition*GridPosition
    | Victory
    | Crashed of BugPatch



type InStage = {
    patch: PatchMap
    playerPos: GridPosition
    goalPos: GridPosition list
    stageMap: StageGrid
    inventory: ObjectType list
    usedBug: BugPatch Set
    prevLog: GameLog list
}

module InStage = 
    // whitelist
    let CanPush = function
        | Box -> true
        | Key _ -> true
        | _ -> false
    let CanFillAbyss = function
        | Box -> true
        | _ -> false
    let CanPutInventory = function
        | Box -> true
        | Key _ -> true
        | _ -> false
    let CanOverLayer = function
        | Key _ -> true
        | _ -> false

    let moveToPos (prev: GridPosition) (next: GridPosition) (stage: InStage) = 
        let stageMap = stage.stageMap
        let patchList = stage.patch
        let targetObjects = stageMap |> StageGrid.objectOnPos prev |> Array.filter (fun x -> x <> Empty)
        let posObjects = stageMap |> StageGrid.objectOnPos next |> Array.filter (fun x -> x <> Empty)
        if Array.isEmpty posObjects then

            Ok NoBug
        else
            Ok NoBug