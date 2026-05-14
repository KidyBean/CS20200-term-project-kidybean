namespace TermProj

open Microsoft.Xna.Framework

type BugPatch = 
    | PlayerCollision
    | BlockCollision

type Update = 
    | Walk
    | PushBlock

type PatchMap = Map<BugPatch, bool>

module PatchMap = 
    let isPatched (bug: BugPatch) (patchMap: PatchMap) = 
        let patch = patchMap |> Map.tryFind bug
        match patch with
        | Some patched when patched -> true
        | _ -> false
    
    let newPatch (bug: BugPatch) (patchMap: PatchMap) = 
        patchMap
        |> Map.add bug true

type StageObjectGrid = ObjectType[,,]
type StageGroundGrid = GroundType[,]
type StageGrid = {
    objectLayer: int
    objects: StageObjectGrid
    ground: StageGroundGrid
}

type GridPosition = { X: int; Y: int }

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
            objects = newObjectGrid
            ground = newGroundGrid
        }
    
    let objectOnPos (pos: GridPosition) (stage: StageGrid) = 
        let layer = stage.objectLayer
        let objectArr = Array.create layer Empty
        for l in 0..layer do
            let object = stage.objects[l, pos.X, pos.Y]
            objectArr[l] <- object
        objectArr
    let groundOnPos (pos: GridPosition) (stage: StageGrid) = stage.ground[pos.X, pos.Y]



