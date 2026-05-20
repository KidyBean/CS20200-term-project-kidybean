namespace TermProj

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

open System
open System.IO


module Stage = 

    let stageUpdate v = [
        InitialStage
        if v >= 0 then Walk
        if v >= 1 then PushBlock
        if v >= 2 then AbyssAndGround
        if v >= 4 then Inventory
        if v >= 7 then KeyAndDoor
    ]
    
    let stageInventoryFlag (update: Update List) = List.contains Inventory update
    let stageGroundFlag (update: Update List) = List.contains AbyssAndGround update

    let recentUpdate (num: int) = 
        let prevUpdate = stageUpdate (num - 1) |> Set.ofList
        let curUpdate = stageUpdate num |> Set.ofList
        Set.difference curUpdate prevUpdate |> Set.toList |> List.tryHead

    let stageToPath (v: int): string = 
        let relativePath = AssetMap.contents + sprintf "/maps/stage%d.csv" v
        Path.Combine(AppContext.BaseDirectory, relativePath)
    
    let Load (num: int) (patch: PatchMap) = 
        let path = stageToPath num
        let update = stageUpdate num
        File.ReadAllText(path)
        |> fun map -> StageParser.makeCompactStage map (stageGroundFlag update)
        |> fun map -> InStage.newStage map patch (stageInventoryFlag update)
    
    let Update = InStage.update



    

    let drawDefault (context: DrawContext) (pos: Vector2) (color: Color) = 
        let shape = AssetMap.getDefaultTexture context
        let scale = Vector2(float32 GameCore.BlockSize, float32 GameCore.BlockSize)
        context.spriteBatch.Draw(shape, pos, System.Nullable<Rectangle>(), color, 0.0f, Vector2.Zero, scale, SpriteEffects.None, 0.0f)

    let drawObject (context: DrawContext) (object: ObjectType) (spec: AssetMap.AssetSpec) (pos: Vector2) = 
        let textureID = AssetMap.ObjectTexture object spec
        let someTexture = Map.tryFind textureID context.assets.textures
        let pixel = Vector2(pos.X*float32 GameCore.BlockSize, pos.Y*float32 GameCore.BlockSize)
        let idx = 
            match object with
            | Key idx -> idx
            | Door idx -> idx
            | _ -> 0
        let color = AssetMap.colorMap[idx]
        match someTexture with
        | Some texture -> 
            let scale = Vector2(float32 GameCore.BlockSize/float32 texture.Width, float32 GameCore.BlockSize/float32 texture.Height)
            context.spriteBatch.Draw(texture, pixel, System.Nullable<Rectangle>(), color, 0.0f, Vector2.Zero, scale, SpriteEffects.None, 0.0f)
        | None -> drawDefault context pixel color
    
    let drawGround (context: DrawContext) (ground: GroundType) (spec: AssetMap.AssetSpec) (pos: Vector2) = 
        let textureID = AssetMap.GroundTexture ground spec
        let someTexture = Map.tryFind textureID context.assets.textures
        let pixel = Vector2(pos.X*float32 GameCore.BlockSize, pos.Y*float32 GameCore.BlockSize)
        let idx = 
            match ground with
            | ObjectGround (Key idx) -> idx
            | ObjectGround (Door idx) -> idx
            | _ -> 0
        let color = AssetMap.colorMap[idx]
        match someTexture with
        | Some texture -> 
            let scale = Vector2(float32 GameCore.BlockSize/float32 texture.Width, float32 GameCore.BlockSize/float32 texture.Height)
            context.spriteBatch.Draw(texture, pixel, System.Nullable<Rectangle>(), color, 0.0f, Vector2.Zero, scale, SpriteEffects.None, 0.0f)
        | None -> ()

    let getSpec (pos: GridPosition) (object: ObjectType) (stage: InStage) = 
        let stageMap = stage.stageMap
        match object with
        | Player -> AssetMap.SpecDirection stage.playerDirection
        | Wall -> 
            AssetMap.SpecDirectionList [
                if StageGrid.isObjectInPos (pos + StageGrid.directionToGrid U) Wall stageMap then U
                if StageGrid.isObjectInPos (pos + StageGrid.directionToGrid D) Wall stageMap then D
                if StageGrid.isObjectInPos (pos + StageGrid.directionToGrid L) Wall stageMap then L
                if StageGrid.isObjectInPos (pos + StageGrid.directionToGrid R) Wall stageMap then R
            ]
        | _ -> AssetMap.NoSpec

    let drawCellInIteration (context: DrawContext) (pos: GridPosition) (objects: ObjectType[]) (stage: InStage) = 
        let realPos = StageGrid.gridPosToVector pos - stage.cameraPos
        objects
        |> Array.iter (fun object ->
                match object with
                | Empty -> ()
                | _ ->
                    let spec = getSpec pos object stage
                    drawObject context object spec realPos
            )

    let drawGroundInIteration (context: DrawContext) (pos: GridPosition) (ground: GroundType) (stage: InStage) = 
        let realPos = StageGrid.gridPosToVector pos - stage.cameraPos
        match ground with
        | Ground -> drawGround context ground AssetMap.NoSpec realPos
        | _ ->
            let upper = pos + { X = 0; Y = -1 }
            if StageGrid.isPosOutOfStage upper stage.stageMap then drawGround context ground AssetMap.NoSpec realPos
            else
                let upperGround = StageGrid.groundOnPos upper stage.stageMap
                let baseGround = if ground = AbyssGround then AbyssGround else Abyss
                let spec =
                    match upperGround with
                    | ObjectGround object -> AssetMap.UpperObject object
                    | Ground -> AssetMap.Upper Ground
                    | _ -> AssetMap.NoSpec
                drawGround context baseGround spec realPos
            match ground with
            | ObjectGround object -> drawGround context ground AssetMap.NoSpec realPos
            | _ -> ()

    let drawObjectMove (context: DrawContext) (gamemove: GameMove) (movetime: float32) (stage: InStage) = 
        match gamemove, stage.moveTime with
        | ObjectMove (objects, from, goto), Some gameTime when movetime <> 0.0f ->
            let delta = StageGrid.gridPosToVector (goto - from)
            let deltaRatio = (gameTime - stage.moveTimeSpent)/movetime
            let realPos = StageGrid.gridPosToVector from + Vector2(delta.X*deltaRatio, delta.Y*deltaRatio)
            objects
            |> Array.iter (fun object ->
                    match object with
                    | Empty -> ()
                    | Player -> drawObject context object (AssetMap.SpecDirection stage.playerDirection) realPos
                    | _ -> drawObject context object AssetMap.NoSpec realPos
                )
        | _ -> ()
    
    let drawStage (context: DrawContext) (stage: InStage) = 
        let (minX, maxX), (minY, maxY) = StageCore.rangeDrawBlock stage
        let stageMap = stage.stageMap
        for x in minX..maxX do
            for y in minY..maxY do
                let pos = { X = x; Y = y }
                let ground = StageGrid.groundOnPos pos stageMap
                let objects = StageGrid.objectOnPos pos stageMap
                drawGroundInIteration context pos ground stage
                drawCellInIteration context pos objects stage
        let movelist, time = stage.movement
        movelist |> List.iter (fun gmove -> drawObjectMove context gmove time stage)

    let drawInventory (context: DrawContext) (stage: InStage) = 
        ()

