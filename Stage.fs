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

    let drawCell (context: DrawContext) (object: ObjectType) (spec: AssetMap.AssetSpec) (pos: Vector2) = 
        let textureID = AssetMap.ObjectTexture object spec
        let someTexture = Map.tryFind textureID context.assets.textures
        let idx = 
            match object with
            | Key idx -> idx
            | Door idx -> idx
            | _ -> 0
        let color = AssetMap.colorMap[idx]
        match someTexture with
        | Some texture -> 
            let scale = Vector2(float32 GameCore.BlockSize/float32 texture.Width, float32 GameCore.BlockSize/float32 texture.Height)
            context.spriteBatch.Draw(texture, pos, System.Nullable<Rectangle>(), color, 0.0f, Vector2.Zero, scale, SpriteEffects.None, 0.0f)
        | None -> drawDefault context pos color

    let drawCellInIteration (context: DrawContext) (pos: GridPosition) (object: ObjectType) (stage: InStage) = 


