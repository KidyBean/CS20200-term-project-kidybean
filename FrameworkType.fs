namespace TermProj

open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

type FontID = 
    | DefaultFont
    | TitleFont

type Fonts = Map<FontID, SpriteFont>

type TextureID = 
    | BasePixel
    | NoTexture // for Blank
    // Object Texture
    | PlayerU
    | PlayerD
    | PlayerL
    | PlayerR
    | FlagTexture
    | WallTexture
    | WallU
    | WallD
    | WallL
    | WallR
    | WallUD
    | WallUL
    | WallUR
    | WallDL
    | WallDR
    | WallLR
    | WallUDL
    | WallUDR
    | WallULR
    | WallDLR
    | WallUDLR
    | SpikeTexture
    | BoxTexture
    | KeyTexture
    | DoorTexture
    // Ground Texture
    | GroundTexture
    | BoxGround
    | GroundCliff
    | BoxCliff
    // UI texture

module debug = 
    let UITextureReady = false


type Textures = Map<TextureID, Texture2D>

type Assets = {
    fonts: Fonts
    textures: Textures
}

type DrawContext = {
    spriteBatch: SpriteBatch
    assets: Assets
}


type ScreenTransform = {
    scale: float32
    offset: Vector2
    transformMatrix: Matrix
}


type MouseInput = {
    pos: Vector2
    prevPos: Vector2
    curMouse: MouseState
    prevMouse: MouseState
}

type KeyboardInput = {
    curKey: KeyBind Option
    prevKey: KeyBind Option
}

type InputState = {
    mouse: MouseInput
    keyboard: KeyboardInput
}

module GameCore = 
    let virtualScreenSize = Vector2(1280.0f, 720.0f)
    let initialInputState = {
        mouse = {
            pos = Vector2.Zero
            prevPos = Vector2.Zero
            curMouse = Unchecked.defaultof<MouseState>
            prevMouse = Unchecked.defaultof<MouseState>
        }
        keyboard = {
            curKey = None
            prevKey = None
        }
    }

    let gameStage: int = 20
    let objectLayer: int = 3
    let GridPadding: int = 20

    let BlockSize: int = 40
    let DeadZoneRatio: float32 = 0.2f
    let cameraTraceSpeed: float32 = 0.1f

    let InventoryStack: int = 4

    let DefaultActionDelay: float32 = 0.5f
    let DefaultBlockMoveDuration: float32 = 0.6f

    let defaultDeltaTime = 0.016f // 60fps - test







module AssetMap = 
    type AssetSpec = 
        | Spec of int
        | SpecDirection of Direction
        | SpecDirectionList of Direction list
        | Upper of GroundType
        | UpperObject of ObjectType
        | NoSpec
    
    let contents = "content"
    let getFont (context: DrawContext) (fontId: FontID) = Map.tryFind fontId context.assets.fonts
    let getDefaultFont (context: DrawContext) = context.assets.fonts[DefaultFont]
    let getTexture (context: DrawContext) (textureId: TextureID) = Map.tryFind textureId context.assets.textures
    let getDefaultTexture (context: DrawContext) = context.assets.textures[BasePixel]

    let textureToAssetName = function
        | BasePixel -> None 
        | NoTexture -> None
        | PlayerU -> Some "texture/PlayerU"
        | PlayerD -> Some "texture/PlayerD"
        | PlayerL -> Some "texture/PlayerL"
        | PlayerR -> Some "texture/PlayerR"
        | _ -> None
    
    let fontToAssetName = function
        | DefaultFont -> "font/DefaultFont"
        | TitleFont -> "font/DefaultFont"
    
    let playerTexture = function
        | SpecDirection U -> PlayerU
        | SpecDirection D -> PlayerD
        | SpecDirection L -> PlayerL
        | SpecDirection R -> PlayerR
        | _ -> PlayerR
    
    let wallTexture = function
        | NoSpec -> WallTexture
        | SpecDirectionList [U] -> WallU
        | SpecDirectionList [D] -> WallD
        | SpecDirectionList [L] -> WallL
        | SpecDirectionList [R] -> WallR
        | SpecDirectionList [U; D] -> WallUD
        | SpecDirectionList [U; L] -> WallUL
        | SpecDirectionList [U; R] -> WallUR
        | SpecDirectionList [D; L] -> WallDL
        | SpecDirectionList [D; R] -> WallDR
        | SpecDirectionList [L; R] -> WallLR
        | SpecDirectionList [U; D; L] -> WallUDL
        | SpecDirectionList [U; D; R] -> WallUDR
        | SpecDirectionList [U; L; R] -> WallULR
        | SpecDirectionList [D; L; R] -> WallDLR
        | SpecDirectionList [U; D; L; R] -> WallUDLR
        | _ -> WallTexture

    
    let ObjectTexture (object: ObjectType) (spec: AssetSpec) = 
        match object with
        | Player -> playerTexture spec
        | Wall -> wallTexture spec
        | Flag -> FlagTexture
        | Spike -> SpikeTexture
        | Box -> BoxTexture
        | Key _ -> KeyTexture
        | Door _ -> DoorTexture
        | Empty -> NoTexture

    let cliffTexture = function
    | Upper Ground -> GroundCliff
    | UpperObject Box -> BoxCliff
    | _ -> NoTexture

    let objectGroundTexture = function
    | Box -> BoxGround
    | v -> ObjectTexture v NoSpec

    let GroundTexture (ground: GroundType) (spec: AssetSpec) = 
        match ground with
        | Ground -> GroundTexture
        | Abyss | AbyssGround -> cliffTexture spec
        | ObjectGround object -> objectGroundTexture object
    
    let hsvColor (h: float32) (s: float32) (v: float32)  = 
        let h = ((h % 360.0f) + 360.0f) % 360.0f
        let c = v * s
        let x = c * (1.0f - abs ((h / 60.0f) % 2.0f - 1.0f))
        let m = v - c

        let r, g, b =
            if h < 60.0f then c, x, 0.0f
            elif h < 120.0f then x, c, 0.0f
            elif h < 180.0f then 0.0f, c, x
            elif h < 240.0f then 0.0f, x, c
            elif h < 300.0f then x, 0.0f, c
            else c, 0.0f, x
        Color(int ((r + m)*255.0f), int ((g + m)*255.0f), int ((b + m)*255.0f))

    let colorBaseOfNum (n: int) = 
        if n = 0 then Color.White
        else
            hsvColor (float32 ((n*143)%360)) 0.8f 0.9f
    
    let colorMap = Array.init 50 colorBaseOfNum
