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


module UI = 
    type HorizontalPos = 
        | Left
        | CenterX
        | Right
    type VerticalPos = 
        | Top
        | CenterY
        | Bottom
    
    type InnerPos =
        | AlignPos of HorizontalPos * VerticalPos
        | CustomPos of Vector2
        | CustomRatioPos of Vector2
    
    type InnerText = {
        content: string
        color: Color
        font: FontID
        scale: float32
        pos: InnerPos
    }

    type InnerTexture = {
        texture: TextureID
        color: Color
        size: Vector2
        pos: InnerPos
    }
    
    type SubScreenInner = 
        | Text of InnerText
        | Texture of InnerTexture

    type SubScreen = {
        inner: SubScreenInner list
        pos: Vector2
        size: Vector2
    }
    
    type ButtonInfo = {
        Id: int
        normalLayout: SubScreen
        hoveredLayout: SubScreen option
        pressedLayout: SubScreen option
    }

    type ButtonCurrent = 
        | Normal
        | Hovered
        | Pressed

    type ScreenUI = {
        buttons: ButtonInfo list
        subscreens: SubScreen list
    }

    type UIAction = 
        | Moveto of GameScreen
        | Blocked
        | Dummy

    type ScreenInteract = {
        buttons: Map<int, UIAction>
        keys: Map<KeyBind, UIAction>
    }

    type ScreenCache = {
        layout: Map<int, ScreenUI>
        interact: Map<int, ScreenInteract>
    }

    let isInButton (mousePos: Vector2) (button: ButtonInfo) =
        let buttonPos = button.normalLayout.pos
        let buttonSize = button.normalLayout.size
        mousePos.X >= buttonPos.X && mousePos.X <= buttonPos.X + buttonSize.X &&
        mousePos.Y >= buttonPos.Y && mousePos.Y <= buttonPos.Y + buttonSize.Y
    
    let getButton (mousePos: Vector2) (screenUI: ScreenUI) =
        screenUI.buttons
        |> List.tryPick (fun button -> if (isInButton mousePos button) then Some button.Id else None)
    
    let getRealPos (innerPos: InnerPos) (size: Vector2) (subScreen: SubScreen) =
        match innerPos with
        | AlignPos (h, v) ->
            let x =
                match h with
                | Left -> subScreen.pos.X + subScreen.size.X*0.1f
                | CenterX -> subScreen.pos.X + (subScreen.size.X - size.X)*0.5f
                | Right -> subScreen.pos.X + subScreen.size.X - size.X - subScreen.size.X*0.1f
            let y =
                match v with
                | Top -> subScreen.pos.Y + subScreen.size.Y*0.1f
                | CenterY -> subScreen.pos.Y + (subScreen.size.Y - size.Y)*0.5f
                | Bottom -> subScreen.pos.Y + subScreen.size.Y - size.Y - subScreen.size.Y*0.1f
            Vector2(x, y)
        | CustomPos pos -> subScreen.pos + pos
        | CustomRatioPos r -> Vector2(subScreen.pos.X + r.X*subScreen.size.X, subScreen.pos.Y + r.Y*subScreen.size.Y)

module AssetMap = 
    type AssetSpec = 
        | Spec of int
        | SpecDirection of Direction
        | SpecDirectionList of Direction list
        | NoSpec
    
    let contents = "content"
    let getFont (context: DrawContext) (fontId: FontID) = Map.tryFind fontId context.assets.fonts
    let getDefaultFont (context: DrawContext) = context.assets.fonts[DefaultFont]
    let getTexture (context: DrawContext) (textureId: TextureID) = Map.tryFind textureId context.assets.textures
    let getDefaultTexture (context: DrawContext) = context.assets.textures[BasePixel]

    let textureToAssetName = function
        | BasePixel -> None 
        | _ -> None
    
    let fontToAssetName = function
        | DefaultFont -> "font/DefaultFont"
        | TitleFont -> "font/DefaultFont"
    
    let playerTexture (spec: AssetSpec) =
        match spec with
        | SpecDirection U -> PlayerU
        | SpecDirection D -> PlayerD
        | SpecDirection L -> PlayerL
        | SpecDirection R -> PlayerR
        | _ -> PlayerR
    
    let wallTexture (spec: AssetSpec) = 
        match spec with
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
