namespace TermProj

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

type FontID = 
    | DefaultFont
    | TitleFont

type Fonts = Map<FontID, SpriteFont>

type TextureID = 
    | BasePixel

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
    let getFont (context: DrawContext) (fontId: FontID) = context.assets.fonts.[fontId]
    let getTexture (context: DrawContext) (textureId: TextureID) = context.assets.textures.[textureId]

    let gameStage: int = 20
    let objectLayer: int = 2
    let GridPadding: int = 4
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