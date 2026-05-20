namespace TermProj

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics

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

    type Transition = {
        transitionType: TransitionType
        duration: float32
    }

    type UIAction = 
        | Moveto of (GameScreen * Transition)
        | StageAction of KeyBind
        | Blocked
        | ExitGame
        | Dummy

    type ScreenInteract = {
        buttons: int -> UIAction option * GameStateChange list
        keys: KeyBind -> UIAction option * GameStateChange list
        handler: unit -> UIAction option * GameStateChange list
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





/// On ScreenMap.fs
module InteractUI = 
    let isLeftPressed (input: InputState) = 
        input.mouse.curMouse.LeftButton = ButtonState.Pressed
    
    let isLeftClicked (input: InputState) = 
        input.mouse.curMouse.LeftButton = ButtonState.Released && input.mouse.prevMouse.LeftButton = ButtonState.Pressed


    /// Get button state with curser position and clicked state - input gives mouse action on screen
    let getButtonState (screen: UI.ScreenUI) (input: InputState) = 
        match UI.getButton input.mouse.pos screen with
        | Some id -> 
            let state = if isLeftPressed input then UI.Pressed else UI.Hovered
            Some (id, state)
        | None -> None
    

    let getClickedButton (screen: UI.ScreenUI) (input: InputState) = 
        if isLeftClicked input then
            let curButton = UI.getButton input.mouse.pos screen
            let prevButton = UI.getButton input.mouse.prevPos screen
            match curButton, prevButton with
            | Some cid, Some pid when cid = pid -> Some cid
            | _ -> None
        else None
    

    /// Order preferance -> 1. Auto Handler 2. Button Click(not hover) 3. Keyboard Interaction 
    /// -> Can Insert many input but Only one action return.
    let tryNextScreenAction (screen: UI.ScreenUI) (interact: UI.ScreenInteract) (input: InputState) = 
        let handlerButtonResult = 
            match interact.handler (), getClickedButton screen input with
            | (Some action, stateChange), _ -> Some action, stateChange
            | (None, _), Some buttonId -> interact.buttons buttonId
            | (None, _), None -> None, []
        match handlerButtonResult, input.keyboard.curKey with
        | (Some action, stateChange), _ -> Some action, stateChange
        | (None, _), Some key when input.keyboard.prevKey <> Some key -> 
            match interact.keys key with
            | None, [] -> Some (UI.StageAction key), []
            | v -> v
        | _, _ -> None, []





/// On ScreenMap.fs
/// module for draw one screen
module DrawUI = 
    /// draw text with font.
    let drawText (spriteBatch: SpriteBatch) (font: SpriteFont) (text: string) (pos: Vector2) (color: Color) (scale: float32) =
        spriteBatch.DrawString(font, text, pos, color,0.0f, Vector2.Zero, scale, SpriteEffects.None, 0.0f)
    /// Draw texture with position and real size
    let drawTexture (spriteBatch: SpriteBatch) (texture: Texture2D) (pos: Vector2) (size: Vector2) (color: Color) =
        let scale = Vector2(size.X/(float32 texture.Width), size.Y/(float32 texture.Height))
        spriteBatch.Draw(texture, pos, System.Nullable<Rectangle>(), color, 0.0f, Vector2.Zero, scale, SpriteEffects.None, 0.0f)
    
    /// draw sub screen in main screen. Offset is for transition
    let subScreenDraw (context: DrawContext) (subScreen: UI.SubScreen) (offset: Vector2) (colorscale: float32) =
        subScreen.inner |> List.iter (function
            | UI.Text innertext ->
                let Somefont = AssetMap.getFont context innertext.font
                match Somefont with
                | Some font ->
                    let textSize = font.MeasureString(innertext.content)*innertext.scale
                    let realpos = UI.getRealPos innertext.pos textSize subScreen
                    drawText context.spriteBatch font innertext.content (realpos + offset) (Color.Multiply(innertext.color, colorscale)) innertext.scale
                | None ->
                    let font = AssetMap.getDefaultFont context 
                    let textSize = font.MeasureString(innertext.content)*innertext.scale
                    let realpos = UI.getRealPos innertext.pos textSize subScreen
                    drawText context.spriteBatch font innertext.content (realpos + offset) (Color.Multiply(innertext.color, colorscale)) innertext.scale
            | UI.Texture innertexture -> 
                let realpos = UI.getRealPos innertexture.pos innertexture.size subScreen
                let someTexture = AssetMap.getTexture context innertexture.texture
                match someTexture with
                | Some texture ->
                    drawTexture context.spriteBatch texture (realpos + offset) innertexture.size (Color.Multiply(innertexture.color, colorscale))
                | None ->
                    let texture = AssetMap.getDefaultTexture context
                    drawTexture context.spriteBatch texture (realpos + offset) innertexture.size (Color.Multiply(innertexture.color, colorscale))
        )
    
    /// draw button with button state.
    let buttonDraw (context: DrawContext) (button: UI.ButtonInfo) (state: UI.ButtonCurrent) (offset: Vector2) =
        match state with
        | UI.Normal -> subScreenDraw context button.normalLayout offset 1.0f
        | UI.Hovered -> 
            match button.hoveredLayout with
            | Some layout -> subScreenDraw context layout offset 1.0f
            | None -> subScreenDraw context button.normalLayout offset 0.7f
        | UI.Pressed ->
            match button.pressedLayout with
            | Some layout -> subScreenDraw context layout offset 1.0f
            | None -> subScreenDraw context button.normalLayout offset 0.3f
    
    
    // for scene test
    let drawBlackScreen (context: DrawContext) (opacity: float32) (offset: Vector2) =
        let black = AssetMap.getDefaultTexture context
        let color = Color(0.0f, 0.0f, 0.0f, opacity)
        drawTexture context.spriteBatch black offset  (Vector2(1280.0f, 720.0f)) color
    /// Draw from ScreenMap
    let screenDraw (context: DrawContext) (screen: UI.ScreenUI) (buttonState: (int * UI.ButtonCurrent) option) (offset: Vector2) =
        screen.subscreens |> List.iter (fun sub -> subScreenDraw context sub offset 1.0f)
        match buttonState with
        | None -> screen.buttons |> List.iter (fun button -> buttonDraw context button UI.Normal offset)
        | Some (buttonId, state) ->
            screen.buttons |> List.iter (fun button -> 
                let state = if button.Id = buttonId then state else UI.Normal
                buttonDraw context button state offset
            )





/// On ScreenMap.fs
module ScreenMapBase = 
    let subScreenOfMainSize (inner: UI.SubScreenInner list): UI.SubScreen = 
        {
            inner = inner
            pos = Vector2(0.0f, 0.0f)
            size = GameCore.virtualScreenSize
        }
    ///defaultScreen
    let DefaultScreen (state: GameState): UI.ScreenUI = { buttons = []; subscreens = [] }
    let DefaultInteract (state: GameState): UI.ScreenInteract = {
        buttons = (fun (state: GameState) (num: int) -> None, []) state
        keys = (fun (state: GameState) (key: KeyBind) -> None, []) state
        handler = (fun (state: GameState) () -> None, []) state
    }
    
    /// Main Menu Screen ---------------------------------------------------------------------
    let MainMenu: UI.ScreenUI = 
        // title
        let titleText: UI.InnerText = { 
            font = DefaultFont 
            content = "Play In Progress"
            color = Color.White
            scale = 3.0f
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }

        let titleScreen: UI.SubScreen = {
            inner = [UI.Text titleText]
            pos = Vector2(100.0f, 100.0f)
            size = Vector2(1080.0f, 200.0f)
        }


        // press enter to start
        let promptText: UI.InnerText = { 
            font = DefaultFont 
            content = "Press Enter to Start"
            color = Color.White
            scale = 1.0f
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let promptScreen: UI.SubScreen = {
            inner = [UI.Text promptText]
            pos = Vector2(100.0f, 500.0f)
            size = Vector2(1080.0f, 100.0f)
        }

        let button: UI.ButtonInfo = {
            Id = 0
            normalLayout = subScreenOfMainSize []
            hoveredLayout = Some (subScreenOfMainSize [])
            pressedLayout = Some (subScreenOfMainSize [])
        }

        // organize
        let subscreens = [titleScreen; promptScreen]
        let buttons = [button]
        { buttons = buttons; subscreens = subscreens }
    

    let MainMenuButton (state: GameState) = function
        | 0 -> Some (UI.Moveto (GameScreen.StageSelect 0, { transitionType = Fade 0.4f; duration = 0.8f })), []
        | _ -> None, []
    let MainMenuKey (state: GameState) = function
        | Confirm -> Some (UI.Moveto (GameScreen.StageSelect 0, { transitionType = Fade 0.4f; duration = 0.8f })), []
        | _ -> None, []
    let MainMenuHandler (state: GameState) () =
        None, []
    
    let MainMenuScreen (state: GameState): UI.ScreenUI = MainMenu
    /// Main Menu Interaction
    let MainMenuInteract (state: GameState): UI.ScreenInteract = {
            buttons = MainMenuButton state
            keys = MainMenuKey state
            handler = MainMenuHandler state
    }






    /// Stage Select Screen ----------------------------------------------------------------
    let StageSelectBase (v: int) : UI.ScreenUI = 
        let patchVerText: UI.InnerText = {
            font = DefaultFont 
            content = sprintf "Patch Ver 1.%02d" v
            color = Color.White
            scale = 2.0f
            pos = UI.AlignPos (UI.CenterX, UI.Top)
        }
        let DescText: UI.InnerText = {
            font = DefaultFont 
            content = "Our First Release"
            color = Color.White
            scale = 1.2f
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let promptText: UI.InnerText = {
            font = DefaultFont 
            content = "Press Enter"
            color = Color.White
            scale = 1.5f
            pos = UI.AlignPos (UI.CenterX, UI.Bottom)
        }
        let levelScreen: UI.SubScreen = {
            inner = [UI.Text patchVerText; UI.Text DescText; UI.Text promptText]
            pos = Vector2(340.0f, 30.0f)
            size = Vector2(600.0f, 660.0f)
        }

        


        let leftButtonInner: UI.InnerTexture = {
            texture = BasePixel
            color = Color.White
            size = Vector2(80.0f, 120.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let leftButtonScreen: UI.SubScreen = {
            inner = [UI.Texture leftButtonInner]
            pos = Vector2(220.0f, 330.0f)
            size = Vector2(80.0f, 120.0f)
        }
        let leftButton: UI.ButtonInfo = {
            Id = 0
            normalLayout = leftButtonScreen
            hoveredLayout = None
            pressedLayout = None
        }




        let rightButtonInner: UI.InnerTexture = {
            texture = BasePixel
            color = Color.White
            size = Vector2(80.0f, 120.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let rightButtonScreen: UI.SubScreen = {
            inner = [UI.Texture rightButtonInner]
            pos = Vector2(980.0f, 330.0f)
            size = Vector2(80.0f, 120.0f)
        }
        let rightButton: UI.ButtonInfo = {
            Id = 1
            normalLayout = rightButtonScreen
            hoveredLayout = None
            pressedLayout = None
        }
        

        let subscreens = [levelScreen]
        let buttons = if v = 0 then [rightButton] else [leftButton; rightButton]
        { buttons = buttons; subscreens = subscreens }


    let _goPrev (state: GameState) = 
        let presentStage = GameState.getPresentStage state
        if presentStage = 0 then None, []
        else
            let nextStage = presentStage - 1
            match GameState.setSelectedStage state nextStage with
            | [] -> Some (UI.Moveto(GameScreen.StageBlockPopup, { transitionType = Popup true; duration = 0.5f })), []
            | v -> Some (UI.Moveto (GameScreen.StageSelect nextStage, { transitionType = Slide R; duration = 0.8f })), v
    let _goNext (state: GameState) = 
        let presentStage = GameState.getPresentStage state
        let nextStage = presentStage + 1
        match GameState.setSelectedStage state nextStage with
        | [] -> Some (UI.Moveto(GameScreen.StageBlockPopup, { transitionType = Popup true; duration = 0.5f })), []
        | v -> Some (UI.Moveto (GameScreen.StageSelect nextStage, { transitionType = Slide L; duration = 0.8f })), v
    let StageSelectButton (state: GameState) = function
        | 0 -> _goPrev state
        | 1 -> _goNext state
        | 2 -> None, []
        | _ -> None, []
    let StageSelectKey (state: GameState) = function
        | Escape -> Some (UI.Moveto (GameScreen.MainMenu, { transitionType = Fade 0.5f; duration = 0.8f })), []
        | Move R -> _goNext state
        | Move L -> _goPrev state
        | Confirm -> None, []
        | _ -> None, []
    let StageSelectHandler (state: GameState) () = 
        None, []
    let StageSelectCache = List.map (fun v -> v, StageSelectBase v) [0..GameCore.gameStage] |> Map.ofList


    let StageSelectScreen (state: GameState) (v: int): UI.ScreenUI = StageSelectCache |> Map.find v
    let StageSelectInteract (state: GameState): UI.ScreenInteract = {
        buttons = StageSelectButton state
        keys = StageSelectKey state
        handler = StageSelectHandler state
    }






    /// Stage Block Popup ----------------------------------------------------------------
    let StageBlockPopupBase: UI.ScreenUI = 
        let PopupOuter: UI.InnerTexture = {
            texture = BasePixel
            color = Color.Navy
            size = Vector2(800.0f, 528.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let PopupInner: UI.InnerTexture = {
            texture = BasePixel
            color = Color.Black
            size = Vector2(784.0f, 512.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let warning: UI.InnerText = { 
            font = DefaultFont 
            content = "Warning"
            color = Color.White
            scale = 1.5f
            pos = UI.AlignPos (UI.Left, UI.Top)
        }
        let promptText: UI.InnerText = { 
            font = DefaultFont 
            content = "\nThe version you are trying to access\n\n is not available yet."
            color = Color.White
            scale = 1.0f
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let PopupScreen: UI.SubScreen = {
            inner = [UI.Texture PopupOuter; UI.Texture PopupInner; UI.Text warning; UI.Text promptText]
            pos = Vector2(240.0f, 96.0f)
            size = Vector2(800.0f, 528.0f)
        }



        let buttonTexture: UI.InnerTexture = {
            texture = BasePixel
            color = Color.Navy
            size = Vector2(48.0f, 48.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let button: UI.SubScreen = {
            inner = [UI.Texture buttonTexture]
            pos = Vector2(968.0f, 120.0f)
            size = Vector2(48.0f, 48.0f)
        }

        let PopupButton: UI.ButtonInfo = {
            Id = 0
            normalLayout = button
            hoveredLayout = None
            pressedLayout = None
        }
        let subscreens = [PopupScreen]
        let buttons = [PopupButton]
        { buttons = buttons; subscreens = subscreens }
    let StageBlockPopupButton (state: GameState) = function
        | 0 -> Some (UI.Moveto (GameScreen.StageSelect (GameState.getPresentStage state), { transitionType = Popup false; duration = 0.5f })), []
        | _ -> None, []
    let StageBlockPopupKey (state: GameState) = function
        | Escape -> Some (UI.Moveto (GameScreen.StageSelect (GameState.getPresentStage state), { transitionType = Popup false; duration = 0.5f })), []
        | _ -> None, []
    let StageBlockPopupHandler (state: GameState) () =
        None, []
    
    let StageBlockPopupScreen (state: GameState): UI.ScreenUI = StageBlockPopupBase
    /// Main Menu Interaction
    let StageBlockPopupInteract (state: GameState): UI.ScreenInteract = {
            buttons = StageBlockPopupButton state
            keys = StageBlockPopupKey state
            handler = StageBlockPopupHandler state
    }

























module ScreenMap = 
    let screenMap (screen: GameScreen) (state: GameState) = 
        match screen with
        | MainMenu -> ScreenMapBase.MainMenuScreen state
        | StageSelect v -> ScreenMapBase.StageSelectScreen state v
        | StageBlockPopup -> ScreenMapBase.StageBlockPopupScreen state
        | _ -> ScreenMapBase.DefaultScreen state
    
    let interactMap (screen: GameScreen) (state: GameState) = 
        match screen with
        | MainMenu -> ScreenMapBase.MainMenuInteract  state
        | StageSelect _ -> ScreenMapBase.StageSelectInteract state
        | StageBlockPopup -> ScreenMapBase.StageBlockPopupInteract state
        | _ -> ScreenMapBase.DefaultInteract state

    /// Get next action from input and gamescreen
    let getNextAction (screen: GameScreen) (state: GameState) (input: InputState) =
        let screenUI = screenMap screen state
        let interact = interactMap screen state
        let buttonState = InteractUI.getButtonState screenUI input
        let action = InteractUI.tryNextScreenAction screenUI interact input
        buttonState, action
    
    /// Draws the current screen with offset for transition
    let drawScreen (context: DrawContext) (screen: GameScreen) (buttonState: (int * UI.ButtonCurrent) option) (state: GameState) (offset: Vector2) =
        match screen with
        | BlackScreen v -> DrawUI.drawBlackScreen context v offset
        | gameScreen ->
            let screenUI = screenMap gameScreen state
            DrawUI.screenDraw context screenUI buttonState offset