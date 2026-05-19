namespace TermProj

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

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
        | Blocked
        | Dummy

    type ScreenInteract = {
        buttons: int -> UIAction option * GameStateChange
        keys: KeyBind -> UIAction option * GameStateChange
        handler: unit -> UIAction option * GameStateChange
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
    

    /// Order preferance -> 1. Button Click(not hover) 2. Keyboard Interaction 
    /// -> Can Insert many input but Only one action return.
    let tryNextScreenAction (screen: UI.ScreenUI) (interact: UI.ScreenInteract) (input: InputState) = 
        let handlerButtonResult = 
            match interact.handler (), getClickedButton screen input with
            | (Some action, stateChange), _ -> Some action, stateChange
            | (None, _), Some buttonId -> interact.buttons buttonId
            | (None, _), None -> None, NoStateChange
        match handlerButtonResult, input.keyboard.curKey with
        | (Some action, stateChange), _ -> Some action, stateChange
        | (None, _), Some key when input.keyboard.prevKey <> Some key -> interact.keys key
        | _, _ -> None, NoStateChange

/// On ScreenMap.fs
module ScreenMap = 
    let subScreenOfMainSize (inner: UI.SubScreenInner list): UI.SubScreen = 
        {
            inner = inner
            pos = Vector2(0.0f, 0.0f)
            size = GameCore.virtualScreenSize
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


        // organize
        let subscreens = [titleScreen; promptScreen]
        let buttons = []
        { buttons = buttons; subscreens = subscreens }
    

    let MainMenuButton (state: GameState) = function
        | _ -> None, NoStateChange
    let MainMenuKey (state: GameState) = function
        | Confirm -> Some (UI.Moveto (GameScreen.StageSelect 0, { transitionType = Fade; duration = 0.8f })), NoStateChange
        | _ -> None, NoStateChange
    let MainMenuHandler (state: GameState) () =
        None, NoStateChange
    
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
            scale = 1.0f
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let promptText: UI.InnerText = {
            font = DefaultFont 
            content = "Press Enter"
            color = Color.White
            scale = 1.0f
            pos = UI.AlignPos (UI.CenterX, UI.Bottom)
        }
        let levelScreen: UI.SubScreen = {
            inner = [UI.Text patchVerText; UI.Text DescText; UI.Text promptText]
            pos = Vector2(100.0f, 100.0f)
            size = Vector2(1080.0f, 620.0f)
        }

        let subscreens = [levelScreen]
        let buttons = []
        { buttons = buttons; subscreens = subscreens }
    
    let StageSelectButton (state: GameState) = function
        | _ -> None, NoStateChange
    let StageSelectKey (state: GameState) = function
        | Escape -> Some (UI.Moveto (GameScreen.MainMenu, { transitionType = Fade; duration = 0.8f })), NoStateChange
        | _ -> None, NoStateChange
    let StageSelectHandler (state: GameState) () = 
        None, NoStateChange

    let StageSelectCache = List.map (fun v -> (v, StageSelectBase v)) [0..GameCore.gameStage] |> Map.ofList
    let StageSelect v = StageSelectCache |> Map.find v
    let StageSelectInteract (state: GameState): UI.ScreenInteract = {
        buttons = StageSelectButton state
        keys = StageSelectKey state
        handler = StageSelectHandler state
    }


module dummyDebug =
    let getTransition currentScreen nextScreen = 
        match currentScreen with
        | MainMenu -> Some (Fade, 0.8f) // Some (Sudden 0.0f, 0.0f)
        | StageSelect a ->
            match nextScreen with
            | StagePlaying _ -> Some (Fade, 1.0f)
            | StageSelect b when b > a -> Some (Slide L, 0.5f)
            | StageSelect b when b < a -> Some (Slide R, 0.5f)
            | _ -> None
        | StagePlaying _ ->
            match nextScreen with
            | StageSelect _ -> Some (Fade, 1.0f)
            | _ -> None
        | _ -> Some (Sudden 0.0f, 0.0f)

    module secondTab = 
        let dummy = 0