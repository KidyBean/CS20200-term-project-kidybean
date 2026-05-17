namespace TermProj

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input


module InteractUI = 
    let isLeftPressed (input: InputState) = 
        input.mouse.curMouse.LeftButton = ButtonState.Pressed
    let isLeftClicked (input: InputState) = 
        input.mouse.curMouse.LeftButton = ButtonState.Released && input.mouse.prevMouse.LeftButton = ButtonState.Pressed

    let getButtonState (screen: UI.ScreenUI) (input: InputState) = 
        match UI.getButton input.mouse.pos screen with
        | Some id -> 
            let state = if (isLeftPressed input) then UI.Pressed else UI.Hovered
            Some (id, state)
        | None -> None
    
    let getClickedButton (screen: UI.ScreenUI) (input: InputState) = 
        if (isLeftClicked input) then
            let curButton = UI.getButton input.mouse.pos screen
            let prevButton = UI.getButton input.mouse.prevPos screen
            match curButton, prevButton with
            | Some cid, Some pid when cid = pid -> Some cid
            | _ -> None
        else None


module ScreenMap = 
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
    /// Main Menu Interaction
    let MainMenuInteract: UI.ScreenInteract = {
        buttons = Map.empty
        keys = Map [
            Confirm, UI.Moveto (StageSelect 0)
        ]
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
    let StageSelectInteractBase (v: int) : UI.ScreenInteract = 
        {
            buttons = Map.empty
            keys = Map.empty
        }
    let StageSelectCache: UI.ScreenCache = 
        let layout = List.map (fun v -> (v, StageSelectBase v)) [0..GameCore.gameStage] |> Map.ofList
        let interact = List.map (fun v -> (v, StageSelectInteractBase v)) [0..GameCore.gameStage] |> Map.ofList
        { layout = layout; interact = interact }
    let StageSelect v = StageSelectCache.layout |> Map.find v
    let StageSelectInteract v = StageSelectCache.interact |> Map.find v





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
                let font = GameCore.getFont context innertext.font
                let textSize = font.MeasureString(innertext.content)*innertext.scale
                let realpos = UI.getRealPos innertext.pos textSize subScreen
                drawText context.spriteBatch font innertext.content (realpos + offset) (Color.Multiply(innertext.color, colorscale)) innertext.scale
            | UI.Texture innertexture -> 
                let realpos = UI.getRealPos innertexture.pos innertexture.size subScreen
                drawTexture context.spriteBatch (GameCore.getTexture context innertexture.texture) (realpos + offset) innertexture.size (Color.Multiply(innertexture.color, colorscale))
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
        let black = GameCore.getTexture context TextureID.BasePixel
        let color = Color(0.0f, 0.0f, 0.0f, opacity)
        drawTexture context.spriteBatch black offset  (Vector2(1280.0f, 720.0f)) color
    // Draw from ScreenMap
    let screenDraw (context: DrawContext) (screen: UI.ScreenUI) (buttonState: (int * UI.ButtonCurrent) option) (offset: Vector2) =
        screen.subscreens |> List.iter (fun sub -> subScreenDraw context sub offset 1.0f)
        match buttonState with
        | None -> screen.buttons |> List.iter (fun button -> buttonDraw context button UI.Normal offset)
        | Some (buttonId, state) ->
            screen.buttons |> List.iter (fun button -> 
                let state = if button.Id = buttonId then state else UI.Normal
                buttonDraw context button state offset
            )




module Screens = 
    type Transition = {
        target: GameScreen
        transitionType: TransitionType
        currentTime: float32
        duration: float32
    }

    type ScreenState = {
        state: GameScreen
        buttonState: (int * UI.ButtonCurrent) option
        transition: Transition option
    }
    let initialState = {
        state = MainMenu
        buttonState = None
        transition = None
    }
    let tryNextScreenAction (screen: UI.ScreenUI) (interact: UI.ScreenInteract) (input: InputState) = 
        let buttonResult = 
            match InteractUI.getClickedButton screen input with
            | Some buttonId -> Map.tryFind buttonId interact.buttons
            | None -> None
        match buttonResult, input.keyboard.curKey with
        | Some action, _ -> Some action
        | None, Some key when input.keyboard.prevKey <> Some key -> Map.tryFind key interact.keys
        | _, _ -> None

    /// Get next action from input and gamescreen
    let getNextAction (currentScreen: GameScreen) (input: InputState) =
        match currentScreen with
        | MainMenu ->
            let buttonState = InteractUI.getButtonState ScreenMap.MainMenu input
            let action = tryNextScreenAction ScreenMap.MainMenu ScreenMap.MainMenuInteract input
            (buttonState, action)
        | _ -> None, None


    /// Determines the transition type based on the current screen and the next screen
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
    /// Updates the screen state when screen goes to a new screen with a transition
    let updateState (currentState: ScreenState) (nextScreen: GameScreen) (transitionDuration: float32) (transitionType: TransitionType) : ScreenState =
        if transitionDuration <= 0.0f then
            { state = nextScreen; buttonState = None; transition = None }
        else
            let newTransition = {
                target = nextScreen
                transitionType = transitionType
                currentTime = 0.0f
                duration = transitionDuration
            }
            { state = currentState.state; buttonState = None; transition = Some newTransition }
    /// Updates the progress of the transition based on delta time
    let updateTransition (currentState: ScreenState) (deltaTime: float32) =
        match currentState.transition with
        | Some transition ->
            let newTime = transition.currentTime + deltaTime
            if newTime >= transition.duration then { state = transition.target; buttonState = None; transition = None }
            else
                let updatedTransition = { transition with currentTime = newTime }
                { currentState with transition = Some updatedTransition }
        | None -> currentState
    /// Screens.update -> use for game update.
    let update (currentState: ScreenState) (input: InputState) (deltaTime: float32) =
        match currentState.transition with
        | Some _ -> (updateTransition currentState deltaTime, Some UI.Blocked)
        | None ->
            match getNextAction currentState.state input with
            | _, Some(UI.Moveto next) -> 
                match getTransition currentState.state next with
                | Some (transitionType, transitionDuration) -> 
                    (updateState currentState next transitionDuration transitionType, Some UI.Blocked)
                | None -> (currentState, None)
            | buttonState, action ->
                let newScreenState = {currentState with buttonState = buttonState}
                (newScreenState, action)
                



    /// Draws the current screen with offset for transition
    let drawScreen (context: DrawContext) (screen: GameScreen) (buttonState: (int * UI.ButtonCurrent) option) (playState: GameState) (offset: Vector2) =
        match screen with
        | BlackScreen v -> DrawUI.drawBlackScreen context v offset
        | MainMenu -> DrawUI.screenDraw context ScreenMap.MainMenu buttonState offset
        | StageSelect v -> DrawUI.screenDraw context (ScreenMap.StageSelect v) buttonState offset
        | _ -> DrawUI.drawBlackScreen context 1.0f offset

    /// Draws the current screen and the transition effect if there is a transition
    let draw (context: DrawContext) (screenState: ScreenState) (playState: GameState) =
        match screenState.transition with
        | Some transition -> 
            let progress = transition.currentTime / transition.duration
            let distancex = GameCore.virtualScreenSize.X
            let distancey = GameCore.virtualScreenSize.Y
            match transition.transitionType with
            | Slide dir ->
                let offset, oppoOffset = 
                    match dir with
                    | L -> (Vector2(-distancex*progress, 0.0f), Vector2(distancex*(1.0f - progress), 0.0f))
                    | R -> (Vector2(distancex*progress, 0.0f), Vector2(-distancex*(1.0f - progress), 0.0f))
                    | U -> (Vector2(0.0f, -distancey*progress), Vector2(0.0f, distancey*(1.0f - progress)))
                    | D -> (Vector2(0.0f, distancey*progress), Vector2(0.0f, -distancey*(1.0f - progress)))
                drawScreen context screenState.state screenState.buttonState playState offset
                drawScreen context transition.target screenState.buttonState playState oppoOffset
            | Fade -> 
                if progress < 0.5f then
                    drawScreen context screenState.state screenState.buttonState playState Vector2.Zero
                else
                    drawScreen context transition.target screenState.buttonState playState Vector2.Zero
                drawScreen context (BlackScreen (1.0f - 2.0f*abs(progress - 0.5f))) screenState.buttonState playState Vector2.Zero
            | Sudden v ->
                if progress < v then
                    drawScreen context screenState.state screenState.buttonState playState Vector2.Zero
                else
                    drawScreen context transition.target screenState.buttonState playState Vector2.Zero
            | Popup isOpen ->
                if isOpen then
                    drawScreen context screenState.state screenState.buttonState playState Vector2.Zero
                    drawScreen context (BlackScreen progress) screenState.buttonState playState Vector2.Zero
                    drawScreen context transition.target screenState.buttonState playState (Vector2(0.0f, -distancey*progress))
                else
                    drawScreen context transition.target screenState.buttonState playState Vector2.Zero
                    drawScreen context (BlackScreen (1.0f - progress)) screenState.buttonState playState Vector2.Zero
                    drawScreen context screenState.state screenState.buttonState playState (Vector2(0.0f, -distancey*(1.0f - progress)))

        | None ->
            drawScreen context screenState.state screenState.buttonState playState Vector2.Zero

    
    
