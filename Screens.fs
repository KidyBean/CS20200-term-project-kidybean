namespace TermProj

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

/// On Screen.fs
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



/// On Screen.fs
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

    /// Get next action from input and gamescreen
    let getNextAction (currentScreen: GameScreen) (state: GameState) (input: InputState) =
        match currentScreen with
        | MainMenu ->
            let buttonState = InteractUI.getButtonState ScreenMap.MainMenu input
            let action = InteractUI.tryNextScreenAction ScreenMap.MainMenu (ScreenMap.MainMenuInteract state) input
            (buttonState, action)
        | _ -> None, (None, NoStateChange)
    
    /// Draws the current screen with offset for transition
    let drawScreen (context: DrawContext) (screen: GameScreen) (buttonState: (int * UI.ButtonCurrent) option) (playState: GameState) (offset: Vector2) =
        match screen with
        | BlackScreen v -> DrawUI.drawBlackScreen context v offset
        | MainMenu -> DrawUI.screenDraw context ScreenMap.MainMenu buttonState offset
        | StageSelect v -> DrawUI.screenDraw context (ScreenMap.StageSelect v) buttonState offset
        | _ -> DrawUI.drawBlackScreen context 1.0f offset






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
    let update (currentState: ScreenState) (state: GameState) (input: InputState) (deltaTime: float32) =
        match currentState.transition with
        | Some _ -> updateTransition currentState deltaTime, (Some UI.Blocked, NoStateChange)
        | None ->
            match getNextAction currentState.state state input with
            | _, (Some(UI.Moveto (next, transition)), statechange) -> 
                updateState currentState next transition.duration transition.transitionType, (Some UI.Blocked, statechange)
            | buttonState, action ->
                let newScreenState = { currentState with buttonState = buttonState }
                newScreenState, action
                




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

    
    
