namespace TermProj

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

module DrawScreen = 
    // base screen size: 1280x720
    type ScreenPosition = { posx: float32; posy: float32 }
    let titlePos = { posx = 100.0f; posy = 100.0f}
    let mainMenuPromptPos = { posx = 100.0f; posy = 150.0f}
    let drawText (spriteBatch: SpriteBatch) (font: SpriteFont) (text: string) (pos: Vector2) (color: Color) =
        spriteBatch.DrawString(font, text, pos, color)
    
    let drawMainScreen (context: drawContext) (offset:Vector2) =
        drawText context.spriteBatch context.assets.fonts.Default "Play In Progress" (Vector2(titlePos.posx, titlePos.posy) + offset) Color.White
        drawText context.spriteBatch context.assets.fonts.Default "Press Any Key to Start" (Vector2(mainMenuPromptPos.posx, mainMenuPromptPos.posy) + offset) Color.White
    
    let drawStageSelectScreen (context: drawContext) (offset:Vector2) =
        drawText context.spriteBatch context.assets.fonts.Default "Stage Select" (Vector2(titlePos.posx, titlePos.posy) + offset) Color.White

module Screens = 
    type Transition = {
        target: GameScreen
        transitionType: TransitionType
        currenttime: float32
        duration: float32
    }

    type ScreenState = {
        state: GameScreen
        transition: Transition option
    }
    let initialState = {
        state = MainMenu
        transition = None
    }

    let getScreenfromKeyMap (currentState: ScreenState) (key: KeyBind) : GameScreen option =
        match currentState.state with
        | _ -> Some MainMenu
    /// Determines the transition type based on the current screen and the next screen
    let getTransition currentScreen nextScreen = 
        match currentScreen with
        | MainMenu -> Some (Sudden 0.0f, 0.0f)
        | StageSelect a ->
            match nextScreen with
            | StagePlaying -> Some (Fade, 1.0f)
            | StageSelect b when b > a -> Some (Slide L, 0.5f)
            | StageSelect b when b < a -> Some (Slide R, 0.5f)
            | _ -> None
        | StagePlaying ->
            match nextScreen with
            | StageSelect _ -> Some (Fade, 1.0f)
            | _ -> None
        | _ -> Some (Sudden 0.0f, 0.0f)
    /// Updates the screen state when screen goes to a new screen with a transition
    let updateState (currentState: ScreenState) (nextScreen: GameScreen) (transitionDuration: float32) (transitionType: TransitionType) : ScreenState =
        if transitionDuration <= 0.0f then
            { state = nextScreen; transition = None }
        else
            let newTransition = {
                target = nextScreen
                transitionType = transitionType
                currenttime = 0.0f
                duration = transitionDuration
            }
            { state = currentState.state; transition = Some newTransition }
    /// Updates the prograss of the transition based on delta time
    let updateTransition (currentState: ScreenState) (deltaTime: float32) : ScreenState =
        match currentState.transition with
        | Some transition ->
            let newTime = transition.currenttime + deltaTime
            if newTime >= transition.duration then { state = transition.target; transition = None }
            else
                let updatedTransition = { transition with currenttime = newTime }
                { currentState with transition = Some updatedTransition }
        | None -> currentState
    /// Screens.update -> use for game update.
    let update (currentState: ScreenState) (nextScreen: GameScreen option) (deltaTime: float32) : ScreenState =
        match currentState.transition, nextScreen with
        | Some _, _ | _, None -> updateTransition currentState deltaTime
        | None, Some next ->
            if next = currentState.state then currentState
            else
                match getTransition currentState.state next with
                | Some (transitionType, transitionDuration) -> updateState currentState next transitionDuration transitionType
                | None -> currentState



    /// Draws the current screen with offset for transition
    let drawScreen (context: drawContext) (screen: GameScreen) (playState: GameState option) (offset: Vector2) =
        match screen with
        | MainMenu -> DrawScreen.drawMainScreen context offset
        | StageSelect _ -> DrawScreen.drawStageSelectScreen context offset
        | _ -> ()

    /// Draws the current screen and the transition effect if there is a transition
    let draw (context: drawContext) (screenState: ScreenState) (playState: GameState Option) =
        match screenState.transition with
        | Some transition -> 
            let progress = transition.currenttime / transition.duration
            let distancex = basic.virtualScreenSize.X
            let distancey = basic.virtualScreenSize.Y
            match transition.transitionType with
            | Slide dir ->
                let offset, oppoOffset = 
                    match dir with
                    | L -> (Vector2(-distancex*progress, 0.0f), Vector2(distancex*(1.0f - progress), 0.0f))
                    | R -> (Vector2(distancex*progress, 0.0f), Vector2(-distancex*(1.0f - progress), 0.0f))
                    | U -> (Vector2(0.0f, -distancey*progress), Vector2(0.0f, distancey*(1.0f - progress)))
                    | D -> (Vector2(0.0f, distancey*progress), Vector2(0.0f, -distancey*(1.0f - progress)))
                drawScreen context screenState.state playState offset
                drawScreen context transition.target playState oppoOffset
            | Fade -> 
                if progress < 0.5f then
                    drawScreen context screenState.state playState Vector2.Zero
                else
                    drawScreen context transition.target playState Vector2.Zero
            | Sudden v ->
                if progress < v then
                    drawScreen context screenState.state playState Vector2.Zero
                else
                    drawScreen context transition.target playState Vector2.Zero
            | Popup isOpen ->
                drawScreen context screenState.state playState Vector2.Zero
                if isOpen then drawScreen context transition.target playState (Vector2(0.0f, -distancey*progress))
                else drawScreen context transition.target playState (Vector2(0.0f, -distancey*(1.0f - progress)))

        | None ->
            drawScreen context screenState.state playState Vector2.Zero


    
    
