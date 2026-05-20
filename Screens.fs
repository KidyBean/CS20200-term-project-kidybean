namespace TermProj

open Microsoft.Xna.Framework

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
        | Some _ -> updateTransition currentState deltaTime, (Some UI.Blocked, [])
        | None ->
            match ScreenMap.getNextAction currentState.state state input with
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
                ScreenMap.drawScreen context screenState.state screenState.buttonState playState offset
                ScreenMap.drawScreen context transition.target screenState.buttonState playState oppoOffset
            | Fade ratio -> 
                if progress <= ratio then
                    ScreenMap.drawScreen context screenState.state screenState.buttonState playState Vector2.Zero
                    ScreenMap.drawScreen context (BlackScreen (1.0f - (ratio - progress)/ratio)) screenState.buttonState playState Vector2.Zero
                else
                    ScreenMap.drawScreen context transition.target screenState.buttonState playState Vector2.Zero
                    ScreenMap.drawScreen context (BlackScreen (1.0f - (progress - ratio)/(1.0f - ratio))) screenState.buttonState playState Vector2.Zero
                
            | Sudden v ->
                if progress < v then
                    ScreenMap.drawScreen context screenState.state screenState.buttonState playState Vector2.Zero
                else
                    ScreenMap.drawScreen context transition.target screenState.buttonState playState Vector2.Zero
            | Popup isOpen ->
                if isOpen then
                    ScreenMap.drawScreen context screenState.state screenState.buttonState playState Vector2.Zero
                    ScreenMap.drawScreen context (BlackScreen progress) screenState.buttonState playState Vector2.Zero
                    ScreenMap.drawScreen context transition.target screenState.buttonState playState (Vector2(0.0f, distancey*(1.0f - progress)))
                else
                    ScreenMap.drawScreen context transition.target screenState.buttonState playState Vector2.Zero
                    ScreenMap.drawScreen context (BlackScreen (1.0f - progress)) screenState.buttonState playState Vector2.Zero
                    ScreenMap.drawScreen context screenState.state screenState.buttonState playState (Vector2(0.0f, distancey*progress))

        | None ->
            ScreenMap.drawScreen context screenState.state screenState.buttonState playState Vector2.Zero

    
    
