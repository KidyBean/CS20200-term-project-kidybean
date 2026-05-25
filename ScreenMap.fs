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
        | (None, _), Some key when input.keyboard.prevKey <> Some key -> interact.keys key
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
    
    let stageDraw (context: DrawContext) (state: GameState) (offset: Vector2) = GameState.drawStage context state offset





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
    let DefaultButtons (state: GameState) = function
        | _ -> None, []
    let DefaultKeys (state: GameState) = function
        | _ -> None, []
    let DefaultHandler (state: GameState) () = None, []
    let DefaultInteract (state: GameState): UI.ScreenInteract = {
        buttons = DefaultButtons state
        keys = DefaultKeys state
        handler = DefaultHandler state
    }
    


    ///
    /// #   #   ###   #####  #   #     #   #  #####  #   #  #   #
    /// ## ##  #   #    #    ##  #     ## ##  #      ##  #  #   #
    /// # # #  #####    #    # # #     # # #  ####   # # #  #   #
    /// #   #  #   #    #    #  ##     #   #  #      #  ##  #   #
    /// #   #  #   #  #####  #   #     #   #  #####  #   #   ### 
    /// Main Menu Screen ---------------------------------------------------------------------
    let MainMenu: UI.ScreenUI = 
        // title
        let titleText: UI.InnerText = { 
            font = TitleFont 
            content = Dialogue.Title
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
            content = Dialogue.startPrompt
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
        | 0 -> Some (UI.Moveto (GameScreen.StageSelect (GameState.getPresentStage state), { transitionType = Fade 0.4f; duration = 0.8f })), []
        | _ -> None, []
    let MainMenuKey (state: GameState) = function
        | Confirm -> Some (UI.Moveto (GameScreen.StageSelect (GameState.getPresentStage state), { transitionType = Fade 0.4f; duration = 0.8f })), []
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






    ///
    /// #####  #####  #      #####  #####  #####
    /// #      #      #      #      #        #  
    /// #####  ####   #      ####   #        #  
    ///     #  #      #      #      #        #  
    /// #####  #####  #####  #####  #####    #  
    /// Stage Select Screen ----------------------------------------------------------------
    let StageSelectBase (v: int) : UI.ScreenUI = 
        let patchVerText: UI.InnerText = {
            font = TitleFont 
            content = Dialogue.patchVer v
            color = Color.White
            scale = 2.0f
            pos = UI.AlignPos (UI.CenterX, UI.Top)
        }
        let DescText: UI.InnerText = {
            font = DefaultFont 
            content = Dialogue.getPromptVer v
            color = Color.White
            scale = 1.1f
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let promptText: UI.InnerText = {
            font = DefaultFont 
            content = Dialogue.startPrompt
            color = Color.White
            scale = 1.4f
            pos = UI.AlignPos (UI.CenterX, UI.Bottom)
        }
        let levelScreenOuter: UI.InnerTexture = {
            texture = BasePixel
            color = Color.Navy
            size = Vector2(560.0f, 656.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let levelScreenInner: UI.InnerTexture = {
            texture = BasePixel
            color = Color.Black
            size = Vector2(544.0f, 640.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let levelScreen: UI.SubScreen = {
            inner = [
                UI.Texture levelScreenOuter
                UI.Texture levelScreenInner
                UI.Text patchVerText
                UI.Text DescText
                UI.Text promptText
            ]
            pos = Vector2(360.0f, 32.0f)
            size = Vector2(560.0f, 656.0f)
        }
        let levelButton: UI.ButtonInfo = {
            Id = 2
            normalLayout = levelScreen
            hoveredLayout = None
            pressedLayout = None
        }

        

        let MoveButtonOuter: UI.InnerTexture = {
            texture = BasePixel
            color = Color.Navy
            size = Vector2(80.0f, 112.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let MoveButtonInner: UI.InnerTexture = {
            texture = BasePixel
            color = Color.Black
            size = Vector2(64.0f, 96.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }

        let lButtonTexture: UI.InnerTexture = {
            texture = ArrowL
            color = Color.White
            size = Vector2(64.0f, 96.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let leftButtonScreen: UI.SubScreen = {
            inner = [UI.Texture MoveButtonOuter; UI.Texture MoveButtonInner; UI.Texture lButtonTexture]
            pos = Vector2(240.0f, 320.0f)
            size = Vector2(80.0f, 112.0f)
        }
        let leftButton: UI.ButtonInfo = {
            Id = 0
            normalLayout = leftButtonScreen
            hoveredLayout = None
            pressedLayout = None
        }




        let rButtonTexture: UI.InnerTexture = {
            texture = ArrowR
            color = Color.White
            size = Vector2(64.0f, 96.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let rightButtonScreen: UI.SubScreen = {
            inner = [UI.Texture MoveButtonOuter; UI.Texture MoveButtonInner; UI.Texture rButtonTexture]
            pos = Vector2(960.0f, 320.0f)
            size = Vector2(80.0f, 112.0f)
        }
        let rightButton: UI.ButtonInfo = {
            Id = 1
            normalLayout = rightButtonScreen
            hoveredLayout = None
            pressedLayout = None
        }
        

        let subscreens = []
        let buttons = if v = 0 then [rightButton] else [leftButton; rightButton]
        { buttons = levelButton :: buttons; subscreens = subscreens }


    let _goPrev (state: GameState) = 
        let presentStage = GameState.getPresentStage state
        if presentStage = 0 then None, []
        else
            let nextStage = presentStage - 1
            match GameState.setSelectedStage state nextStage with
            | [] -> Some (UI.Moveto(GameScreen.StageBlockPopup, { transitionType = Popup true; duration = 0.3f })), []
            | v -> Some (UI.Moveto (GameScreen.StageSelect nextStage, { transitionType = Slide R; duration = 0.8f })), v
    let _goNext (state: GameState) = 
        let presentStage = GameState.getPresentStage state
        let nextStage = presentStage + 1
        match GameState.setSelectedStage state nextStage with
        | [] -> Some (UI.Moveto(GameScreen.StageBlockPopup, { transitionType = Popup true; duration = 0.3f })), []
        | v -> Some (UI.Moveto (GameScreen.StageSelect nextStage, { transitionType = Slide L; duration = 0.8f })), v
    let StageSelectButton (state: GameState) = function
        | 0 -> _goPrev state
        | 1 -> _goNext state
        | 2 -> Some (UI.Moveto (GameScreen.StageLoader, { transitionType = Fade 1.0f; duration = 0.4f })), []
        | 3 -> Some (UI.Moveto (GameScreen.PatchNote, { transitionType = Popup true; duration = 0.3f })), []
        | _ -> None, []
    let StageSelectKey (state: GameState) = function
        | Escape -> Some (UI.Moveto (GameScreen.MainMenu, { transitionType = Fade 0.5f; duration = 0.8f })), []
        | Move R -> _goNext state
        | Move L -> _goPrev state
        | Confirm -> Some (UI.Moveto (GameScreen.StageLoader, { transitionType = Fade 1.0f; duration = 0.4f })), []
        | _ -> None, []
    let StageSelectHandler (state: GameState) () = 
        if GameState.isStagePatched (GameState.getPresentStage state) state then None, []
        else
            let action = UI.Moveto (GameScreen.PatchNote, { transitionType = Sudden 1.0f; duration = 0.0f })
            Some action, []
            

    let StageSelectCache = List.map (fun v -> v, StageSelectBase v) [0..GameCore.gameStage] |> Map.ofList

    let StageSelectScreen (state: GameState) (v: int): UI.ScreenUI = StageSelectCache |> Map.find v
    let StageSelectInteract (state: GameState): UI.ScreenInteract = {
        buttons = StageSelectButton state
        keys = StageSelectKey state
        handler = StageSelectHandler state
    }

    let comebackToStageSelect (state: GameState) = UI.Moveto (GameScreen.StageSelect (GameState.getPresentStage state), { transitionType = Popup false; duration = 0.3f })






    ///
    /// ####   ###   #####  #####  #   #
    /// #   # #   #    #    #      #   #
    /// ####  #####    #    #      #####
    /// #     #   #    #    #      #   #
    /// #     #   #    #    #####  #   #
    /// PatchNote ----------------------------------------------------------------
    let PatchNoteBase: UI.ScreenUI = 
        let PopupOuter: UI.InnerTexture = {
            texture = BasePixel
            color = Color.Navy
            size = Vector2(1024.0f, 592.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let PopupInner: UI.InnerTexture = {
            texture = BasePixel
            color = Color.Black
            size = Vector2(1008.0f, 576.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        
        let PopupScreen: UI.SubScreen = {
            inner = [UI.Texture PopupOuter; UI.Texture PopupInner]
            pos = Vector2(128.0f, 64.0f)
            size = Vector2(1024.0f, 592.0f)
        }



        let buttonOuter: UI.InnerTexture = {
            texture = BasePixel
            color = Color.Navy
            size = Vector2(56.0f, 56.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let buttonInner: UI.InnerTexture = {
            texture = BasePixel
            color = Color.Black
            size = Vector2(40.0f, 40.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let buttonTexture: UI.InnerTexture = {
            texture = X
            color = Color.White
            size = Vector2(40.0f, 40.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let button: UI.SubScreen = {
            inner = [UI.Texture buttonOuter; UI.Texture buttonInner; UI.Texture buttonTexture]
            pos = Vector2(1072.0f, 88.0f)
            size = Vector2(56.0f, 56.0f)
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
    let PatchNoteButton (state: GameState) = function
        | 0 -> 
            let action = comebackToStageSelect state
            let change = GameState.addPatchOnStage state
            Some action, change
        | _ -> None, []
    let PatchNoteKey (state: GameState) = function
        | Escape ->
            let action = comebackToStageSelect state
            let change = GameState.addPatchOnStage state
            Some action, change
        | _ -> None, []
    let PatchNoteHandler (state: GameState) () =
        let change = GameState.setLastUpdate state
        if List.isEmpty change then None, []
        else Some UI.Blocked, change
    
    let PatchNoteScreen (state: GameState): UI.ScreenUI = 
        let popUp = List.head PatchNoteBase.subscreens
        let stagenum = GameState.getPresentStage state
        let recent = Stage.recentUpdate stagenum
        let patch = GameState.getPatch stagenum state

        let upperRatio = (0.2f, 0.27f)
        let lowerRatio = (0.57f, 0.64f)
        let horizontal = 0.1f

        let PatchVer: UI.SubScreenInner = UI.Text {
            font = TitleFont 
            content = Dialogue.patchVer stagenum
            color = Color.White
            scale = 1.5f
            pos = UI.CustomRatioPos (Vector2(horizontal*0.3f, 0.06f))
        }

        let popUpAdd = 
            match recent with
            | Some func -> 
                let newFuncTitle: UI.SubScreenInner = UI.Text {
                    font = DefaultFont 
                    content = Dialogue.newUpdate
                    color = Color.White
                    scale = 0.75f
                    pos = UI.CustomRatioPos (Vector2(horizontal*0.5f, fst upperRatio))
                }
                let newPatchTitle: UI.SubScreenInner = UI.Text {
                    font = DefaultFont 
                    content = Dialogue.bugFix
                    color = Color.White
                    scale = 0.75f
                    pos = UI.CustomRatioPos (Vector2(horizontal*0.5f, fst lowerRatio))
                }


                let newFuncPrompt: UI.SubScreenInner = UI.Text {
                    font = DefaultFont 
                    content = Dialogue.updatePrompt func
                    color = Color.White
                    scale = 0.666f
                    pos = UI.CustomRatioPos (Vector2(horizontal*0.7f, snd upperRatio))
                }
                let newPatchPrompt: UI.SubScreenInner = UI.Text {
                    font = DefaultFont 
                    content = Dialogue.patchPrompt patch
                    color = Color.White
                    scale = 0.666f
                    pos = UI.CustomRatioPos (Vector2(horizontal*0.7f, snd lowerRatio))
                }
                [PatchVer; newFuncTitle; newPatchTitle; newFuncPrompt; newPatchPrompt]
            | None ->
                let newPatchTitle: UI.SubScreenInner = UI.Text {
                    font = DefaultFont 
                    content = Dialogue.bugFix
                    color = Color.White
                    scale = 0.75f
                    pos = UI.CustomRatioPos (Vector2(horizontal*0.5f, fst upperRatio))
                }
                let newPatchPrompt: UI.SubScreenInner = UI.Text {
                    font = DefaultFont 
                    content = Dialogue.patchPrompt patch
                    color = Color.White
                    scale = 0.666f
                    pos = UI.CustomRatioPos (Vector2(horizontal*0.7f, snd upperRatio))
                }
                [PatchVer; newPatchTitle; newPatchPrompt]
        { PatchNoteBase with subscreens = [{popUp with inner = popUp.inner @ popUpAdd}] }
    /// Main Menu Interaction
    let PatchNoteInteract (state: GameState): UI.ScreenInteract = {
        buttons = PatchNoteButton state
        keys = PatchNoteKey state
        handler = PatchNoteHandler state
    }







    ///
    /// ####   #       ###   #####  #   #
    /// #   #  #      #   #  #      #  # 
    /// ####   #      #   #  #      ###  
    /// #   #  #      #   #  #      #  # 
    /// ####   #####   ###   #####  #   #
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
            font = TitleFont
            content = Dialogue.warning
            color = Color.White
            scale = 1.5f
            pos = UI.AlignPos (UI.Left, UI.Top)
        }
        let promptText: UI.InnerText = { 
            font = DefaultFont 
            content = Dialogue.blockPrompt
            color = Color.White
            scale = 1.0f
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let PopupScreen: UI.SubScreen = {
            inner = [UI.Texture PopupOuter; UI.Texture PopupInner; UI.Text warning; UI.Text promptText]
            pos = Vector2(240.0f, 96.0f)
            size = Vector2(800.0f, 528.0f)
        }



        let buttonOuter: UI.InnerTexture = {
            texture = BasePixel
            color = Color.Navy
            size = Vector2(56.0f, 56.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let buttonInner: UI.InnerTexture = {
            texture = BasePixel
            color = Color.Black
            size = Vector2(40.0f, 40.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let buttonTexture: UI.InnerTexture = {
            texture = X
            color = Color.White
            size = Vector2(40.0f, 40.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let button: UI.SubScreen = {
            inner = [UI.Texture buttonOuter; UI.Texture buttonInner; UI.Texture buttonTexture]
            pos = Vector2(960.0f, 120.0f)
            size = Vector2(56.0f, 56.0f)
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
        | 0 -> Some (comebackToStageSelect state), []
        | _ -> None, []
    let StageBlockPopupKey (state: GameState) = function
        | Escape -> Some (comebackToStageSelect state), []
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







    ///
    /// #       ###    ###   ####   #####  #### 
    /// #      #   #  #   #  #   #  #      #   #
    /// #      #   #  #####  #   #  ####   #### 
    /// #      #   #  #   #  #   #  #      #  # 
    /// #####   ###   #   #  ####   #####  #   #
    /// Stage Loader ----------------------------------------------------------------
    let StageLoaderHandler (state: GameState) () = 
        let stagenum = GameState.getPresentStage state
        let change = GameState.loadStage state stagenum
        if List.isEmpty change then
            Some (UI.Moveto(GameScreen.StageBlockPopup, { transitionType = Popup true; duration = 0.3f })), []
        else
            let action = UI.Moveto (GameScreen.StagePlaying, { transitionType = Fade 0.0f; duration = 0.4f })
            Some action, change
    let StageLoaderScreen (state: GameState): UI.ScreenUI = DefaultScreen state
    let StageLoaderInteract (state: GameState): UI.ScreenInteract = {
        buttons = DefaultButtons state
        keys = DefaultKeys state
        handler = StageLoaderHandler state
    }







    ///
    /// #####  #####   ###    ###   #####      ####   #       ###   #   #
    /// #        #    #   #  #      #          #   #  #      #   #   # # 
    /// #####    #    #####  # ###  ####       ####   #      #####    #  
    ///     #    #    #   #  #   #  #          #      #      #   #    #  
    /// #####    #    #   #   ###   #####      #      #####  #   #    #  
    /// Stage Playing ----------------------------------------------------------------
    let StagePlayingBase: UI.ScreenUI = 
        let buttonOuter: UI.InnerTexture = {
            texture = BasePixel
            color = Color.Navy
            size = Vector2(56.0f, 56.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let buttonInner: UI.InnerTexture = {
            texture = BasePixel
            color = Color.Black
            size = Vector2(40.0f, 40.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }

        let overButtonTexture: UI.InnerTexture = {
            texture = Pause
            color = Color.White
            size = Vector2(40.0f, 40.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let OverButton: UI.SubScreen = {
            inner = [UI.Texture buttonOuter; UI.Texture buttonInner; UI.Texture overButtonTexture]
            pos = Vector2(1200.0f, 24.0f)
            size = Vector2(56.0f, 56.0f)
        }
        
        let underButtonTexture: UI.InnerTexture = {
            texture = Tuto
            color = Color.White
            size = Vector2(40.0f, 40.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let UnderButton: UI.SubScreen = {
            inner = [UI.Texture buttonOuter; UI.Texture buttonInner; UI.Texture underButtonTexture]
            pos = Vector2(1200.0f, 104.0f)
            size = Vector2(56.0f, 56.0f)
        }
        let lButtonTexture: UI.InnerTexture = {
            texture = ArrowL
            color = Color.White
            size = Vector2(64.0f, 96.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let tutorialButton: UI.ButtonInfo = {
            Id = 0
            normalLayout = UnderButton
            hoveredLayout = None
            pressedLayout = None
            
        }
        let pauseButton: UI.ButtonInfo = {
            Id = 1
            normalLayout = OverButton
            hoveredLayout = None
            pressedLayout = None
        }
        let subscreens = []
        let buttons = [tutorialButton; pauseButton]
        { buttons = buttons; subscreens = subscreens }
    let StagePlayingButton (state: GameState) = function
        | 0 ->
            let action = UI.Moveto (GameScreen.Tutorial, { transitionType = Popup true; duration = 0.3f })
            Some action, []
        | 1 -> 
            let action = UI.Moveto (GameScreen.PauseMenu, { transitionType = Popup true; duration = 0.3f })
            Some action, []
        | _ -> None, []
    let StagePlayingKey (state: GameState) = function
        | Escape -> 
            let action = UI.Moveto (GameScreen.PauseMenu, { transitionType = Popup true; duration = 0.3f })
            Some action, []
        | v -> Some (UI.StageAction v), []
    let StagePlayingHandler (state: GameState) () =
        if GameState.needTutorial (GameState.getPresentStage state) state then
            let action = UI.Moveto (GameScreen.Tutorial, { transitionType = Sudden 1.0f; duration = 0.0f })
            Some action, []
        else 
            match GameState.stageEndState state with
            | StageVictory -> 
                let action = UI.Moveto (GameScreen.VictoryScreen, { transitionType = Sudden 1.0f; duration = 0.5f })
                Some action, []
            | StageCrashed _ -> 
                let action = UI.Moveto (GameScreen.BrokenScreen, { transitionType = Sudden 1.0f; duration = 0.5f })
                Some action, []
            | _ -> None, []
    
    let StagePlayingScreen (state: GameState): UI.ScreenUI = StagePlayingBase
    /// Main Menu Interaction
    let StagePlayingInteract (state: GameState): UI.ScreenInteract = {
        buttons = StagePlayingButton state
        keys = StagePlayingKey state
        handler = StagePlayingHandler state
    }
    let comebackToStagePlay = UI.Moveto (GameScreen.StagePlaying, { transitionType = Popup false; duration = 0.3f })






    ///
    /// #####  #   #  #####   ###   ####   #####   ###   #    
    ///   #    #   #    #    #   #  #   #    #    #   #  #    
    ///   #    #   #    #    #   #  ####     #    #####  #    
    ///   #    #   #    #    #   #  #  #     #    #   #  #    
    ///   #     ###     #     ###   #   #  #####  #   #  #####
    /// Tutorial Popup ----------------------------------------------------------------
    let TutorialBase: UI.ScreenUI = 
        let PopupOuter: UI.InnerTexture = {
            texture = BasePixel
            color = Color.Navy
            size = Vector2(1024.0f, 592.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let PopupInner: UI.InnerTexture = {
            texture = BasePixel
            color = Color.Black
            size = Vector2(1008.0f, 576.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let PopupScreen: UI.SubScreen = {
            inner = [UI.Texture PopupOuter; UI.Texture PopupInner]
            pos = Vector2(128.0f, 64.0f)
            size = Vector2(1024.0f, 592.0f)
        }



        let buttonOuter: UI.InnerTexture = {
            texture = BasePixel
            color = Color.Navy
            size = Vector2(56.0f, 56.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let buttonInner: UI.InnerTexture = {
            texture = BasePixel
            color = Color.Black
            size = Vector2(40.0f, 40.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let buttonTexture: UI.InnerTexture = {
            texture = X
            color = Color.White
            size = Vector2(40.0f, 40.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let button: UI.SubScreen = {
            inner = [UI.Texture buttonOuter; UI.Texture buttonInner; UI.Texture buttonTexture]
            pos = Vector2(1072.0f, 88.0f)
            size = Vector2(56.0f, 56.0f)
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
    let TutorialButton (state: GameState) = function
        | 0 ->
            let action = comebackToStagePlay
            let change = GameState.tutorialPlayed (GameState.getPresentStage state)
            Some action, change
        | _ -> None, []
    let TutorialKey (state: GameState) = function
        | Escape ->
            let action = comebackToStagePlay
            let change = GameState.tutorialPlayed (GameState.getPresentStage state)
            Some action, change
        | _ -> None, []
    let TutorialHandler (state: GameState) () =
        None, []
    let TutorialScreen (state: GameState) = 
        let popUp = List.head TutorialBase.subscreens
        let stagenum = GameState.getPresentStage state
        let horizontal = 0.1f
        let upperRatio = (0.06f, 0.2f)

        let title = UI.Text {
            font = TitleFont 
            content = Dialogue.tutorial
            color = Color.White
            scale = 1.5f
            pos = UI.CustomRatioPos (Vector2(horizontal*0.3f, fst upperRatio))
        }
        let prompt = UI.Text {
            font = DefaultFont 
            content = Dialogue.tutorialPrompt (Stage.lastUpdate (GameState.getPresentStage state))
            color = Color.White
            scale = 0.666f
            pos = UI.CustomRatioPos (Vector2(horizontal*0.7f, snd upperRatio))
        }
        let popUpAdd = [title; prompt]
        { TutorialBase with subscreens = [{popUp with inner = popUp.inner @ popUpAdd}] }

    let TutorialInteraction (state: GameState): UI.ScreenInteract = {
        buttons = TutorialButton state
        keys = TutorialKey state
        handler = TutorialHandler state
    }






    ///
    /// ####    ###   #   #  #####  #####
    /// #   #  #   #  #   #  #      #    
    /// ####   #####  #   #  #####  #### 
    /// #      #   #  #   #      #  #    
    /// #      #   #   ###   #####  #####
    /// Pause Popup ----------------------------------------------------------------
    let PauseBase: UI.ScreenUI = 
        let PopupOuter = UI.Texture {
            texture = BasePixel
            color = Color.Navy
            size = Vector2(512.0f, 592.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let PopupInner = UI.Texture {
            texture = BasePixel
            color = Color.Black
            size = Vector2(496.0f, 576.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let prompt = UI.Text {
            font = TitleFont 
            content = Dialogue.pause
            color = Color.White
            scale = 2f
            pos = UI.AlignPos (UI.CenterX, UI.Top)
        }
        let PopupScreen: UI.SubScreen = {
            inner = [PopupOuter; PopupInner; prompt]
            pos = Vector2(384.0f, 64.0f)
            size = Vector2(512.0f, 592.0f)
        }



        let buttonOuter = UI.Texture {
            texture = BasePixel
            color = Color.Navy
            size = Vector2(432.0f, 80.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }
        let buttonInner = UI.Texture {
            texture = BasePixel
            color = Color.Black
            size = Vector2(416.0f, 64.0f)
            pos = UI.AlignPos (UI.CenterX, UI.CenterY)
        }



        let promptResume = UI.Text {
            font = DefaultFont 
            content = Dialogue.resume
            color = Color.White
            scale = 1.5f
            pos = UI.AlignPos (UI.CenterX, UI.Top)
        }
        let screenResume: UI.SubScreen = {
            inner = [buttonOuter; buttonInner; promptResume]
            pos = Vector2(424.0f, 304.0f)
            size = Vector2(432.0f, 72.0f)
        }
        let buttonResume: UI.ButtonInfo = {
            Id = 0
            normalLayout = screenResume
            hoveredLayout = None
            pressedLayout = None
        }
        let promptRestart = UI.Text {
            font = DefaultFont 
            content = Dialogue.restart
            color = Color.White
            scale = 1.5f
            pos = UI.AlignPos (UI.CenterX, UI.Top)
        }
        let screenRestart: UI.SubScreen = {
            inner = [buttonOuter; buttonInner; promptRestart]
            pos = Vector2(424.0f, 408.0f)
            size = Vector2(432.0f, 72.0f)
        }
        let buttonRestart: UI.ButtonInfo = {
            Id = 1
            normalLayout = screenRestart
            hoveredLayout = None
            pressedLayout = None
        }
        let promptExit = UI.Text {
            font = DefaultFont 
            content = Dialogue.exit
            color = Color.White
            scale = 1.5f
            pos = UI.AlignPos (UI.CenterX, UI.Top)
        }
        let screenExit: UI.SubScreen = {
            inner = [buttonOuter; buttonInner; promptExit]
            pos = Vector2(424.0f, 512.0f)
            size = Vector2(432.0f, 72.0f)
        }
        let buttonExit: UI.ButtonInfo = {
            Id = 2
            normalLayout = screenExit
            hoveredLayout = None
            pressedLayout = None
        }

        let subscreens = [PopupScreen]
        let buttons = [buttonResume; buttonRestart; buttonExit]
        { buttons = buttons; subscreens = subscreens }
    let PauseButton (state: GameState) = function
        | 0 -> Some comebackToStagePlay, []
        | 1 -> Some (UI.Moveto (GameScreen.StageLoader, { transitionType = Fade 1.0f; duration = 0.4f })), GameState.ExitStage ()
        | 2 -> Some (UI.Moveto (GameScreen.StageSelect (GameState.getPresentStage state), { transitionType = Fade 0.5f; duration = 0.6f })), GameState.ExitStage ()
        | _ -> None, []
    let PauseKey (state: GameState) = function
        | Escape -> Some comebackToStagePlay, []
        | _ -> None, []
    let PauseHandler (state: GameState) () =
        None, []
    
    let PauseScreen (state: GameState): UI.ScreenUI = PauseBase
    let PauseInteract (state: GameState): UI.ScreenInteract = {
        buttons = PauseButton state
        keys = PauseKey state
        handler = PauseHandler state
    }






    ///
    /// #   #  #####  #####  #####   ###   ####   #   #
    /// #   #    #    #        #    #   #  #   #   # # 
    /// #   #    #    #        #    #   #  ####     #  
    ///  # #     #    #        #    #   #  #  #     #  
    ///   #    #####  #####    #     ###   #   #    #  
    /// Victory Screen ----------------------------------------------------------------
    let victoryBase: UI.ScreenUI = 
        // title
        let titleText: UI.InnerText = { 
            font = TitleFont 
            content = Dialogue.victory
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
            content = Dialogue.gonextResult
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
    

    let gotoStageResultButton (state: GameState) = function
        | 0 -> Some (UI.Moveto (GameScreen.StageResult, { transitionType = Fade 0.4f; duration = 0.8f })), []
        | _ -> None, []
    let gotoStageResultKey (state: GameState) = function
        | Confirm -> Some (UI.Moveto (GameScreen.StageResult, { transitionType = Fade 0.4f; duration = 0.8f })), []
        | _ -> None, []
    
    let VictoryScreen (state: GameState): UI.ScreenUI = victoryBase
    /// Main Menu Interaction
    let VictoryInteract (state: GameState): UI.ScreenInteract = {
            buttons = gotoStageResultButton state
            keys = gotoStageResultKey state
            handler = DefaultHandler state
    }






    ///
    /// #####  ####    ###   #####  #   #  #####  #### 
    /// #      #   #  #   #  #      #   #  #      #   #
    /// #      ####   #####  #####  #####  ####   #   #
    /// #      #  #   #   #      #  #   #  #      #   #
    /// #####  #   #  #   #  #####  #   #  #####  #### 
    /// Crashed Screen ----------------------------------------------------------------
    let CrashedBase: UI.ScreenUI = 
        // title
        let titleText: UI.InnerText = { 
            font = TitleFont 
            content = Dialogue.crashed
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
            content = Dialogue.gonextResult
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
    
    let CrashedScreen (state: GameState): UI.ScreenUI = CrashedBase
    /// Main Menu Interaction
    let CrashedInteract (state: GameState): UI.ScreenInteract = {
            buttons = gotoStageResultButton state
            keys = gotoStageResultKey state
            handler = DefaultHandler state
    }






    ///
    /// ####   #####  #####  #   #  #      #####
    /// #   #  #      #      #   #  #        #  
    /// ####   ####   #####  #   #  #        #  
    /// #  #   #          #  #   #  #        #  
    /// #   #  #####  #####   ###   #####    #  
    /// Result Screen ----------------------------------------------------------------
    let ResultBase: UI.ScreenUI = 
        let title: UI.SubScreenInner = UI.Text {
            content = Dialogue.result
            color = Color.White
            font = DefaultFont
            scale = 1.5f
            pos = UI.CustomRatioPos (Vector2(0.1f, 0.05f))
        }
        let mainScreen = subScreenOfMainSize [title]
        let button: UI.ButtonInfo = {
            Id = 0
            normalLayout = subScreenOfMainSize []
            hoveredLayout = None
            pressedLayout = None
        }
        let subscreens = [mainScreen]
        let buttons = [button]
        { buttons = buttons; subscreens = subscreens }
    let ResultButton (state: GameState) = function
        | 0 -> 
            let action = UI.Moveto (GameScreen.StageSelect (GameState.getPresentStage state + 1), { transitionType = Fade 1.0f; duration = 0.3f })
            let change = GameState.ExitStage () @ GameState.setSelectedStage state (GameState.getPresentStage state + 1)
            Some action, change
        | _ -> None, []
    let ResultKey (state: GameState) = function
        | Confirm -> 
            let action = UI.Moveto (GameScreen.StageSelect (GameState.getPresentStage state + 1), { transitionType = Fade 1.0f; duration = 0.3f })
            let change = GameState.ExitStage () @ GameState.setSelectedStage state (GameState.getPresentStage state + 1)
            Some action, change
        | _ -> None, []
    let ResultHandler (state: GameState) () =
        match GameState.StageResult state with
        | Some result ->
            let vict = GameState.howMapVictory (GameState.getPresentStage state) state
            let update = GameState.addStageFlag (GameState.getPresentStage state) result.victoryType state
            match vict, update with
            | _, [] -> None, []
            | [Defeat], change -> 
                let patchAction = GameState.getNextPatch result.usedBug result.crashedWith
                Some UI.Blocked, change @ patchAction
            | _, change -> Some UI.Blocked, change
        | None -> None, []

    let ResultScreen (state: GameState) = 
        if Option.isNone state.inStage then
            ResultBase
        else
            let screen = List.head ResultBase.subscreens
            let screenAdd =
                match GameState.StageResult state with
                | Some result ->
                    let vict = UI.Text {
                        content = Dialogue.victoryType result.victoryType
                        color = Color.White
                        font = TitleFont 
                        scale = 2.5f
                        pos = UI.CustomRatioPos (Vector2(0.05f, 0.15f))
                    }
                    let time = UI.Text {
                        content = Dialogue.timeSpend result.timeSpend
                        color = Color.White
                        font = DefaultFont
                        scale = 1.0f
                        pos = UI.CustomRatioPos (Vector2(0.07f, 0.32f))
                    }
                    let bugUsed = UI.Text {
                        content = Dialogue.exploitPrompt result.usedBug
                        color = Color.White
                        font = DefaultFont
                        scale = 1.0f
                        pos = UI.CustomRatioPos (Vector2(0.07f, 0.41f))
                    }
                    let crash = UI.Text {
                        content = Dialogue.crashPrompt result.crashedWith
                        color = Color.White
                        font = DefaultFont
                        scale = 1.0f
                        pos = UI.CustomRatioPos (Vector2(0.07f, 0.54f))
                    }
                    let gonext = UI.Text {
                        content = Dialogue.goNextPrompt result.usedBug
                        color = Color.White
                        font = DefaultFont
                        scale = 1.5f
                        pos = UI.AlignPos (UI.CenterX, UI.Bottom)
                    }
                    [vict; time; bugUsed; crash; gonext]
                
                | None -> []
            { ResultBase with subscreens = [{screen with inner = screen.inner @ screenAdd}] }

    let ResultInteract (state: GameState): UI.ScreenInteract = {
            buttons = ResultButton state
            keys = ResultKey state
            handler = ResultHandler state
    }









module ScreenMap = 
    let screenMap (screen: GameScreen) (state: GameState) = 
        match screen with
        | MainMenu -> ScreenMapBase.MainMenuScreen state
        | StageSelect v -> ScreenMapBase.StageSelectScreen state v
        | PatchNote -> ScreenMapBase.PatchNoteScreen state
        | StageBlockPopup -> ScreenMapBase.StageBlockPopupScreen state
        | StageLoader -> ScreenMapBase.StageLoaderScreen state
        | StagePlaying -> ScreenMapBase.StagePlayingScreen state
        | Tutorial -> ScreenMapBase.TutorialScreen state
        | PauseMenu -> ScreenMapBase.PauseScreen state
        | VictoryScreen -> ScreenMapBase.VictoryScreen state
        | BrokenScreen -> ScreenMapBase.CrashedScreen state
        | StageResult -> ScreenMapBase.ResultScreen state
        | _ -> ScreenMapBase.DefaultScreen state
    
    let interactMap (screen: GameScreen) (state: GameState) = 
        match screen with
        | MainMenu -> ScreenMapBase.MainMenuInteract  state
        | StageSelect _ -> ScreenMapBase.StageSelectInteract state
        | PatchNote -> ScreenMapBase.PatchNoteInteract state
        | StageBlockPopup -> ScreenMapBase.StageBlockPopupInteract state
        | StageLoader -> ScreenMapBase.StageLoaderInteract state
        | StagePlaying -> ScreenMapBase.StagePlayingInteract state
        | Tutorial -> ScreenMapBase.TutorialInteraction state
        | PauseMenu -> ScreenMapBase.PauseInteract state
        | VictoryScreen -> ScreenMapBase.VictoryInteract state
        | BrokenScreen -> ScreenMapBase.CrashedInteract state
        | StageResult -> ScreenMapBase.ResultInteract state
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
        | StagePlaying ->
            let screenUI = screenMap StagePlaying state
            DrawUI.stageDraw context state offset
            DrawUI.screenDraw context screenUI buttonState offset
        | gameScreen ->
            let screenUI = screenMap gameScreen state
            DrawUI.screenDraw context screenUI buttonState offset