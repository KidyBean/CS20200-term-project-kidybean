namespace TermProj

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

type MainGame() as self =
    inherit Game()
    let graphic = new GraphicsDeviceManager(self)
    let mutable mainContext: DrawContext  = {
        spriteBatch = Unchecked.defaultof<SpriteBatch>
        assets = Unchecked.defaultof<Assets>
    }
    let mutable viewTransform = { scale = 1.0f; offset = Vector2.Zero; transformMatrix = Matrix.Identity }

    let mutable screenState = Screens.initialState
    let mutable playState = GameState.initialGameState
    let mutable inputState = GameCore.initialInputState
    
    let ScaleTransform () = 
        let viewport = self.GraphicsDevice.Viewport
        let windowSize = Vector2(float32 viewport.Width, float32 viewport.Height)
        let scale = min (windowSize.X/GameCore.virtualScreenSize.X) (windowSize.Y/GameCore.virtualScreenSize.Y)
        let offset = (windowSize - GameCore.virtualScreenSize*scale)*0.5f
        { 
            scale = scale
            offset = offset
            transformMatrix = Matrix.CreateScale(scale, scale, 1.0f)
                *Matrix.CreateTranslation(offset.X, offset.Y, 0.0f)
        }
    let PosInVirtual (transform: ScreenTransform) (pos: Vector2) = (pos - transform.offset)/transform.scale
    

    do
        self.Content.RootDirectory <- AssetMap.contents
        self.IsMouseVisible <- true
        self.Window.AllowUserResizing <- true


    override self.Initialize() =
        base.Initialize()


    override self.LoadContent() =
        let loadSpriteBatch = new SpriteBatch(self.GraphicsDevice)
        let loadAssets = {
            fonts = 
                AssetMap.fontToAssetName
                |> List.map (fun (name, path) -> 
                    try
                        name, self.Content.Load<SpriteFont>(path)
                    with
                    | _ -> name, self.Content.Load<SpriteFont>("font/DefaultFont")
                ) |> Map.ofList
            textures = 
                AssetMap.textureToAssetName
                |> List.choose (fun (name, path) -> 
                    match path with
                    | Some path ->
                        try 
                            Some (name, self.Content.Load<Texture2D>(path))
                        with
                        | _ -> None
                    | None -> None
                ) 
                |> Map.ofList
                |> Map.add BasePixel (new Texture2D(self.GraphicsDevice, 1, 1))
        }
        loadAssets.fonts.[DefaultFont].LineSpacing <- 50
        loadAssets.textures.[BasePixel].SetData([|Color.White|])
        mainContext <- { spriteBatch = loadSpriteBatch; assets = loadAssets }
    

    override self.Update(gameTime) =
        viewTransform <- ScaleTransform()
        let mouse = Mouse.GetState()
        let keyboard = Keyboard.GetState()
        let mouseVirtualPos = PosInVirtual viewTransform (Vector2(float32 mouse.X, float32 mouse.Y))
        let newKey = KeyMap.tryActionFind keyboard
        let keyInput = { curKey = newKey; prevKey = inputState.keyboard.curKey }
        let mouseInput = { pos = mouseVirtualPos; prevPos = inputState.mouse.pos; curMouse = mouse; prevMouse = inputState.mouse.curMouse}
        inputState <- { mouse = mouseInput; keyboard = keyInput }
        let deltaTime = float32 gameTime.ElapsedGameTime.TotalSeconds
        
        let newScreen, (action, gameStateChange) = Screens.update screenState playState inputState deltaTime
        screenState <- newScreen
        match action with
        | Some UI.Blocked -> []
        | _ when screenState.state <> StagePlaying -> []
        | Some (UI.StageAction action) -> GameState.updateStage playState (Some action) deltaTime
        | _ -> GameState.updateStage playState None deltaTime
        |> GameState.stateChangeAdd gameStateChange
        |> fun change -> playState <- GameState.update change playState
        base.Update(gameTime)


    override self.Draw(gameTime) =
        self.GraphicsDevice.Clear(Color.Black)
        mainContext.spriteBatch.Begin(samplerState = SamplerState.PointClamp, transformMatrix = viewTransform.transformMatrix)
        Screens.draw mainContext screenState playState
        mainContext.spriteBatch.End()
        base.Draw(gameTime)
