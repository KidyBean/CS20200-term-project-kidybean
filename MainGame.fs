namespace TermProj

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

type MainGame() as self =
    inherit Game()
    let graphic = new GraphicsDeviceManager(self)
    let mutable spriteBatch: SpriteBatch = Unchecked.defaultof<SpriteBatch>
    let mutable assets = Unchecked.defaultof<Assets>
    let mutable screenState = Screens.initialState
    
    let ScaleTransform () = 
        let viewport = self.GraphicsDevice.Viewport
        let virtualWidth = 1280.0f
        let virtualHeight = 720.0f

        let scaleX = float32 viewport.Width/virtualWidth
        let scaleY = float32 viewport.Height/virtualHeight
        let fullScale = min scaleX scaleY

        let ScaledWidth = virtualWidth*fullScale
        let ScaledHeight = virtualHeight*fullScale
        let offsetX = (float32 viewport.Width - ScaledWidth)/2.0f
        let offsetY = (float32 viewport.Height - ScaledHeight)/2.0f

        Matrix.CreateScale(fullScale)*Matrix.CreateTranslation(offsetX, offsetY, 0.0f)

    

    do
        self.Content.RootDirectory <- "content"
        self.IsMouseVisible <- true
        self.Window.AllowUserResizing <- true

    override self.Initialize() =
        base.Initialize()

    override self.LoadContent() =
        spriteBatch <- new SpriteBatch(self.GraphicsDevice)
        assets <- {
            fonts = {
                Default = self.Content.Load<SpriteFont>("Default")
            }
            textures = {
                basePixel = new Texture2D(self.GraphicsDevice, 1, 1)
            }
        }
        assets.textures.basePixel.SetData([|Color.White|])
    override self.Update(gameTime) =
        let keyboard = Keyboard.GetState()

        if keyboard.IsKeyDown(Keys.Escape) then
            self.Exit()

        base.Update(gameTime)

    override self.Draw(gameTime) =
        self.GraphicsDevice.Clear(Color.Black)
        spriteBatch.Begin(transformMatrix = ScaleTransform())
        // Draw your game objects here
        spriteBatch.End()
        base.Draw(gameTime)
