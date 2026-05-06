open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

type Game1() as this =
    inherit Game()

    let graphics = new GraphicsDeviceManager(this)
    let mutable spriteBatch: SpriteBatch = Unchecked.defaultof<SpriteBatch>

    do
        this.Content.RootDirectory <- "Content"
        this.IsMouseVisible <- true

    override this.Initialize() =
        base.Initialize()

    override this.LoadContent() =
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)

    override this.Update(gameTime) =
        let keyboard = Keyboard.GetState()

        if keyboard.IsKeyDown(Keys.Escape) then
            this.Exit()

        base.Update(gameTime)

    override this.Draw(gameTime) =
        this.GraphicsDevice.Clear(Color.CornflowerBlue)
        base.Draw(gameTime)

[<EntryPoint>]
let main argv =
    use game = new Game1()
    game.Run()
    0