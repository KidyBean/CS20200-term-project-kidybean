namespace TermProj

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

type Fonts = {
    Default: SpriteFont
}

type Textures = {
    basePixel: Texture2D
}

type Assets = {
    fonts: Fonts
    textures: Textures
}

type drawContext = {
    spriteBatch: SpriteBatch
    assets: Assets
    mousepos: Vector2
}

type ScreenTransform = {
    scale: float32
    offset: Vector2
    transformMatrix: Matrix
}

module basic = 
    let virtualScreenSize = Vector2(1280.0f, 720.0f)
    let defaultdeltaTime = 0.016f