namespace TermProj

open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

type FontID = 
    | DefaultFont
    | TitleFont
    | MiddleFont

type Fonts = Map<FontID, SpriteFont>

type TextureID = 
    | BasePixel
    | NoTexture // for Blank
    // Object Texture
    | PlayerU
    | PlayerD
    | PlayerL
    | PlayerR
    | FlagTexture
    | WallTexture
    | WallU
    | WallD
    | WallL
    | WallR
    | WallUD
    | WallUL
    | WallUR
    | WallDL
    | WallDR
    | WallLR
    | WallUDL
    | WallUDR
    | WallULR
    | WallDLR
    | WallUDLR
    | SpikeTexture
    | BoxTexture
    | KeyTexture
    | DoorTexture
    // Ground Texture
    | GroundTexture
    | BoxGround
    | GroundCliff
    | BoxCliff
    // UI texture
    | X
    | ArrowL
    | ArrowR
    | Pause
    | Tuto
    | Focus
    | Cell

module debug = 
    let UITextureReady = false


type Textures = Map<TextureID, Texture2D>

type Assets = {
    fonts: Fonts
    textures: Textures
}

type DrawContext = {
    spriteBatch: SpriteBatch
    assets: Assets
}


type ScreenTransform = {
    scale: float32
    offset: Vector2
    transformMatrix: Matrix
}


type MouseInput = {
    pos: Vector2
    prevPos: Vector2
    curMouse: MouseState
    prevMouse: MouseState
}

type KeyboardInput = {
    curKey: KeyBind Option
    prevKey: KeyBind Option
}

type InputState = {
    mouse: MouseInput
    keyboard: KeyboardInput
}

module GameCore = 
    let virtualScreenSize = Vector2(1280.0f, 720.0f)
    let initialInputState = {
        mouse = {
            pos = Vector2.Zero
            prevPos = Vector2.Zero
            curMouse = Unchecked.defaultof<MouseState>
            prevMouse = Unchecked.defaultof<MouseState>
        }
        keyboard = {
            curKey = None
            prevKey = None
        }
    }

    let gameStage: int = 20
    let objectLayer: int = 3
    let GridPadding: int = 20

    let BlockSize: int = 64
    let DeadZoneRatio: float32 = 0.3f
    let cameraTraceSpeed: float32 = 0.1f

    let InventoryStack: int = 4

    let DefaultActionDelay: float32 = 0.2f
    let DefaultBlockMoveDuration: float32 = 0.6f

    let defaultDeltaTime = 0.016f // 60fps - test







module AssetMap = 
    type AssetSpec = 
        | Spec of int
        | SpecDirection of Direction
        | SpecDirectionList of Direction list
        | Upper of GroundType
        | UpperObject of ObjectType
        | NoSpec
    
    let contents = "content"
    let getFont (context: DrawContext) (fontId: FontID) = Map.tryFind fontId context.assets.fonts
    let getDefaultFont (context: DrawContext) = context.assets.fonts[DefaultFont]
    let getTexture (context: DrawContext) (textureId: TextureID) = Map.tryFind textureId context.assets.textures
    let getDefaultTexture (context: DrawContext) = context.assets.textures[BasePixel]

    let textureToAssetName = [
        BasePixel, None 
        NoTexture, None
        PlayerU, Some "texture/PlayerU"
        PlayerD, Some "texture/PlayerD"
        PlayerL, Some "texture/PlayerL"
        PlayerR, Some "texture/PlayerR"
        FlagTexture, Some "texture/FlagTexture"
        WallTexture, Some "texture/WallTexture"
        WallU, Some "texture/WallU"
        WallD, Some "texture/WallD"
        WallL, Some "texture/WallL"
        WallR, Some "texture/WallR"
        WallUD, Some "texture/WallUD"
        WallUL, Some "texture/WallUL"
        WallUR, Some "texture/WallUR"
        WallDL, Some "texture/WallDL"
        WallDR, Some "texture/WallDR"
        WallLR, Some "texture/WallLR"
        WallUDL, Some "texture/WallUDL"
        WallUDR, Some "texture/WallUDR"
        WallULR, Some "texture/WallULR"
        WallDLR, Some "texture/WallDLR"
        WallUDLR, Some "texture/WallUDLR"
        SpikeTexture, Some "texture/SpikeTexture"
        BoxTexture, Some "texture/BoxTexture"
        KeyTexture, Some "texture/KeyTexture"
        DoorTexture, Some "texture/DoorTexture"
    // Ground Texture
        GroundTexture, Some "texture/GroundTexture"
        BoxGround, Some "texture/BoxGround"
        GroundCliff, Some "texture/GroundCliff"
        BoxCliff, Some "texture/BoxCliff"
    // UI Texture
        X, Some "texture/X"
        ArrowL, Some "texture/ArrowL"
        ArrowR, Some "texture/ArrowR"
        Pause, Some "texture/Pause"
        Tuto, Some "texture/Tuto"
        Focus, Some  "texture/Focus"
        Cell, Some  "texture/Cell"
    ]
    
    let fontToAssetName = [
        DefaultFont, "font/DefaultFont"
        TitleFont, "font/TitleFont"
        MiddleFont, "font/MiddleFont"
    ]
        
    
    let playerTexture = function
        | SpecDirection U -> PlayerU
        | SpecDirection D -> PlayerD
        | SpecDirection L -> PlayerL
        | SpecDirection R -> PlayerR
        | _ -> PlayerR
    
    let wallTexture = function
        | NoSpec -> WallTexture
        | SpecDirectionList [U] -> WallU
        | SpecDirectionList [D] -> WallD
        | SpecDirectionList [L] -> WallL
        | SpecDirectionList [R] -> WallR
        | SpecDirectionList [U; D] -> WallUD
        | SpecDirectionList [U; L] -> WallUL
        | SpecDirectionList [U; R] -> WallUR
        | SpecDirectionList [D; L] -> WallDL
        | SpecDirectionList [D; R] -> WallDR
        | SpecDirectionList [L; R] -> WallLR
        | SpecDirectionList [U; D; L] -> WallUDL
        | SpecDirectionList [U; D; R] -> WallUDR
        | SpecDirectionList [U; L; R] -> WallULR
        | SpecDirectionList [D; L; R] -> WallDLR
        | SpecDirectionList [U; D; L; R] -> WallUDLR
        | _ -> WallTexture

    
    let ObjectTexture (object: ObjectType) (spec: AssetSpec) = 
        match object with
        | Player -> playerTexture spec
        | Wall -> wallTexture spec
        | Flag -> FlagTexture
        | Spike -> SpikeTexture
        | Box -> BoxTexture
        | Key _ -> KeyTexture
        | Door _ -> DoorTexture
        | Empty -> NoTexture

    let cliffTexture = function
    | Upper Ground -> GroundCliff
    | UpperObject Box -> BoxCliff
    | _ -> NoTexture

    let objectGroundTexture = function
    | Box -> BoxGround
    | v -> ObjectTexture v NoSpec

    let GroundTexture (ground: GroundType) (spec: AssetSpec) = 
        match ground with
        | Ground -> GroundTexture
        | Abyss | AbyssGround -> cliffTexture spec
        | ObjectGround object -> objectGroundTexture object
    
    let hsvColor (h: float32) (s: float32) (v: float32)  = 
        let h = ((h % 360.0f) + 360.0f) % 360.0f
        let c = v * s
        let x = c * (1.0f - abs ((h / 60.0f) % 2.0f - 1.0f))
        let m = v - c

        let r, g, b =
            if h < 60.0f then c, x, 0.0f
            elif h < 120.0f then x, c, 0.0f
            elif h < 180.0f then 0.0f, c, x
            elif h < 240.0f then 0.0f, x, c
            elif h < 300.0f then x, 0.0f, c
            else c, 0.0f, x
        Color(int ((r + m)*255.0f), int ((g + m)*255.0f), int ((b + m)*255.0f))

    let colorBaseOfNum (n: int) = 
        if n = 0 then Color.White
        else
            hsvColor (float32 ((n*143)%360)) 0.8f 0.9f
    
    let colorMap = Array.init 50 colorBaseOfNum

module Dialogue = 
    let Title = "Play In Prograss"
    let startPrompt = "Press Enter to Start"




    let patchVer v = sprintf "Patch v0.%02d" v
    let promptVerMap = Map [
        0, "Our First Release"
        1, "Block pushing\nhas been added\ninto game."
    ]
    let getPromptVer v = 
        match Map.tryFind v promptVerMap with
        | Some dialogue -> dialogue
        | None -> ""




    let newUpdate = "Content Update"
    let bugFix = "Bug Fixes"    
    let minorStability  = "Applied minor stability improvements."
    let updatePromptMap = Map [
        Walk, "
Our game has finally been released.

Although this is our first beta version, our brilliant development team has made sure
that the game is almost perfectly stable.".TrimStart('\r', '\n')

        PushBlock, "
Block pushing has been added.

Players can now push designated blocks within the stage.
This feature should provide a stable and reliable gameplay experience.".TrimStart('\r', '\n')

        AbyssAndGround, "
Ground tiles have been added to stage.

Players can now experience more varied stage terrain.
This system has been carefully implemented for stable gameplay.".TrimStart('\r', '\n')

        Inventory, "
The inventory system has been added.

Players can now collect and manage items during gameplay.
All stored items should remain exactly where they are expected to be.".TrimStart('\r', '\n')

        KeyAndDoor, "
Keys and doors have been added.

Players can now unlock doors using keys found within the stage.
This feature should make progression more structured and reliable.".TrimStart('\r', '\n')

    ]
    let patchPromptMap = Map [
        PlayerCollisionExploit, "
A minor issue allowed the player to ignore map collision under certain conditions.
Although this was obviously not a serious problem,
we patched it to preserve the intended gameplay experience.".TrimStart('\r', '\n')

        StagePositionOutCrash, "
A minor out-of-bounds issue has been reported.
In rare cases, the player could leave the valid stage area,
which may have caused the game to crash.
Additional boundary checks have been added.".TrimStart('\r', '\n')

        WrongObjectPushExploit, "
A minor object pushing issue has been confirmed.
In rare cases, blocks that were not intended to be movable could be pushed.
Push validation has been corrected to preserve the intended stage design.".TrimStart('\r', '\n')

        ObjectCollisionExploit, "
A minor object collision issue has been reported.
Under certain conditions, pushed blocks could ignore collision with other blocks.
Object collision checks have been improved.".TrimStart('\r', '\n')

        AbyssCheckExploit, "
A minor abyss detection issue has been reported.
In rare cases, falling into the abyss did not result in defeat.
Abyss checks have been corrected to preserve the intended gameplay experience.".TrimStart('\r', '\n')

        WrongAbyssObjectExploit, "
A minor abyss interaction issue has been confirmed.
In rare cases, objects not intended to fill the abyss could still create ground.
Abyss object validation has been improved.".TrimStart('\r', '\n')

        WrongInventoryPutExploit, "
A minor inventory validation issue has been reported.
In rare cases, objects not intended to be stored could be placed in the inventory.
Inventory checks have been corrected.".TrimStart('\r', '\n')

        InventoryLayerStackCrash, "
A minor inventory capacity issue has been reported.
In rare cases, the inventory could exceed its maximum capacity,
which may have caused the game to crash.
Inventory capacity checks have been added.".TrimStart('\r', '\n')

        PutDownOverlapExploit, "
A minor item placement issue has been confirmed.
In rare cases, inventory items could be placed on already occupied tiles.
Placement validation has been added to prevent unintended overlap.".TrimStart('\r', '\n')

        AnyKeyUsedExploit, "
A minor key validation issue has been reported.
In rare cases, a door could be unlocked with a key meant for another door.
Key-door matching has been corrected to preserve the intended progression.".TrimStart('\r', '\n')
    ]
    let updatePrompt (update: Update) = 
        match Map.tryFind update updatePromptMap with
        | Some dialogue -> dialogue
        | None -> ""    
    let patchPrompt (patchsome: BugPatch option) = 
        match patchsome with
        | Some patch ->
            match Map.tryFind patch patchPromptMap with
            | Some dialogue -> dialogue + "\n" + minorStability
            | None -> minorStability
        | None -> minorStability

    let warning = "Warning"
    let blockPrompt = "\nThe version you are trying to access\n\n is not available yet."



    let tutorial = "Tutorial"
    let tutorialMap = Map [
        Walk, "
Move with W A S D or the arrow keys.

Press ESC to pause.

Reach the flag to clear the stage.

Avoid spikes. Touching them will kill you.".TrimStart('\r', '\n')

        PushBlock, "
Some blocks can be pushed.

Move toward a pushable block to push it.

Use blocks to create a path or clear the way.".TrimStart('\r', '\n')

        AbyssAndGround, "
The abyss is dangerous.

Falling into the abyss will kill you.

Some objects can fill the abyss and create new ground.".TrimStart('\r', '\n')

        Inventory, "
Some objects can be stored in your inventory.

Press E to pick up an object.

Press F to put it down.

Press a number key to change inventory slots.".TrimStart('\r', '\n')

        KeyAndDoor, "
Keys can unlock doors.

Each key only works with its matching door.

Find the correct key to open the way forward.

Walk into a door to unlock it with a key.".TrimStart('\r', '\n')
    ]
    let tutorialPrompt update = 
        match Map.tryFind update tutorialMap with
        | Some dialogue -> dialogue
        | None -> ""    




    let pause = "PAUSE"
    let resume = "RESUME"
    let restart = "RESTART"
    let exit = "EXIT STAGE"


    let victory = "! VICTORY !"
    let crashed = "! GAME CRAHSED !"
    let gonextResult = "Press Enter to See Result"

    let result = "RESULT"
    let gonext = "Press Enter to Go Next"
    let gonextPatch = "Press Enter to Report Issue"

    let victoryType = function
        | Normal -> "Victory! (Normal)"
        | Crash -> "Game Crahsed!"
        | Exploit -> "Victory! (With Exploit)"
        | _ -> "Error"

    let formatTime seconds =
        let totalCentiseconds = int (seconds * 100.0f)
        let minutes = totalCentiseconds / 6000
        let secondsPart = (totalCentiseconds / 100) % 60
        let centiseconds = totalCentiseconds % 100
        sprintf "%02d:%02d.%02d" minutes secondsPart centiseconds
    let timeSpend seconds = "Clear Time : " + formatTime seconds

    let bugPromptMap = Map [
        PlayerCollisionExploit, "PLAYER COLLISION, "
        StagePositionOutCrash, "Unexpected Player Position: Out of Stage Boundary"
        WrongObjectPushExploit, "INVALID OBJECT PUSH, "
        ObjectCollisionExploit, "OBJECT COLLISION, "
        AbyssCheckExploit, "ABYSS BYPESS, "
        WrongAbyssObjectExploit, "INVALID ABYSS FILL, "
        WrongInventoryPutExploit, "INVALID INVENTORY STORAGE, "
        InventoryLayerStackCrash, "Unexpected Inventory Entity: Out of Inventory Boundary"
        PutDownOverlapExploit, "ITEM OVERLAP, "
        AnyKeyUsedExploit, "INVALID KEY UNLOCK, "
    ]

    let exploitPrompt usedBug = 
        let exploit = "Used Exploit:\n"
        if Set.isEmpty usedBug then exploit + "None"
        else
            usedBug
            |> Set.fold (
                fun prompt bug -> 
                match Map.tryFind bug bugPromptMap with
                | Some dialogue when bug <> StagePositionOutCrash && bug <> InventoryLayerStackCrash -> 
                    prompt + dialogue
                | _ -> prompt + ""    
            ) exploit
    
    let crashPrompt err = 
        let crash = "Game Crashed With:\n"
        if Option.isNone err then ""
        else
            match Map.tryFind err.Value bugPromptMap with
            | Some dialogue -> crash + dialogue
            | None -> ""

    let goNextPrompt usedBug = 
        if Set.isEmpty usedBug then gonext
        else gonextPatch
            