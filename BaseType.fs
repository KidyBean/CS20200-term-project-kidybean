namespace TermProj

type GameScreen = 
    | MainMenu
    | StageSelect of int
    | Tutorial
    | StagePlaying
    | PauseMenu
    | GameOver
    | BrokenScreen
    | VictoryScreen
    | EndScreen
    | SaveLoad // after this screen is optional
    | CustomScreen of string
    | NoScreen

type StageResult = 
    | Normal
    | Crash
    | Exploit
    | Defeat

type ObjectType = 
    | Player
    | Flag
    | Wall
    | Door
    | Box
    | Key
    | Spike

type GroundType = 
    | Ground
    | Abyss // Empty Space
    | ObjectGround of ObjectType // Abyss filled with an object

type Direction =
    | U // Up
    | D // Down
    | L // Left
    | R // Right

type KeyBind = 
    | NextScreen // baseKey: Enter
    | Select // baseKey: Space, left click
    | MoveUp // baseKey: W, up arrow
    | MoveDown // baseKey: S, down arrow
    | MoveLeft // baseKey: A, left arrow
    | MoveRight // baseKey: D, right arrow
    | PutDown // baseKey: E, right click
    | Escape // baseKey: Escape

type TransitionType = 
    | Slide of Direction
    | Fade
    | Sudden of float32
    | Playing
    | Popup of bool

type GameState = {
    playerPos: (int * int)
}