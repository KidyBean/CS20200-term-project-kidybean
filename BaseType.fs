namespace TermProj

type GameScreen = 
    | MainMenu
    | BlackScreen of float32
    | StageSelect of int
    | Tutorial of int
    | StagePlaying of int
    | PauseMenu
    | GameOver
    | BrokenScreen
    | VictoryScreen
    | EndScreen
    | SaveLoad // after this screen is optional
    | CustomScreen of string
    | NoScreen

type StageState = 
    | Normal
    | Crash
    | Exploit
    | Defeat
    | Blocked

type ObjectType = 
    | Empty
    | Player
    | Flag
    | Wall
    | Spike
    | Box
    | Door of int
    | Key of int

type GroundType = 
    | AbyssGround // base terrain
    | Ground
    | Abyss // Empty Space
    | ObjectGround of ObjectType // Abyss filled with an object

type Direction =
    | U // Up
    | D // Down
    | L // Left
    | R // Right

type KeyBind = 
    | Confirm // baseKey: Enter
    | Select // baseKey: Space
    | MoveUp // baseKey: W, up arrow
    | MoveDown // baseKey: S, down arrow
    | MoveLeft // baseKey: A, left arrow
    | MoveRight // baseKey: D, right arrow
    | PutDown // baseKey: E, right click
    | Escape // baseKey: Escape
    | Number of int

type TransitionType = 
    | Slide of Direction
    | Fade
    | Sudden of float32
    | Popup of bool