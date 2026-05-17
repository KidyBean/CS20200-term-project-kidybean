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
    | StageBlocked

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
    | Move of Direction // baseKey: WASD, Direction arrow
    | GetObject // baseKey: E
    | PutDown // baseKey: F
    | Escape // baseKey: Escape
    | Reset // baseKey: R
    | Number of int

type TransitionType = 
    | Slide of Direction
    | Fade
    | Sudden of float32
    | Popup of bool