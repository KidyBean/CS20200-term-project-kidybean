namespace TermProj

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

module KeyMap = 
    let keyMapping = Map [
        Keys.Enter, Confirm
        Keys.Space, Select
        Keys.W, MoveUp
        Keys.S, MoveDown
        Keys.A, MoveLeft
        Keys.D, MoveRight
        Keys.E, PutDown
        Keys.Escape, Escape
        Keys.D1, Number 1
        Keys.D2, Number 2
        Keys.D3, Number 3
        Keys.D4, Number 4
    ]

    let tryActionFind (keyState: KeyboardState) = 
        keyMapping |> Map.tryPick (fun key action -> if keyState.IsKeyDown(key) then Some action else None )