namespace TermProj

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

module KeyMap = 
    let keyMapping = [
        Keys.Enter, Confirm
        Keys.Space, Select
        Keys.E, Select
        Keys.W, MoveUp
        Keys.Up, MoveUp
        Keys.S, MoveDown
        Keys.Down, MoveDown
        Keys.A, MoveLeft
        Keys.Left, MoveLeft
        Keys.D, MoveRight
        Keys.Right, MoveRight
        Keys.F, PutDown
        Keys.Escape, Escape
        Keys.D1, Number 1
        Keys.D2, Number 2
        Keys.D3, Number 3
        Keys.D4, Number 4
    ]

    let tryActionFind (keyState: KeyboardState) = 
        keyMapping |> List.tryPick (fun (key, action) -> if keyState.IsKeyDown(key) then Some action else None )