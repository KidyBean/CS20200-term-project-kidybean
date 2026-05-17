namespace TermProj

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

module KeyMap = 
    let keyMapping = [
        Keys.Enter, Confirm
        Keys.W, Move U
        Keys.Up, Move U
        Keys.S, Move D
        Keys.Down, Move D
        Keys.A, Move L
        Keys.Left, Move L
        Keys.D, Move R
        Keys.Right, Move R
        Keys.E, GetObject
        Keys.F, PutDown
        Keys.Escape, Escape
        Keys.R, Reset
        Keys.D1, Number 1
        Keys.D2, Number 2
        Keys.D3, Number 3
        Keys.D4, Number 4
        Keys.D5, Number 5
        Keys.D6, Number 6
        Keys.D7, Number 7
        Keys.D8, Number 8
        Keys.D9, Number 9
        Keys.D0, Number 0
        Keys.NumPad0, Number 0
        Keys.NumPad1, Number 1
        Keys.NumPad2, Number 2
        Keys.NumPad3, Number 3
        Keys.NumPad4, Number 4
        Keys.NumPad5, Number 5
        Keys.NumPad6, Number 6
        Keys.NumPad7, Number 7
        Keys.NumPad8, Number 8
        Keys.NumPad9, Number 9
    ]

    let tryActionFind (keyState: KeyboardState) = 
        keyMapping |> List.tryPick (fun (key, action) -> if keyState.IsKeyDown(key) then Some action else None )