module TermProj.Program

[<EntryPoint>]
let main argv =
    use game = new MainGame()
    game.Run()
    0