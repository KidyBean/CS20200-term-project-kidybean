namespace TermProj

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

module GS = 
    type StageVictoryFlag = bool*bool*bool
    type StageVictoryMap = Map<int, StageVictoryFlag>

    let rec flagListtoList (acc: StageState list) = function
        | [] -> List.rev acc
        | (stageState, flag) :: tl -> 
            if flag then flagListtoList (stageState :: acc) tl 
            else flagListtoList acc tl
    let getFlagToList (victoryFlag: StageVictoryFlag) = 
        let (n, c, e) = victoryFlag
        let flagList = [(Normal, n); (Crash, c); (Exploit, e)]
        flagListtoList [] flagList

    let howMapVictory (stagenum: int) (victoryMap: StageVictoryMap) = 
        if stagenum < 0 || stagenum > GameCore.gameStage then [StageBlocked]
        else
            let prevFlag = victoryMap |> Map.tryFind (stagenum - 1)
            match prevFlag with
            | Some (n, c, e) when n || c || e -> [StageBlocked]
            | _ ->
                let stageFlag = victoryMap |> Map.find stagenum
                let stateList = getFlagToList stageFlag
                match stateList with
                | [] -> [Defeat]
                | v -> v
    
    let addStageFlag (stagenum: int) (stageState: StageState) (victoryMap: StageVictoryMap) = 
        let stageFlag = victoryMap |> Map.find stagenum
        let newFlag, changed = 
            match stageState, stageFlag with
            | Normal, (n, c, e) when not n -> ((true, c, e), true)
            | Crash, (n, c, e) when not c -> ((n, true, e), true)
            | Exploit, (n, c, e) when not e -> ((n, c, true), true)
            | _, _ -> (stageFlag, false)
        if changed then Some (victoryMap |> Map.add stagenum newFlag)
        else None

type GameState = {
    stageResult: GS.StageVictoryMap
    lastPatchList: PatchMap
    lastSelectedStage: int
    inStage: InStage option
}

module GameState =
    let initialGameState = {
        stageResult = Map.empty
        lastPatchList = Set.empty
        lastSelectedStage = 0
        inStage = None
    }
    let howMapVictory (stagenum: int) (gameState: GameState) = GS.howMapVictory stagenum gameState.stageResult
    let addStageFlag (stagenum: int) (stageState: StageState) (gameState: GameState) = 
        match (GS.addStageFlag stagenum stageState gameState.stageResult) with
        | Some changed -> Some {gameState with stageResult = changed}
        | None -> None