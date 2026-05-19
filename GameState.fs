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

    /// StageBlock -> Cannot goto stage / stage unreachable
    /// Defeat -> Stage reachable but not Victory
    /// Victory [Normal, Crash, Exploit]
    let howMapVictory (stagenum: int) (victoryMap: StageVictoryMap) = 
        if stagenum < 0 || stagenum > GameCore.gameStage then [StageBlocked]
        else
            let prevFlag = victoryMap |> Map.tryFind (stagenum - 1)
            let stageFlag = victoryMap |> Map.tryFind stagenum
            match prevFlag, stageFlag with
            | None, None when stagenum <> 0 -> [StageBlocked]
            | Some (n, c, e), None when not ( n || c || e ) -> [StageBlocked]
            | _, None -> [Defeat]
            | _, Some flags ->
                let stateList = getFlagToList flags
                match stateList with
                | [] -> [Defeat]
                | v -> v
    
    let addStageFlag (stagenum: int) (stageState: StageState) (victoryMap: StageVictoryMap) = 
        let stageFlag = victoryMap |> Map.tryFind stagenum |> Option.defaultValue (false, false, false)
        let newFlag, changed = 
            match stageState, stageFlag with
            | Normal, (n, c, e) when not n -> ((true, c, e), true)
            | Crash, (n, c, e) when not c -> ((n, true, e), true)
            | Exploit, (n, c, e) when not e -> ((n, c, true), true)
            | _, _ -> (stageFlag, false)
        if changed then Some (victoryMap |> Map.add stagenum newFlag)
        else None

type GameState = {
    selectedStage: int
    lastPatchList: PatchMap
    needPatchList: PatchMap
    tutorialPlayed: Map<int, bool>
    stagePatchList: Map<int, PatchMap>
    stageResult: GS.StageVictoryMap
    inStage: InStage option
}

type GameStateChange = 
    | SelectedStageChange of int
    | PatchListSet of PatchMap
    | NeedPatchListChange of PatchMap
    | TutorialPlayedSet of int * bool
    | StagePatchListSet of int * PatchMap
    | StageResultChange of GS.StageVictoryMap
    | InStageChange of InStage option
    | NoStateChange


module GameState =
    let initialGameState = {
        stageResult = Map.empty
        lastPatchList = Set.empty
        needPatchList = Set.empty
        tutorialPlayed = Map.empty
        stagePatchList = Map.empty
        selectedStage = 0
        inStage = None
    }
    let howMapVictory (stagenum: int) (gameState: GameState) = GS.howMapVictory stagenum gameState.stageResult

    let stageResultCall (gameState: GameState) = 
        let inStage = gameState.inStage
        match inStage with
        | Some stage -> 
            match fst stage.movement with
            | StageVictory :: _ -> StageVictory
            | PlayerDead :: _ -> PlayerDead
            | StageCrashed err :: _ -> StageCrashed err
            | _ -> NoAction
        | None -> NoAction

    let needTutorial (gameState: GameState) = 
        match Map.tryFind gameState.selectedStage gameState.tutorialPlayed with
        | Some false | None -> true
        | Some true -> false

    let addStageFlag (stagenum: int) (stageState: StageState) (gameState: GameState) = 
        match (GS.addStageFlag stagenum stageState gameState.stageResult) with
        | Some changed -> StageResultChange changed
        | None -> NoStateChange
    
