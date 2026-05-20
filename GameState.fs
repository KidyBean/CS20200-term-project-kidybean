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
    needPatch: BugPatch option
    tutorialPlayed: Map<int, bool>
    stagePatchList: Map<int, PatchMap>
    stageResult: GS.StageVictoryMap
    inStage: InStage option
}

type GameStateChange = 
    | SelectedStageChange of int
    | PatchListAdd of BugPatch
    | NeedPatchSet of BugPatch
    | TutorialPlayedSet of int * bool
    | StagePatchListSet of int * PatchMap
    | StageResultChange of GS.StageVictoryMap
    | InStageChange of InStage option


module GameState =
    let initialGameState = {
        selectedStage = 0
        lastPatchList = Set.empty
        needPatch = None
        tutorialPlayed = Map.empty
        stageResult = Map.empty
        stagePatchList = Map.empty
        inStage = None
    }

    /// input need to be sort by timeLine
    /// list: [A; B; C; ...] -> timeline: A -> B -> C
    let stateChangeMerge (changeList: GameStateChange list list) = 
        changeList
        |> List.reduce (fun acc change -> acc @ change)
    
    /// for pipeline a |> GameState.stateChangeAdd b : a -> b
    let stateChangeAdd (change2: GameState list) (change1: GameState list) = change1 @ change2

    let getPresentStage (state: GameState) = state.selectedStage
    let setSelectedStage (state: GameState) (stagenum: int) = 
        let result = GS.howMapVictory stagenum state.stageResult
        match result with
        | [StageBlocked] -> []
        | _ -> [SelectedStageChange stagenum]

    let needTutorial (gameState: GameState) (stagenum: int) = 
        match Map.tryFind stagenum gameState.tutorialPlayed with
        | Some false | None -> true
        | Some true -> false
    let tutorialPlayed (stagenum: int) = TutorialPlayedSet (stagenum, true)

    let howMapVictory (stagenum: int) (gameState: GameState) = GS.howMapVictory stagenum gameState.stageResult
    let addStageFlag (stagenum: int) (stageState: StageState) (gameState: GameState) = 
        match (GS.addStageFlag stagenum stageState gameState.stageResult) with
        | Some changed -> [StageResultChange changed]
        | None -> []

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
    let getInStageTimeSpend (state: GameState) = 
        match state.inStage with
        | Some stage -> Some stage.fullTimeSpent
        | None -> None
    let getUsedBug (state: GameState) = 
        match state.inStage with
        | Some stage -> Some stage.usedBug
        | None -> None
    let setLastPatch (state: GameState) (bugSet: PatchMap) = 
        if Set.isEmpty bugSet then []
        else
            let bugArr = Set.toArray bugSet
            let rIdx = System.Random().Next(bugArr.Length)
            [NeedPatchSet bugArr[rIdx]; PatchListAdd bugArr[rIdx]]

    let loadStage (state: GameState) (stagenum: int) = 
        let result = Map.tryFind stagenum state.stagePatchList
        let stage = 
            match result with
            | Some patch -> Stage.Load stagenum patch
            | None -> Stage.Load stagenum state.lastPatchList
        [InStageChange (Some stage)]
    /// Return Stage Change and input used
    /// if input is not used, it can be used on next action chain (in 1 update)
    let updateStage (state: GameState) (key: KeyBind) (deltaTime: float32) = 
        match state.inStage with
        | Some stage -> 
            let stageChange, inputUsed = Stage.Update key stage deltaTime
            [InStageChange (Some stageChange)], inputUsed
        | None -> [], false
    let ExitStage () = [InStageChange None]



    
