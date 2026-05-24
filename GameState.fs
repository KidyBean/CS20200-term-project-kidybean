namespace TermProj

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

module GS = 
    type StageVictoryFlag = bool*bool*bool
    type StageVictoryMap = Map<int, StageVictoryFlag>

    let initialFlag: StageVictoryFlag = false, false, false

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
        let stageFlag = victoryMap |> Map.tryFind stagenum |> Option.defaultValue initialFlag
        let newFlag, changed = 
            match stageState, stageFlag with
            | Normal, (n, c, e) when not n -> ((true, c, e), true)
            | Crash, (n, c, e) when not c -> ((n, true, e), true)
            | Exploit, (n, c, e) when not e -> ((n, c, true), true)
            | _, _ -> (stageFlag, false)
        if changed then Some (stagenum, newFlag)
        else None

type GameState = {
    selectedStage: int
    lastPatchList: PatchMap
    needPatch: BugPatch option
    tutorialPlayed: Map<int, bool>
    stageResult: GS.StageVictoryMap
    stagePatchList: Map<int, PatchMap>
    inStage: InStage option
}

type GameStateChange = 
    | SelectedStageChange of int
    | PatchListAdd of BugPatch
    | NeedPatchSet of BugPatch option
    | TutorialPlayedSet of int * bool
    | StageResultChange of int * GS.StageVictoryFlag
    | StagePatchListSet of int * PatchMap
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
    let stateChangeAdd (change1: GameStateChange list) (change2: GameStateChange list) = change1 @ change2

    let isStagePatched (stagenum: int) (state: GameState) = Option.isSome (Map.tryFind stagenum state.stagePatchList)

    // selectedStage
    let getPresentStage (state: GameState) = state.selectedStage
    let setSelectedStage (state: GameState) (stagenum: int) = 
        let result = GS.howMapVictory stagenum state.stageResult
        match result with
        | [StageBlocked] -> []
        | _ -> [SelectedStageChange stagenum]
    
    // lastPatchlist, needPatch
    let getNextPatch (bugSet: PatchMap) (crashErr: BugPatch option) = 
        match crashErr with
        | Some err -> [NeedPatchSet (Some err)]
        | None ->
            match Stage.getNextUpdate bugSet with
            | Some patch -> [NeedPatchSet (Some patch)]
            | None -> []
    let setLastUpdate (state: GameState) = 
        match state.needPatch with
        | Some patch -> [PatchListAdd patch; NeedPatchSet None]
        | None -> []
    let needUpdate (state: GameState) = state.needPatch

    let getPatch (stagenum: int) (state: GameState) = 
        if Option.isSome (needUpdate state) then needUpdate state
        else
            let prevPatch = Map.tryFind (stagenum - 1) state.stagePatchList |> Option.defaultValue state.lastPatchList
            Map.tryFind stagenum state.stagePatchList |> Option.defaultValue state.lastPatchList
            |> Set.difference prevPatch 
            |> Seq.tryHead
    
    let addPatchOnStage (state: GameState) = 
        let stagenum = state.selectedStage
        let result = Map.tryFind stagenum state.stagePatchList
        if Option.isSome result then []
        else [StagePatchListSet (stagenum, state.lastPatchList)]
    
    // tutorialPlayed
    let needTutorial (stagenum: int) (state: GameState) = 
        match Map.tryFind stagenum state.tutorialPlayed with
        | Some false | None -> true
        | Some true -> false
    let tutorialPlayed (stagenum: int) = [TutorialPlayedSet (stagenum, true)]
    // stageResult
    let howMapVictory (stagenum: int) (state: GameState) = GS.howMapVictory stagenum state.stageResult

    let addStageFlag (stagenum: int) (stageState: StageState) (state: GameState) = 
        match GS.addStageFlag stagenum stageState state.stageResult with
        | Some changed -> [StageResultChange changed]
        | None -> []
    
    // instage, stagePatch
    let stageEndState (state: GameState) = 
        match state.inStage with
        | Some stage -> Stage.stageResultCall stage
        | None -> NoAction
    let StageResult (state: GameState) = 
        match state.inStage with
        | Some stage -> Stage.getStageResult stage
        | None -> None

    let loadStage (state: GameState) (stagenum: int) = 
        let result = Map.tryFind stagenum state.stagePatchList
        match result with
        | Some patch -> 
            let stage = Stage.Load stagenum patch
            if Option.isSome stage then [InStageChange stage]
            else []
        | None -> 
            let stage = Stage.Load stagenum state.lastPatchList
            if Option.isSome stage then [StagePatchListSet (stagenum, state.lastPatchList); InStageChange stage]
            else []
    /// Return Stage Change and input used
    /// if input is not used, it can be used on next action chain (in 1 update)
    let updateStage (state: GameState) (key: KeyBind option) (deltaTime: float32) = 
        match state.inStage with
        | Some stage -> 
            let stageChange = Stage.update key stage deltaTime
            [InStageChange (Some stageChange)]
        | None -> []
    let ExitStage () = [InStageChange None]


    let drawStage (context: DrawContext) (state: GameState) (offset: Vector2) = 
        match state.inStage with
        | Some stage -> Stage.drawGame context stage offset
        | None -> ()
        


    
    let updateState (change: GameStateChange) (state: GameState) =
        match change with
        | SelectedStageChange stagenum -> { state with selectedStage = stagenum }
        | PatchListAdd patch -> { state with lastPatchList = Set.add patch state.lastPatchList }
        | NeedPatchSet patch -> { state with needPatch = patch }
        | TutorialPlayedSet (stagenum, set) -> { state with tutorialPlayed = Map.add stagenum set state.tutorialPlayed }
        | StageResultChange (stagenum, newFlag) -> { state with stageResult = Map.add stagenum newFlag state.stageResult }
        | StagePatchListSet (stagenum, bugSet) -> { state with stagePatchList = Map.add stagenum bugSet state.stagePatchList }
        | InStageChange (stage) -> { state with inStage = stage }
    
    let update (changeList: GameStateChange List) (state: GameState) = 
        changeList
        |> List.fold (fun (state: GameState) (change: GameStateChange) -> updateState change state) state 

    
