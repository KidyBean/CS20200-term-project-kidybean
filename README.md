# Play in Progress
CS-20200 Term Project / 20200154 Kim Junwoo

**Play in Progress** is a small 2D puzzle game implemented in F# using .NET 10 and MonoGame.

The player controls a character in each stage and tries to reach the goal. Each stage contains intentional bugs that imitate mistakes made during game development.

---
## Getting Started
### Requirements
* .NET 10 SDK
  Verify with: `dotnet --version` (should show `10.x.x`)
* MonoGame content tools
  These can be restored with `dotnet tool restore`.

### Run
```bash
# Windows
run.bat

# Unix / macOS
chmod +x run.sh
./run.sh

# Or directly
dotnet tool restore
dotnet restore
dotnet run --project term-proj.fsproj
```

Alternatively, if your terminal is already in the repository root, the following may also work:

```bash
dotnet run
```

---
## How to Play
### Goal

The goal of each stage is to move the player character to the flag position.

A stage can be cleared in three ways:

1. **Normal Clear**
   The player reaches the goal without using any intentional bug.

2. **Exploit Clear**
   The player reaches the goal after using an intentional exploit.

3. **Crash Clear**
   The player triggers a specific bug that causes a crash. The stage is forcibly cleared, and the player can proceed to the next stage.

### Controls
| Input             | Action           |
| ----------------- | ---------------- |
| W, ArrowUp        | Move Up          |
| S, ArrowDown      | Move Down        |
| A, ArrowLeft      | Move Left        |
| D, ArrowRight     | Move Right       |
| E                 | Pick Up Item     |
| F                 | Take Out Item    |
| Number 1, 2, 3, 4 | Inventory Select |
| Mouse Click       | Menu Selection   |
| ESC               | Pause / Return   |

The keyboard controls are also explained in the in-game instructions.

### Game Screens
The game contains the following screens:

* Main screen
* Stage selection screen
* Stage gameplay screen
* Result screen
* Patch note screen

Except for the first stage, each stage can only be attempted after the previous stage has been cleared. Stages that have already been cleared can be replayed.

### Bug and Patch Note System
The game includes ten different intentional bugs.

When the player uses a bug during a stage, the game records that bug usage. After clearing the stage, the patch note shows an update that fixes one of the bugs used by the player. The patch note displays explicit bug names and descriptions.

A patched bug can no longer be used in later stages.

---
## Project Structure
```text
CS20200-term-project-kidybean/
├── content/                 # MonoGame content files and assets
├── mapEditor/               # Map editor used to construct stages
├── BaseType.fs              # Basic shared types
├── FrameworkType.fs         # Framework-level types
├── KeyboardMap.fs           # Keyboard input mapping
├── StageBase.fs             # Stage base definitions
├── StagePlaying.fs          # Stage gameplay logic
├── Stage.fs                 # Stage construction and stage drawing logic
├── GameState.fs             # Game state management
├── ScreenMap.fs             # Screen mapping
├── Screens.fs               # Screen implementations
├── MainGame.fs              # Main MonoGame game class
├── Program.fs               # Entry point
├── term-proj.fsproj         # .NET 10 F# project file
├── dotnet-tools.json        # MonoGame tool configuration
└── README.md
```
## Assets and Credits
- Pixel sprites: Created manually for this project.
- Font: Galmuri font series by Lee Minseo, licensed under the SIL Open Font License 1.1.
- Framework: MonoGame.

---
## Requirements
### Checklist
* [x] The game includes at least three stages and at least three different bugs.
* [x] Every stage is clearable through the intended method.
* [x] The game includes simple pixel sprites for the player and stage objects.
* [x] Different stage objects are visually distinguishable.
* [x] The game detects whether the player used an intentional bug during a stage.
* [x] Bug usage is visually represented by visible bug-related stage elements and their gameplay effects.
* [x] The patch note shown after clearing a stage states that one of the bugs used by the player will be fixed.
* [x] The patch note displays explicit bug names and descriptions.

### Changes and Clarifications from Proposal
The final implementation follows the main requirements of the original proposal. Some parts were expanded or implemented in a more concrete form:

* The original proposal required at least three stages, and the final game includes seven stages.
* The original proposal required at least three different intentional bugs, and the final game includes ten different bugs.
* The original proposal stated that the use of each bug would be represented by a visual element during the stage. In the final game, this is implemented mainly through visible bug-related objects, tiles, and the resulting gameplay behavior, rather than through separate visual effects for every bug.

These changes do not remove the original requirements. The stage count and bug count were expanded, and the visual representation of bugs was implemented through the stage design and observable behavior.

---
## LLM Usage
### Parts Assisted by the LLM
I used an LLM as an auxiliary tool during this project. It was mainly used to clarify basic MonoGame usage, MGCB/content management, small F# implementation details, and specific debugging issues that came up during development.

For MonoGame and MGCB, I used the LLM to better understand content loading, texture and font management, virtual screen rendering, and basic drawing-related behavior. For F#, I used it only for small implementation questions, such as collection handling, option values, and pattern matching.

I also used the LLM to discuss specific debugging issues related to UI display, patch notes, saved state values, and stage behavior. In addition, I used it to refine English wording in project documents, README descriptions, and in-game UI text. Occasionally, I used it to discuss whether small implementation ideas fit my existing code structure.

### Parts Done Manually
The overall game concept, stage design, sprites, main implementation decisions, and testing were done manually. The LLM did not generate the full project, decide the final game mechanics, create the sprites, or replace manual testing.

I manually revised the LLM outputs to match my actual project structure and game design. In particular, I checked MonoGame and F# suggestions against the actual implementation, adjusted project documents when the suggested wording was too restrictive, and verified the game behavior by running and testing the game myself. When a suggestion did not fit the intended stage rules or existing code structure, I modified or discarded it manually.

### Limitations of the LLM
The LLM was not always accurate with project-specific details. The parts that required the most manual correction were the exact bug and patch progression rules, the meaning of some game states, and how certain implementation details fit into my existing F# code.

Because the project uses intentional bugs and patch mechanics as part of the game design, some behaviors that looked like implementation problems were actually intended game mechanics. These parts had to be reviewed and corrected manually based on the actual design and gameplay.
