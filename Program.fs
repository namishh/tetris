type BlockChar =
    | Real
    | Ghost

let blockToChar = function
    | Real -> "██"
    | Ghost -> "░░"

type Color =
    | Cyan
    | Yellow
    | Purple
    | Green
    | Red
    | Blue
    | Orange
    | Reset

let colorToAnsi = function
    | Cyan -> "\x1b[36m"
    | Yellow -> "\x1b[33m"
    | Purple -> "\x1b[35m"
    | Green -> "\x1b[32m"
    | Red -> "\x1b[31m"
    | Blue -> "\x1b[34m"
    | Orange -> "\x1b[38;5;208m"
    | Reset -> "\x1b[0m"

type Tetrimino = { Name: string; Blocks: (int * int) list; Color: Color }

type Cell = Empty | Filled of Color
type Board = Cell[,]

type GameState = {
    board: Board
    rng: System.Random
    lines: int
    currentPiece: Tetrimino
    currentPosition: (int * int)
    nextPiece: Tetrimino
    level: int
    gameOver: bool
    pieceHeld: Tetrimino option  
}

let tetriminos = [
    { Name = "I"; Blocks = [0,0; 1,0; 2,0; 3,0]; Color = Cyan }
    { Name = "O"; Blocks = [0,0; 1,0; 0,1; 1,1]; Color = Yellow }
    { Name = "T"; Blocks = [0,0; 1,0; 2,0; 1,1]; Color = Purple }
    { Name = "S"; Blocks = [1,0; 2,0; 0,1; 1,1]; Color = Green }
    { Name = "Z"; Blocks = [0,0; 1,0; 1,1; 2,1]; Color = Red }
    { Name = "J"; Blocks = [0,0; 0,1; 1,1; 2,1]; Color = Blue }
    { Name = "L"; Blocks = [2,0; 0,1; 1,1; 2,1]; Color = Orange }
]

let printTetrimino (tetrimino: Tetrimino) =
    printfn "Piece: %s" tetrimino.Name
    let minX = tetrimino.Blocks |> List.map fst |> List.min
    let maxX = tetrimino.Blocks |> List.map fst |> List.max
    let minY = tetrimino.Blocks |> List.map snd |> List.min
    let maxY = tetrimino.Blocks |> List.map snd |> List.max
    
    for y in minY .. maxY do
        for x in minX .. maxX do
            if tetrimino.Blocks |> List.contains (x, y) then
                printf "%s%s%s" (colorToAnsi tetrimino.Color) (blockToChar Real) (colorToAnsi Reset) 
            else
                printf "  "
        printfn ""
    printfn ""

let emptyBoard(): Board =
    Array2D.init 20 10 (fun _ _ -> Empty)

let randomTetrimino (rng: System.Random): Tetrimino =
    let index = rng.Next(0, List.length tetriminos)
    List.item index tetriminos

let createGameState(): GameState =
    let rng = System.Random()
    let initialPiece = randomTetrimino rng
    let nextPiece = randomTetrimino rng
    let heldPiece = None  
    let board = emptyBoard()
    { board = board; rng = rng; currentPiece = initialPiece; lines = 0; currentPosition = (3, 0); nextPiece = nextPiece; level = 1; gameOver = false; pieceHeld = heldPiece }

let drawBoard (state: GameState) =
    let board = state.board
    let posX, posY = state.currentPosition
    
    printfn "┏━━ namishh/tetris ━━┓"


    for y in 0 .. (Array2D.length1 board - 1) do
        printf "┃"
        for x in 0 .. (Array2D.length2 board - 1) do
            let isActivePiece = 
                state.currentPiece.Blocks 
                |> List.exists (fun (blockX, blockY) -> 
                    posX + blockX = x && posY + blockY = y)
            
            if isActivePiece then
                printf "%s%s%s" (colorToAnsi state.currentPiece.Color) (blockToChar Real) (colorToAnsi Reset)
            else
                match board.[y, x] with
                | Empty -> printf "  "
                | Filled color -> printf "%s%s%s" (colorToAnsi color) (blockToChar Real) (colorToAnsi Reset)
        printfn "┃"
    
    printf "┗"
    for _ in 0 .. (Array2D.length2 board - 1) do
        printf "━━"
    printfn "┛"

let pieceDisplayLines (piece: Tetrimino) =
    let blocks = piece.Blocks |> Set.ofList
    [ for y in 0 .. 1 do
        let line =
            [ for x in 0 .. 3 do
                if blocks.Contains (x, y) then
                    colorToAnsi piece.Color + blockToChar Real + colorToAnsi Reset
                else
                    "  " ]
            |> String.concat ""
        yield line ]

let drawBox title content x y =
    System.Console.SetCursorPosition(x, y)
    printfn "┏━ %s ━┓" title
    for line in content do
        System.Console.SetCursorPosition(x, System.Console.CursorTop)
        printf "┃%s ┃" line
        printfn ""
    System.Console.SetCursorPosition(x, System.Console.CursorTop)
    printfn "┗━━━━━━━━━┛"

let drawGameState (state: GameState) =
    System.Console.SetCursorPosition(0, 0)
    drawBoard state

    let infoX = 24  

    let nextY = 0
    let nextContent = pieceDisplayLines state.nextPiece
    drawBox "after" nextContent infoX nextY

    let heldY = nextY + 4
    let heldContent = 
        match state.pieceHeld with
        | Some piece -> pieceDisplayLines piece  
        | None -> ["        "; "        "]  
    drawBox "saved" heldContent infoX heldY

    let linesY = heldY + 4  
    let linesContent = [sprintf "%-8d" state.lines] 
    drawBox "lines" linesContent infoX linesY

    let levelY = linesY + 3  
    let levelContent = [sprintf "%-8d" state.level]
    drawBox "level" levelContent infoX levelY

    System.Console.Out.Flush()

let collides (board: Board) (piece: Tetrimino) (row, col) =
    piece.Blocks 
    |> List.exists (fun (blockX, blockY) -> 
        let x = blockX + col
        let y = blockY + row
        
        x < 0 || x >= Array2D.length2 board || 
        y >= Array2D.length1 board || 
        y >= 0 && 
         match board.[y, x] with
          | Empty -> false
          | Filled _ -> true
    )

let tick (state: GameState) =
    let currx, curry = state.currentPosition
    let newPos = currx, curry + 1
    if collides state.board state.currentPiece newPos then
        let newPiece = state.nextPiece
        let newNextPiece = randomTetrimino state.rng

        let updatedBoard = 
            state.currentPiece.Blocks
            |> List.fold (fun board (blockX, blockY) ->
                let x = blockX + currx
                let y = blockY + curry
                if x >= 0 && x < Array2D.length2 board && y >= 0 && y < Array2D.length1 board then
                    board.[y, x] <- Filled state.currentPiece.Color
                board
            ) state.board       

        let startPosition = (4, 0)
        { state with currentPosition = startPosition
                     currentPiece = newPiece
                     nextPiece = newNextPiece
                     board = updatedBoard}
    else 
        { state with currentPosition = newPos }    

let rec gameLoop (state: GameState) = 
    if state.gameOver then
        System.Console.SetCursorPosition(0, Array2D.length1 state.board + 10)
        printfn "Game Over! Lines Cleared: %d" state.lines
    else
        drawGameState state
        let newState = tick state
        
        System.Threading.Thread.Sleep 400
        gameLoop newState

[<EntryPoint>]
let main argv =
    System.Console.OutputEncoding <- System.Text.Encoding.UTF8
    System.Console.CursorVisible <- false
    System.Console.Clear()
    let initial = createGameState()
    gameLoop initial
    0