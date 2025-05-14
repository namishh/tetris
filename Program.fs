open System.Threading


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

let tetriminos = [
    { Name = "I"; Blocks = [(0,0); (1,0); (2,0); (3,0)]; Color = Cyan }
    { Name = "O"; Blocks = [(0,0); (1,0); (0,1); (1,1)]; Color = Yellow }
    { Name = "T"; Blocks = [(0,0); (1,0); (2,0); (1,1)]; Color = Purple }
    { Name = "S"; Blocks = [(1,0); (2,0); (0,1); (1,1)]; Color = Green }
    { Name = "Z"; Blocks = [(0,0); (1,0); (1,1); (2,1)]; Color = Red }
    { Name = "J"; Blocks = [(0,0); (0,1); (1,1); (2,1)]; Color = Blue }
    { Name = "L"; Blocks = [(2,0); (0,1); (1,1); (2,1)]; Color = Orange }
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

type Cell = Empty | Filled of Color
type Board = Cell[,]

type GameState = {
  board: Board
}

let rec gameLoop(tl: Tetrimino list) index = 
    System.Console.Clear()
    let tetrimino = tl.[index]

    printTetrimino tetrimino

    Thread.Sleep(500)

    let nextIndex = (index + 1) % tetriminos.Length
    
    gameLoop tetriminos nextIndex

[<EntryPoint>]
let main argv =
    gameLoop tetriminos 0

    0 