type Tetrimino = { Name: string; Blocks: (int * int) list; Color: string }

let blockChar = "██"

let cyan = "\x1b[36m"
let yellow = "\x1b[33m"
let purple = "\x1b[35m"
let green = "\x1b[32m"
let red = "\x1b[31m"
let blue = "\x1b[34m"
let orange = "\x1b[38;5;208m"
let reset = "\x1b[0m"

let tetriminos = [
    { Name = "I"; Blocks = [(0,0); (1,0); (2,0); (3,0)]; Color = cyan }
    { Name = "O"; Blocks = [(0,0); (1,0); (0,1); (1,1)]; Color = yellow }
    { Name = "T"; Blocks = [(0,0); (1,0); (2,0); (1,1)]; Color = purple }
    { Name = "S"; Blocks = [(1,0); (2,0); (0,1); (1,1)]; Color = green }
    { Name = "Z"; Blocks = [(0,0); (1,0); (1,1); (2,1)]; Color = red }
    { Name = "J"; Blocks = [(0,0); (0,1); (1,1); (2,1)]; Color = blue }
    { Name = "L"; Blocks = [(2,0); (0,1); (1,1); (2,1)]; Color = orange }
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
                printf "%s%s%s" tetrimino.Color blockChar reset
            else
                printf "  "
        printfn ""
    printfn ""

tetriminos |> List.iter printTetrimino

while true do
    () 