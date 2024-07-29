open System
open System.IO


type grid = int[,]

type indexpair = int * int

type record = Map<int, indexpair list>

let createInitialGrid () : grid =
    Array2D.init 8 14 (fun _ _ -> Random().Next(0, 10))

//let get112 (g: grid) (x: int) : int = g[x/14,x%14]

//let getpair (g: grid) ((i, j): indexpair) : int = g[i,j]

let adjacent ((a, b): indexpair) ((c, d): indexpair) : bool =
    abs (a - c) < 2 && abs (b - d) < 2 && not (a = c && b = d)

let replaceElement (arr: 'a[,]) (row: int) (col: int) (newValue: 'a) : 'a[,] =
    let newArr = Array2D.copy arr
    newArr[row, col] <- newValue
    newArr

let rec score (g: grid) : int =
    let rec scloop (i: int) (j: int) (mem: record) : record =
        if i = 7 && j = 14 then mem else
        if j = 14 then scloop (i + 1) 0 mem else
        let x = g[i,j]
        let l = mem[x]
        scloop i (j + 1) <| Map.add x ((i, j) :: l) mem
    let mem = scloop 0 0 <| Map<int, indexpair list>([for u in 0..9 -> (u, [])])
    let rec loop (sc: int) =
        let rec loop2 (acc: indexpair list list) (w: int) : indexpair list list =
            if w = 0 then acc else
            loop2 (mem[w % 10] :: acc) <| w / 10
        let scl = loop2 [] sc
        let cando (lst: indexpair list list) : bool =
            let rec dfs (index: int) (current: indexpair) (choices: indexpair list) =
                if index = scl.Length then true else
                let currentList = List.item index lst
                List.exists (fun pr ->
                    if adjacent pr current then dfs (index + 1) pr <| pr :: choices
                    else false) currentList
            match lst with
            | [] -> false
            | h :: _ -> List.exists (fun pr -> dfs 1 pr [pr]) h
        if cando scl then loop <| sc + 1 else sc - 1
    loop 1

let printgrid (g: grid) (sc: int) =
    for i = 0 to 7 do
        for j = 0 to 13 do
            printf "%d" g[i,j]
        done
        printfn ""
    done
    printfn "score = %d\n" sc

let readgrid () : grid =
    let rec loop (r: int) (acc: int list list) : int list list =
        if r = 8 then acc else
        let row = Console.ReadLine() |> (fun x -> [for i in 0..13 -> x[i] - '0' |> int])
        loop (r + 1) <| row :: acc
    //loop 0 [] |> array2D
    loop 0 [] |> List.rev |> array2D

let rec loop (d: int) (g: grid) : bool =
    let mutable currentgrid = Array2D.copy g
    let currentscore = score g
    printgrid g currentscore
    let mutable nextscore = currentscore
    let mutable warehouse: grid list = []
    for i = 0 to 7 do
        for j = 0 to 13 do
            let e = currentgrid[i,j]
            for x = 0 to 9 do
                if x <> e then
                    let tempgrid = replaceElement currentgrid i j x
                    let s = score tempgrid
                    if s > nextscore then
                        nextscore <- s
                        currentgrid <- tempgrid
                    if s = nextscore then
                        warehouse <- tempgrid :: warehouse
            done
        done
    done
    if nextscore > 5884 then
        printfn "################\n################\n"
        printgrid currentgrid nextscore
        printfn "\n################\n################"
        true
    else if nextscore > currentscore then
        loop d currentgrid
    else
        let len = warehouse.Length
        let mutable count = 0
        let rec bad (ll: grid list) : bool =
            match ll with
            | [] -> printfn ""; false
            | h :: t ->
                count <- count + 1
                printfn "depth %d: searching (%d/%d)..." d count len
                let rec loop2 (x: int) : bool =
                    if x = 1120 then bad t else
                    let p, q, r = x / 140, (x % 140) / 10, x % 10
                    if r = h[p,q] then loop2 <| x + 1 else
                    let tempgrid = replaceElement h p q r
                    let s = score tempgrid
                    if s > nextscore then
                        let nxt = loop (d + 1) tempgrid
                        if not nxt then loop2 <| x + 1
                        else true
                    else
                        loop2 <| x + 1
                loop2 0
        bad warehouse

let rec exec () =
    let initialgrid : grid = createInitialGrid ()
    //let initialgrid : grid = readgrid ()
    let res = initialgrid |> loop 0
    if not res then exec ()

let () = exec ()
