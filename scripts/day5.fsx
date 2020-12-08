open System.IO

type Seat =
    { Row: int
      Column: int }
    member s.Id() = s.Row * 8 + s.Column

let seatId (s: Seat) = s.Id()

let halve (s: int array) (c: char) =
    let mid = s.Length / 2
    match c with
    | 'F'
    | 'L' -> s.[..mid - 1]
    | 'B'
    | 'R' -> s.[mid..]
    | _ -> failwithf "unsupported search key %c" c

let binLocate (arr: string) (size: int) =
    arr
    |> Seq.fold halve [| 0 .. size |]
    |> Seq.exactlyOne

let sep (l: string) = (l.[..6], l.[7..])

let locateSeat (rows: string, columns: string) =
    let row = binLocate rows 127
    let col = binLocate columns 7
    { Row = row; Column = col }

let seats =
    File.ReadAllLines("data/day5.txt")
    |> Seq.map (sep >> locateSeat)
    |> Seq.toArray

let max = seats |> Seq.maxBy seatId
printfn "Greatest Id: %i" (max.Id())

let min = seats |> Seq.minBy seatId
printfn "Min: %i" (min.Id())

let regSeatIds = seats |> Seq.map seatId |> Set.ofSeq

let foundSeat =
    seq { min.Id() .. 128 * max.Id() }
    |> Seq.tryFind (fun sid -> Set.contains sid regSeatIds |> not)

match foundSeat with
| Some sid -> printfn "Your seat is %i" sid
| None -> printfn "Yous aint got no seats sir..."
