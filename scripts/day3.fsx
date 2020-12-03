open System.IO

type Level = { Spaces: Space [] }

and Space =
    | Empty = '.'
    | Tree = '#'

module Space =
    let FromChar (c: char) =
        if c = '#' then Space.Tree else Space.Empty

type Point = { X: int; Y: int }

type Map(levels: array<Level>) =
    let levels = levels
    let mutable current = (0, 0)

    let advToSpaceAt (p: Point) =
        let (x, y) = current

        let newX =
            let l = levels.[y]
            let adv = x + p.X
            if adv < l.Spaces.Length then adv else adv - l.Spaces.Length

        let newY = y + p.Y
        current <- (newX, newY)
        levels.[newY].Spaces.[newX]

    member m.Move(next: Point) =
        let y = snd current
        let newY = y + next.Y
        if newY >= levels.Length then None else advToSpaceAt next |> Some

    member m.Height() = levels.Length

    member m.Reset() = current <- (0, 0)

let parseLevel (line: string) =
    let spaces =
        line |> Seq.map Space.FromChar |> Seq.toArray

    { Spaces = spaces }

let parseMap (input: array<string>) =
    input |> Seq.map parseLevel |> Seq.toArray |> Map

let map =
    File.ReadAllLines("data/day3.txt") |> parseMap

let findTreesHit (movement: Point) =
    map.Reset()
    Seq.init (map.Height()) (fun _ -> movement)
    |> Seq.map map.Move
    |> Seq.filter (fun s -> s.IsSome && s.Value = Space.Tree)
    |> Seq.length
    |> int64

let treesHitAt31 = findTreesHit { X = 3; Y = 1 }
printfn "Trees Hit @(3,1): %i" treesHitAt31

[ treesHitAt31
  findTreesHit { X = 1; Y = 1 }
  findTreesHit { X = 5; Y = 1 }
  findTreesHit { X = 7; Y = 1 }
  findTreesHit { X = 1; Y = 2 } ]
|> Seq.reduce (*)
|> printfn "Discovery quotient: %i"
