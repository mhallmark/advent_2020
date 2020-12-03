open System.IO

let key = 2020

let input =
    File.ReadAllLines("data/day1.txt")
    |> Array.map int

let inputSet = set (input)

let otherAddendExists (sum: int) (add: int) =
    let want = sum - add
    Set.contains want inputSet

// O(n log n) ?
let found1 =
    input |> Array.find (otherAddendExists key)

let answer1 = (key - found1) * found1

printfn "First Answer: %i" answer1

let mul (i, j, k) = i * j * k

// O(n) sq?
let answer2 =
    input
    |> Array.map (fun i ->
        let tkey = key - i

        let other =
            input |> Array.tryFind (otherAddendExists tkey)

        match other with
        | Some o ->
            let t = key - (i + o)
            Some(i, t, o)
        | None -> None)
    |> Array.filter Option.isSome
    |> Array.map Option.get
    |> Seq.map mul
    |> Seq.distinct
    |> Seq.exactlyOne

printfn "Second Answer: %i" answer2
