open System.IO

type Credentials(atts: Map<string, string>) =
    let req =
        seq {
            "byr"
            "iyr"
            "eyr"
            "hgt"
            "hcl"
            "ecl"
            "pid"
        }
        |> Set.ofSeq

    let atts = atts

    member c.IsValid() = req |> Set.forall atts.ContainsKey

let parseInt (s: string) =
    match System.Int32.TryParse(s) with
    | (true, i) -> i
    | (false, _) ->
        printfn "%s" s
        0

let parseRows (lines: string seq) =
    let parseAtt (seg: string) = seg.Split(':')

    lines
    |> Seq.map (fun line -> line.Split(' ') |> Seq.map parseAtt)
    |> Seq.collect (Seq.map (fun att -> (att.[0], att.[1])))
    |> Map.ofSeq

let collectBatches (lines: string array) =
    let mutable batches = List.Empty
    let mutable curBatch = List.Empty

    for line in lines do
        if line = "" then
            do batches <- curBatch :: batches
               curBatch <- List.Empty
        else
            do curBatch <- line :: curBatch

    batches

File.ReadAllLines("data/day4.txt")
|> collectBatches
|> Seq.map (parseRows >> Credentials)
|> Seq.filter (fun c -> c.IsValid())
|> Seq.length
|> printfn "Valid: %i"
