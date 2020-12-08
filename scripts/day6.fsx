open System.IO

let collectAnswers (lines: string array) =
    seq {
        let mutable curAnswers = ""

        for line in lines do
            if line = "" then
                yield curAnswers |> Seq.distinct |> Seq.length
                curAnswers <- ""
            else
                do curAnswers <- curAnswers + line

        yield curAnswers |> Seq.distinct |> Seq.length
    }

let lines = File.ReadAllLines("data/day6.txt")

lines
|> collectAnswers
|> Seq.sum
|> printfn "Yesses: %i"

let collectAllAnswered (lines: string array) =
    seq {
        let mutable (people, curAnswers) = (0, "")

        let collectGroupYesses () =
            curAnswers
            |> Seq.groupBy (id)
            |> Seq.map (fun (c, ss) -> (c, Seq.length ss))
            |> Seq.filter (fun (_, l) -> l = people)
            |> Seq.length

        for line in lines do
            if line = "" then
                yield collectGroupYesses ()
                people <- 0
                curAnswers <- ""
            else
                do curAnswers <- curAnswers + line
                   people <- people + 1

        yield collectGroupYesses ()
    }

lines
|> collectAllAnswered
|> Seq.sum
|> printfn "All Yesses: %i"
