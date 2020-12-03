open System.IO

type Requirement =
    { Char: char
      I1: int
      I2: int }

    member req.ValidateByCountRange(pass: string) =
        let charCount =
            pass
            |> Seq.sumBy (fun c -> if c = req.Char then 1 else 0)

        charCount >= req.I1 && charCount <= req.I2

    member req.ValidateByEXORPosition(pass: string) =
        let i1 = req.I1 + 1
        let i2 = req.I2 + 1
        let i1c = pass.[req.I1 - 1]
        let i2c = pass.[req.I2 - 1]
        i1c <> i2c && (i1c = req.Char || i2c = req.Char)

let parse (line: string) =
    let segs = line.Split(':')
    let pass = segs.[1].Trim()
    let reqSegs = segs.[0].Split(' ')
    let c = reqSegs.[1]
    let rangeSegs = reqSegs.[0].Split('-')

    ({ Char = char (c)
       I1 = int (rangeSegs.[0])
       I2 = int (rangeSegs.[1]) },
     pass)


let passIsValidPerOldSledJobRequirements (req: Requirement, pass: string) = req.ValidateByCountRange pass

let passIsValidPerTobogganCorporatePolicy (req: Requirement, pass: string) = req.ValidateByEXORPosition pass

let passWithReqs =
    File.ReadAllLines("data/day2.txt")
    |> Seq.map parse

passWithReqs
|> Seq.filter passIsValidPerOldSledJobRequirements
|> Seq.length
|> printfn "Valid Passwords Per Sled Job Policy: %i"

passWithReqs
|> Seq.filter passIsValidPerTobogganCorporatePolicy
|> Seq.length
|> printfn "Valid Passwords Per Toboggan Corporate Policy: %i"
