open System.IO

type Attribute =
    | BirthYear of string
    | IssueYear of string
    | ExpirtaionYear of string
    | Height of string
    | HairColor of string
    | EyeColor of string
    | PassportId of string
    | CounrtyId of string

type Credentials =
    { BirthYear: string
      IssueYear: string
      ExpirationYear: string
      Height: string
      HairColor: string
      EyeColor: string
      PassportId: string
      CountryId: string }

    member c.IsValid() =
        let isNotNull = not << isNull

        c.BirthYear
        |> isNotNull
        && c.IssueYear |> isNotNull
        && c.ExpirationYear |> isNotNull
        && c.Height |> isNotNull
        && c.HairColor |> isNotNull
        && c.EyeColor |> isNotNull
        && c.PassportId |> isNotNull

    override c.ToString() =
        sprintf
            "BirthYear: %s; IssueYear: %s; ExpirationYear: %s; Height: %s; HairColor: %s; EyeColor: %s; PassportId: %s; CountryId: %s"
            c.BirthYear
            c.IssueYear
            c.ExpirationYear
            c.Height
            c.HairColor
            c.EyeColor
            c.PassportId
            c.CountryId

let makeCreds (atts: Attribute seq) =
    let mutable byr, iyr, eyr, hgt, hcl, ecl, pid, cid =
        (null, null, null, null, null, null, null, null)

    for att in atts do
        match att with
        | BirthYear by -> byr <- by
        | IssueYear iy -> iyr <- iy
        | ExpirtaionYear ey -> eyr <- ey
        | Height h -> hgt <- h
        | HairColor hc -> hcl <- hc
        | EyeColor ec -> ecl <- ec
        | PassportId p -> pid <- p
        | CounrtyId c -> cid <- c

    let creds =
        { BirthYear = byr
          IssueYear = iyr
          ExpirationYear = eyr
          Height = hgt
          HairColor = hcl
          EyeColor = ecl
          PassportId = pid
          CountryId = cid }

    creds

let parseRows (lines: string seq) =
    let parseAtt (seg: string) =
        match seg.Split(':') with
        | [| "byr"; v |] -> BirthYear v
        | [| "iyr"; v |] -> IssueYear v
        | [| "eyr"; v |] -> ExpirtaionYear v
        | [| "hgt"; v |] -> Height v
        | [| "hcl"; v |] -> HairColor v
        | [| "ecl"; v |] -> EyeColor v
        | [| "pid"; v |] -> PassportId v
        | [| "cid"; v |] -> CounrtyId v
        | a when a.Length <> 2 -> failwith (sprintf "parse error for line '%s' - expected length of 2" seg)
        | [| att; _ |] -> failwith (sprintf "unknown attribute %s" att)
        | _ -> failwith (sprintf "invalid value for line '%s'" seg)

    lines
    |> Seq.map (fun line -> line.Split(' ') |> Seq.map parseAtt)
    |> Seq.reduce Seq.append

let collectBatches (lines: string array) =
    let mutable batches = List.Empty
    let mutable curBatch = List.Empty

    for line in lines do
        if line = "" then
            do batches <- curBatch :: batches
               curBatch <- List.Empty
        else
            do curBatch <- line :: curBatch

    batches <- curBatch :: batches

    batches

File.ReadAllLines("data/day4.txt")
|> collectBatches
|> Seq.map (parseRows >> makeCreds)
|> Seq.filter (fun c -> c.IsValid())
|> Seq.length
|> printfn "Valid: %i"
