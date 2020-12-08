open System.IO

module Validate =
    // I hate regex...
    let private validColorChars = "0123456789abcdef" |> Set.ofSeq

    let private parseInt (s: string) = System.Int32.TryParse(s)

    let private yearValidator atleast atmost att =
        if (String.length att) <> 4 then
            false
        else
            match parseInt att with
            | (true, i) -> i >= atleast && i <= atmost
            | (false, _) -> false

    // byr (Birth Year) - four digits; at least 1920 and at most 2002.
    let BirthYear = yearValidator 1920 2002

    // iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    let IssueYear = yearValidator 2010 2020

    // eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    let ExpirationYear = yearValidator 2020 2030

    // hgt (Height) - a number followed by either cm or in:
    // If cm, the number must be at least 150 and at most 193.
    // If in, the number must be at least 59 and at most 76.
    let Height (att: string) =
        let numContent = (String.length att) - 2
        let num = att.[..numContent - 1]
        let uom = att.[numContent..]

        let uomValidator (i: int) =
            match uom with
            | "cm" -> i >= 150 && i <= 193
            | "in" -> i >= 59 && i <= 76
            | _ -> false

        match parseInt num with
        | (true, i) -> uomValidator i
        | (false, _) -> false

    // hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    let HairColor (att: string) =
        if att.[0] <> '#' || (String.length att) <> 7 then
            false
        else
            att.[1..]
            |> Seq.forall (fun c -> validColorChars |> Set.contains c)

    // ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    let EyeColor (att: string) =
        match att with
        | "amb"
        | "blu"
        | "brn"
        | "gry"
        | "grn"
        | "hzl"
        | "oth" -> true
        | _ -> false

    // pid (Passport ID) - a nine-digit number, including leading zeroes.
    let PassportId (att: string) = (String.length att) = 9

    let CountryId (_: string) = true


type Attribute =
    | BirthYear of string
    | IssueYear of string
    | ExpirtaionYear of string
    | Height of string
    | HairColor of string
    | EyeColor of string
    | PassportId of string
    | CounrtyId of string


type Credentials(atts: Attribute seq) =
    let mutable byr, iyr, eyr, hgt, hcl, ecl, pid, cid =
        (null, null, null, null, null, null, null, null)

    do
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

    member c.BirthYear = byr
    member c.IssueYear = iyr
    member c.ExpirationYear = eyr
    member c.Height = hgt
    member c.HairColor = hcl
    member c.EyeColor = ecl
    member c.PassportId = pid
    member c.CountryId = cid

    member c.ContainsReqFields() =
        let isNotNull = not << isNull

        c.BirthYear
        |> isNotNull
        && c.IssueYear |> isNotNull
        && c.ExpirationYear |> isNotNull
        && c.Height |> isNotNull
        && c.HairColor |> isNotNull
        && c.EyeColor |> isNotNull
        && c.PassportId |> isNotNull


    member c.IsValid() =
        if not (c.ContainsReqFields()) then
            false
        else
            seq {
                Validate.BirthYear c.BirthYear
                Validate.CountryId c.CountryId
                Validate.ExpirationYear c.ExpirationYear
                Validate.EyeColor c.EyeColor
                Validate.HairColor c.HairColor
                Validate.Height c.Height
                Validate.IssueYear c.IssueYear
                Validate.PassportId c.PassportId
            }
            |> Seq.forall (id)


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

let creds =
    File.ReadAllLines("data/day4.txt")
    |> collectBatches
    |> Seq.map (parseRows >> Credentials)

creds
|> Seq.filter (fun c -> c.ContainsReqFields())
|> Seq.length
|> printfn "Contains Req Fields: %i"

creds
|> Seq.filter (fun c -> c.IsValid())
|> Seq.length
|> printfn "Valid Passports: %i"
