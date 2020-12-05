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

type Credentials(atts: Attribute seq) as this =
    do
        for att in atts do
            match att with
            | BirthYear by -> this.BirthYear <- by
            | IssueYear iy -> this.IssueYear <- iy
            | ExpirtaionYear ey -> this.ExpirationYear <- ey
            | Height h -> this.Height <- h
            | HairColor hc -> this.HairColor <- hc
            | EyeColor ec -> this.EyeColor <- ec
            | PassportId pid -> this.PassportId <- pid
            | CounrtyId cid -> this.CountryId <- cid

    new() = Credentials(Seq.empty<Attribute>)

    member val BirthYear = null with get, set
    member val IssueYear = null with get, set
    member val ExpirationYear = null with get, set
    member val Height = null with get, set
    member val HairColor = null with get, set
    member val EyeColor = null with get, set
    member val PassportId = null with get, set
    member val CountryId = null with get, set

    member c.IsValid() =
        let isNotNull = not << isNull

        let valid =
            c.BirthYear
            |> isNotNull
            && c.IssueYear |> isNotNull
            && c.ExpirationYear |> isNotNull
            && c.Height |> isNotNull
            && c.Height |> isNotNull
            && c.EyeColor |> isNotNull
            && c.PassportId |> isNotNull

        if not valid
        then do printfn "NOT VALID: %s" (c.ToString())

        valid

    member c.Update(att: Attribute) =
        match att with
        | BirthYear by -> c.BirthYear <- by
        | IssueYear iy -> c.IssueYear <- iy
        | ExpirtaionYear ey -> c.ExpirationYear <- ey
        | Height h -> c.Height <- h
        | HairColor hc -> c.HairColor <- hc
        | EyeColor ec -> c.EyeColor <- ec
        | PassportId pid -> c.PassportId <- pid
        | CounrtyId cid -> c.CountryId <- cid

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

type CredentialsCollection() =
    let mutable creds = [ Credentials() ]

    member _.Add(cred: Credentials) = creds <- cred :: creds

    member _.Head() = creds.Head

    member _.Creds() = creds
(*
    byr (Birth Year)
    iyr (Issue Year)
    eyr (Expiration Year)
    hgt (Height)
    hcl (Hair Color)
    ecl (Eye Color)
    pid (Passport ID)
    cid (Country ID)
*)

let parseInt (s: string) =
    match System.Int32.TryParse(s) with
    | (true, i) -> i
    | (false, _) ->
        printfn "%s" s
        0

let parseRow (line: string) =
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

    line.Split(' ') |> Seq.map parseAtt

let collectBatches (lines: string array) =
    seq {
        let mutable ls = List.empty<string>
        for line in lines do
            if line <> "" then ls <- line :: ls else yield ls
    }

let generator (creds: CredentialsCollection) (line: string) =
    if line = ""
    then do creds.Add(Credentials())
    else parseRow line |> Seq.iter (creds.Head()).Update
    creds

File.ReadAllLines("data/day4.txt")
|> collectBatches
|> Seq.map (fun rows ->
    rows
    |> Seq.map parseRow
    |> Seq.reduce Seq.append
    |> Credentials)
|> Seq.filter (fun c -> c.IsValid())
|> Seq.length
|> printfn "Valid: %i"
// File.ReadAllLines("data/day4.txt")
// |> Seq.fold generator (CredentialsCollection())
// |> (fun c -> c.Creds())
// |> Seq.filter (fun c -> c.IsValid())
// |> Seq.length
// |> printfn "Total Valid: %i"
