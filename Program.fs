open System

let noteToNumber note =
    let number =
        match note with
        | "B#" | "C"  -> 0
        | "C#" | "Db" -> 1
        | "D"         -> 2
        | "D#" | "Eb" -> 3
        | "E"  | "Fb" -> 4
        | "E#" | "F"  -> 5
        | "F#" | "Gb" -> 6
        | "G"         -> 7
        | "G#" | "Ab" -> 8
        | "A"         -> 9
        | "A#" | "Bb" -> 10
        | "B"  | "Cb" -> 11
        | _ -> failwith "Error: Unknown note"
    number

let numberToFlatNote number =
    let note =
        match number with
        | 0  -> "C"
        | 1  -> "Db"
        | 2  -> "D"
        | 3  -> "Eb"
        | 4  -> "E"
        | 5  -> "F"
        | 6  -> "Gb"
        | 7  -> "G"
        | 8  -> "Ab"
        | 9  -> "A"
        | 10 -> "Bb"
        | 11 -> "B"
        | _ -> failwith "Error: Out of bounds"
    note

let numberToSharpNote number =
    let note =
        match number with
        | 0  -> "C"
        | 1  -> "C#"
        | 2  -> "D"
        | 3  -> "D#"
        | 4  -> "E"
        | 5  -> "F"
        | 6  -> "F#"
        | 7  -> "G"
        | 8  -> "G#"
        | 9  -> "A"
        | 10 -> "A#"
        | 11 -> "B"
        | _  -> failwith "Error: Out of bounds"
    note

let nameToIntArr name =
    let intArr =
        match name with
        | "maj"     -> [|0; 4; 7|]
        | "min"     -> [|0; 3; 7|]
        | "aug"     -> [|0; 4; 8|]
        | "dim"     -> [|0; 3; 6|]
        | "maj6"    -> [|0; 4; 7; 9|]
        | "min6"    -> [|0; 3; 7; 9|]
        | "7"       -> [|0; 4; 7; 10|]
        | "maj7"    -> [|0; 4; 7; 11|]
        | "min7"    -> [|0; 3; 7; 10|]
        | "aug7"    -> [|0; 4; 8; 10|]
        | "dim7"    -> [|0; 3; 6; 9|]
        | "m7b5"    -> [|0; 3; 6; 10|]
        | "minmaj7" -> [|0; 3; 7; 11|]
        | "augm7"   -> [|0; 4; 8; 11|]
        | "dmaj7"   -> [|0; 3; 6; 11|]
        | "7b5"     -> [|0; 4; 6; 10|]
        | "maj7b5"  -> [|0; 4; 6; 11|]
        | "sus2"    -> [|0; 2; 7|]
        | "sus4"    -> [|0; 5; 7|]
        | _ -> failwith "Error: Unknown name"
    intArr

let convert (note:string) name =
    let flat = note.Contains('b')
    let sharp = note.Contains('#')
    let func =
        match name with
        | "maj"    | "aug"  | "maj6"
        | "maj7"   | "aug7" | "augm7"
        | "maj7b5" | "sus2" | "sus4" ->
            if flat then
               numberToFlatNote
            else
               numberToSharpNote
        | "min"  | "dim"     | "min6"
        | "7"    | "min7"    | "dim7"
        | "m7b5" | "minmaj7" | "dmaj7"
        | "7b5" ->
            if sharp then
                numberToSharpNote
            else
                numberToFlatNote
        | _ -> failwith "Error: Unknown name"
    func

let createChord note name =
    let noteNumber = noteToNumber note
    let intArr = nameToIntArr name
    let notes = Array.create intArr.Length noteNumber
    let notesZip = Array.zip notes intArr
    let convertFunc = convert note name
    let result =
        notesZip
        |> Array.map (fun (x, y) -> x + y)
        |> Array.map (fun x -> x % 12)
        |> Array.map convertFunc
    result

[<EntryPoint>]
let main(args : string[]) =
    if args.Length <> 2 then
        failwith "Error: Expected arguments <note> and <name>"
    let note, name = args.[0], args.[1]
    let result = createChord note name
    printfn "Chord: %A" result
    0
