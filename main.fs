open System

let noteToNumber note =
    let number =
        match note with
        | "B#" -> 0  | "C"  -> 0
        | "C#" -> 1  | "Db" -> 1
        | "D"  -> 2
        | "D#" -> 3  | "Eb" -> 3
        | "E"  -> 4  | "Fb" -> 4
        | "E#" -> 5  | "F"  -> 5
        | "F#" -> 6  | "Gb" -> 6
        | "G"  -> 7
        | "G#" -> 8  | "Ab" -> 8
        | "A"  -> 9
        | "A#" -> 10 | "Bb" -> 10
        | "B"  -> 11 | "Cb" -> 11
    number

let numberToFlatNote number =
    let note =
        match number with
        | 0 -> "C"
        | 1 -> "Db"
        | 2 -> "D"
        | 3 -> "Eb"
        | 4 -> "E"
        | 5 -> "F"
        | 6 -> "Gb"
        | 7 -> "G"
        | 8 -> "Ab"
        | 9 -> "A"
        | 10 -> "Bb"
        | 11 -> "B"
    note

let numberToSharpNote number =
    let note =
        match number with
        | 0 -> "C"
        | 1 -> "C#"
        | 2 -> "D"
        | 3 -> "D#"
        | 4 -> "E"
        | 5 -> "F"
        | 6 -> "F#"
        | 7 -> "G"
        | 8 -> "G#"
        | 9 -> "A"
        | 10 -> "A#"
        | 11 -> "B"
    note

let convertToFlat array = 
    array
    |> Array.map numberToFlatNote

let convertToSharp array = 
    array
    |> Array.map numberToSharpNote

let normalize number =
    number % 12

let major note = 
    let noteNumber = noteToNumber note
    let result =
        [|noteNumber; noteNumber + 4; noteNumber + 7|]
        |> Array.map normalize
    result

let minor note = 
    let noteNumber = noteToNumber note
    let result =
        [|noteNumber; noteNumber + 3; noteNumber + 7|]
        |> Array.map normalize
    result

[<EntryPoint>]
let main(args) =
    if args.[0] = "major" then
        let majorChord = major args.[1]
        if args.[1].Contains('b') then
            printfn "Major Chord: %A" (convertToFlat majorChord)
        else
            printfn "Major Chord: %A" (convertToSharp majorChord)
    elif args.[0] = "minor" then
        let minorChord = minor args.[1]
        if args.[1].Contains('#') then
            printfn "Minor Chord: %A" (convertToSharp minorChord)
        else
            printfn "Minor Chord: %A" (convertToFlat minorChord)
    0
