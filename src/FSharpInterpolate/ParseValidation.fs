module ParseValidation

#if INTERACTIVE
#r "nuget: FSToolkit.ErrorHandling"
#endif

open FsToolkit.ErrorHandling


let (|IsFloat|_|) (s: string) = BaseParsing.parseFloat s

let (|IsEmpty|_|) (s: string) = if s <> "" then Some s else None

type ValidationError =
    | InvalidData of name: string * value: string
    | LineFormat of expected: string * value: string

let validationErrorToString err =
    match err with
    | InvalidData(name, value) -> sprintf "Parse error at %s: %s" name value
    | LineFormat(expected, value) -> sprintf "Wrong line format, expected '%s', got '%s'" expected value

let validateFloat (s: string) =
    match s with
    | IsFloat s -> Ok s
    | _ -> Error(InvalidData("Float", s))

let parseLine (s: string) =
    match s.Split() with
    | [| lhs; rhs |] -> Ok(lhs, rhs)
    | _ -> Error([ LineFormat("float float", s) ])

let parseFloatPair (lhs: string) (rhs: string) =
    validation {
        let! validLhs = validateFloat lhs
        and! validRhs = validateFloat rhs
        return (validLhs, validRhs)
    }

let parseAndValidate s =
    result {
        let! (lhs, rhs) = parseLine s
        return! parseFloatPair lhs rhs
    }
