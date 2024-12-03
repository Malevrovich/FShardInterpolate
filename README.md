`Скрябин Иван P34092`
`335146`

# Лабораторная работа 3

## Парсинг и валидация:
```f#
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
```

## Интерполяция:
```f#
type InterpolationMethod =
    | Lagrange of power: int16
    | Linear
```

```f#
let interpolator method window =
    match method with
    | Linear ->
        fun x ->
            let x1 = window |> Seq.head |> fst
            let x2 = window |> Seq.last |> fst
            let y1 = window |> Seq.head |> snd
            let y2 = window |> Seq.last |> snd

            y1 + (x - x1) * (y2 - y1) / (x2 - x1)
    | Lagrange power ->
        fun x ->
            window
            |> Seq.mapi (fun idx (x_i, y_i) ->
                y_i
                * (window
                   |> Seq.removeAt idx
                   |> Seq.map (fun (x_j, y_j) -> (x - x_j) / (x_i - x_j))
                   |> Seq.reduce (*)))
            |> Seq.sum
```

```f#
let windowedMap processors nums =
    let maxWindowSize = fst (processors |> Seq.maxBy fst)

    nums
    |> Seq.scan
        (fun (prevWindow, _) x ->
            let window =
                if List.length prevWindow < maxWindowSize then
                    List.append prevWindow [ x ]
                else
                    List.append (List.tail prevWindow) [ x ]

            let outSeq =
                processors
                |> Seq.map (fun (windowSize, proc) ->
                    if windowSize <= List.length window then
                        Some(window |> List.skip (List.length window - windowSize) |> proc)
                    else
                        None)

            window, outSeq)
        (List.empty, Seq.empty)
    |> Seq.map snd

let interpolateStream points methods step =
    let processors =
        methods
        |> Seq.map (fun m -> windowSize m, fun window -> interpolate m step window)

    points |> windowedMap processors |> Seq.skip 1
```

## Тесты

```f#
let checkFunction f methods points =
    interpolateStream points methods 0.1
    |> Seq.iter (fun out_seq ->
        out_seq
        |> Seq.iter (fun maybe_points ->
            match maybe_points with
            | Some out_seq -> out_seq |> Seq.iter (fun (x, y) -> Assert.True(abs (f x - y) <= 0.1))
            | None -> ()))

[<Property>]
let ``Check linear function`` (k: int, l: int list) =
    let f = fun (x: float) -> float k * x

    l
    |> List.sort
    |> List.distinct
    |> List.map (fun x -> (float x, float (f x)))
    |> checkFunction f [ Linear; Lagrange(int16 2) ]


[<Property>]
let ``Check square function`` (l: int list) =
    let f = fun (x: float) -> float x * x

    l
    |> List.sort
    |> List.distinct
    |> List.map (fun x -> (float x, float (f x)))
    |> checkFunction f [ Lagrange(int16 3) ]
```
