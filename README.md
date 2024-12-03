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

Пример выполнения:
```
malevrovich@NB-2814:~/fsharp/FShardInterpolate/src/FSharpInterpolate$ dotnet run 1.0 linear lagrange=4
step: 1.00
methods: seq [Linear; Lagrange 4s]
0 0
1.571 1
Linear
0.00    1.00
0.00    0.64

3.142 0
Linear
1.57    2.57
1.00    0.36

4.712 -1
Linear
3.14    4.14
0.00    -0.64

Lagrange 4s
0.00    1.00    2.00    3.00    4.00
0.00    0.97    0.84    0.12    -0.67

12.568 0
Linear
4.71    5.71    6.71    7.71    8.71    9.71    10.71   11.71
-1.00   -0.87   -0.75   -0.62   -0.49   -0.36   -0.24   -0.11

Lagrange 4s
1.57    2.57    3.57    4.57    5.57    6.57    7.57    8.57    9.57    10.57   11.57
1.00    0.37    -0.28   -0.91   -1.49   -1.95   -2.26   -2.38   -2.25   -1.84   -1.11
```

```
malevrovich@NB-2814:~/fsharp/FShardInterpolate/src/FSharpInterpolate$ dotnet run 1.0 linear lagrange=4
step: 1.00
methods: seq [Linear; Lagrange 4s]
-1 
Found errors:
Parse error at Float: 
Skipping line
2 a
Found errors:
Parse error at Float: a
Skipping line
```

```
malevrovich@NB-2814:~/fsharp/FShardInterpolate/src/FSharpInterpolate$ dotnet run 1.0 lineeear lagrange=4
Unknown arg lineeear
Usage: step interpolate [linear|lagrange={int16}]...
```
