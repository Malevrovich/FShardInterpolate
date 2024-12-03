open Interpolation

open Plotly.NET

let (|IsLinear|_|) (s: string) =
    if s.Equals("linear") then Some Linear else None

let (|IsLagrange|_|) (s: string) =
    match s.Split("=") with
    | [| "lagrange"; value |] ->
        match BaseParsing.parseInt16 value with
        | Some(int) -> Some(Lagrange(int))
        | None -> None
    | _ -> None

let parseCLIArgMethod (s: string) =
    match s with
    | IsLinear m -> Ok m
    | IsLagrange m -> Ok m
    | _ -> Error(sprintf "Unknown arg %s" s)

let readLines =
    Seq.initInfinite (fun _ ->
        try
            System.Console.ReadLine()
        with ex ->
            null)
    |> Seq.takeWhile (not << isNull)

[<EntryPoint>]
let main (args) =
    let parsedStep = BaseParsing.parseFloat args[0]

    let parsedMethods = args |> Seq.skip 1 |> Seq.map parseCLIArgMethod

    if parsedStep.IsSome && parsedMethods |> Seq.forall Result.isOk then
        let step = parsedStep.Value
        let methods = parsedMethods |> Seq.choose Result.toOption
        printfn "step: %.2f" step
        printfn "methods: %A" methods

        let points =
            readLines
            |> Seq.map ParseValidation.parseAndValidate
            |> Seq.choose (fun parseRes ->
                match parseRes with
                | Ok point -> Some point
                | Error errList ->
                    printfn "Found errors:"

                    errList
                    |> Seq.map ParseValidation.validationErrorToString
                    |> Seq.reduce (fun x y -> x + "\n" + y)
                    |> printfn "%s"

                    printfn "Skipping line"
                    None)
            |> Seq.cache

        interpolateStream points methods step
        |> Seq.iter (fun out_seq ->
            out_seq
            |> Seq.zip methods
            |> Seq.iter (fun (m, maybe_points) ->
                match maybe_points with
                | Some out_seq ->
                    printfn "%A" m
                    out_seq |> Seq.map fst |> Seq.iter (printf "%0.2f\t")
                    printfn ""
                    out_seq |> Seq.map snd |> Seq.iter (printf "%0.2f\t")
                    printfn "\n"
                | None -> ()))

        0
    else
        if parsedStep.IsNone then
            printfn "Unexpected step value: %s" args[0]

        parsedMethods
        |> Seq.choose (fun r ->
            match r with
            | Ok r -> None
            | Error s -> Some s)
        |> Seq.iter (printfn "%s")

        printfn "Usage: step interpolate [linear|lagrange={int16}]..."
        0
