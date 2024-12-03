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

let windowedMap processors nums =
    let maxWindowSize = fst (processors |> Seq.maxBy fst)

    nums
    |> Seq.scan
        (fun (prevWindow, prev_out_seq) x ->
            let window =
                if List.length prevWindow < maxWindowSize then
                    List.append prevWindow [ x ]
                else
                    List.append (List.tail prevWindow) [ x ]

            let out_seq =
                processors
                |> Seq.map (fun (windowSize, proc) ->
                    if windowSize <= List.length window then
                        Some(window |> List.skip (List.length window - windowSize) |> proc)
                    else
                        None)

            window, out_seq)
        (List.empty, Seq.empty)
    |> Seq.map snd

[<EntryPoint>]
let main (args) =
    let parsedStep = BaseParsing.parseFloat args[0]

    let parsedMethods = args |> Seq.skip 1 |> Seq.map parseCLIArgMethod

    if parsedStep.IsSome && parsedMethods |> Seq.forall Result.isOk then
        let step = parsedStep.Value
        let methods = parsedMethods |> Seq.choose Result.toOption
        printfn "step: %.2f" step
        printfn "methods: %A" methods

        // let points =
        //     readLines
        //     |> Seq.map ParseValidation.parseAndValidate
        //     |> Seq.choose (fun parseRes ->
        //         match parseRes with
        //         | Ok point -> Some point
        //         | Error errList ->
        //             printfn "Found errors:"

        //             errList
        //             |> Seq.map ParseValidation.validationErrorToString
        //             |> Seq.reduce (fun x y -> x + "\n" + y)
        //             |> printfn "%s"

        //             printfn "Skipping line"
        //             None)
        //     |> Seq.cache
        let points =
            Seq.initInfinite (fun n -> (float) n)
            |> Seq.map (fun x -> (x, cos x))
            |> Seq.takeWhile (fun x -> fst x < (float) 100)


        let processors =
            methods
            |> Seq.map (fun m -> windowSize m, fun window -> interpolate m step window)

        points
        |> windowedMap processors
        |> Seq.map (fun out_seq ->
            out_seq
            |> Seq.zip methods
            |> Seq.iter (fun (m, maybe_points) ->
                match maybe_points with
                | Some out_seq ->
                    printfn "%A" m
                    out_seq |> Seq.map fst |> Seq.iter (printf "%0.2f\t")
                    printfn ""
                    out_seq |> Seq.map snd |> Seq.iter (printf "%0.2f\t")
                    printfn ""
                | None -> ())

            out_seq)
        |> Seq.skip 1
        |> Seq.fold
            (fun acc out_seq ->
                Seq.zip acc out_seq
                |> Seq.map (fun (acc_points, maybe_points) ->
                    match maybe_points with
                    | Some points -> Seq.append acc_points points |> Seq.sortBy fst |> Seq.distinctBy fst
                    | None -> acc_points))
            (Seq.init (Seq.length methods) (fun _ -> Seq.empty))
        |> Seq.zip methods
        |> Seq.map (fun (m, points) ->
            let x, y = points |> Seq.unzip

            Chart.Line(x, y)
            |> Chart.withTraceName (Name = m.ToString())
            |> Chart.withLineStyle (Width = 2.0, Dash = StyleParam.DrawingStyle.Solid))
        |> Seq.append
            [ let points =
                  Seq.initInfinite (fun n -> (float) n / 100.0)
                  |> Seq.map (fun x -> (x, cos x))
                  |> Seq.takeWhile (fun x -> fst x < 100.0)

              let x, y = points |> Seq.unzip

              Chart.Line(x, y)
              |> Chart.withTraceName (Name = "Orig")
              |> Chart.withLineStyle (Width = 2.0, Dash = StyleParam.DrawingStyle.Solid) ]
        |> Chart.combine
        |> Chart.show
        // |> Seq.fold
        //     (fun acc out_seq ->
        //         Seq.zip acc out_seq
        //         |> Seq.map (fun (acc_points, maybe_points) ->
        //             match maybe_points with
        //             | Some points -> Seq.append acc_points [ points ]
        //             | None -> acc_points))
        //     (Seq.init (Seq.length methods) (fun _ -> Seq.empty))
        // |> Seq.zip methods
        // |> Seq.iter (fun (m, points) ->
        //     printfn "%A" m

        //     points
        //     |> Seq.iter (fun part ->
        //         printf "|"
        //         part |> Seq.map fst |> Seq.iter (printf "%0.2f\t"))

        //     printfn ""

        //     points

        //     |> Seq.iter (fun part ->
        //         printf "|"
        //         part |> Seq.map snd |> Seq.iter (printf "%0.2f\t"))

        //     printfn "")
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
