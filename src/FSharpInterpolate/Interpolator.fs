module Interpolation

type InterpolationMethod =
    | Lagrange of power: int16
    | Linear

let windowSize m =
    match m with
    | Lagrange pow -> int pow
    | Linear -> 2

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

let interpolate method step window =
    let startX, startY = Seq.head window
    let finishX, finishY = Seq.last window

    Seq.initInfinite (fun x -> startX + step * float x)
    |> Seq.takeWhile (fun x -> x <= finishX)
    |> Seq.map (fun x -> x, interpolator method window x)
