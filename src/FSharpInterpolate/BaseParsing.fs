module BaseParsing

let parseFloat (s: string) =
    match
        System.Double.TryParse(
            s,
            System.Globalization.NumberStyles.Number,
            System.Globalization.CultureInfo.InvariantCulture
        )
    with
    | true, n -> Some n
    | _ -> None

let parseInt16 (s: string) =
    match
        System.Int16.TryParse(
            s,
            System.Globalization.NumberStyles.Number,
            System.Globalization.CultureInfo.InvariantCulture
        )
    with
    | true, n -> Some n
    | _ -> None
