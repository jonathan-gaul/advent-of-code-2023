// For more information see https://aka.ms/fsharp-console-apps

let lines = System.IO.File.ReadLines("input.txt")

let total =
    lines 
    |> Seq.map (fun s ->
        let digits = s |> Seq.where System.Char.IsDigit
        let first = digits |> Seq.head
        let last = digits |> Seq.last

        int (first - '0') * 10 + int (last - '0'))
    |> Seq.sum

total |> printf "%d"