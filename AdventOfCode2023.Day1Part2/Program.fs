// For more information see https://aka.ms/fsharp-console-apps

let lines = System.IO.File.ReadLines("input.txt")

let numbers = [ "zero"; "0"; "one"; "1"; "two"; "2"; "three"; "3"; "four"; "4"; "five"; "5"; "six"; "6"; "seven"; "7"; "eight"; "8"; "nine"; "9" ]

let expr = 
    numbers 
    |> fun s -> $"(?=.*?({s}))?"
    |> (+) "^"

let total =
    lines 
    |> Seq.map (fun s ->
        let first = 
            numbers            
            |> List.mapi (fun i num -> ((i + 2) / 2 - 1, s.IndexOf(num)))
            |> List.filter (fun (_, p) -> p >= 0)
            |> List.sortBy snd
            |> List.head
            |> fst

        let last = 
            numbers 
            |> List.mapi (fun i num -> ((i + 2) / 2 - 1, s.LastIndexOf(num)))
            |> List.filter (fun (_, p) -> p >= 0)
            |> List.sortByDescending snd
            |> List.head
            |> fst
            
        printf "%d / %d\n" first last 

        first * 10 + last)
    |> Seq.sum

total |> printf "%d"