let lines = System.IO.File.ReadLines("input.txt") |> Seq.toArray

let expr = "((?:\d+?)+)"
let operators = set "*?!#$@/#%=+-&"

let total = 
    lines
    |> Array.mapi (fun i line -> 
        let prev = if i <= 0 then "" else lines[i - 1]
        let next = if i < lines.Length - 1 then lines[i + 1] else ""

        System.Text.RegularExpressions.Regex.Matches(line, expr)
        |> Seq.map (fun m -> m.Index, System.Int32.Parse(m.Value), m.Value.Length)
        |> Seq.map (fun (i, n, l) -> 
            let surrounding =                     
                seq {
                    if i > 0 then yield line[i - 1]
                    if i + l < line.Length then yield line[i + l]
                    for p in [i-1..i+l] do    
                        if p >= 0 && p < prev.Length then yield prev[p]
                        if p >= 0 && p < next.Length then yield next[p]
                }

            let hasOperator = 
                surrounding                 
                |> set
                |> Set.intersect operators
                |> Set.isEmpty
                |> not
                    
            if hasOperator then n else 0)
        |> Seq.sum)
    |> Array.sum

printf "%d" total