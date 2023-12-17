
let lines = System.IO.File.ReadLines("input.txt")

let trim = System.StringSplitOptions.TrimEntries

type Round = 
    { red: int 
      green: int
      blue: int }

type Game =
    { gameId: int 
      rounds: Round list }

let valueOr0 n (d: System.Collections.Generic.IDictionary<string, int>) = match d.TryGetValue(n) with | true, v -> v | _ -> 0

let games = 
    lines
    |> Seq.map (fun s -> 
        let parts = s.Split(":", trim)

        let game = 
            { gameId = parts |> Seq.head |> (fun g -> g.Split(" ") |> Seq.skip 1 |> Seq.head) |> int
              rounds = 
                parts 
                |> Seq.last 
                |> (fun gameRounds -> 
                    gameRounds.Split(";", trim)
                    |> Seq.map(fun roundText -> 
                        roundText.Split(",", trim)
                        |> Seq.map(fun e -> 
                            let entryParts = e.Split(" ", trim)
                            (entryParts[1], int entryParts[0]))
                        |> dict)                
                    |> Seq.map(fun colours -> 
                        { red = colours |> valueOr0 "red"
                          green = colours |> valueOr0 "green"
                          blue = colours |> valueOr0 "blue" } ))
                |> Seq.toList } 

        game)

let total =
    games
    |> Seq.map (fun g ->
        let maxRed = g.rounds |> List.map (fun r -> r.red) |> List.max
        let maxGreen = g.rounds |> List.map (fun r -> r.green) |> List.max
        let maxBlue = g.rounds |> List.map (fun r -> r.blue) |> List.max
        maxRed * maxGreen * maxBlue )
    |> Seq.sum

printf "%d" total