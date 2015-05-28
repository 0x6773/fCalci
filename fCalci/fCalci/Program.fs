open System
open System.Text.RegularExpressions

let (|RegexMatch2|_|) (pattern : string) (input : string) = 
    let result = Regex.Match(input, pattern)
    if result.Success then
        match (List.tail [for g in result.Groups -> g.Value]) with
        | fst :: snd :: []   
            ->  let x = float <| fst 
                let y = float <| snd
                Some(x, y)
        | _ -> failwithf "Match Succeeded, but unable to find elements."
    else None            

let (|Zero|Add|Subs|Mult|Div|Equl|) = 
    function
    | RegexMatch2 @"([0-9.]+)\+([0-9.]+)" (fst, snd)
        -> Add (fst + snd)
    | RegexMatch2 @"([0-9.]+)\-([0-9.]+)" (fst, snd)
        -> Subs (fst - snd)
    | RegexMatch2 @"([0-9.]+)\*([0-9.]+)" (fst, snd)
        -> Mult (fst * snd)
    | RegexMatch2 @"([0-9.]+)\/([0-9.]+)" (fst, snd)
        -> Div (fst / snd)          
    | RegexMatch2 @"([0-9.]+)\=([0-9.]+)" (fst, snd)
        -> Equl (fst = snd)          
    | _ -> Zero

let help()=
    printfn "fCalci Help"
    printfn "Type 'E' to exit"
    ()

[<EntryPoint>]
let main argv = 

    let mutable inn = ""

    while inn.ToUpper() <> "E" do
        inn <- Console.ReadLine().Trim().Replace(" ","")

        match inn with
        | "E" -> ()
        | "hp" -> help()
        | Add res | Subs res | Mult res | Div res
            ->  printfn "%s => %g" inn res
        | Equl resb
            -> printfn "%s => %b" inn resb
        | Zero 
            -> printfn "0"
    0
