open System
open System.Text.RegularExpressions

let (|RegexMatch2|_|) (pattern : string) (input : string) = 
    let result = Regex.Match(input, pattern)
    if result.Success then
        match (List.tail [for g in result.Groups -> g.Value]) with
        |  pre :: fst :: snd :: suff :: [ ] 
            ->  let x = float <| fst 
                let y = float <| snd
                Some(pre, x, y, suff)
        | _ -> failwithf "Match Succeeded, but unable to find elements."
    else None            

let (|Done|Add|Subs|Mult|Div|Equl|) = 
    function
    | RegexMatch2 @"(.+[\+\-\*\/])*([0-9.]+)\/([0-9.]+)([\+\-\*\/].+)*" (pre, fst, snd, suff)
        -> Div ( sprintf "%s%g%s" pre (fst / snd) suff)
    | RegexMatch2 @"(.+[\+\-\*])*([0-9.]+)\*([0-9.]+)([\+\-\*].+)*" (pre, fst, snd, suff)
        -> Mult ( sprintf "%s%g%s" pre (fst * snd) suff)
    | RegexMatch2 @"(.+[\+\-])*([0-9.]+)\+([0-9.]+)([\+\-].+)*" (pre, fst, snd, suff)
        -> Add ( sprintf "%s%g%s" pre (fst + snd) suff)
    | RegexMatch2 @"(.+[\-])*([0-9.]+)\-([0-9.]+)([\-].+)*" (pre, fst, snd, suff)
        -> Subs ( sprintf "%s%g%s" pre (fst - snd) suff)
    | RegexMatch2 @"([0-9.]+)\=([0-9.]+)" (pre, fst, snd, suff)
        -> Equl (fst = snd)
    | ans -> Done (ans)

[<EntryPoint>]
let main argv = 

    while true do
        let inn = Console.ReadLine().Trim().Replace(" ","")

        let rec calculate (input : string) =
            match input with
            | "E" -> ()
            | Div res | Mult res | Add res | Subs res
                -> calculate res
            | Equl resb
                -> printfn "%s => %b" inn resb
            | Done res
                -> printfn "%s => %s" inn res
        calculate inn
    0
