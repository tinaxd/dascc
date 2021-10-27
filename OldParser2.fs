module DasParser

let intSpec = Sprache.Parse.String "int"

let reservedVars = 
    let gprNs =[0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15] in
    let gprs = List.map (fun n -> "GPR" + n.ToString()) gprNs in
    ["A"; "B"; "Out"; "In"] @ gprs

let varname =
    query {
        Sprache.Parse.Select(Sprache.Parse.Once(Sprache.Parse.Letter),
            fun x ->
        for rest in Sprache.Parse.Many(Sprache.Parse.LetterOrDigit) do
        select 
    }

let varname =
     Sprache.Parse.Once(Sprache.Parse.Letter) do
    for rest in Sprache.Parse.Many(Sprache.Parse.LetterOrDigit) do
    select 
