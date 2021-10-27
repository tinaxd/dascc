module Parser

open FParsec

let ws = spaces
let ws1 = spaces1
let pLineEnd = pstring ";"

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let intSpec = pstring "int"

let reservedVars = 
    let gprNs =[0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15] in
    let gprs = List.map (fun n -> "GPR" + n.ToString()) gprNs in
    ["A"; "B"; "Out"; "In"] @ gprs

let parseVarname =
    let synth a b = a.ToString() + b.ToString() in
    let name = pipe2 (asciiLetter) (many (asciiLetter <|> digit)) synth in
    name
    // match name with
    // | Success(name, _, _) ->
    //     if List.contains reservedVars name
    //         then Reply(Error, ErrorMessageList(ErrorMessage.Unexpected("found reserved varname")))
    //         else Reply(name)

let parseInteger =
    let rec lstToInt lst acc =
        match lst with
        | [] -> acc
        | x::xs ->
            let xInt = int x in
            lstToInt xs (acc * 10 + xInt)
    (many1 digit) |>> lstToInt

let rec parseExpression =
    parseInteger >>? (attempt parseParenExp)
and parseParenExp = pchar '(' >>. parseExpression .>> pchar ')'

let varDecl =
    let pVarname = intSpec >>. ws1 >>. parseVarname .>> (pstring "=") in
    let pExp = parseExpression .>> pLineEnd in
    pipe2 pVarname pExp (fun name exp -> (name, exp))

let parseAll source = run parser source
