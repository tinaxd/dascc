module Parser

open FSharpPlus

let sEmpty txt = String.length txt = 0

type ParseResult<'a> =
    | Yes of 'a * string // (result, rest)
    | No of string * string // (error, rest)
    // static member (>>=) (r, f : 'a -> 'b) =
    //     match r with
    //     | Yes(r, rest) -> Yes(f r, rest)
    //     | No(e, rest) -> No(e, rest)

type Parser<'a> =
    string -> ParseResult<'a>

exception ParserInternalException

let (>>=) (p : Parser<'a>) f : Parser<'b> =
    let parse txt =
        match p txt with
        | Yes(r, rest) -> Yes(f r, rest)
        | No(e, rest) -> No(e, rest)
    in parse

let (>>^) (r : ParseResult<'a>) f : ParseResult<'b> =
    match r with
    | Yes(r, re) -> Yes(f r, re)
    | No(e, re) -> No(e, re)

let runParser p txt =
    match p txt with
    | Yes(result, _) -> Ok(result)
    | No(e, _) -> Error(e)

let pchar ch : Parser<char> =
    let parse txt =
        if sEmpty txt
            then No("empty txt", txt)
            else
                if (char txt.[0]) = ch
                    then Yes(ch, txt.[1..])
                    else No("char mismatch", txt)
    in parse

let pchartoken (ch, token) =
    pchar ch >>= (fun _ -> token)

let many p : Parser<'a list>=
    let parse txt =
        let rec manyI p acc t =
            match p t with
            | Yes(result, rest) -> manyI p (List.append acc [result]) rest
            | No(_, rest) -> Yes(acc, rest)
        in
        manyI p [] txt
    in parse

let many1 p : Parser<'a list> =
    let parse txt =
        let rec manyI p acc count t =
            match p t with
            | Yes(result, rest) -> manyI p (List.append acc [result]) (count+1) rest
            | No(_, rest) -> (count, (acc, rest))
        in
        let (count, (lst, rest)) = manyI p [] 0 txt in
        if count = 0
            then No("no one matched", rest)
            else Yes(lst, rest)
    in parse

let tryP p : Parser<'a> =
    let parse txt =
        match p txt with
        | Yes(_) as y -> y
        | No(e, _) -> No(e, txt) // No with the old state
    in parse

let choose ps : Parser<'a> =
    let parse txt =
        let rec chooseInter ps txt =
            match ps with
            | [] -> No("no parser matched", txt)
            | x::xs ->
                match x txt with
                | Yes(_) as y -> y
                | No(_, rest) -> chooseInter xs rest
        in chooseInter ps txt
    in parse

let chooseLazy ps : Parser<'a> =
    let parse txt =
        let rec chooseInter (ps : Lazy<Parser<'a>> list) txt =
            match ps with
            | [] -> No("no parser matched", txt)
            | x::xs ->
                match x.Force() txt with
                | Yes(_) as y -> y
                | No(_, rest) -> chooseInter xs rest
        in chooseInter ps txt
    in parse

let concat ps : Parser<'a list> =
    let parse txt =
        let rec concatInter ps acc txt =
            match ps with
            | [] -> Yes(acc, txt)
            | x::xs ->
                match x txt with
                | No(e, r) -> No(e, r)
                | Yes(result, rest) ->
                    concatInter xs (List.append acc [result]) rest
        in concatInter ps [] txt
    in parse


let concat2 p1 p2 =
    let parse txt =
        match p1 txt with
        | Yes(r1, rest1) ->
            match p2 rest1 with
            | Yes(r2, rest2) -> Yes((r1, r2), rest2)
            | No(e, r2) -> No(e, r2)
        | No(e, r1) -> No(e, r1)
    in parse

let concat3 p1 p2 p3 =
    let parse txt =
        match p1 txt with
        | Yes(r1, rest1) ->
            match p2 rest1 with
            | Yes(r2, rest2) ->
                match p3 rest2 with
                | Yes(r3, rest3) -> Yes((r1,r2,r3), rest3)
                | No(e, r3) -> No(e, r3)
            | No(e, r2) -> No(e, r2)
        | No(e, r1) -> No(e, r1)
    in parse

let (.>>) p1 p2 =
    concat2 p1 p2 >>= (fun (r1, _) -> r1)

let (>>.) p1 p2 =
    concat2 p1 p2 >>= (fun (_, r2) -> r2)

let (..>>) p1 (p2 : Lazy<Parser<'b>>) =
    concat2 p1 (p2.Force()) >>= (fun (r1, _) -> r1)

let (>>..) p1 (p2 : Lazy<Parser<'b>>) =
    concat2 p1 (p2.Force()) >>= (fun (_, r2) -> r2)

let (<|>) p1 p2 = choose [p1; p2]

let pspace = choose [(pchar ' '); (pchar '\t'); (pchar '\n'); (pchar '\r')]

let pspaces = many pspace

let pspaces1 = many1 pspace

let pdigit =
    let digits = ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9'] in
    choose (List.map pchar digits)

let pdigits = many1 pdigit

let pstring str =
    (concat (List.map pchar (String.toList str))) >>= Array.ofList >>= System.String.Concat

let pstringtoken (str, token) =
    pstring str >>= (fun _ -> token)

let intExp =
    let lstToInt lst =
        let rec inter lst acc =
            match lst with
            | [] -> acc
            | x::xs -> inter xs (acc*10 + (int (x.ToString()))) in
        let num = inter lst 0 in
        AST.ImmExp(num)
    in pdigits >>= lstToInt

type OperatorChar =
    | ADD
    | SUB
    | MUL

let operatorExp =
    choose [pchartoken ('+', ADD); pchartoken ('-', SUB); pchartoken ('*', MUL)]

let rec expression = chooseLazy [lazy intExp; lazy parenExp]
and parenExp = (pchar '(') >>. pspaces >>.. lazy expression .>> pspaces .>> (pchar ')')
and operatorExp =
    exp1 = expression .>> spaces

let reservedVars = ["Out"; "In"]

let pReservedVars = choose (List.map pstring reservedVars)

let makeStorageAST outOrIn =
    match outOrIn with
    | "Out" -> Some(AST.OutPort)
    | "In" -> Some(AST.InPort)
    | _ -> None

let assignment = 
    let assignee = (pReservedVars .>> pspaces) in
    let exp = (pchar '=') >>. pspaces >>. expression in
    let makeAST (varname, expr) =
        match makeStorageAST varname with
        | None -> raise ParserInternalException
        | Some(storage) ->
            AST.Assignment(storage, expr)
    in
    (concat2 assignee exp) >>= makeAST
