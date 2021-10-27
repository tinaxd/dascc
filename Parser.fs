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

let pAsciiChar : Parser<char> =
    let parse txt =
        if sEmpty txt
            then No("empty txt", txt)
            else
                if System.Char.IsLetterOrDigit(char txt.[0])
                    then Yes(txt.[0], txt.[1..])
                    else No("not a ascii char", txt)
    in parse

let pAsciiNonDigit : Parser<char> =
    let parse txt =
        if sEmpty txt
            then No("empty txt", txt)
            else
                if System.Char.IsLetter(char txt.[0])
                    then Yes(txt.[0], txt.[1..])
                    else No("not a ascii char", txt)
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

let lazyTryP (p : Lazy<Parser<'a>>) : Parser<'a> =
    let parse txt =
        match p.Force() txt with
        | Yes(_) as y -> y
        | No(e, _) -> No(e, txt)
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

let stmtSep = pchar ';'

type OperatorChar =
    | ADD
    | SUB
    | MUL

let pOperator =
    choose [pchartoken ('+', ADD); pchartoken ('-', SUB); pchartoken ('*', MUL)]

let pAddOperator = choose [pchartoken ('+', ADD); pchartoken ('-', SUB)]
let pMulOperator = choose [pchartoken ('*', MUL)]

let variable =
    concat2 pAsciiNonDigit (many pAsciiChar)
    >>= fun (first, rest) -> first::rest
    >>= Array.ofList
    >>= System.String.Concat

let varExp = variable >>= AST.VarExp

let rec expression1 =
    let parenExp = lazy ((pchar '(') >>. pspaces >>.. lazyExpression .>> pspaces .>> (pchar ')')) in
    lazy (chooseLazy[parenExp; lazy intExp; lazy varExp;])
and expression2 = lazy(
    let mulOperatorExp = lazy(
        let exp1 = expression1.Force() .>> pspaces in
        let op = pMulOperator .>> pspaces in
        let exp2 = expression2.Force() in
        concat3 exp1 op exp2 >>= fun (exp1, op, exp2) ->
            match op with
            | ADD -> AST.AddExp(exp1, exp2)
            | SUB -> AST.SubExp(exp1, exp2)
            | MUL -> AST.MulExp(exp1, exp2)
        )
    in chooseLazy [lazy (lazyTryP mulOperatorExp); expression1]
    )
and expression3 = lazy(
    let addOperatorExp = lazy(
        let exp1 = expression2.Force() .>> pspaces in
        let op = pAddOperator .>> pspaces in
        let exp2 = expression3.Force() in
        concat3 exp1 op exp2 >>= fun (exp1, op, exp2) ->
            match op with
            | ADD -> AST.AddExp(exp1, exp2)
            | SUB -> AST.SubExp(exp1, exp2)
            | MUL -> AST.MulExp(exp1, exp2)
        )
    in chooseLazy [lazy (lazyTryP addOperatorExp); expression2]
    )
and lazyExpression = expression3
and expression = expression3.Force()

let reservedVars = ["Out"; "In"]

let pReservedVars = choose (List.map pstring reservedVars)

let makeStorageAST outOrIn =
    match outOrIn with
    | "Out" -> Some(AST.OutPort)
    | "In" -> Some(AST.InPort)
    | s -> Some(AST.Variable(s))

let assignment = 
    let assignee = (variable .>> pspaces) in
    let exp = (pchar '=') >>. pspaces >>. expression .>> pspaces .>> stmtSep in
    let makeAST (varname, expr) =
        match makeStorageAST varname with
        | None -> raise ParserInternalException
        | Some(storage) ->
            AST.Assignment(storage, expr)
    in
    (concat2 assignee exp) >>= makeAST

let declAssignment =
    let assignee = (pstring "int ") >>. pspaces >>. variable .>> pspaces in
    let exp = (pchar '=') >>. pspaces >>. expression .>> pspaces .>> stmtSep in
    let makeAST (varname, expr) =
        match makeStorageAST varname with
        | None -> raise ParserInternalException
        | Some(storage) ->
            AST.Assignment(storage, expr)
    in
    (concat2 assignee exp) >>= makeAST


let statement =
    choose [(tryP declAssignment); assignment]

let program = many statement
