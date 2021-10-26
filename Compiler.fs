module Compiler

type Location =
    | GPR of Assembly.RegisterNumber
    | RegA
    | RegB
    | InPort
    | OutPort

type State = {
    Variables : Map<string, Location>;
}

let newState =
    {
        Variables = Map.empty
    }

let immediateMover loc =
        match loc with
        | GPR(n) ->
            Some(fun imm -> [
                Assembly.MOV_B_Imm imm;
                Assembly.MOV_GRP_B n
            ])
        | RegA ->
            Some(fun imm -> [
                Assembly.MOV_A_Imm imm;
            ])
        | RegB ->
            Some(fun imm -> [
                Assembly.MOV_B_Imm imm;
            ])
        | InPort -> None
        | OutPort ->
            Some(fun imm -> [
                Assembly.MOV_OUT_Imm imm;
            ])

let resolveLocation state store =
    match store with
    | AST.Variable(v) -> state.Variables.TryFind(v)
    | AST.InPort -> Some(InPort)
    | AST.OutPort -> Some(OutPort)

let rec movExpression state (loc : Location) (exp : AST.Expression) =
    match exp with
    | AST.ImmExp(imm) ->
        match (immediateMover loc) with
        | None -> None
        | Some(f) -> Some(f imm)
    | AST.AddExp(e1, e2) ->
        // e1 を RegA へ
        None
    | AST.SubExp(e1, e2) ->
        // e1 を RegA へ
        match movExpression state RegB e2 with
            | None -> None
            | Some(inst2) ->
                match movExpression state RegA e1 with
                | None -> None
                | Some(inst1) ->
                        Some(inst2 @ inst1 @ [
                            Assembly.SUB_A_B
                        ])
        
            

let compileAssignment state store exp =
    match resolveLocation state store with
    | Some(loc) -> movExpression state loc exp
    | None -> None

let declareVar state varname =
    let oks =
        let pred i = Seq.contains i (Seq.map (fun (_,a) -> a) (Map.toSeq state.Variables)) in
        List.filter pred (List.map (fun x -> GPR(x)) [0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15])
    in
    match oks with
    | [] -> None
    | x::_ ->
        ignore (state.Variables.Add(varname, x))
        Some(x)
 