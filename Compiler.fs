module Compiler

type Location =
    | GPR of Assembly.RegisterNumber
    | RegA
    | RegB
    | InPort
    | OutPort

type State = {
    Variables : Map<string, Location>;
    UsedRegisters : Set<int>;
}

let newState =
    {
        Variables = Map.empty
        UsedRegisters = Set.empty
    }

let allRegisters = [0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15]

let findFreeRegisters state = List.filter (fun x -> not (Set.contains x state.UsedRegisters)) allRegisters

let useTempRegister state =
    match findFreeRegisters state with
    | [] -> (None, state)
    | x::_ ->
        let newState = {
                state with
                    UsedRegisters = Set.add x state.UsedRegisters
            } in
        (Some(x), newState)

let registerVariable state varname =
    let availables = findFreeRegisters state in
    match availables with
    | [] -> (None, state)
    | x::_ ->
        (Some(x), {
            state with
                Variables = Map.add varname (GPR(x)) state.Variables;
                UsedRegisters = Set.add x state.UsedRegisters;
        })

let immediateMover loc =
        match loc with
        | GPR(n) ->
            Some(fun imm -> [
                Assembly.MOV_B_Imm imm;
                Assembly.MOV_GPR_B n
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

exception CompilerException

let resolveLocation state store =
    match store with
    | AST.Variable(v) -> state.Variables.TryFind(v)
    | AST.InPort -> Some(InPort)
    | AST.OutPort -> Some(OutPort)

let moveFromA loc =
    match loc with
    | RegA -> []
    | RegB -> [Assembly.MOV_B_A]
    | GPR(n) -> [Assembly.MOV_B_A; Assembly.MOV_GPR_B(n)]
    | OutPort -> [Assembly.MOV_B_A; Assembly.MOV_OUT_B]
    | InPort -> raise CompilerException

let moveFromB loc =
    match loc with
    | RegB -> []
    | RegA -> [Assembly.MOV_A_B]
    | GPR(n) -> [Assembly.MOV_GPR_B(n)]
    | OutPort -> [Assembly.MOV_OUT_B]
    | InPort -> raise CompilerException

let movFromGPR loc gpr =
    match loc with
    | RegB -> Ok([Assembly.MOV_B_GPR gpr])
    | RegA -> Ok([Assembly.MOV_B_GPR gpr; Assembly.MOV_A_B])
    | OutPort -> Ok([Assembly.MOV_B_GPR gpr; Assembly.MOV_OUT_B])
    | InPort -> Error("cannot mov to In")
    | GPR(n) -> Ok([Assembly.MOV_B_GPR gpr; Assembly.MOV_GPR_B n])

let moveFromIn loc =
    match loc with
    | RegB -> Ok([Assembly.MOV_B_IN])
    | RegA -> Ok([Assembly.MOV_B_IN; Assembly.MOV_A_B])
    | OutPort -> Ok([Assembly.MOV_B_IN; Assembly.MOV_OUT_B])
    | InPort -> Error("cannot mov to In")
    | GPR(n) -> Ok([Assembly.MOV_B_IN; Assembly.MOV_GPR_B n])

let rec movExpression state (loc : Location) (exp : AST.Expression) =
    match exp with
    | AST.ImmExp(imm) ->
        match (immediateMover loc) with
        | None -> Error("cannot mov to loc (== InPort)")
        | Some(f) -> Ok(f imm)
    | AST.AddExp(e1, e2) ->
        // e2 ??? -1 ????????? movExpression
        movExpression state loc (AST.SubExp(e1, AST.MulExp(e2, AST.ImmExp(15))))
    | AST.SubExp(e1, e2) ->
        movSubExp state loc e1 e2
    | AST.MulExp(e1, e2) ->
        movMulExp state loc e1 e2
    | AST.VarExp(v) ->
        match resolveLocation state (AST.Variable(v)) with
        | None -> Error(sprintf "cannot locate variable %A" v)
        | Some(GPR(gpr)) -> movFromGPR loc gpr
        | Some(_) -> raise CompilerException // unreachable
    | AST.InExp ->
        moveFromIn loc
    | AST.OutExp ->
        Error("cannot mov from Out")

and movSubExp state loc e1 e2 =
    // e2 ??? RegB ?????????
        match movExpression state RegB e2 with
        | Error(_) as e -> e
        | Ok(inst2) ->
            // e2 ?????????????????? GPR ?????????
            let (gpr, state2) = useTempRegister state in
            match gpr with
            | None -> Error("insufficient registers")
            | Some(gpr) ->
                let taihi = [
                        Assembly.MOV_GPR_B(gpr)
                    ] in
                // e1 ??? RegA ?????????
                match movExpression state2 RegA e1 with
                | Error(_) as e -> e
                | Ok(inst1) ->
                        // e2 ?????????????????? GPR ?????? RegB ?????????
                        let fukki = [Assembly.MOV_B_GPR(gpr)] in
                        Ok(inst2 @ taihi @ inst1 @ fukki @ [
                            Assembly.SUB_A_B
                        ] @ (moveFromA loc))

and movMulExp state loc e1 e2 =
    // e2 ??? RegB ?????????
    match movExpression state RegB e2 with
    | Error(_) as e -> e
    | Ok(inst2) ->
        let (gpr, state2) = useTempRegister state in
        match gpr with
        | None -> Error("insufficient registers")
        | Some(gpr) ->
            let taihi = [Assembly.MOV_GPR_B(gpr)] in
            match movExpression state2 RegA e1 with
            | Error(_) as e -> e
            | Ok(inst1) ->
                let fukki = [Assembly.MOV_B_GPR(gpr)] in
                // ??????4???????????????????
                Ok(inst2 @ taihi @ inst1 @ fukki @ [Assembly.MULT_A_B] @ (moveFromB loc))

let compileAssignment state store exp =
    match resolveLocation state store with
    | Some(loc) -> movExpression state loc exp
    | None -> Error(sprintf "cannot resolve location for %A" store)

let compileDeclaration state varname initExp =
    match registerVariable state varname with
    | (Some(gpr), newState) ->
        match movExpression newState (GPR(gpr)) initExp with
        | Ok(mov) -> Ok(mov, newState)
        | Error(e) -> Error(e, newState)
    | (None, newState) -> Error("cannot allocate variable on register", newState)

let compileStatement state stmt =
    let inject newState result =
        match result with
        | Ok(r) -> Ok(r, newState)
        | Error(e) -> Error(e, newState) in
    match stmt with
    | AST.Assignment(storage, exp) -> inject state (compileAssignment state storage exp)
    | AST.VarDeclaration(varname, initExp) -> compileDeclaration state varname initExp

let compileStatements state stmts =
    let rec inter newState stmts acc =
        match stmts with
        | [] -> Ok(acc, newState)
        | x::xs ->
            match compileStatement newState x with
            | Ok(r, ns) -> inter ns xs (List.append acc r)
            | Error(e, ns) -> Error(e, ns)
    in inter state stmts []
