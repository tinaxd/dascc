// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    let exp =
        AST.SubExp(
            AST.ImmExp 1,
            AST.SubExp(
                AST.ImmExp 2,
                AST.ImmExp 3
            )
        ) in
    let state = Compiler.newState in
    //ignore (Compiler.declareVar state "foo")
    let result = Compiler.compileAssignment state (AST.OutPort) exp
    match result with
    | Some(lst) -> printf "%A\n"lst
    | None -> printf "None\n"
    0 // return an integer exit code
