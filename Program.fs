// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
module DasCC

open System

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    let exp =
        AST.AddExp(
            AST.ImmExp 4,
            AST.SubExp(
                AST.ImmExp 2,
                AST.ImmExp 3
            )
        ) in
    let state = Compiler.newState in
    //ignore (Compiler.declareVar state "foo")
    let result = Compiler.compileAssignment state (AST.OutPort) exp
    match result with
    | Ok(lst) -> printf "%A\n" lst
    | Error(e) -> printf "%A\n" e
    0 // return an integer exit code
