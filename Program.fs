// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
module DasCC

open System

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    let source = System.IO.File.ReadAllText("program.das") in
    let stmts = Parser.program source in
    let initState = Compiler.newState in
    //ignore (Compiler.declareVar state "foo")
    match stmts with
    | Parser.No(e, _) ->
        printfn "%A" e
        1
    | Parser.Yes(stmts, _) ->
        let result = Compiler.compileStatements initState stmts in
        match result with
        | Ok(lst, _) ->
            printfn "Compilation successful."
            List.iter (fun x -> printfn "%A" x) lst
        | Error(e) -> printf "%A\n" e
        0 // return an integer exit code
