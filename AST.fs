module AST

type Expression =
    | ImmExp of Assembly.Immediate
    | AddExp of Expression * Expression
    | SubExp of Expression * Expression
    | MulExp of Expression * Expression

type Storage =
    | Variable of string
    | InPort
    | OutPort

type Statement =
    | Assignment of Storage * Expression
    | VarDeclaration of string
    | Conditional of Expression * List<Statement> * List<Statement> option
