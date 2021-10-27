module Assembly

type RegisterNumber = int

type Immediate = int

type Instruction =
      MOV_A_Imm of Immediate
    | ADD_A_Imm of Immediate
    | MOV_A_B
    | MOV_A_In
    | MOV_B_Imm of Immediate
    | ADD_B_Imm of Immediate
    | MOV_B_A
    | MOV_B_IN
    | MOV_OUT_Imm of Immediate
    | MOV_OUT_B
    | MOV_B_GPR of RegisterNumber
    | MOV_GPR_B of RegisterNumber
    | SUB_A_B
    | MULT_A_B
    | JNC_Imm of Immediate
    | JMP_Imm of Immediate
