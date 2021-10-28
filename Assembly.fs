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

let asBin inst =
    match inst with
    | MOV_A_Imm(i) -> (0b0000 <<< 4) + i
    | ADD_A_Imm(i) -> (0b0001 <<< 4) + i
    | MOV_A_B -> (0b0010 <<< 4)
    | MOV_A_In -> (0b0011 <<< 4)
    | MOV_B_Imm(i) -> (0b0100 <<< 4) + i
    | ADD_B_Imm(i) -> (0b0101 <<< 4) + i
    | MOV_B_A -> (0b0110 <<< 4)
    | MOV_B_IN -> (0b0111 <<< 4)
    | MOV_OUT_Imm(i) -> (0b1000 <<< 4) + i
    | MOV_OUT_B -> (0b1001 <<< 4)
    | MOV_B_GPR(i) -> (0b1010 <<< 4) + i
    | MOV_GPR_B(i) -> (0b1011 <<< 4) + i
    | SUB_A_B -> (0b1100 <<< 4)
    | MULT_A_B -> (0b1101 <<< 4)
    | JNC_Imm(i) -> (0b1110 <<< 4) + i
    | JMP_Imm(i) -> (0b1111 <<< 4) + i
