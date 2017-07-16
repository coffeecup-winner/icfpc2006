module ByteCodePrinter

let print (instr : uint32) : string =
    let opcode = instr &&& 0xf0000000u >>> 28
    let a = (int32) (instr &&& 0b0000_0000_0000_0000_0000_0001_1100_0000u >>> 6)
    let b = (int32) (instr &&& 0b0000_0000_0000_0000_0000_0000_0011_1000u >>> 3)
    let c = (int32) (instr &&& 0b0000_0000_0000_0000_0000_0000_0000_0111u)
    match opcode with
    | UvmOpCodes.ConditionalMove ->
        sprintf "if (r%d) r%d := r%d" c a b
    | UvmOpCodes.ArrayIndex ->
        sprintf "r%d := arr[r%d][r%d]" a b c
    | UvmOpCodes.ArrayAmendment ->
        sprintf "arr[r%d][r%d] := r%d" a b c
    | UvmOpCodes.Addition ->
        sprintf "r%d := r%d + r%d" a b c
    | UvmOpCodes.Multiplication ->
            sprintf "r%d := r%d * r%d" a b c
    | UvmOpCodes.Division ->
        sprintf "r%d := r%d / r%d" a b c
    | UvmOpCodes.NotAnd ->
        sprintf "r%d := ~(r%d & r%d)" a b c
    | UvmOpCodes.Halt ->
        sprintf "halt"
    | UvmOpCodes.Allocation ->
        sprintf "r%d := new [r%d]" b c
    | UvmOpCodes.Abandonment ->
        sprintf "delete [] r%d" c
    | UvmOpCodes.Output ->
        sprintf "write(r%d)" c
    | UvmOpCodes.Input ->
        sprintf "r%d := read()" c
    | UvmOpCodes.LoadProgram ->
        sprintf "load r%d:r%d" b c
    | UvmOpCodes.Orthography ->
        let reg = (int32) (instr &&& 0b0000_1110_0000_0000_0000_0000_0000_0000u >>> 25)
        let value = instr &&& 0b0000_0001_1111_1111_1111_1111_1111_1111u
        sprintf "r%d := 0x%08x" reg value
    | x ->
        sprintf "invalid 0x%08x" instr

let print_all (script : uint32 array) : string array =
    Array.map print script
