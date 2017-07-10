open System.IO
open System.Collections.Generic
open System

let read_script filename =
    use stream = File.OpenRead filename
    use reader = new BinaryReader(stream)
    let count = (int32) stream.Length / 4
    let result = Array.zeroCreate count
    let mutable i = 0
    while (i < count) do
        result.[i] <- BitConverter.ToUInt32(reader.ReadBytes(4) |> Array.rev, 0)
        i <- i + 1
    result

module OpCodes =
    [<Literal>]
    let ConditionalMove = 0u
    [<Literal>]
    let ArrayIndex = 1u
    [<Literal>]
    let ArrayAmendment = 2u
    [<Literal>]
    let Addition = 3u
    [<Literal>]
    let Multiplication = 4u
    [<Literal>]
    let Division = 5u
    [<Literal>]
    let NotAnd = 6u
    [<Literal>]
    let Halt = 7u
    [<Literal>]
    let Allocation = 8u
    [<Literal>]
    let Abandonment = 9u
    [<Literal>]
    let Output = 10u
    [<Literal>]
    let Input = 11u
    [<Literal>]
    let LoadProgram = 12u
    [<Literal>]
    let Orthography = 13u

type Machine () =
    let registers : uint32 array = [|0u; 0u; 0u; 0u; 0u; 0u; 0u; 0u|]
    let arrays : List<uint32 array> = new List<uint32 array>()
    let mutable pointer = 0u
    let mutable program = [||] // a copy of array 0 for faster access
    let _in = Console.OpenStandardInput()
    let out = Console.OpenStandardOutput()

    member m.Registers = registers
    member m.Arrays = arrays
    member m.Pointer
        with get () = pointer
        and set (value) = pointer <- value
    member m.NextInstr () =
        let instr = program.[(int32) pointer]
        pointer <- pointer + 1u
        instr
    member m.In = _in
    member m.Out = out
    
    member m.LoadProgram new_program =
        let program_copy = Array.copy new_program
        match arrays.Count with
        | 0 -> arrays.Add program_copy
        | _ -> arrays.[0] <- program_copy
        program <- arrays.[0]
        ()

let step (machine : Machine) : bool =
    let instr = machine.NextInstr()
    let opcode = instr &&& 0xf0000000u >>> 28
    let a = (int32) (instr &&& 0b0000_0000_0000_0000_0000_0001_1100_0000u >>> 6)
    let b = (int32) (instr &&& 0b0000_0000_0000_0000_0000_0000_0011_1000u >>> 3)
    let c = (int32) (instr &&& 0b0000_0000_0000_0000_0000_0000_0000_0111u)
    match opcode with
    | OpCodes.ConditionalMove ->
        match machine.Registers.[c] with
        | 0u -> ()
        | _ -> machine.Registers.[a] <- machine.Registers.[b]
        true
    | OpCodes.ArrayIndex ->
        machine.Registers.[a] <- machine.Arrays.[(int32) machine.Registers.[b]].[(int32) machine.Registers.[c]]
        true
    | OpCodes.ArrayAmendment ->
        machine.Arrays.[(int32) machine.Registers.[a]].[(int32) machine.Registers.[b]] <- machine.Registers.[c]
        true
    | OpCodes.Addition ->
        machine.Registers.[a] <- machine.Registers.[b] + machine.Registers.[c]
        true
    | OpCodes.Multiplication ->
        machine.Registers.[a] <- machine.Registers.[b] * machine.Registers.[c]
        true
    | OpCodes.Division ->
        machine.Registers.[a] <- machine.Registers.[b] / machine.Registers.[c]
        true
    | OpCodes.NotAnd ->
        machine.Registers.[a] <- ~~~ (machine.Registers.[b] &&& machine.Registers.[c])
        true
    | OpCodes.Halt ->
        false
    | OpCodes.Allocation ->
        let array = Array.zeroCreate ((int32) machine.Registers.[c])
        machine.Arrays.Add(array)
        machine.Registers.[b] <- (uint32) (machine.Arrays.Count - 1)
        true
    | OpCodes.Abandonment ->
        machine.Arrays.[(int32) machine.Registers.[c]] <- null
        true
    | OpCodes.Output ->
        machine.Out.WriteByte((byte) machine.Registers.[c])
        true
    | OpCodes.Input ->
        //let key = Console.Read()
        //match (char) key with
        //| '\r' -> ()
        //| '\n' -> machine.Registers.[c] <- 0xffu
        //| _ -> machine.Registers.[c] <- (uint32) (key &&& 0xff)
        let key = machine.In.ReadByte()
        match key with
        | 0x00 -> machine.Registers.[c] <- 0xffu
        | _ -> machine.Registers.[c] <- (uint32) ((byte) key)
        true
    | OpCodes.LoadProgram ->
        match (int32) machine.Registers.[b] with
        | 0 -> ()
        | id -> machine.LoadProgram machine.Arrays.[id]
        machine.Pointer <- machine.Registers.[c]
        true
    | OpCodes.Orthography ->
        let reg = (int32) (instr &&& 0b0000_1110_0000_0000_0000_0000_0000_0000u >>> 25)
        let value = instr &&& 0b0000_0001_1111_1111_1111_1111_1111_1111u
        machine.Registers.[reg] <- value
        true
    | x -> invalidOp("Invalid op code: " + x.ToString())

let rec run machine : unit =
    match step machine with
    | true -> run machine
    | false -> ()

[<EntryPoint>]
let main argv = 
    let script = read_script argv.[0]
    let machine = Machine()
    machine.LoadProgram script
    run machine
    0
