module JitCompiler

open System
open System.Collections.Generic
open System.IO
open System.Reflection.Emit
open Generic

type JitCompiledMachineData () =
    [<DefaultValue>]
    val mutable r0 : uint32
    [<DefaultValue>]
    val mutable r1 : uint32
    [<DefaultValue>]
    val mutable r2 : uint32
    [<DefaultValue>]
    val mutable r3 : uint32
    [<DefaultValue>]
    val mutable r4 : uint32
    [<DefaultValue>]
    val mutable r5 : uint32
    [<DefaultValue>]
    val mutable r6 : uint32
    [<DefaultValue>]
    val mutable r7 : uint32
    [<DefaultValue>]
    val mutable _in : Stream
    [<DefaultValue>]
    val mutable out : Stream
    [<DefaultValue>]
    val mutable arrays : System.Collections.Generic.List<uint32 array>
    [<DefaultValue>]
    val mutable pc : uint32

    member this.script : uint32 array =
        this.arrays.[0]

let new_machine_data () : JitCompiledMachineData =
    let data = new JitCompiledMachineData()
    data.arrays <- new System.Collections.Generic.List<uint32 array>()
    data.arrays.Add(null) // zero array is special and is stored in a script field
    data

module TypeInfo =
    module Machine =
        let Type = typeof<JitCompiledMachineData>
        let Registers =
            [0..7]
            |> List.map (fun r -> Type.GetField("r" + r.ToString()))
            |> List.toArray
        let In = Type.GetField("_in")
        let Out = Type.GetField("out")
        let Arrays = Type.GetField("arrays")
        let PC = Type.GetField("pc")

    module UInt32 =
        let Type = typeof<uint32>

    module UInt32Array =
        let Type = typeof<uint32[]>
        let Clone = Type.GetMethod("Clone")

    module ArraysList =
        let Type = typeof<System.Collections.Generic.List<uint32[] array>>
        let GetItem = Type.GetMethod("get_Item")
        let SetItem = Type.GetMethod("set_Item")
        let Add = Type.GetMethod("Add")
        let Count = Type.GetMethod("get_Count")

    module Stream =
        let Type = typeof<Stream>
        let ReadByte = Type.GetMethod("ReadByte")
        let WriteByte = Type.GetMethod("WriteByte")

let compile (il : ILGenerator) (instr : uint32) : unit =
    let opcode = instr &&& 0xf0000000u >>> 28
    let a = (int32) (instr &&& 0b0000_0000_0000_0000_0000_0001_1100_0000u >>> 6)
    let b = (int32) (instr &&& 0b0000_0000_0000_0000_0000_0000_0011_1000u >>> 3)
    let c = (int32) (instr &&& 0b0000_0000_0000_0000_0000_0000_0000_0111u)
    match opcode with
    | UvmOpCodes.ConditionalMove ->
        let label = il.DefineLabel()
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Registers.[c])
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Cgt_Un)
        il.Emit(OpCodes.Brfalse_S, label)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Registers.[b])
        il.Emit(OpCodes.Stfld, TypeInfo.Machine.Registers.[a])
        il.MarkLabel(label)
    | UvmOpCodes.ArrayIndex ->
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Arrays)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Registers.[b])
        il.EmitCall(OpCodes.Callvirt, TypeInfo.ArraysList.GetItem, null)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Registers.[c])
        il.Emit(OpCodes.Ldelem_U4)
        il.Emit(OpCodes.Stfld, TypeInfo.Machine.Registers.[a])
    | UvmOpCodes.ArrayAmendment ->
        let label = il.DefineLabel()
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Registers.[a])
        il.Emit(OpCodes.Cgt_Un)
        il.Emit(OpCodes.Brfalse_S, label)
        il.ThrowException(typeof<NotSupportedException>)
        il.MarkLabel(label)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Arrays)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Registers.[a])
        il.EmitCall(OpCodes.Callvirt, TypeInfo.ArraysList.GetItem, null)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Registers.[b])
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Registers.[c])
        il.Emit(OpCodes.Stelem_I4)
    | UvmOpCodes.Addition ->
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Registers.[b])
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Registers.[c])
        il.Emit(OpCodes.Add)
        il.Emit(OpCodes.Stfld, TypeInfo.Machine.Registers.[a])
    | UvmOpCodes.Multiplication ->
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Registers.[b])
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Registers.[c])
        il.Emit(OpCodes.Mul)
        il.Emit(OpCodes.Stfld, TypeInfo.Machine.Registers.[a])
    | UvmOpCodes.Division ->
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Registers.[b])
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Registers.[c])
        il.Emit(OpCodes.Div_Un)
        il.Emit(OpCodes.Stfld, TypeInfo.Machine.Registers.[a])
    | UvmOpCodes.NotAnd ->
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Registers.[b])
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Registers.[c])
        il.Emit(OpCodes.And)
        il.Emit(OpCodes.Not)
        il.Emit(OpCodes.Stfld, TypeInfo.Machine.Registers.[a])
    | UvmOpCodes.Halt ->
        il.Emit(OpCodes.Ldc_I4, (int) 0xffffffff)
        il.Emit(OpCodes.Ret)
    | UvmOpCodes.Allocation ->
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Arrays)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Registers.[c])
        il.Emit(OpCodes.Newarr, TypeInfo.UInt32.Type)
        il.EmitCall(OpCodes.Callvirt, TypeInfo.ArraysList.Add, null)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Arrays)
        il.EmitCall(OpCodes.Callvirt, TypeInfo.ArraysList.Count, null)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Sub)
        il.Emit(OpCodes.Stfld, TypeInfo.Machine.Registers.[b])
    | UvmOpCodes.Abandonment ->
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Arrays)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Registers.[c])
        il.Emit(OpCodes.Ldnull)
        il.EmitCall(OpCodes.Callvirt, TypeInfo.ArraysList.SetItem, null)
    | UvmOpCodes.Output ->
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Out)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Registers.[c])
        il.Emit(OpCodes.Conv_U1)
        il.EmitCall(OpCodes.Callvirt, TypeInfo.Stream.WriteByte, null)
    | UvmOpCodes.Input ->
        let _else = il.DefineLabel()
        let _end = il.DefineLabel()
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.In)
        il.EmitCall(OpCodes.Callvirt, TypeInfo.Stream.ReadByte, null)
        il.Emit(OpCodes.Dup)
        // if key == 0
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Ceq)
        il.Emit(OpCodes.Brfalse_S, _else)
        // then key = 0xff
        il.Emit(OpCodes.Pop)
        il.Emit(OpCodes.Ldc_I4, (int32) 0xff)
        il.Emit(OpCodes.Br_S, _end)
        // else cast to byte
        il.MarkLabel(_else)        
        il.Emit(OpCodes.Conv_U)
        // store
        il.MarkLabel(_end)        
        il.Emit(OpCodes.Stfld, TypeInfo.Machine.Registers.[c])
    | UvmOpCodes.LoadProgram ->
        let label = il.DefineLabel()
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Registers.[b])
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Cgt_Un)
        il.Emit(OpCodes.Brfalse_S, label)
        // replace the code array (0)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Arrays)
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Arrays)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Registers.[b])
        il.EmitCall(OpCodes.Callvirt, TypeInfo.ArraysList.GetItem, null)
        il.EmitCall(OpCodes.Callvirt, TypeInfo.UInt32Array.Clone, null)
        il.EmitCall(OpCodes.Callvirt, TypeInfo.ArraysList.SetItem, null)
        // return the new PC
        il.MarkLabel(label)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, TypeInfo.Machine.Registers.[c])
        il.Emit(OpCodes.Ret)
    | UvmOpCodes.Orthography ->
        let reg = (int32) (instr &&& 0b0000_1110_0000_0000_0000_0000_0000_0000u >>> 25)
        let value = instr &&& 0b0000_0001_1111_1111_1111_1111_1111_1111u
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldc_I4, (int32) value)
        il.Emit(OpCodes.Stfld, TypeInfo.Machine.Registers.[reg])
    | x -> invalidOp("Invalid op code: " + x.ToString())
    #if DEBUG
    il.Emit(OpCodes.Ldarg_0)
    il.Emit(OpCodes.Ldarg_0)
    il.Emit(OpCodes.Ldfld, TypeInfo.Machine.PC)
    il.Emit(OpCodes.Ldc_I4_1)
    il.Emit(OpCodes.Add)
    il.Emit(OpCodes.Stfld, TypeInfo.Machine.PC)
    #endif

let jit_compile (pc : uint32) (script : uint32 array) : Func<JitCompiledMachineData, uint32> =
    let method = new DynamicMethod("run_UVM_" + pc.ToString("x8"), typeof<uint32>, [|typeof<JitCompiledMachineData>|], typeof<JitCompiledMachineData>)
    let il = method.GetILGenerator()
    let mutable do_compile = true
    let mutable count = 0
    let max_count = 256
    while do_compile do
        let instr = script.[(int) pc + count]
        compile il instr
        count <- count + 1
        if count > max_count || (int) pc + count = script.Length || instr >>> 28 = UvmOpCodes.LoadProgram then
            do_compile <- false
    il.Emit(OpCodes.Ldc_I4, (int) pc + count)
    il.Emit(OpCodes.Ret)
    method.CreateDelegate(typeof<Func<JitCompiledMachineData, uint32>>) :?> Func<JitCompiledMachineData, uint32>

type JitCompiledMachine () =
    let data : JitCompiledMachineData = new_machine_data()

    let print_state (script : uint32 array) : string =
        let listing =
            script
            |> Seq.skip ((int) data.pc - 5)
            |> Seq.take 11
            |> Seq.toArray
            |> ByteCodePrinter.print_all
        let print_listing to_skip to_take point =
            listing
            |> Seq.indexed
            |> Seq.skip to_skip
            |> Seq.take to_take
            |> Seq.map (fun (i, s) -> sprintf " %s %04x %s" (if point then "->" else "  ") i s)
            |> fun lines -> String.Join("\n", lines) + "\n"
        let print_arrays =
            data.arrays
            |> Seq.map (fun a -> if a = null then "<null>" else a.Length.ToString())
            |> fun arrays -> String.Join(", ", arrays)
        sprintf "Data:\n" +
        sprintf "r0 = 0x%08x; r1 = 0x%08x; r2 = 0x%08x; r3 = 0x%08x;\n" data.r0 data.r1 data.r2 data.r3 +
        sprintf "r4 = 0x%08x; r5 = 0x%08x; r6 = 0x%08x; r7 = 0x%08x;\n" data.r4 data.r5 data.r6 data.r7 +
        sprintf "arr = [%s]; pc = 0x%04x;\n" print_arrays data.pc +
        sprintf "Code:\n" +
        print_listing 0 5 false +
        print_listing 5 1 true +
        print_listing 6 5 false

    let run (script : uint32 array) : unit =
        data.arrays.[0] <- script
        let mutable current_script = null
        let mutable code_cache : Func<JitCompiledMachineData, uint32> array = null
        let mutable pc : uint32 = 0u
        while (pc <> 0xffffffffu) do
            if not (Object.ReferenceEquals(current_script, data.script)) then
                current_script <- data.script
                code_cache <-
                    current_script
                        |> Array.map (fun _ -> null)
            let mutable code = code_cache.[(int) pc]
            if Object.ReferenceEquals(code, null) then
                code <- jit_compile pc current_script
                code_cache.[(int) pc] <- code
            #if DEBUG
            try
            #endif
            pc <- (code).Invoke(data)
            #if DEBUG
            with
            | e ->
                let message =
                    print_state current_script +
                    sprintf "Exception:\n" +
                    e.Message
                raise (exn(message, e))
            #endif
            data.pc <- pc

    interface IMachine with
        member this.R0
            with get () = data.r0
            and set value = data.r0 <- value

        member this.R1
            with get () = data.r1
            and set value = data.r1 <- value

        member this.R2
            with get () = data.r2
            and set value = data.r2 <- value

        member this.R3
            with get () = data.r3
            and set value = data.r3 <- value

        member this.R4
            with get () = data.r4
            and set value = data.r4 <- value

        member this.R5
            with get () = data.r5
            and set value = data.r5 <- value

        member this.R6
            with get () = data.r6
            and set value = data.r6 <- value

        member this.R7
            with get () = data.r7
            and set value = data.r7 <- value

        member this.Arrays
            with get () = data.arrays

        member this.SetIOStreams _in out =
            data._in <- _in
            data.out <- out

        member this.Run (script : uint32 array) =
            run script
