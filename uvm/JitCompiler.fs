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

let new_machine_data () : JitCompiledMachineData =
    let data = new JitCompiledMachineData()
    data.arrays <- new System.Collections.Generic.List<uint32 array>()
    data

module Field =
    let Registers =
        [0..7]
        |> List.map (fun r -> typeof<JitCompiledMachineData>.GetField("r" + r.ToString()))
        |> List.toArray
    let In = typeof<JitCompiledMachineData>.GetField("_in")
    let Out = typeof<JitCompiledMachineData>.GetField("out")
    let Arrays = typeof<JitCompiledMachineData>.GetField("arrays")

let compile (il : ILGenerator) (instr : uint32) : unit =
    let opcode = instr &&& 0xf0000000u >>> 28
    let a = (int32) (instr &&& 0b0000_0000_0000_0000_0000_0001_1100_0000u >>> 6)
    let b = (int32) (instr &&& 0b0000_0000_0000_0000_0000_0000_0011_1000u >>> 3)
    let c = (int32) (instr &&& 0b0000_0000_0000_0000_0000_0000_0000_0111u)
    match opcode with
    | UvmOpCodes.ConditionalMove ->
        let label = il.DefineLabel()
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, Field.Registers.[c])
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Cgt_Un)
        il.Emit(OpCodes.Brfalse_S, label)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, Field.Registers.[b])
        il.Emit(OpCodes.Stfld, Field.Registers.[a])
        il.MarkLabel(label)
    | UvmOpCodes.ArrayIndex ->
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, Field.Arrays)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, Field.Registers.[b])
        il.EmitCall(OpCodes.Callvirt, typeof<System.Collections.Generic.List<uint32[] array>>.GetMethod("get_Item"), null)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, Field.Registers.[c])
        il.Emit(OpCodes.Ldelem_U4)
        il.Emit(OpCodes.Stfld, Field.Registers.[a])
    | UvmOpCodes.ArrayAmendment ->
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, Field.Arrays)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, Field.Registers.[a])
        il.EmitCall(OpCodes.Callvirt, typeof<System.Collections.Generic.List<uint32[] array>>.GetMethod("get_Item"), null)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, Field.Registers.[b])
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, Field.Registers.[c])
        il.Emit(OpCodes.Stelem_I4)
    | UvmOpCodes.Addition ->
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, Field.Registers.[b])
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, Field.Registers.[c])
        il.Emit(OpCodes.Add)
        il.Emit(OpCodes.Stfld, Field.Registers.[a])
    | UvmOpCodes.Multiplication ->
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, Field.Registers.[b])
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, Field.Registers.[c])
        il.Emit(OpCodes.Mul)
        il.Emit(OpCodes.Stfld, Field.Registers.[a])
    | UvmOpCodes.Division ->
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, Field.Registers.[b])
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, Field.Registers.[c])
        il.Emit(OpCodes.Div_Un)
        il.Emit(OpCodes.Stfld, Field.Registers.[a])
    | UvmOpCodes.NotAnd ->
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, Field.Registers.[b])
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, Field.Registers.[c])
        il.Emit(OpCodes.And)
        il.Emit(OpCodes.Not)
        il.Emit(OpCodes.Stfld, Field.Registers.[a])
    | UvmOpCodes.Halt ->
        il.Emit(OpCodes.Ldc_I4, (int) 0xffffffff)
        il.Emit(OpCodes.Ret)
    | UvmOpCodes.Allocation ->
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, Field.Arrays)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, Field.Registers.[c])
        il.Emit(OpCodes.Newarr, typeof<uint32>)
        il.EmitCall(OpCodes.Callvirt, typeof<System.Collections.Generic.List<uint32 array>>.GetMethod("Add"), null)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, Field.Arrays)
        il.EmitCall(OpCodes.Callvirt, typeof<System.Collections.Generic.List<uint32 array>>.GetMethod("get_Count"), null)
        il.Emit(OpCodes.Ldc_I4_1)
        il.Emit(OpCodes.Sub)
        il.Emit(OpCodes.Stfld, Field.Registers.[b])
    | UvmOpCodes.Abandonment ->
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, Field.Arrays)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, Field.Registers.[c])
        il.Emit(OpCodes.Ldnull)
        il.EmitCall(OpCodes.Callvirt, typeof<System.Collections.Generic.List<uint32 array>>.GetMethod("set_Item"), null)
    | UvmOpCodes.Output ->
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, Field.Out)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, Field.Registers.[c])
        il.Emit(OpCodes.Conv_U1)
        il.EmitCall(OpCodes.Callvirt, typeof<Stream>.GetMethod("WriteByte"), null)
    | UvmOpCodes.Input ->
        let _else = il.DefineLabel()
        let _end = il.DefineLabel()
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, Field.In)
        il.EmitCall(OpCodes.Callvirt, typeof<Stream>.GetMethod("ReadByte"), null)
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
        il.Emit(OpCodes.Stfld, Field.Registers.[c])
    | UvmOpCodes.LoadProgram ->
        ()
    | UvmOpCodes.Orthography ->
        let reg = (int32) (instr &&& 0b0000_1110_0000_0000_0000_0000_0000_0000u >>> 25)
        let value = instr &&& 0b0000_0001_1111_1111_1111_1111_1111_1111u
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldc_I4, (int32) value)
        il.Emit(OpCodes.Stfld, Field.Registers.[reg])
    | x -> invalidOp("Invalid op code: " + x.ToString())

let jit_compile (pc : uint32) (script : uint32 array) : Func<JitCompiledMachineData, uint32> =
    let method = new DynamicMethod("run_UVM_" + pc.ToString("x8"), typeof<uint32>, [|typeof<JitCompiledMachineData>|], typeof<JitCompiledMachineData>)
    let il = method.GetILGenerator()
    Array.ForEach(script, fun instr -> compile il instr)
    il.Emit(OpCodes.Ldc_I4, (int) 0xffffffff)
    il.Emit(OpCodes.Ret)
    method.CreateDelegate(typeof<Func<JitCompiledMachineData, uint32>>) :?> Func<JitCompiledMachineData, uint32>

type JitCompiledMachine () =
    let data : JitCompiledMachineData = new_machine_data()

    member private this.Run (data : JitCompiledMachineData) (script : uint32 array) : unit =
        let code_cache = new Dictionary<uint32, Func<JitCompiledMachineData, uint32>>()
        let mutable pc : uint32 = 0u
        while (pc <> 0xffffffffu) do
            let code : Func<JitCompiledMachineData, uint32> ref = ref null
            if not (code_cache.TryGetValue(pc, code)) then
                code.Value <- jit_compile pc script
                code_cache.Add(pc, !code)
            pc <- (!code).Invoke(data)

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
            this.Run data script
