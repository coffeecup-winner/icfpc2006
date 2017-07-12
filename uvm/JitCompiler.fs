module JitCompiler

open System
open System.Reflection
open System.Reflection.Emit
open Generic

[<AbstractClass>]
type JitCompiledMachine () =
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

    abstract member Run : unit -> unit

    interface IMachine with
        member this.R0
            with get () = this.r0
            and set value = this.r0 <- value

        member this.R1
            with get () = this.r1
            and set value = this.r1 <- value

        member this.R2
            with get () = this.r2
            and set value = this.r2 <- value

        member this.R3
            with get () = this.r3
            and set value = this.r3 <- value

        member this.R4
            with get () = this.r4
            and set value = this.r4 <- value

        member this.R5
            with get () = this.r5
            and set value = this.r5 <- value

        member this.R6
            with get () = this.r6
            and set value = this.r6 <- value

        member this.R7
            with get () = this.r7
            and set value = this.r7 <- value

        member this.Run () =
            this.Run()

let compile (il : ILGenerator) (registers : FieldInfo array) (instr : uint32) : unit =
    let opcode = instr &&& 0xf0000000u >>> 28
    let a = (int32) (instr &&& 0b0000_0000_0000_0000_0000_0001_1100_0000u >>> 6)
    let b = (int32) (instr &&& 0b0000_0000_0000_0000_0000_0000_0011_1000u >>> 3)
    let c = (int32) (instr &&& 0b0000_0000_0000_0000_0000_0000_0000_0111u)
    match opcode with
    | UvmOpCodes.ConditionalMove ->
        let label = il.DefineLabel()
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, registers.[c])
        il.Emit(OpCodes.Ldc_I4_0)
        il.Emit(OpCodes.Cgt_Un)
        il.Emit(OpCodes.Brfalse_S, label)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, registers.[b])
        il.Emit(OpCodes.Stfld, registers.[a])
        il.MarkLabel(label)
    | UvmOpCodes.ArrayIndex ->
        ()
    | UvmOpCodes.ArrayAmendment ->
        ()
    | UvmOpCodes.Addition ->
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, registers.[b])
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, registers.[c])
        il.Emit(OpCodes.Add)
        il.Emit(OpCodes.Stfld, registers.[a])
    | UvmOpCodes.Multiplication ->
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, registers.[b])
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, registers.[c])
        il.Emit(OpCodes.Mul)
        il.Emit(OpCodes.Stfld, registers.[a])
    | UvmOpCodes.Division ->
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, registers.[b])
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, registers.[c])
        il.Emit(OpCodes.Div_Un)
        il.Emit(OpCodes.Stfld, registers.[a])
    | UvmOpCodes.NotAnd ->
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, registers.[b])
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, registers.[c])
        il.Emit(OpCodes.And)
        il.Emit(OpCodes.Not)
        il.Emit(OpCodes.Stfld, registers.[a])
    | UvmOpCodes.Halt ->
        il.Emit(OpCodes.Ret)
    | UvmOpCodes.Allocation ->
        ()
    | UvmOpCodes.Abandonment ->
        ()
    | UvmOpCodes.Output ->
        ()
    | UvmOpCodes.Input ->
        ()
    | UvmOpCodes.LoadProgram ->
        ()
    | UvmOpCodes.Orthography ->
        let reg = (int32) (instr &&& 0b0000_1110_0000_0000_0000_0000_0000_0000u >>> 25)
        let value = instr &&& 0b0000_0001_1111_1111_1111_1111_1111_1111u
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldc_I4, (int32) value)
        il.Emit(OpCodes.Stfld, registers.[reg])
    | x -> invalidOp("Invalid op code: " + x.ToString())

let build (type_builder : TypeBuilder) (registers : FieldInfo array) (script : uint32 array) : unit =
    let method_builder = type_builder.DefineMethod("Run", MethodAttributes.Public ||| MethodAttributes.Virtual)
    let il = method_builder.GetILGenerator()
    Array.ForEach(script, fun instr -> compile il registers instr)
    il.Emit(OpCodes.Ret)
    ()

let jit_compile(script : uint32 array) : IMachine =
    let assembly_name = new AssemblyName()
    assembly_name.Name <- "UVM"
    let domain = AppDomain.CurrentDomain
    let assembly_builder = domain.DefineDynamicAssembly(assembly_name, AssemblyBuilderAccess.RunAndSave)
    let module_builder = assembly_builder.DefineDynamicModule(assembly_name.Name, "UVM.dll")
    let type_builder = module_builder.DefineType("UVM.Machine", TypeAttributes.Public ||| TypeAttributes.Class, typeof<JitCompiledMachine>)
    let registers =
        [0..7]
        |> List.map (fun r -> typeof<JitCompiledMachine>.GetField("r" + r.ToString()))
        |> List.toArray
    build type_builder registers script
    let _type = type_builder.CreateType()
    // TODO: optionally assembly_builder.Save("UVM.dll")
    Activator.CreateInstance(_type) :?> IMachine
