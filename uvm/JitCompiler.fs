module JitCompiler

open System
open System.Reflection
open System.Reflection.Emit

let compile (il : ILGenerator) (registers : FieldBuilder array) (instr : uint32) : unit =
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

let build (type_builder : TypeBuilder) (script : uint32 array) : unit =
    let registers = [|
        type_builder.DefineField("r0", typeof<uint32>, FieldAttributes.Public)
        type_builder.DefineField("r1", typeof<uint32>, FieldAttributes.Public)
        type_builder.DefineField("r2", typeof<uint32>, FieldAttributes.Public)
        type_builder.DefineField("r3", typeof<uint32>, FieldAttributes.Public)
        type_builder.DefineField("r4", typeof<uint32>, FieldAttributes.Public)
        type_builder.DefineField("r5", typeof<uint32>, FieldAttributes.Public)
        type_builder.DefineField("r6", typeof<uint32>, FieldAttributes.Public)
        type_builder.DefineField("r7", typeof<uint32>, FieldAttributes.Public)
    |]
    let method_builder = type_builder.DefineMethod("Run", MethodAttributes.Public)
    let il = method_builder.GetILGenerator()
    Array.ForEach(script, fun instr -> compile il registers instr)
    il.Emit(OpCodes.Ret)
    ()

let jit_compile(script : uint32 array) =
    let assembly_name = new AssemblyName()
    assembly_name.Name <- "UVM"
    let domain = AppDomain.CurrentDomain
    let assembly_builder = domain.DefineDynamicAssembly(assembly_name, AssemblyBuilderAccess.RunAndSave)
    let module_builder = assembly_builder.DefineDynamicModule(assembly_name.Name, "UVM.dll")
    let type_builder = module_builder.DefineType("UVM.Machine", TypeAttributes.Public ||| TypeAttributes.Class)
    build type_builder script
    let _type = type_builder.CreateType()
    // TODO: optionally assembly_builder.Save("UVM.dll")
    _type
