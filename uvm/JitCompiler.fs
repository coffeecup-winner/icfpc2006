module JitCompiler

open System
open System.Reflection
open System.Reflection.Emit

let build (type_builder : TypeBuilder) (script : uint32 array) : unit =
    type_builder.DefineField("r0", typeof<uint32>, FieldAttributes.Private) |> ignore
    type_builder.DefineField("r1", typeof<uint32>, FieldAttributes.Private) |> ignore
    type_builder.DefineField("r2", typeof<uint32>, FieldAttributes.Private) |> ignore
    type_builder.DefineField("r3", typeof<uint32>, FieldAttributes.Private) |> ignore
    type_builder.DefineField("r4", typeof<uint32>, FieldAttributes.Private) |> ignore
    type_builder.DefineField("r5", typeof<uint32>, FieldAttributes.Private) |> ignore
    type_builder.DefineField("r6", typeof<uint32>, FieldAttributes.Private) |> ignore
    type_builder.DefineField("r7", typeof<uint32>, FieldAttributes.Private) |> ignore
    let method_builder = type_builder.DefineMethod("Run", MethodAttributes.Public)
    let il = method_builder.GetILGenerator()
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
