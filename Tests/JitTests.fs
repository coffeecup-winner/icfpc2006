namespace JitTests

open System
open NUnit.Framework
open JitCompiler

module Emit =
    let orthography (register : int) (value : uint32) : uint32 =
        UvmOpCodes.Orthography <<< 28 ||| (uint32) (register &&& 0x7 <<< 25) ||| (value &&& 0x01ffffffu)

[<TestFixture>]
module JitTests =
    let uncurry f (a, b) = f a b

    let run (_type : Type) (register_overrides : (string * uint32) list) =
        let method = _type.GetMethod("Run")
        let instance = Activator.CreateInstance(_type)
        register_overrides
        |> List.map (fun (r, v) -> (_type.GetField(r).SetValue(instance, v)))
        |> ignore
        method.Invoke(instance, [||]) |> ignore
        instance

    let assert_register (_type : Type) (machine : obj) (register : int) (value : uint32) : unit =
        Assert.That(_type.GetField("r" + register.ToString()).GetValue(machine), Is.EqualTo(value))

    [<Test>]
    let empty_script () =
        jit_compile [||] |> ignore

    [<Test>]
    let orthography () =
        let register_data = [
            0, 0x00u
            1, 0x01u
            2, 0x01fu
            3, 0x01ffu
            4, 0x01fffu
            5, 0x01ffffu
            6, 0x01fffffu
            7, 0x01ffffffu
        ]
        let script =
            register_data
            |> List.map (uncurry Emit.orthography)
            |> List.toArray
        let _type = jit_compile script
        let machine = run _type ["r0", 42u]
        register_data
        |> List.map (uncurry (assert_register _type machine))
        |> ignore
