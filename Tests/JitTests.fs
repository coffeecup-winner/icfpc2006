namespace JitTests

open System
open NUnit.Framework
open JitCompiler

module Emit =
    let orthography (register : int) (value : uint32) : uint32 =
        UvmOpCodes.Orthography <<< 28 ||| (uint32) (register &&& 0x7 <<< 25) ||| (value &&& 0x01ffffffu)
    
    let common (a : int) (b : int) (c : int) : uint32 =
        (uint32) ((a &&& 0x7 <<< 6) ||| (b &&& 0x7 <<< 3) ||| (c &&& 0x7))
    
    let conditional_move (a : int) (b : int) (c : int) : uint32 =
        UvmOpCodes.ConditionalMove <<< 28 ||| common a b c
    
    let addition (a : int) (b : int) (c : int) : uint32 =
        UvmOpCodes.Addition <<< 28 ||| common a b c

    let multiplication (a : int) (b : int) (c : int) : uint32 =
        UvmOpCodes.Multiplication <<< 28 ||| common a b c

    let division (a : int) (b : int) (c : int) : uint32 =
        UvmOpCodes.Division <<< 28 ||| common a b c

    let not_and (a : int) (b : int) (c : int) : uint32 =
        UvmOpCodes.NotAnd <<< 28 ||| common a b c

    let halt : uint32 =
        UvmOpCodes.Halt <<< 28

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
    let conditional_move () =
        let _type =
            jit_compile [|
                Emit.conditional_move 0 1 2
                Emit.conditional_move 3 4 5
                Emit.conditional_move 5 6 7
            |]
        let machine =
            run _type [
                "r1", 1u
                "r4", 2u
                "r5", 3u
                "r6", 4u
                "r7", 5u
            ]
        assert_register _type machine 0 0u
        assert_register _type machine 1 1u
        assert_register _type machine 2 0u
        assert_register _type machine 3 2u
        assert_register _type machine 4 2u
        assert_register _type machine 5 4u
        assert_register _type machine 6 4u
        assert_register _type machine 7 5u

    [<Test>]
    let addition () =
        let _type =
            jit_compile [|
                Emit.addition 0 1 2
                Emit.addition 3 4 5
                Emit.addition 5 6 7
            |]
        let machine =
            run _type [
                "r1", 0u
                "r2", 1u
                "r4", 2u
                "r5", 3u
                "r6", 0xffffffffu
                "r7", 5u
            ]
        assert_register _type machine 0 1u
        assert_register _type machine 1 0u
        assert_register _type machine 2 1u
        assert_register _type machine 3 5u
        assert_register _type machine 4 2u
        assert_register _type machine 5 4u
        assert_register _type machine 6 0xffffffffu
        assert_register _type machine 7 5u

    [<Test>]
    let multiplication () =
        let _type =
            jit_compile [|
                Emit.multiplication 0 1 2
                Emit.multiplication 3 4 5
                Emit.multiplication 5 6 7
            |]
        let machine =
            run _type [
                "r1", 0u
                "r2", 1u
                "r4", 2u
                "r5", 3u
                "r6", 0xffffffffu
                "r7", 5u
            ]
        assert_register _type machine 0 0u
        assert_register _type machine 1 0u
        assert_register _type machine 2 1u
        assert_register _type machine 3 6u
        assert_register _type machine 4 2u
        assert_register _type machine 5 0xfffffffbu
        assert_register _type machine 6 0xffffffffu
        assert_register _type machine 7 5u
    
    [<Test>]
    let division () =
        let _type =
            jit_compile [|
                Emit.division 0 1 2
                Emit.division 3 4 5
                Emit.division 5 6 7
            |]
        let machine =
            run _type [
                "r1", 0u
                "r2", 1u
                "r4", 2u
                "r5", 3u
                "r6", 0xffffffffu
                "r7", 5u
            ]
        assert_register _type machine 0 0u
        assert_register _type machine 1 0u
        assert_register _type machine 2 1u
        assert_register _type machine 3 0u
        assert_register _type machine 4 2u
        assert_register _type machine 5 0x33333333u
        assert_register _type machine 6 0xffffffffu
        assert_register _type machine 7 5u

    [<Test>]
    let not_and () =
        let _type =
            jit_compile [|
                Emit.not_and 0 1 2
                Emit.not_and 3 4 5
                Emit.not_and 5 6 7
            |]
        let machine =
            run _type [
                "r1", 0u
                "r2", 0xffffffffu
                "r4", 0xffffffffu
                "r5", 0xffffffffu
                "r6", 0xffffffffu
                "r7", 0xaaaaaaaau
            ]
        assert_register _type machine 0 0xffffffffu
        assert_register _type machine 1 0u
        assert_register _type machine 2 0xffffffffu
        assert_register _type machine 3 0u
        assert_register _type machine 4 0xffffffffu
        assert_register _type machine 5 0x55555555u
        assert_register _type machine 6 0xffffffffu
        assert_register _type machine 7 0xaaaaaaaau

    [<Test>]
    let halt () =
        let _type =
            jit_compile [|
                Emit.orthography 0 42u
                Emit.orthography 1 42u
                Emit.orthography 2 42u
                Emit.orthography 3 42u
                Emit.halt
                Emit.orthography 4 42u
                Emit.orthography 5 42u
                Emit.orthography 6 42u
                Emit.orthography 7 42u
            |]
        let machine =
            run _type []
        assert_register _type machine 0 42u
        assert_register _type machine 1 42u
        assert_register _type machine 2 42u
        assert_register _type machine 3 42u
        assert_register _type machine 4 0u
        assert_register _type machine 5 0u
        assert_register _type machine 6 0u
        assert_register _type machine 7 0u

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
