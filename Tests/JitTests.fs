namespace JitTests

open System.IO
open System.Text
open NUnit.Framework
open Generic
open JitCompiler

module Emit =
    let common (a : int) (b : int) (c : int) : uint32 =
        (uint32) ((a &&& 0x7 <<< 6) ||| (b &&& 0x7 <<< 3) ||| (c &&& 0x7))
    
    let conditional_move (a : int) (b : int) (c : int) : uint32 =
        UvmOpCodes.ConditionalMove <<< 28 ||| common a b c
    
    let array_index (a : int) (b : int) (c : int) : uint32 =
        UvmOpCodes.ArrayIndex <<< 28 ||| common a b c

    let array_amendment (a : int) (b : int) (c : int) : uint32 =
        UvmOpCodes.ArrayAmendment <<< 28 ||| common a b c

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

    let output (c : int) : uint32 =
        UvmOpCodes.Output <<< 28 ||| common 0 0 c

    let input (c : int) : uint32 =
        UvmOpCodes.Input <<< 28 ||| common 0 0 c

    let orthography (register : int) (value : uint32) : uint32 =
        UvmOpCodes.Orthography <<< 28 ||| (uint32) (register &&& 0x7 <<< 25) ||| (value &&& 0x01ffffffu)

[<TestFixture>]
module JitTests =
    [<Test>]
    let empty_script () =
        jit_compile [||] |> ignore
    
    [<Test>]
    let conditional_move () =
        let machine =
            jit_compile [|
                Emit.conditional_move 0 1 2
                Emit.conditional_move 3 4 5
                Emit.conditional_move 5 6 7
            |]
        machine.R1 <- 1u
        machine.R4 <- 2u
        machine.R5 <- 3u
        machine.R6 <- 4u
        machine.R7 <- 5u
        machine.Run()
        Assert.That(machine.R0, Is.EqualTo(0u))
        Assert.That(machine.R1, Is.EqualTo(1u))
        Assert.That(machine.R2, Is.EqualTo(0u))
        Assert.That(machine.R3, Is.EqualTo(2u))
        Assert.That(machine.R4, Is.EqualTo(2u))
        Assert.That(machine.R5, Is.EqualTo(4u))
        Assert.That(machine.R6, Is.EqualTo(4u))
        Assert.That(machine.R7, Is.EqualTo(5u))
    
    [<Test>]
    let array_index () =
        let machine =
            jit_compile [|
                Emit.array_index 0 1 2
                Emit.array_index 3 4 5
                Emit.array_index 5 6 7
            |]
        machine.R1 <- 0u
        machine.R2 <- 0u
        machine.R4 <- 0u
        machine.R5 <- 1u
        machine.R6 <- 1u
        machine.R7 <- 0u
        machine.Arrays.Add [|42u; 100u|]
        machine.Arrays.Add [|0xffu|]
        machine.Run()
        Assert.That(machine.R0, Is.EqualTo(42u))
        Assert.That(machine.R1, Is.EqualTo(0u))
        Assert.That(machine.R2, Is.EqualTo(0u))
        Assert.That(machine.R3, Is.EqualTo(100u))
        Assert.That(machine.R4, Is.EqualTo(0u))
        Assert.That(machine.R5, Is.EqualTo(0xffu))
        Assert.That(machine.R6, Is.EqualTo(1u))
        Assert.That(machine.R7, Is.EqualTo(0u))

    [<Test>]
    let array_amendment () =
        let machine =
            jit_compile [|
                Emit.array_amendment 0 1 2
                Emit.array_amendment 3 4 5
                Emit.array_amendment 5 6 7
            |]
        machine.R0 <- 0u
        machine.R1 <- 0u
        machine.R2 <- 42u
        machine.R3 <- 0u
        machine.R4 <- 1u
        machine.R5 <- 1u
        machine.R6 <- 0u
        machine.R7 <- 100u
        machine.Arrays.Add [|0u; 0u|]
        machine.Arrays.Add [|0u|]
        machine.Run()
        Assert.That(machine.Arrays.[0], Is.EquivalentTo([|42u; 1u|]))
        Assert.That(machine.Arrays.[1], Is.EquivalentTo([|100u|]))

    [<Test>]
    let addition () =
        let machine =
            jit_compile [|
                Emit.addition 0 1 2
                Emit.addition 3 4 5
                Emit.addition 5 6 7
            |]
        machine.R1 <- 0u
        machine.R2 <- 1u
        machine.R4 <- 2u
        machine.R5 <- 3u
        machine.R6 <- 0xffffffffu
        machine.R7 <- 5u
        machine.Run()
        Assert.That(machine.R0, Is.EqualTo(1u))
        Assert.That(machine.R1, Is.EqualTo(0u))
        Assert.That(machine.R2, Is.EqualTo(1u))
        Assert.That(machine.R3, Is.EqualTo(5u))
        Assert.That(machine.R4, Is.EqualTo(2u))
        Assert.That(machine.R5, Is.EqualTo(4u))
        Assert.That(machine.R6, Is.EqualTo(0xffffffffu))
        Assert.That(machine.R7, Is.EqualTo(5u))

    [<Test>]
    let multiplication () =
        let machine =
            jit_compile [|
                Emit.multiplication 0 1 2
                Emit.multiplication 3 4 5
                Emit.multiplication 5 6 7
            |]
        machine.R1 <- 0u
        machine.R2 <- 1u
        machine.R4 <- 2u
        machine.R5 <- 3u
        machine.R6 <- 0xffffffffu
        machine.R7 <- 5u
        machine.Run()
        Assert.That(machine.R0, Is.EqualTo(0u))
        Assert.That(machine.R1, Is.EqualTo(0u))
        Assert.That(machine.R2, Is.EqualTo(1u))
        Assert.That(machine.R3, Is.EqualTo(6u))
        Assert.That(machine.R4, Is.EqualTo(2u))
        Assert.That(machine.R5, Is.EqualTo(0xfffffffbu))
        Assert.That(machine.R6, Is.EqualTo(0xffffffffu))
        Assert.That(machine.R7, Is.EqualTo(5u))
    
    [<Test>]
    let division () =
        let machine =
            jit_compile [|
                Emit.division 0 1 2
                Emit.division 3 4 5
                Emit.division 5 6 7
            |]
        machine.R1 <- 0u
        machine.R2 <- 1u
        machine.R4 <- 2u
        machine.R5 <- 3u
        machine.R6 <- 0xffffffffu
        machine.R7 <- 5u
        machine.Run()
        Assert.That(machine.R0, Is.EqualTo(0u))
        Assert.That(machine.R1, Is.EqualTo(0u))
        Assert.That(machine.R2, Is.EqualTo(1u))
        Assert.That(machine.R3, Is.EqualTo(0u))
        Assert.That(machine.R4, Is.EqualTo(2u))
        Assert.That(machine.R5, Is.EqualTo(0x33333333u))
        Assert.That(machine.R6, Is.EqualTo(0xffffffffu))
        Assert.That(machine.R7, Is.EqualTo(5u))

    [<Test>]
    let not_and () =
        let machine =
            jit_compile [|
                Emit.not_and 0 1 2
                Emit.not_and 3 4 5
                Emit.not_and 5 6 7
            |]
        machine.R1 <- 0u
        machine.R2 <- 0xffffffffu
        machine.R4 <- 0xffffffffu
        machine.R5 <- 0xffffffffu
        machine.R6 <- 0xffffffffu
        machine.R7 <- 0xaaaaaaaau
        machine.Run()
        Assert.That(machine.R0, Is.EqualTo(0xffffffffu))
        Assert.That(machine.R1, Is.EqualTo(0u))
        Assert.That(machine.R2, Is.EqualTo(0xffffffffu))
        Assert.That(machine.R3, Is.EqualTo(0u))
        Assert.That(machine.R4, Is.EqualTo(0xffffffffu))
        Assert.That(machine.R5, Is.EqualTo(0x55555555u))
        Assert.That(machine.R6, Is.EqualTo(0xffffffffu))
        Assert.That(machine.R7, Is.EqualTo(0xaaaaaaaau))

    [<Test>]
    let halt () =
        let machine =
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
        machine.Run()
        Assert.That(machine.R0, Is.EqualTo(42u))
        Assert.That(machine.R1, Is.EqualTo(42u))
        Assert.That(machine.R2, Is.EqualTo(42u))
        Assert.That(machine.R3, Is.EqualTo(42u))
        Assert.That(machine.R4, Is.EqualTo(0u))
        Assert.That(machine.R5, Is.EqualTo(0u))
        Assert.That(machine.R6, Is.EqualTo(0u))
        Assert.That(machine.R7, Is.EqualTo(0u))

    [<Test>]
    let orthography () =
        let machine =
            jit_compile [|
                Emit.orthography 0 0x00u
                Emit.orthography 1 0x01u
                Emit.orthography 2 0x01fu
                Emit.orthography 3 0x01ffu
                Emit.orthography 4 0x01fffu
                Emit.orthography 5 0x01ffffu
                Emit.orthography 6 0x01fffffu
                Emit.orthography 7 0x01ffffffu
            |]
        machine.R0 <- 42u
        machine.Run()
        Assert.That(machine.R0, Is.EqualTo(0x00u))
        Assert.That(machine.R1, Is.EqualTo(0x01u))
        Assert.That(machine.R2, Is.EqualTo(0x01fu))
        Assert.That(machine.R3, Is.EqualTo(0x01ffu))
        Assert.That(machine.R4, Is.EqualTo(0x01fffu))
        Assert.That(machine.R5, Is.EqualTo(0x01ffffu))
        Assert.That(machine.R6, Is.EqualTo(0x01fffffu))
        Assert.That(machine.R7, Is.EqualTo(0x01ffffffu))

    [<Test>]
    let output () =
        let machine =
            jit_compile [|
                Emit.output 0
                Emit.output 1
                Emit.output 2
                Emit.output 3
                Emit.output 4
                Emit.output 5
                Emit.output 6
                Emit.output 7
            |]
        machine.R0 <- (uint32) 'a'
        machine.R1 <- (uint32) 'b'
        machine.R2 <- (uint32) 'c'
        machine.R3 <- (uint32) 'd'
        machine.R4 <- (uint32) 'e'
        machine.R5 <- (uint32) 'f'
        machine.R6 <- (uint32) 'g'
        machine.R7 <- (uint32) 'h'
        use out_stream = new MemoryStream()
        machine.SetIOStreams null out_stream
        machine.Run()
        Assert.That(Encoding.UTF8.GetString(out_stream.GetBuffer(), 0, 8), Is.EqualTo("abcdefgh"))

    [<Test>]
    let input () =
        let machine =
            jit_compile [|
                Emit.input 0
                Emit.input 1
                Emit.input 2
                Emit.input 3
                Emit.input 4
                Emit.input 5
                Emit.input 6
                Emit.input 7
            |]
        use in_stream = new MemoryStream()
        in_stream.Write(Encoding.UTF8.GetBytes("abcdefgh"), 0, 8)
        in_stream.Position <- 0L
        machine.SetIOStreams in_stream null
        machine.Run()
        Assert.That(machine.R0, Is.EqualTo((uint32) 'a'))
        Assert.That(machine.R1, Is.EqualTo((uint32) 'b'))
        Assert.That(machine.R2, Is.EqualTo((uint32) 'c'))
        Assert.That(machine.R3, Is.EqualTo((uint32) 'd'))
        Assert.That(machine.R4, Is.EqualTo((uint32) 'e'))
        Assert.That(machine.R5, Is.EqualTo((uint32) 'f'))
        Assert.That(machine.R6, Is.EqualTo((uint32) 'g'))
        Assert.That(machine.R7, Is.EqualTo((uint32) 'h'))
